# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(glue)
library(here)

# Functions ---------------------------------------------------------------
implied_mu_next <- function(mu, alpha) { 1 - alpha + (2 * alpha - 1) * mu }

mu_hat_t_plus_1_given_a_t <- function(alpha, mu_t, q, a_t, f00, f11) {

  mu1h <- p_w_t_given_s_t(w_t = 1, s_t = a_t, mu_t = mu_t, q = q)

  mu_hat_t_plus_1_given_mu_hat_t(alpha = alpha,
                                 a_t = a_t,
                                 mu1h = mu1h,
                                 f00 = f00,
                                 f11 = f11)
}

p_w_t_given_s_t <- function(w_t, s_t, mu_t, q) {

  denom <- 1 + as_sign(s_t) * (2 * mu_t - 1) * q

  case_when(w_t == 1 & s_t == 1 ~ (1 + q) * mu_t / denom,
            w_t == 0 & s_t == 1 ~ (1 - q) * (1 - mu_t) / denom,
            w_t == 1 & s_t == 0 ~ (1 - q) * mu_t / denom,
            w_t == 0 & s_t == 0 ~ (1 + q) * (1 - mu_t) / denom)
}

as_sign <- function(x) { 2*x - 1 }

mu_hat_t_plus_1_given_mu_hat_t <- function(alpha, a_t, mu1h, f00, f11) {

  f0 <- function(a) ifelse(a == 0, f00, 1)
  f1 <- function(a) ifelse(a == 1, f11, 1)

  alpha * f1(a_t) * mu1h + (1 - alpha) * f0(a_t) * (1 - mu1h)
}

plot_posterior_omega_2 <- function(tb) {
  gg <- ggplot(tb,
               aes(x = mu_0,
                   y = mu_hat_2,
                   group = a_1,
                   color = a_1)) +
    theme_bw(base_size = 13) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = expression(atop("Prior probability of crisis in initial period", mu[0])),
         y = expression(atop("Posterior probability of crisis in final period", hat(mu)[2])),
         color  = "Policy in\nmiddle period") +
    theme_bw(base_size = 13) +
    geom_line(size = 1.5)

  if (n_distinct(tb$q_lab) == 1 &
      n_distinct(tb$f_lab) == 1) {

    gg <- gg +
      theme(legend.position = "bottom",
            legend.direction = "horizontal") +
      scale_color_colorblind(guide = guide_legend(nrow = 2, byrow = TRUE))

  } else {

    gg <- gg +
      facet_grid(q_lab ~ f_lab,
                 labeller = labeller(.rows = label_value,
                                     .cols = label_parsed)) +
      scale_color_colorblind()
  }

  gg

}

plot_update <- function(tb) {
  gg <- ggplot(tb,
               aes(x = mu_0,
                   y = mu_hat_2 - mu_2,
                   group = a_1,
                   color = a_1)) +
    theme_bw(base_size = 13) +
    scale_x_continuous(limits = c(0, 1)) +
    labs(x = expression(atop("Prior probability of crisis in initial period", mu[0])),
         y = expression(atop("Update about crisis in final period", hat(mu)[2] - mu[2])),
         color  = "Policy in\nmiddle period") +
    theme_bw(base_size = 13) +
    geom_line(size = 1.5)

  if (n_distinct(tb$q_lab) == 1 &
      n_distinct(tb$f_lab) == 1) {

    gg <- gg +
      theme(legend.position = "bottom",
            legend.direction = "horizontal") +
      scale_color_colorblind(guide = guide_legend(nrow = 2, byrow = TRUE))

  } else {

    gg <- gg +
      facet_grid(q_lab ~ f_lab,
                 labeller = labeller(.rows = label_value,
                                     .cols = label_parsed)) +
      scale_color_colorblind()
  }

  gg

}

# Xwalks ------------------------------------------------------------------
policy_xwalk <- tibble(f00 = c(0.9, 0.5, 0.1),
                       f11 = c(0.9, 0.5, 0.1),
                       faith = c("Ineffective policy",
                                 "Moderately effective policy",
                                 "Very effective policy")) %>%
  mutate(f_lab = glue("atop(paste('{faith}'), f[0](0) == ~f[1](1) == {f00})"),
         f_lab = as_factor(f_lab))

q_xwalk <- tibble(q = c(0.25, 0.5, 0.75),
                  q_lab = c("Low prior credibility\n(q = 0.25)",
                            "Med. prior credibility\n(q = 0.5)",
                            "High prior credibility\n(q = 0.75)")) %>%
  mutate(q_lab = as_factor(q_lab))


# Posterior Omega 2 -------------------------------------------------------
post_omega_grid <- crossing(q = q_xwalk$q,
                            nesting(f00 = policy_xwalk$f00,
                                    f11 = policy_xwalk$f11),
                            a_1  = c(0, 1),
                            mu_0 = seq(0, 1, by = 0.02)) %>%
  mutate(mu_1 = implied_mu_next(mu_0, 0.9),
         mu_2 = implied_mu_next(mu_1, 0.9),
         mu_hat_2 = mu_hat_t_plus_1_given_a_t(alpha = 0.9,
                                              mu_t = mu_1,
                                              q = q,
                                              a_t = a_1,
                                              f00 = f00,
                                              f11 = f11)) %>%
  inner_join(policy_xwalk, by = c("f00", "f11")) %>%
  inner_join(q_xwalk, by = "q") %>%
  mutate(a_1 = factor(glue("{ifelse(a_1 == 0, 'Weak', 'Strong')} policy (a = {a_1})"),
                      levels = c("Weak policy (a = 0)",
                                 "Strong policy (a = 1)")))

gg_post_omega_2 <- plot_posterior_omega_2(post_omega_grid)

gg_action_effect_mod <- post_omega_grid %>%
  filter(f00 == 0.5,
         q == 0.5) %>%
  plot_posterior_omega_2()

gg_update <- plot_update(post_omega_grid)

gg_update_mod <- post_omega_grid %>%
  filter(f00 == 0.5,
         q == 0.5) %>%
  plot_update()


# Save --------------------------------------------------------------------
ggsave(filename = here("out", "posterior_omega_2.pdf"),
       plot = gg_post_omega_2,
       width = 10, height = 6, units = "in")

ggsave(filename = here("out", "posterior_omega_2_moderate.pdf"),
       plot = gg_action_effect_mod,
       width = 10, height = 5, units = "in")

ggsave(filename = here("out", "posterior_omega_2_update.pdf"),
       plot = gg_update,
       width = 10, height = 6, units = "in")

ggsave(filename = here("out", "posterior_omega_2_update_moderate.pdf"),
       plot = gg_update_mod,
       width = 10, height = 5, units = "in")
