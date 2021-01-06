# Notes -------------------------------------------------------------------
# The graphs in this file uncouple s_t and a_t
# We first let people update according to a particular s_t
# Then we let a_t vary


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(glue)
library(here)

# Functions ---------------------------------------------------------------
implied_mu_next <- function(mu, alpha) { 1 - alpha + (2 * alpha - 1) * mu }

mu_hat_t_next_given_a_t <- function(a_t, mu_hat_t, alpha, q, f00, f11) {

  f0 <- function(a) ifelse(a == 0, f00, 1)
  f1 <- function(a) ifelse(a == 1, f11, 1)

  alpha * f1(a_t) * mu_hat_t + (1 - alpha) * f0(a_t) * (1 - mu_hat_t)
}

mu_t_hat_given_s_t <- function(s_t, mu_t, q) {

  denom <- 1 + as_sign(s_t) * (2 * mu_t - 1) * q
  numer <- ifelse(s_t == 1,
                  (1 + q) * mu_t,
                  (1 - q) * mu_t)
  numer / denom

}

as_sign <- function(x) { 2*x - 1 }

plot_action_effect <- function(tb) {
  gg <- ggplot(tb,
               aes(x = mu_0,
                   y = update,
                   color = a_1,
                   group = a_1)) +
    scale_x_continuous(limits = c(0, 1)) +
    labs(x = expression(atop("Prior probability of crisis in initial period", mu[0])),
         y = expression(atop("Update about crisis in final period", hat(mu)[2](r[0], r[1] == 1) - mu[2])),
         color  = "Policy in\nmiddle period") +
    theme_bw(base_size = 13) +
    scale_color_colorblind(guide = guide_legend(nrow = 2, byrow = TRUE)) +
    geom_line(size = 1.5)

  if (n_distinct(tb$f_lab) > 1) {
    gg <- gg + facet_grid(. ~ f_lab,
                           labeller = labeller(.rows = label_value,
                                               .cols = label_parsed))
  } else {
    gg <- gg + theme(legend.position = "bottom",
                     legend.direction = "horizontal")
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

# Action Effect -----------------------------------------------------------
action_effect_grid <- crossing(alpha = 0.95,
                               mu_0 = seq(0, 1, 0.02),
                               nesting(f00 = policy_xwalk$f00,
                                       f11 = policy_xwalk$f11),
                               s_1 = 0,
                               a_1 = c(0, 1)) %>%
  mutate(mu_1 = implied_mu_next(mu_0, alpha),
         mu_2 = implied_mu_next(mu_1, alpha),
         mu_hat_1 = mu_t_hat_given_s_t(s_t = s_1,
                                       mu_t = mu_1,
                                       q = 0.5),
         mu_hat_2 = mu_hat_t_next_given_a_t(a_t = a_1,
                                            mu_hat_t = mu_hat_1,
                                            alpha = alpha,
                                            q = 0.5,
                                            f00 = f00,
                                            f11 = f11),
         update = mu_hat_2 - mu_2) %>%
  inner_join(policy_xwalk, by = c("f00", "f11")) %>%
  mutate(a_1 = factor(glue("{ifelse(a_1 == 0, 'Weak', 'Strong')} policy (a = {a_1})"),
                      levels = c("Weak policy (a = 0)",
                                 "Strong policy (a = 1)")))

gg_action_effect <- plot_action_effect(action_effect_grid)

gg_action_effect_mod <- action_effect_grid %>%
  filter(f00 == 0.5) %>%
  plot_action_effect()


# Save --------------------------------------------------------------------
ggsave(filename = here("out", "action_effect.pdf"),
       plot = gg_action_effect,
       width = 10, height = 5, units = "in")

ggsave(filename = here("out", "action_effect_moderate.pdf"),
       plot = gg_action_effect_mod,
       width = 10, height = 5, units = "in")
