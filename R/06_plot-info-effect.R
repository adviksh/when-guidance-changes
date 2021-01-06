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

plot_info_effect <- function(tb) {
  gg <- ggplot(tb,
               aes(x = mu_0,
                   y = update,
                   group = s_1,
                   color = s_1)) +
    scale_x_continuous(limits = c(0, 1)) +
    labs(x = expression(atop("Prior probability of crisis in initial period", mu[0])),
         y = expression(atop("Update about crisis in final period", hat(mu)[2](r[0], r[1] == 1) - mu[2])),
         color  = "Signal in\nmiddle period") +
    theme_bw(base_size = 13) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "darkgrey") +
    geom_line(size = 1.5)

  if (n_distinct(tb$q_lab) > 1) {
    gg <- gg + facet_grid(q_lab ~ .) +
      scale_color_colorblind()
  } else {
    gg <- gg + theme(legend.position = "bottom",
                     legend.direction = "horizontal") +
      scale_color_colorblind(guide = guide_legend(nrow = 2, byrow = TRUE))
  }
  gg
}

# Xwalks ------------------------------------------------------------------
q_xwalk <- tibble(q = c(0.25, 0.5, 0.75),
                  q_lab = c("Low prior credibility\n(q = 0.25)",
                            "Med. prior credibility\n(q = 0.5)",
                            "High prior credibility\n(q = 0.75)")) %>%
  mutate(q_lab = as_factor(q_lab))

# Information Effect ------------------------------------------------------
info_effect_grid <- crossing(alpha = 0.95,
                             f00   = 0.5,
                             f11   = 0.5,
                             mu_0 = seq(0, 1, 0.02),
                             q = q_xwalk$q,
                             s_1 = c(0, 1),
                             a_1 = 0) %>%
  mutate(mu_1 = implied_mu_next(mu_0, alpha),
         mu_2 = implied_mu_next(mu_1, alpha),
         mu_hat_1 = mu_t_hat_given_s_t(s_t = s_1,
                                       mu_t = mu_1,
                                       q = q),
         mu_hat_2 = mu_hat_t_next_given_a_t(a_t = a_1,
                                            mu_hat_t = mu_hat_1,
                                            alpha = alpha,
                                            q = q,
                                            f00 = f00,
                                            f11 = f11),
         update = mu_hat_2 - mu_2) %>%
  inner_join(q_xwalk, by = "q") %>%
  mutate(a_1 = factor(glue("a = {a_1}")),
         s_1 = factor(glue("{ifelse(s_1 == 0, 'Normal', 'Crisis')} signal (s = {s_1})"),
                      levels = c("Normal signal (s = 0)",
                                 "Crisis signal (s = 1)")))

gg_info_effect <- plot_info_effect(info_effect_grid)
gg_info_effect_med <- info_effect_grid %>%
  filter(q == 0.5) %>%
  plot_info_effect()

# Save --------------------------------------------------------------------
ggsave(filename = here("out", "information_effect.pdf"),
       plot = gg_info_effect,
       width = 10, height = 5, units = "in")

ggsave(filename = here("out", "information_effect_medium.pdf"),
       plot = gg_info_effect_med,
       width = 10, height = 5, units = "in")
