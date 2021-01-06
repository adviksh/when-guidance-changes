# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(glue)
library(here)

# Functions ---------------------------------------------------------------
implied_mu1 <- function(alpha, mu0) { 1 - alpha - mu0 + 2 * alpha * mu0 }

p_A_r1e1 <- function(alpha, mu0, q) {
  mu1 <- implied_mu1(alpha = alpha, mu0 = mu0)

  nm <- mu1 * q
  dn <- nm + 0.5 * (1 - q)

  nm / dn
}

p_A_r0e0_r1e1 <- function(alpha, mu0, q) {

  nm <- (1 - mu0) * (1 - alpha) * q
  dn <- nm + 0.25 * (1 - q)

  nm / dn
}

p_A_r0e1_r1e1 <- function(alpha, mu0, q) {

  nm <- mu0 * alpha * q
  dn <- nm + 0.25 * (1 - q)

  nm / dn
}

reports_to_expression <- function(s) {
  s %>%
    str_remove("p_A_") %>%
    str_replace_all("e","==") %>%
    str_replace("_", ", ") %>%
    str_replace("r0", "r[0]") %>%
    str_replace("r1", "r[1]") %>%
    factor %>%
    fct_rev()
}

plot_posterior_a <- function(tb, color_labs, color_vals) {
  gg <- ggplot(tb,
               mapping = aes(x = mu0,
                             y = value,
                             color = name)) +
    labs(x = expression(atop("Prior probability of crisis in initial period", mu[0])),
         y = "Posterior government credibility\nP(A | reports)",
         color  = "Reports") +
    theme_bw(base_size = 13) +
    scale_x_continuous(limits = 0:1, minor_breaks = NULL) +
    scale_y_continuous(limits = 0:1, minor_breaks = NULL) +
    scale_linetype(labels = color_labs) +
    geom_line(size = 1.5)

  if (n_distinct(tb$q_lab) == 1 &
      n_distinct(tb$alpha_lab) == 1) {

    gg <- gg +
      theme(legend.position = "bottom",
            legend.direction = "horizontal") +
      scale_color_manual(values = color_vals,
                         labels = color_labs,
                         guide = guide_legend(nrow = 2, byrow = TRUE))

  } else {

    gg <- gg +
      facet_grid(alpha_lab ~ q_lab) +
      scale_color_manual(values = color_vals,
                         labels = color_labs,
                         guide = guide_legend(nrow = 2, byrow = TRUE))
  }

  gg
}

# Xwalks ------------------------------------------------------------------
q_xwalk <- tibble(q = c(0.25, 0.5, 0.75),
                  q_lab = c("Low prior credibility\n(q = 0.25)",
                            "Med. prior credibility\n(q = 0.5)",
                            "High prior credibility\n(q = 0.75)")) %>%
  mutate(q_lab = as_factor(q_lab))

alpha_xwalk <- tibble(alpha = c(0.5, 0.7, 0.9),
                      alpha_lab = c("Independent states\n(\u03B1 = 0.5)",
                                    "Sticky states\n(\u03B1 = 0.7)",
                                    "Highly sticky states\n(\u03B1 = 0.9)")) %>%
  mutate(alpha_lab = as_factor(alpha_lab))

# Posterior A -------------------------------------------------------------
post_a_grid <- crossing(alpha = alpha_xwalk$alpha,
                        q     = q_xwalk$q,
                        mu0   = seq(0, 1, 0.02)) %>%
  mutate(p_A_r1e1      = pmap_dbl(., p_A_r1e1),
         p_A_r0e0_r1e1 = pmap_dbl(., p_A_r0e0_r1e1),
         p_A_r0e1_r1e1 = pmap_dbl(., p_A_r0e1_r1e1)) %>%
  pivot_longer(cols = starts_with("p_A")) %>%
  mutate(name = reports_to_expression(name)) %>%
  inner_join(q_xwalk, by = "q") %>%
  inner_join(alpha_xwalk, by = "alpha")

report_labels <- c("r[1]==1" = expression(r[1]==1),
                   "r[0]==0, r[1]==1" = expression(paste(r[0]==0, ", ", r[1]==1)),
                   "r[0]==1, r[1]==1" = expression(paste(r[0]==1, ", ", r[1]==1)))

report_colors <- c("r[1]==1" = "black",
                   "r[0]==0, r[1]==1" = "#CC79A7",
                   "r[0]==1, r[1]==1" = "#F0E442")

gg_steady_shifty <- post_a_grid %>%
  filter(name != "r[0]==1, r[1]==1") %>%
  plot_posterior_a(color_labs = report_labels,
                   color_vals = report_colors)

gg_steady_shifty_mod <-  post_a_grid %>%
  filter(name != "r[0]==1, r[1]==1",
         q == 0.5,
         alpha == 0.9) %>%
  plot_posterior_a(color_labs = report_labels,
                   color_vals = report_colors)

gg_steady_shifty_2 <- plot_posterior_a(post_a_grid,
                                       color_labs = report_labels,
                                       color_vals = report_colors)

# Output ------------------------------------------------------------------
ggsave(filename = here("out", "posterior_credibility.pdf"),
       plot = gg_steady_shifty,
       device = grDevices::cairo_pdf,
       width = 10, height = 6, units = "in")

ggsave(filename = here("out", "posterior_credibility_moderate.pdf"),
       plot = gg_steady_shifty_mod,
       device = grDevices::cairo_pdf,
       width = 10, height = 5, units = "in")
