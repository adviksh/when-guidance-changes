# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gtable)
library(ggthemes)
library(glue)
library(here)

# Functions ---------------------------------------------------------------
implied_mu_next <- function(mu, alpha) { 1 - alpha + (2 * alpha - 1) * mu }

mu_hat_2 <- function(r_0, r_1, mu_0, alpha, q) {
  mh1 <- mu_hat_1(r_0, r_1, mu_0, alpha, q)
  1 - alpha + (2 * alpha - 1) * mh1
}

mu_hat_1 <- function(r_0, r_1, mu_0, alpha, q) {
  p_a  <- posterior_a(r_0, r_1, mu_0, alpha, q)
  mu_1 <- implied_mu_next(alpha, mu_0)

  p_a + (1 - p_a) * mu_1
}

posterior_a <- function(r_0, r_1, mu_0, alpha, q) {
  case_when(is.na(r_0) & r_1 == 1 ~ posterior_a_r0en_r1e1(mu_0, alpha, q),
            r_0 == 0 & r_1 == 1   ~ posterior_a_r0e0_r1e1(mu_0, alpha, q))
}

posterior_a_r0en_r1e1 <- function(mu_0, alpha, q) {
  nm <- (1 - mu_0) * (1 - alpha) * q + mu_0 * alpha * q
  dn <- nm + 0.5 * (1 - q)
  nm / dn
}

posterior_a_r0e0_r1e1 <- function(mu_0, alpha, q) {
  nm <- (1 - mu_0) * (1 - alpha) * q
  dn <- nm + 0.25 * (1 - q)
  nm / dn
}


# Xwalks ------------------------------------------------------------------
q_xwalk <- tibble(q = c(0.25, 0.5, 0.75),
                  q_lab = c("Low prior credibility\n(q = 0.25)",
                            "Med. prior credibility\n(q = 0.5)",
                            "High prior credibility\n(q = 0.75)")) %>%
  mutate(q_lab = as_factor(q_lab))

alpha_xwalk <- tibble(alpha = c(0.6, 0.75, 0.9),
                      alpha_lab = c("Slightly sticky states\n(\u03B1 = 0.6)",
                                    "Sticky states\n(\u03B1 = 0.75)",
                                    "Highly sticky states\n(\u03B1 = 0.9)")) %>%
  mutate(alpha_lab = as_factor(alpha_lab))

# Posterior mu_2 -----------------------------------------------------------
post_grid_tb <- crossing(alpha = alpha_xwalk$alpha,
                         q     = q_xwalk$q,
                         mu_0  = seq(0, 1, 0.02),
                         r_0   = c(NA, 0),
                         r_1   = 1) %>%
  mutate(mu_1 = implied_mu_next(mu_0, alpha),
         post_a   = posterior_a(r_0 = r_0,
                                r_1 = r_1,
                                mu_0 = mu_0,
                                q = q,
                                alpha = alpha),
         mu_hat_1 = mu_hat_1(r_0 = r_0,
                             r_1 = r_1,
                             mu_0 = mu_0,
                             q = q,
                             alpha = alpha),
         update = mu_hat_1 - mu_1,
         r_0    = ifelse(is.na(r_0), "none", r_0),
         r_0    = factor(r_0, levels = c("none", "0"))) %>%
  group_by(alpha, q, mu_0, r_1) %>%
  mutate(update_ratio = sum(update * (r_0 == "0")) / sum(update * (r_0 == "none"))) %>%
  ungroup() %>%
  inner_join(q_xwalk, by = "q") %>%
  inner_join(alpha_xwalk, by = "alpha")

post_pane_tb <- filter(post_grid_tb,
                       alpha == 0.9,
                       q == 0.75)


# Plotting ----------------------------------------------------------------
colors         <- ggthemes::colorblind_pal()(3)[3:2]
base_text_size <- 21
anno_text_size <- 7

# Updated beliefs about crisis
pane_update <- ggplot(post_pane_tb,
                      aes(x = mu_0,
                          y = update,
                          color = r_0)) +
  labs(x = expression(paste("Prior probability of crisis in initial period:  ",
                            mu[0])),
       y = expression(paste("Belief update:  ",
                            mu[1]^{paste(r[0], ", ",  r[1])} - mu[1])),
       color  = "Reports") +
  theme_bw(base_size = base_text_size) +
  theme(panel.grid.minor = element_blank()) +
  guides(color = FALSE) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = colors) +
  geom_line(size = 3) +
  annotate("segment",
           x = 0.625, xend = 0.75,
           y = 0.320, yend = 0.35) +
  annotate("text",
           x = 0.755,
           y = 0.35,
           size = anno_text_size,
           hjust = "left",
           label = expression(atop(paste("Consistent report"),
                                   paste("(", r[0] == symbol("\306"), ", ", r[1] == 1, ")")))) +
  annotate("segment",
           x = 0.5, xend = 0.375,
           y = 0.18, yend = 0.15) +
  annotate("text",
           x = 0.370,
           y = 0.15,
           hjust = "right",
           size = anno_text_size,
           label = expression(atop(paste("Inonsistent report"),
                                   paste("(", r[0] == 0, ", ", r[1] == 1, ")"))))

# Ratio of updates
pane_ratio <- ggplot(post_pane_tb,
                     aes(x = mu_0,
                         y = update_ratio)) +
  labs(x = expression(paste("Prior probability of crisis in initial period:  ",
                            mu[0])),
       y = expression(paste("Update ratio:  ",
                            frac(mu[1]^paste("Inconsistent") - mu[1],
                                 mu[1]^paste("Consistent") - mu[1])))) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  theme_bw(base_size = base_text_size) +
  theme(panel.grid.minor = element_blank()) +
  geom_line(size = 3)

# Posterior government credibility
pane_cred <- ggplot(post_pane_tb,
                    aes(x = mu_0,
                        y = post_a,
                        color = r_0)) +
  labs(x = expression(paste("Prior probability of crisis in initial period:  ",
                            mu[0])),
       y = expression(paste("Posterior govt. credibility:  ",
                            q^{paste(r[0], ", ", r[1])})),
       color  = "Reports") +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  theme_bw(base_size = base_text_size) +
  theme(panel.grid.minor = element_blank()) +
  guides(color = FALSE) +
  scale_color_manual(values = colors) +
  geom_line(size = 3) +
  annotate("segment",
           x = 0.5, xend = 0.625,
           y = 0.74, yend = 0.7) +
  annotate("text",
           x = 0.630,
           y = 0.7,
           hjust = "left",
           size = anno_text_size,
           label = expression(atop(paste("Consistent report"),
                                   paste("(", r[0] == symbol("\306"), ", ", r[1] == 1, ")")))) +
  annotate("segment",
           x = 0.5,  xend = 0.375,
           y = 0.365, yend = 0.3) +
  annotate("text",
           x = 0.370,
           y = 0.3,
           hjust = "right",
           size = anno_text_size,
           label = expression(atop(paste("Inconsistent report"),
                                   paste("(", r[0] == 0, ", ", r[1] == 1, ")"))))


# Output ------------------------------------------------------------------
ggsave(filename = here("out", "post_steady_shifty_update.pdf"),
       plot = pane_update,
       width = 14, height = 6, units = "in")

ggsave(filename = here("out", "post_steady_shifty_ratio.pdf"),
       plot = pane_ratio,
       width = 14, height = 6, units = "in")

ggsave(filename = here("out", "post_steady_shifty_cred.pdf"),
       plot = pane_cred,
       width = 14, height = 6, units = "in")
