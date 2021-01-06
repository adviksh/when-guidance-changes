# Libraries ---------------------------------------------------------------
library(tidyverse)
library(glue)
library(here)


# Helpers -----------------------------------------------------------------
calc_q_n1 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  numer <- (1 - mu0_1) * ((1 - alpha) / 2) * q + mu0_1 * alpha * q
  denom <- numer + (1 / 3) * (1 - q)
  numer / denom
}

calc_q_01 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  numer <- mu0_0 * q * (1 - alpha) / 2
  denom <- numer + (1 / 9) * (1 - q)
  numer / denom
}

calc_q_0n <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  numer <- mu0_0 * q
  denom <- numer + (1 / 3) * (1 - q)
  numer / denom
}

calc_q_1n <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  numer <- mu0_1 * q
  denom <- numer + (1 / 3) * (1 - q)
  numer / denom
}

calc_q_00 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  numer <- mu0_0 * alpha * q
  denom <- numer + (1 / 9) * (1 - q)
  numer / denom
}

calc_q_n0 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  numer <- mu0_0 * (2*alpha - 1) * q + ((1 - alpha) / 2) * q
  denom <- numer + (1 / 3) * (1 - q)
  numer / denom
}

calc_cred_loss_3 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  q_01 <- calc_q_01(mu0_0, mu0_1, mu0_2, alpha, q)
  q_n1 <- calc_q_n1(mu0_0, mu0_1, mu0_2, alpha, q)

  -(q_01 - q_n1)
}

calc_cred_loss_4 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  q_0n <- calc_q_0n(mu0_0, mu0_1, mu0_2, alpha, q)

  -(q_0n - q)
}

calc_cred_loss_5 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  q_00 <- calc_q_00(mu0_0, mu0_1, mu0_2, alpha, q)
  q_n0 <- calc_q_n0(mu0_0, mu0_1, mu0_2, alpha, q)

  -(q_00 - q_n0)
}

calc_cred_loss_6 <- function(mu0_0, mu0_1, mu0_2, alpha, q) {
  q_0n <- calc_q_0n(mu0_0, mu0_1, mu0_2, alpha, q)
  q_1n <- calc_q_1n(mu0_0, mu0_1, mu0_2, alpha, q)

  -(q_0n - q_1n)
}

plot_cred_loss <- function(prior, const, cred_loss, data) {

  plt <- ggplot(data,
                aes(x = prior, y = cred_loss)) +
    theme_bw(base_size = 21) +
    geom_line(size = 1.5)

  # Label X-axis
  if (prior == "mu0_0") {

    plt <- plt + labs(x = expression(paste("Prior probability of no crisis: ",
                                           mu[0](0))))

  } else if (prior == "mu0_1") {

    plt <- plt + labs(x = expression(paste("Prior probability of moderate crisis: ",
                                           mu[0](1))))

  } else if (prior == "mu0_2") {

    plt <- plt + labs(x = expression(paste("Prior probability of severe crisis: ",
                                           mu[0](2))))

  } else {

    stop("unrecognized value for `prior`: ", prior)

  }

  # Label Y-axis
  if (cred_loss == "cred_loss_3") {

    plt <- plt + labs(y = expression(paste("Credibility loss: ",
                                           -(q^n - q^c))))

  } else if (cred_loss == "cred_loss_4") {

    plt <- plt + labs(y = expression(paste("Credibility loss: ",
                                           -(q^o - q))))

  } else {

    stop("unrecognized value for `cred_loss`: ", cred_loss)

  }

  plt
}

dashify <- function(x) { str_replace_all(x, "_", "-") }


# Setup -------------------------------------------------------------------
step_size <- 0.01

prior_grid <- bind_rows(tibble(mu0_0 = seq(0.8, 0.1, by = -step_size),
                               mu0_1 = seq(0.1, 0.8, by = step_size),
                               mu0_2 = 0.1),
                        tibble(mu0_0 = 0.1,
                               mu0_1 = seq(0.8, 0.1, by = -step_size),
                               mu0_2 = seq(0.1, 0.8, by = step_size))) %>%
  mutate(q = 0.75,
         alpha = 0.9)

post_grid <- mutate(prior_grid,
                    row_num = row_number(),
                    cred_loss_3 = pmap_dbl(prior_grid, calc_cred_loss_3),
                    cred_loss_4 = pmap_dbl(prior_grid, calc_cred_loss_4),
                    cred_loss_5 = pmap_dbl(prior_grid, calc_cred_loss_5),
                    cred_loss_6 = pmap_dbl(prior_grid, calc_cred_loss_6))


# Plot --------------------------------------------------------------------
anno_text_size <- 7

x_brk <- quantile(post_grid$row_num,
                  c(0, 0.25, 0.5, 0.75, 1))

x_lbl <- c(expression(atop(mu[0](0)==0.8,
                           atop(textstyle(mu[0](1)==0.1),
                                atop(scriptscriptstyle(""),
                                     textstyle(mu[0](2)==0.1))))),
           expression(atop(phantom(x) %->% phantom(y),
                           atop(textstyle(paste("increasing ", mu[0](1))),
                                atop(scriptscriptstyle(""),
                                     textstyle(paste("fixing ", mu[0](2))))))),
           expression(atop(mu[0](0)==0.1,
                           atop(textstyle(mu[0](1)==0.8),
                                atop(scriptscriptstyle(""),
                                     textstyle(mu[0](2)==0.1))))),
           expression(atop(phantom(x) %->% phantom(y),
                           atop(textstyle(paste("increasing ", mu[0](2))),
                                atop(scriptscriptstyle(""),
                                     textstyle(paste("fixing ", mu[0](0))))))),
           expression(atop(mu[0](0)==0.1,
                           atop(textstyle(mu[0](1)==0.1),
                                atop(scriptscriptstyle(""),
                                     textstyle(mu[0](2)==0.8))))))

clr_pal <- c("#000000", "#009E73", "#CC79A7", "#0072B2")


cred_loss <- ggplot(post_grid,
       aes(x = row_num)) +
  theme_bw(base_size = 21) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Prior beliefs about initial period",
       y = "Loss of govt. credibility") +
  guides(color = FALSE) +
  scale_x_continuous(breaks = x_brk,
                     labels = x_lbl) +
  geom_hline(yintercept = 0,
             color = "darkgrey") +
  annotate("text",
           x = 9,
           y = 0.57,
           size = anno_text_size,
           color = clr_pal[1],
           fontface = "bold",
           hjust = "left",
           label = expression(atop(paste("Inconsistent vs. Consistent"),
                                   paste(-(q^{paste(0, ",", 1)}-q^{paste(symbol("\306"), ",", 1)}))))) +
  annotate("text",
           x = 73,
           y = 0.19,
           size = anno_text_size,
           color = clr_pal[2],
           fontface = "bold",
           hjust = "left",
           label = expression(atop(paste("Early Optimistic vs. None"),
                                   paste(-(q^{paste(0, ",",symbol("\306"))}-q^{paste(symbol("\306"), ",", symbol("\306"))}))))) +
  annotate("text",
           x = 100,
           y = -0.25,
           size = anno_text_size,
           color = clr_pal[3],
           fontface = "bold",
           hjust = "left",
           label = expression(atop(paste("Optimistic vs. Late Optimistic"),
                                   paste(-(q^{paste(0, ",", 0)}-q^{paste(symbol("\306"), ",", 0)}))))) +
  annotate("text",
           x = 73,
           y = 0.47,
           size = anno_text_size,
           color = clr_pal[4],
           fontface = "bold",
           hjust = "left",
           label = expression(atop(paste("Late Optimistic vs. Late Pessimistic"),
                                   paste(-(q^{paste(symbol("\306"), ",", 0)}-q^{paste(symbol("\306"), ",", 1)}))))) +
  geom_line(aes(y = cred_loss_3,
                color = "cred_loss_3"),
            size = 3) +
  geom_line(aes(y = cred_loss_4,
                color = "cred_loss_4"),
            size = 3) +
  geom_line(aes(y = cred_loss_5,
                color = "cred_loss_5"),
            size = 3) +
  geom_line(aes(y = cred_loss_6,
                color = "cred_loss_6"),
            size = 3) +
  scale_color_manual(values = clr_pal)


# Save --------------------------------------------------------------------
ggsave(filename = here("out", "three-state_credibility-loss.pdf"),
       plot = cred_loss,
       width = 14, height = 8, units = "in")

ggsave(filename = here("out", "three-state_credibility-loss.png"),
       plot = cred_loss,
       width = 14, height = 8, units = "in")
