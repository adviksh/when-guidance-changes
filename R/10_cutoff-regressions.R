# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(estimatr)
library(broom)

# Helpers -----------------------------------------------------------------
lm_cutoff <- function(cutoff, data) {

  data$update_mag_deaths_raw <- data$update_mag_deaths_raw <= cutoff


  model <- lm_robust(update_mag_deaths_raw ~ shifty + ri_stratum,
                     data = data)

  model %>%
    tidy %>%
    as_tibble() %>%
    transmute(term,
              estimate,
              conf_lo_95 = conf.low,
              conf_hi_95 = conf.high,
              conf_lo_90 = estimate + qt(0.05, df = df) * std.error,
              conf_hi_90 = estimate + qt(0.95, df = df) * std.error)

}

pretty_cutoffs <- function(x) {
  case_when(x <= 1000   ~ as.character(x),
            x <= 999999 ~ paste0(round(x/1000), "k"),
            TRUE        ~ paste0(round(x/1000000), "m"))
}


# Load Data ---------------------------------------------------------------
tb_raw <- haven::read_dta(here("data", "public_data.dta"),
                          encoding = "latin1")



# Regressions -------------------------------------------------------------
tb <- mutate(tb_raw,
             update_mag_deaths_raw = exp(update_mag_deaths) - 1)

# Setup:
cutoffs <- c(0,
             1 * 10^seq(2, 6),
             2 * 10^seq(2, 6),
             5 * 10^seq(2, 6))

config  <- crossing(cutoff = cutoffs)

# Fit:
coef_tb <- mutate(config,
                  coefs = pmap(config, lm_cutoff,
                               data = tb))

# Format for plots
gg_tb <- coef_tb %>%
  unnest(cols = "coefs") %>%
  mutate(cutoff = pretty_cutoffs(cutoff),
         cutoff = factor(cutoff, levels = unique(cutoff))) %>%
  filter(term == "shifty")


# Plotting ----------------------------------------------------------------
cutoff_plot <- gg_tb %>%
  ggplot(aes(x = cutoff,
             y = estimate)) +
  labs(x = "Size cutoff",
       y = "Treatment effect on\nprob( |update| <= size cutoff )") +
  geom_hline(yintercept = 0,
             color = "red") +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin = conf_lo_95,
                    ymax = conf_hi_95),
                size = 1,
                width = 0.33) +
  geom_linerange(aes(ymin = conf_lo_90,
                     ymax = conf_hi_90),
                 size = 4,
                 color = "darkgrey") +
  geom_point(pch = 22,
             fill = "white",
             size = 5,
             stroke = 2)


# Save --------------------------------------------------------------------
ggsave(filename = here("out", "update_mag_deaths_cutoffs_abs.pdf"),
       plot = cutoff_plot,
       device = grDevices::cairo_pdf,
       width = 14, height = 6, units = "in")
