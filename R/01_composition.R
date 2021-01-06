# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(glue)

# Helpers -----------------------------------------------------------------
se_mean <- function(x) { sd(x, na.rm = T) / sqrt(sum(!is.na(x))) }

tb_to_list <- function(tb, names_from, values_from) {
  as.list(set_names(tb[[values_from]], tb[[names_from]]))
}

tb_to_nested_list <- function(tb, nest_order) {

  if (length(nest_order) == 2) {
    ret <- tb_to_list(tb,
                      names_from  = nest_order[1],
                      values_from = nest_order[2])
    return(ret)
  }

}

round_thousandth <- scales::number_format(accuracy = 0.001)

signed_thousandth <- function(x) {
  x_scl <- round_thousandth(x)
  ifelse(x_scl < 0,
         as.character(x_scl),
         paste0("+", x_scl))
}

align_rows <- function(rows) {

  row_cells <- rows %>%
    str_remove("\\\\+$") %>%
    str_split(" & ") %>%
    map(trimws)

  max_size <- max(cell_size(unlist(row_cells)))

  row_cells %>%
    map_depth(2, resize_cell, size = max_size + 2) %>%
    map_chr(paste, collapse = "&") %>%
    paste("\\\\")

}

diff_prop <- function(x, n) { diff(x / n) }

diff_prop_p_val <- function(x, n) {

  tryCatch(prop.test(x, n)$p.value,
           error = function(e) NA)

}

diff_mean_p_val <- function(x, y) { t.test(x, y,)$p.value }

cell_size <- function(cell) {
  ifelse(str_detect(cell, "multicolumn"),
         str_length(cell) / 2,
         str_length(cell))
}

resize_cell <- function(cell, size) {

  if (str_detect(cell, "hline")) return(cell)

  str_pad(cell,
          width = ifelse(str_detect(cell, "multicolumn"),
                         yes = size * 2 + 1,
                         no = size),
          side = ifelse(str_detect(cell, "textbf"),
                        yes = "right",
                        no  = "both"))

}


file_slug <- function(filepaths) {
  components <- str_split(filepaths, pattern = "/")
  files      <- map_chr(components, tail, 1)
  str_remove(files, "\\.[a-z]+$")
}

# Load Data ---------------------------------------------------------------
experiments_raw  <- read_rds(here("data", "public_data.rds"))
demos_usa_raw    <- read_csv(here("data", "demographics_usa.csv"))


# US Composition ----------------------------------------------------------
usa <- demos_usa_raw %>%
  mutate(share = round_thousandth(share),
         share = case_when(share == "0.000" ~ "---",
                           TRUE ~ share)) %>%
  nest(subcat_data = c(subcategory, share)) %>%
  mutate(subcat_data = map(subcat_data,
                           tb_to_list,
                           names_from = "subcategory",
                           values_from = "share")) %>%
  tb_to_list(names_from = "category",
             values_from = "subcat_data")



# Subset Data -------------------------------------------------------------
experiment <- experiments_raw %>%
  filter(experiment == "e_2ss",
         finished != "False",
         # Must pass attention checks
         str_detect(attn_check_color, "puce") | attn_check_7 == 7) %>%
  mutate(treatment = ifelse(tmt_steady_0_shifty_1 == 1, "treatment", "control"),
         sex = gender) %>%
  mutate(govt_weak        = govt_not_strong_base,
         govt_appropriate = govt_appropriate_base,
         govt_overreact   = govt_overreact_base,
         govt_info    = govt_has_info_base,
         covid_news   = follow_news,
         covid_deaths = death_count_base,
         across(starts_with("govt"), as.numeric),
         covid_news = as.numeric(covid_news),
         covid_deaths = as.numeric(str_remove_all(covid_deaths, ",")),
         covid_deaths = log(covid_deaths + 1))

# Demographics -------------------------------------------------------------
demos_exp_wide <- experiment %>%
  select(treatment,
         sex,
         race,
         political_party,
         region,
         hispanic,
         education,
         hhi,
         age)

category_counts <- demos_exp_wide %>%
  pivot_longer(cols = c(-treatment),
               names_to  = "cat",
               values_to = "subcat") %>%
  count(treatment, cat, subcat,
        name = "subcat_n") %>%
  group_by(treatment, cat) %>%
  mutate(cat_n        = sum(subcat_n),
         subcat_share = subcat_n / cat_n) %>%
  ungroup()

cat_p_vals <- category_counts %>%
  select(-subcat_share) %>%
  pivot_wider(names_from  = "treatment",
              values_from = c("subcat_n", "cat_n")) %>%
  transmute(cat,
            subcat,
            subcat_n = map2(subcat_n_control, subcat_n_treatment, c),
            cat_n    = map2(cat_n_control, cat_n_treatment, c)) %>%
  transmute(cat,
            subcat,
            diff_val = map2_dbl(subcat_n, cat_n, diff_prop),
            diff_p   = map2_dbl(subcat_n, cat_n, diff_prop_p_val),
            diff_val = signed_thousandth(diff_val),
            diff_p   = round_thousandth(diff_p)) %>%
  pivot_longer(c("diff_val", "diff_p"),
               names_to = "measure",
               values_to = "value")

demos_tall <- category_counts %>%
  select(-cat_n) %>%
  pivot_longer(c("subcat_n", "subcat_share")) %>%
  mutate(treatment = ifelse(treatment == "treatment",
                            "tmt", "ctl"),
         name = ifelse(name == "subcat_n",
                       "n", "m"),
         value = ifelse(name == "n",
                        as.character(value),
                        round_thousandth(value))) %>%
  unite("measure", treatment, name)


# Continuous Measures -----------------------------------------------------
# Continuous variables
cont_wide <- experiment %>%
  select(treatment,
         govt_weak,
         govt_appropriate,
         govt_overreact,
         govt_info,
         covid_news,
         covid_deaths)


cont_nested <- cont_wide %>%
  pivot_longer(cols = c(-treatment), names_to = "variable") %>%
  group_by(treatment, variable) %>%
  summarize(values = list(value),
            .groups = "drop") %>%
  pivot_wider(names_from = "treatment",
              values_from = "values")

cont_summary <- cont_nested %>%
  mutate(variable,
         ctl_mean = map_dbl(control, mean),
         ctl_se   = map_dbl(control, se_mean),
         tmt_mean = map_dbl(treatment, mean),
         tmt_se   = map_dbl(treatment, se_mean),
         diff_p   = map2_dbl(treatment, control, diff_mean_p_val)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  transmute(variable,
            ctl_m = as.character(glue("{ctl_mean} ({ctl_se})")),
            tmt_m = as.character(glue("{tmt_mean} ({tmt_se})")),
            diff_p = as.character(diff_p))

cont_tall <- cont_summary %>%
  pivot_longer(cols = c("ctl_m", "tmt_m", "diff_p"),
               names_to = "measure",
               values_to = "value") %>%
  separate(variable, c("cat", "subcat"), sep = "_")


# Format for Tables -------------------------------------------------------
# Convert to nested lists
d <- bind_rows(demos_tall,
               cont_tall,
               cat_p_vals) %>%
  nest(subcat_data = c(subcat, value)) %>%
  mutate(subcat_data = map(subcat_data,
                           tb_to_list,
                           names_from = "subcat",
                           values_from = "value")) %>%
  nest(cat_data = c(cat, subcat_data)) %>%
  mutate(cat_data = map(cat_data,
                        tb_to_list,
                        names_from = "cat",
                        values_from = "subcat_data")) %>%
  tb_to_list(names_from = "measure",
             values_from = "cat_data")

n_e <- nrow(experiment)

n_e_tmt <- count(experiment, treatment) %>%
  mutate(treatment = ifelse(treatment == "treatment",
                            "tmt", "ctl")) %>%
  tb_to_list(names_from = "treatment",
             values_from = "n")


# F Tests -----------------------------------------------------------------
regression_f_test <- function(formula, data) {
  model <- lm(formula, data)
  waldtest(model, vcov = vcovHC(model, type = "HC0"))
}

formula_raw <- tmt_steady_0_shifty_1 ~ sex + political_party + region + race + hispanic +
  education + hhi + age + govt_weak + govt_appropriate + govt_overreact +
  govt_info + covid_news + covid_deaths

formula_adj <- update(formula_raw, . ~ . + stratum)

model_raw <- lm(formula_raw, experiment)
model_adj <- lm(formula_adj, experiment)

f_raw <- signif(waldtest(model_raw, vcov = vcovHC(model_raw, type = "HC3"))[["Pr(>F)"]][2],
                3)
f_adj <- signif(waldtest(model_raw, vcov = vcovHC(model_adj, type = "HC3"))[["Pr(>F)"]][2],
                3)


# Input to Table ----------------------------------------------------------
tb_template_files <- c(here("R", "template_composition.txt"))

tb_templates <- map(tb_template_files, read_lines)

fill_template <- function(template) {
  tp_glued <- map(template, glue, .open = "<", .close = ">")
  map_chr(tp_glued, as.character)
}

tb_filled   <- map(tb_templates, fill_template)

data_rows <- map(tb_filled,
                 ~which(str_detect(.x, "&")))

meta_rows <- map2(tb_filled, data_rows,
                  ~setdiff(seq_along(.x), .y))

for (ii in seq_along(tb_filled)) {
  tb_filled[[ii]][data_rows[[ii]]] <- align_rows(tb_filled[[ii]][data_rows[[ii]]])
}


# Write -------------------------------------------------------------------
tb_filled_slugs <- tb_template_files %>%
  map_chr(file_slug) %>%
  str_replace("template", "tb")

tb_filled_paths <- here("out", glue("{tb_filled_slugs}.tex"))

walk2(tb_filled, tb_filled_paths,
      write_lines)
