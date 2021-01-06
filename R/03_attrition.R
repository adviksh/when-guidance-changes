# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(glue)
library(systemfit)
library(fastDummies)


# Helpers -----------------------------------------------------------------
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

fill_template <- function(template) {
  template %>%
    map(glue, .open = "<", .close = ">") %>%
    map_chr(as.character)
}


file_slug <- function(filepaths) {
  components <- str_split(filepaths, pattern = "/")
  files      <- map_chr(components, tail, 1)
  str_remove(files, "\\.[a-z]+$")
}


str_subset_any <- function(string, patterns) {
  sets <- map(patterns, str_subset, string = string)
  unique(unlist(sets, use.names = FALSE))
}

# Load Data ---------------------------------------------------------------
experiments_raw   <- read_rds(here("data", "public_data.rds"))


# Transform Data ----------------------------------------------------------
e2_ss <- experiments_raw %>%
  filter(experiment == "e_2ss") %>%
  mutate(attn_pass_puce = str_detect(attn_check_color, "puce"),
         attn_pass_7    = attn_check_7 == 7,
         # if you took both checks, you must pass one of them to enter study sample
         attn_pass = case_when(!is.na(attn_check_color) & !is.na(attn_check_7) ~ attn_pass_puce | attn_pass_7,
                               TRUE ~ TRUE),
         progress = as.integer(progress)) %>%
  filter(attn_pass == TRUE)

e2_ss_analysis <- e2_ss %>%
  transmute(tmt  = as.integer(tmt_steady_0_shifty_1),
            drop = as.integer(finished == "False"),
            ctl_comp = (1 - tmt) * (1 - drop),
            ctl_drop = (1 - tmt) * drop,
            tmt_comp = tmt * (1 - drop),
            tmt_drop = tmt * drop,
            all = "all",
            sex = gender,
            race,
            party = political_party,
            region,
            hisp = hispanic,
            educ = education,
            hhi,
            age,
            govt_not_strong_base,
            govt_appropriate_base,
            govt_overreact_base,
            govt_has_info_base,
            follow_news,
            death_count_base) %>%
  mutate(across(starts_with("govt"), as.numeric),
         follow_news = as.numeric(follow_news),
         death_count_base = as.numeric(str_remove_all(death_count_base, ",")),
         death_count_base = log(death_count_base + 1))


# Attrition Rates ---------------------------------------------------------
all_demos <- c("all", "sex", "race", "party", "region", "hisp", "educ", "hhi",
               "age")

a_raw <- e2_ss_analysis %>%
  pivot_longer(cols = all_of(all_demos),
               names_to  = "category",
               values_to = "subcategory") %>%
  group_by(category, subcategory) %>%
  summarize(tmt_n = sum(tmt),
            ctl_n = sum(1 - tmt),
            tmt_drop_n    = sum(tmt_drop),
            ctl_drop_n    = sum(ctl_drop),
            .groups = "drop") %>%
  mutate(tmt_drop_rate = tmt_drop_n / tmt_n,
         ctl_drop_rate = ctl_drop_n / ctl_n,
         drop_n = map2(ctl_drop_n, tmt_drop_n, c),
         n = map2(ctl_n, tmt_n, c),
         test = map2(drop_n, n, prop.test),
         pval = map_dbl(test, "p.value")) %>%
  # rename for shorthand, to match the table template
  # ex: from here on, "tmt_n" gives the number of attritors in tmt
  transmute(category,
            subcategory,
            tmt_n = tmt_drop_n,
            ctl_n = ctl_drop_n,
            tmt_rate = tmt_drop_rate,
            ctl_rate = ctl_drop_rate,
            pval)

a <- a_raw %>%
  mutate(tmt_rate = round_thousandth(tmt_rate),
         ctl_rate = round_thousandth(ctl_rate),
         pval = round_thousandth(pval),
         pval = ifelse(tmt_n + ctl_n < 10,
                       "---", pval)) %>%
  nest(test_data = c(tmt_n, ctl_n, tmt_rate, ctl_rate, pval)) %>%
  nest(subcat_data = c(subcategory, test_data)) %>%
  mutate(subcat_data = map(subcat_data, tb_to_list,
                           names_from  = "subcategory",
                           values_from = "test_data")) %>%
  tb_to_list(names_from = "category",
             values_from = "subcat_data")


# Modeling Setup ----------------------------------------------------------
message(glue("Removing {sum(is.na(e2_ss_analysis$hisp))} rows with missing hispanic status."))
message(glue("We can't fit SUR across groups of different sizes (which happens if some obs are missing)"))

test_demos <- setdiff(all_demos, "all")

modeling_tb <- e2_ss_analysis %>%
  filter(!is.na(hisp)) %>%
  dummy_cols(test_demos,
             remove_first_dummy = FALSE,
             remove_most_frequent_dummy = FALSE,
             ignore_na = TRUE,
             remove_selected_columns = TRUE) %>%
  rename_with(str_replace_all, pattern = "-", replacement = "_") %>%
  rename_with(str_replace_all, pattern = "\\+", replacement = "p")

# variables that we may put on the LHS, removing some smaller baseline categories
demo_candidates <- names(modeling_tb) %>%
  setdiff(c("all",
            "stratum",
            "tmt", "drop",
            "ctl_comp", "ctl_drop", "tmt_comp", "tmt_drop")) %>%
  setdiff(c("sex_male", "race_native", "party_other", "region_midw",
            "hisp_hispanic", "educ_less_hs_other", "hhi_200k", "age_70p"))

# Find groups with few people who dropped
# We won't check for attrition in such small bins
subcat_drop_counts <- modeling_tb %>%
  pivot_longer(cols = str_subset_any(names(.), test_demos),
               names_to = "dummy",
               values_to = "dummy_value") %>%
  filter(dummy_value == 1) %>%
  count(dummy, drop)

extract_small_counts <- function(tb, cutoff) {
  tb %>%
    filter(n < cutoff) %>%
    pull(dummy) %>%
    unique
}

small_subcats <- tibble(cutoff  = 0,
                        subcats = map(cutoff, extract_small_counts,
                                      tb = subcat_drop_counts))

model_config <- mutate(small_subcats,
                       dummies = map(subcats, ~setdiff(demo_candidates, .x)),
                       formulas = map(dummies,
                                      map,
                                      reformulate,
                                      termlabels = c("-1", "ctl_comp", "ctl_drop", "tmt_comp", "tmt_drop")))

models <- transmute(model_config,
                    cutoff,
                    model = map(formulas, systemfit,
                                data  = modeling_tb,
                                method = "SUR"))


# Hypothesis Test Setup ---------------------------------------------------
bal_template <- "eq_tmt_comp = eq_ctl_comp"

att_template <- "eq_tmt_drop = eq_ctl_drop"

ivr_template <- c("eq_tmt_comp = eq_ctl_comp",
                  "eq_tmt_drop = eq_ctl_drop")

bal_specs <- model_config %>%
  mutate(eq_nums = map(formulas, ~paste0("eq", seq_along(.x)))) %>%
  transmute(cutoff,
            spec = map(eq_nums, map,
                       str_replace_all,
                       string = bal_template,
                       pattern = "eq"),
            spec_pooled = map(spec, unlist))

att_specs <- model_config %>%
  mutate(eq_nums = map(formulas, ~paste0("eq", seq_along(.x)))) %>%
  transmute(cutoff,
            spec = map(eq_nums, map,
                       str_replace_all,
                       string = att_template,
                       pattern = "eq"),
            spec_pooled = map(spec, unlist))

ivr_specs <- model_config %>%
  mutate(eq_nums = map(formulas, ~paste0("eq", seq_along(.x)))) %>%
  transmute(cutoff,
            spec = map(eq_nums, map,
                       str_replace_all,
                       string = ivr_template,
                       pattern = "eq"),
            spec_pooled = map(spec, unlist))


# Hypothesis Tests --------------------------------------------------------
bal_tests_pooled <- map2(models$model, bal_specs$spec_pooled, linearHypothesis)
att_tests_pooled <- map2(models$model, att_specs$spec_pooled, linearHypothesis)
ivr_tests_pooled <- map2(models$model, ivr_specs$spec_pooled, linearHypothesis)

f_raw <- ivr_tests_pooled %>%
  map(list("Pr(>F)", 2)) %>%
  map(round_thousandth)
names(f_raw) <- models$cutoff



# Cumulative Attrition Graph ----------------------------------------------
attr_graph_config <- tibble(progress_cutoff = sort(unique(e2_ss$progress)))

attr_graph_tb <- attr_graph_config %>%
  mutate(cumulative_attrition = map_dbl(progress_cutoff,
                                        ~mean(e2_ss$progress < .x)),
         progress_cutoff = progress_cutoff / 100)

attr_graph <- ggplot() +
  labs(x = "Survey progress",
       y = "Cumulative attrition rate",
       fill = "Section") +
  scale_fill_manual(values = list(`COVID priors` = "grey",
                                  `Treatment` = "#0072B2")) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  geom_rect(aes(fill = "COVID priors",
                xmin = 0.1225, xmax = 0.2175,
                ymin = 0, ymax = 0.35),
            alpha = 0.5) +
  geom_rect(aes(fill = "Treatment"),
            xmin = 0.2675, xmax = 0.3125,
            ymin = 0, ymax = 0.35,
            alpha = 0.5) +
  geom_rect(aes(fill = "Treatment"),
            xmin = 0.44, xmax = 0.48,
            ymin = 0, ymax = 0.35,
            alpha = 0.5) +
  geom_line(data =  attr_graph_tb,
            aes(x = progress_cutoff,
                y = cumulative_attrition)) +
  geom_point(data =  attr_graph_tb,
             aes(x = progress_cutoff,
                 y = cumulative_attrition))

# Input to Table ----------------------------------------------------------
tb_template_files <- c(here("R", "template_attrition.txt"))

tb_template <- read_lines(tb_template_files)

tb_filled   <- fill_template(tb_template)

data_rows <- which(str_detect(tb_filled, "&"))

meta_rows <- setdiff(seq_along(tb_filled), data_rows)

tb_filled[data_rows] <- align_rows(tb_filled[data_rows])


# Write -------------------------------------------------------------------
tb_filled_slug <- tb_template_files %>%
  file_slug %>%
  str_replace("template", "tb")

tb_filled_path <- here("out", glue("{tb_filled_slug}.tex"))

write_lines(tb_filled, tb_filled_path)
