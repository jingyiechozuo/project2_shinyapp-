# Data preparation, variable list, and helper functions we need in app.R

library(tidyverse)

# read data

prep_vars <- c("Little", "Medium", "Considerable", "Extensive")
data <- read.csv("onet_2026_clean.csv") |>
  mutate(preparation = factor(job_zone, levels = c("Little", "Medium", "Considerable", "Extensive"))) |>
  select(-job_zone)

# categorical variables
group_levels <- sort(unique(data$major_group))
skill_levels <- sort(unique(data$skill_type))

prep_vars <- c("Little", "Medium", "Considerable", "Extensive")
cat_vars <- c("Occupation Group" = "major_group",
              "Preparation Level" = "preparation",
              "Skill Type" = "skill_type")

factor_vars <- c("Communication" = "communication",
                 "Technical" = "technical",
                 "Analytical" = "analytical",
                 "Management" = "management")

numeric_vars <- names(data)[sapply(data, is.numeric)]

# helper functions

# let variables names more reader friendly
label <- function(var) {
  str_to_title(str_replace_all(var, "_", " "))
}

# number, mean, median, and SD of numeric variables at differnet levels of categorical variables
summarise_by <- function(df, group_var, num_var) {
  df |>
    group_by(.data[[group_var]]) |>
    summarise(n = n(),
              mean = mean(.data[[num_var]]),
              median = median(.data[[num_var]]),
              sd = sd(.data[[num_var]]))
}