# Load dependencies
library(tidyverse)
library(future)
library(tidymodels)

# Set working directory
source("functions.R")

# Specific data-prep
glm_data <- readr::read_rds("data/df_epinic.rds") %>%
  filter(yr > 2011) %>%
  mutate(across(.cols = c("SAPS_consciousness_level","edu"), 
                ~factor(., ordered = FALSE)
                )
  ) %>%  # Drop the order to avoid using orthogonal polynomials
  mutate(psych = as.logical(`Mood Disorder`+ `Anxiety Disorder` + `Psychotic Disorder` + `Substance Abuse`), .keep = 'unused') %>%
  mutate(across(where(is.logical), as_factor),
         saps_adj = saps_less_age_gcs(SAPS_total_score, age, SAPS_consciousness_level),
         ad_treatment = as_factor(ad_treatment),
         sex_female = as_factor(sex_female),
         AvdNamn = as_factor(AvdNamn),
         CCIw = relevel(factor(case_when(CCIw == 0 ~ "0",
                          CCIw == 1 ~ "1",
                          CCIw >1 ~ ">1"), ordered = FALSE), ref = "0"),
         CONT_ICU_LOS_DAYS = cut(CONT_ICU_LOS_DAYS, 
                                 breaks = c(0, 1, 4, 11, max(CONT_ICU_LOS_DAYS)),
                                 include.lowest = TRUE)) %>%
  select(-SAPS_total_score, -AvdNamn)

# Bootstrapping the data to calculate CI's ----
library(broom)
library(mice)

# Plan on how to resolve futures
plan(multisession)

# Create a future containing the bootstrap sampling expression
future_bt_resamples <- future({
  bootstraps(glm_data, 
             times = 2000, 
             apparent = TRUE)
}, seed = 1444)

bt_resamples <- value(future_bt_resamples)

# Function to fit multiple imputed model on bootstrapped samples
mice_fit_glm <- function(splits, ...){
  mod <- mice(analysis(splits), method = "rf", m = 5, maxit = 5, seed = 1444) %>%
    with(., glm(ad_treatment ~ sex_female + age + edu + Hypertension + Diabetes +
                  CCIw + SAPS_consciousness_level + CONT_ICU_LOS_DAYS + any_imv + 
                  psych + saps_adj + yr,
                family = "binomial")
    )
  summary(pool(mod)) %>% 
    as_tibble() %>%
    select(-df) %>%
    mutate(term = as.character(term))
}

boot_models <- bt_resamples %>%
  mutate(model = map(splits, mice_fit_glm))

# Using the percentile method for CI
pctl_conf <- rsample::int_pctl(boot_models, model) %>%
  mutate(across(2:4, exp))

# Save the table
write_rds(pctl_conf, "data/glm_pct_conf.rds")
save.image("data/glm_environ.RData")
