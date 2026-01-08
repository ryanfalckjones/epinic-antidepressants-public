library(tidyverse)

if (!exists("cohort")) source("init.R")
if (!exists("df$epinic")) df_epinic_incl_id <- readr::read_rds("df_epinic_incl_id.rds")
absence <- readr::read_rds("data/absence.rds") %>%
  left_join(select(filter(cohort, age < 61), c(LopNr, sir_adm_date, DODSDAT_ROUND_DOWN)), relationship = "many-to-many") %>%
  filter(sir_adm_date < ymd("2023-01-01")) %>%
  filter(is.na(DODSDAT_ROUND_DOWN) | DODSDAT_ROUND_DOWN > (sir_adm_date + years(2))) %>%
  select(-DODSDAT_ROUND_DOWN)

prior <- absence %>%
  filter(!is.na(sir_adm_date) & int_overlaps(time_interval, interval(start = sir_adm_date - days(366), end = sir_adm_date - days(2)))) %>%
  mutate(int_id = row_number(.),
         time_start = ymd(int_start(time_interval)), 
         time_end = ymd(int_end(time_interval))) %>%
  pivot_longer(cols = time_start:time_end) %>%
  group_by(int_id) %>%
  mutate(date = list(seq(min(value), max(value), by = "1 day"))) %>%
  filter(name == "time_start") %>%
  select(-name, -value) %>% 
  unnest(date) %>%
  ungroup() %>%
  select(-int_id, -time_interval) %>%
  filter(date %within% interval(start = sir_adm_date - days(366), end = sir_adm_date - days(2))) %>%
  group_by(LopNr, sir_adm_date, date, leave_type) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(LopNr, sir_adm_date, date) %>%
  summarise(tot_leave_factor = sum(leave_factor)) %>%
  mutate(tot_leave_factor = if_else(tot_leave_factor > 1, 1, tot_leave_factor)) %>%
  ungroup() %>%
  group_by(LopNr, sir_adm_date) %>%
  summarise(total_days_prior = sum(tot_leave_factor)) %>%
  ungroup()

post <- absence %>%
  filter(!is.na(sir_adm_date) & int_overlaps(time_interval, interval(end = sir_adm_date + days(2*365), start = sir_adm_date + days(366)))) %>%
  mutate(int_id = row_number(.),
         time_start = ymd(int_start(time_interval)), 
         time_end = ymd(int_end(time_interval))) %>%
  pivot_longer(cols = time_start:time_end) %>%
  group_by(int_id) %>%
  mutate(date = list(seq(min(value), max(value), by = "1 day"))) %>%
  filter(name == "time_start") %>%
  select(-name, -value) %>% 
  unnest(date) %>%
  ungroup() %>%
  select(-int_id, -time_interval) %>%
  filter(date %within% interval(end = sir_adm_date + days(2*365), start = sir_adm_date + days(366))) %>%
  group_by(LopNr, sir_adm_date, date, leave_type) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(LopNr, sir_adm_date, date) %>%
  summarise(tot_leave_factor = sum(leave_factor)) %>%
  mutate(tot_leave_factor = if_else(tot_leave_factor > 1, 1, tot_leave_factor)) %>%
  ungroup() %>%
  group_by(LopNr, sir_adm_date) %>%
  summarise(total_days_post = sum(tot_leave_factor)) %>%
  ungroup()

sjp_sa <- full_join(prior, post, by = c("LopNr", "sir_adm_date")) %>% 
  mutate(across(3:4, ~if_else(is.na(.), 0, .))) %>% 
  mutate(delta = total_days_post - total_days_prior) %>%
  select(-starts_with("total")) %>%
  full_join(
    df_epinic_incl_id %>% 
      select(CONT_ICU_ID, ad_treatment) %>% 
      left_join(cohort %>% select(CONT_ICU_ID, LopNr, sir_adm_date)) %>% 
      select(-CONT_ICU_ID)
  ) %>%
  filter(!is.na(ad_treatment)) %>%
  mutate(delta = if_else(is.na(delta), 0, delta)) %>%
  filter(sir_adm_date > ymd("2011-12-31"))