db <- conDb("database") # Connect database

dbWriteTable(db, 'pts', cohort['LopNr'], temporary = TRUE) # Write a temporary table to the DB with the cohort id's

# ATC code for antidepressants = N06A
# N06AA = non-selective MAOi such as clomipramine and amitriptylene, 
# likely prescribed for other indication than depression but are here 
# included in order to use the same selection as EvO.

antidepressants_dispensed <- 
  "SELECT LopNr, ATC, EDATUM, FDATUM, subnamn
 FROM LMED
 WHERE ATC LIKE '%N06A%'
  AND LopNr IN (SELECT LopNr FROM pts);" %>%
  sqlQuery() %>% 
  as_tibble() %>%
  group_by(LopNr) %>%
  distinct(EDATUM) %>%
  mutate(date = EDATUM)

dbDisconnect(db)
rm(db)

observation_dates <- 
  cohort %>% 
  select(LopNr, sir_adm_date, DODSDAT_ROUND_MID, CONT_ICU_ID) %>%
  distinct() %>%
  mutate(start_date = sir_adm_date %m-% years(5), # To be able to handle leap years, %m-% must be used (see lubridate::)
         end_date = case_when(!is.na(DODSDAT_ROUND_MID) & DODSDAT_ROUND_MID <= sir_adm_date %m+% years(2) ~ DODSDAT_ROUND_MID,
                              sir_adm_date %m+% years(2) > date('2024-12-31') ~ date('2024-12-31'),
                              TRUE ~ sir_adm_date %m+% years(2))
  ) %>%
  select(-DODSDAT_ROUND_MID) %>%
  pivot_longer(cols = start_date:end_date, values_to = 'date') %>%
  select(-name) %>%
  group_by(CONT_ICU_ID) %>%
  complete(date = seq.Date(min(date), max(date), by = 'day')) %>%
  fill(sir_adm_date, .direction = 'down') %>%
  fill(LopNr, .direction = 'down')

antidepressant_exposure <- 
  observation_dates %>%
  left_join(antidepressants_dispensed) %>%
  arrange(date, .by_group = TRUE) %>%
  rename(last_exp = EDATUM) %>%
  fill(last_exp, .direction = 'down') %>%
  mutate(exposure = data.table::fifelse(!is.na(last_exp) & date <= last_exp + days(90), TRUE, FALSE),
         follow_time = as.integer(date-min(date)),
         relative_time = as.integer(date-sir_adm_date))

antidepressant_graph <- antidepressant_exposure %>% 
  group_by(relative_time, exposure) %>%
  summarise(count = n()) %>%
  mutate(per = 100 * count/sum(count)) %>%
  filter(exposure,
         relative_time > -600 & relative_time <600)

# Antidepressant-free cases are pt's with no dispensed prescriptions within
# 365 days before ICU-admission
no_prior_antidepressants <-
  antidepressant_exposure %>%
  filter(date == sir_adm_date) %>%
  mutate(prior_ad = case_when(is.na(last_exp) ~ FALSE,
                              last_exp < sir_adm_date - days(365) ~ FALSE,
                              last_exp >= sir_adm_date - days(365) ~ TRUE)) %>%
  filter(!prior_ad) %>%
  select(CONT_ICU_ID) %>%
  pull()

# Post-ICU exposure to antidepressants are calculated for 365 days after admission
exposed_cases <-
  antidepressant_exposure %>%
  filter(date > sir_adm_date & last_exp > sir_adm_date & last_exp <= sir_adm_date + days(365)) %>%
  select(CONT_ICU_ID) %>%
  pull()

# Create a new list of the current study cohort
df <- NULL

df$epinic <- 
  cohort %>%
  filter(CONT_ICU_ID %in% no_prior_antidepressants) %>%
  filter(mort365 == 0) %>% # Patients who have not been followed for a year are excluded along with patients who died within a year
  mutate(ad_treatment = if_else(CONT_ICU_ID %in% exposed_cases, TRUE, FALSE),
         edu = factor(case_when(between(as.numeric(edu), 1, 2) ~ "Primary & Lower Secondary",
                                between(as.numeric(edu), 3, 4) ~ "Upper Secondary",
                                between(as.numeric(edu), 5, 6) ~ "Tertiary",
                                edu == 7 ~ 'Research Degree'),
                      levels = c('Primary & Lower Secondary',
                                 'Upper Secondary',
                                 'Tertiary',
                                 'Research Degree'),
                      ordered = TRUE),
         yr = year(sir_adm_date)) %>%
  select(CONT_ICU_ID, 
         ad_treatment, 
         
         # Demographics
         sex_female, 
         age, 
         edu, 
         
         # Co-morbidities
         Hypertension, 
         Diabetes, 
         `Mood Disorder`, 
         `Anxiety Disorder`, 
         `Psychotic Disorder`, 
         `Substance Abuse`, 
         ASAH, 
         ICH, 
         `Ischaemic Stroke`, 
         CCIw,
         
         # ICU-variables
         SAPS_AMV, 
         SAPS_consciousness_level, 
         SAPS_total_score,
         vent_duration_minutes,
         CONT_ICU_LOS_MINS,
         yr,
         AvdNamn
  ) %>%
  mutate(CONT_ICU_LOS_DAYS = CONT_ICU_LOS_MINS/(60*24),
         sex_female = as.logical(sex_female),
         any_imv = if_else(!is.na(vent_duration_minutes) | !is.na(SAPS_AMV), TRUE, FALSE),
         ad_treatment = if_else(ad_treatment, TRUE, FALSE),
         SAPS_consciousness_level = factor(SAPS_consciousness_level, levels = c("I (GCS ≥13)", 
                                                                                "II (GCS 7-12)",
                                                                                "III (GCS 6)",
                                                                                "IV (GCS 5)",
                                                                                "V (GCS ≤4)"),
                                           ordered = TRUE),
         AIS = `Ischaemic Stroke`,
         .keep = 'unused')

# Check the levels of ordered factors to make sure the order is correct
df$epinic$edu
df$epinic$SAPS_consciousness_level

# Remove intermediate variables
rm(antidepressant_exposure, antidepressants_dispensed, observation_dates)

# Save RDS 
readr::write_rds(df$epinic %>% select(-CONT_ICU_ID), "data/df_epinic_365.rds")
readr::write_rds(df$epinic, "data/df_epinic_incl_id_365.rds")
readr::write_rds(antidepressant_graph, "data/df_epinic_ad_graph_365.rds")