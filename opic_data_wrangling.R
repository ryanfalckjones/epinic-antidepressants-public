
source("source/init.R")

# Establish SQLite Connection
opic <- conDb("opic_db")

# Read SQL-query
opic_query <- readr::read_file("source/opic_cte.sql")

# Read the OPIC Db to memory and process it to remove all neurovascular main diagnoses
opic.sir <- dbGetQuery(opic, opic_query) %>%
  as_tibble() %>%
  mutate(sir_adm_date = ymd(str_sub(sir_adm_time, start = 1L, end = 10L)),
         sir_dsc_date = ymd(str_sub(sir_dsc_time, start = 1L, end = 10L)),
         icu_los = as.numeric(ymd_hm(sir_dsc_time) - ymd_hm(sir_adm_time))/60, # Minutes
         female_sex = if_else(grepl('K', sex, ignore.case = T), TRUE, FALSE),
         age = as.integer(age),
         across(contains("DODSDAT_ROUND"), ~ymd(.))) %>%
  mutate(DODSDAT_ROUND_DOWN = if_else(DODSDAT_ROUND_DOWN <= sir_dsc_date, sir_dsc_date + 1, DODSDAT_ROUND_DOWN),
         DODSDAT_ROUND_MID = if_else(DODSDAT_ROUND_MID <= sir_dsc_date, sir_dsc_date + 1, DODSDAT_ROUND_MID)) %>%
  anti_join(dbGetQuery(opic, "SELECT LopNr
                              FROM SIR_DIAGNOSER
                              WHERE ICD10 LIKE 'I6%';"
  )
  ) %>%
  mutate(InskrTidpunkt = sir_adm_time) %>%
  calculate_mortality("2018-12-31") %>%
  select(-InskrTidpunkt) %>%
  as_tibble()

# Get LISA information 

LISA <- NULL
LISA$y17 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_old FROM LISA_2017") %>%
  mutate(yr = 2017)
LISA$y16 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_Old FROM LISA_2016") %>%
  mutate(yr = 2016)
LISA$y15 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_Old FROM LISA_2015") %>%
  mutate(yr = 2015)
LISA$y14 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_Old FROM LISA_2014") %>%
  mutate(yr = 2014)
LISA$y13 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_Old FROM LISA_2013") %>%
  mutate(yr = 2013)
LISA$y12 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_Old FROM LISA_2012") %>%
  mutate(yr = 2012)
LISA$y11 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_Old FROM LISA_2011") %>%
  mutate(yr = 2011)
LISA$y10 <- dbGetQuery(opic, "SELECT LopNr, Sun2000niva_Old FROM LISA_2010") %>%
  mutate(yr = 2010)

opic.lisa <- imap(LISA, ~rename(.x, edu = 2)) %>% 
  reduce(full_join) %>% 
  mutate(edu = if_else(edu == '*', NA, edu)) %>% # LISA uses * as NA-val
  semi_join(opic.sir, by = 'LopNr') %>% # Only retain patients in SIR
  left_join(opic.sir %>% select(LopNr, sir_adm_date)) %>%
  filter(yr == as.numeric(year(sir_adm_date))) 

rm(LISA)

# Get comorbidity information

opic.comorbs <- opic.sir %>% 
  select(LopNr, sir_adm_date) %>%
  rcmswe::rcmswe('opic_db', LMED = T) %>%
  mutate(sir_adm_date = ymd(index_date))

# Get information from LMED regarding antidepressants

# Write a temporary table to the DB with the opic.sir id's
dbWriteTable(opic, 'pts', opic.sir['LopNr'], temporary = TRUE)

# ATC code for antidepressants = N06A
# N06AA = non-selective MAOi such as clomipramine and amitriptylene, 
# likely prescribed for other indication than depression but are here 
# included in order to use the same selection as opic.

opic.antidepressants_dispensed <- 
  dbGetQuery(opic, 
             "SELECT LopNr, ATC, EDATUM, FDATUM, subnamn
 FROM LMED
 WHERE ATC LIKE '%N06A%'
  AND LopNr IN (SELECT LopNr FROM pts);") %>%
  dtplyr::lazy_dt() %>%
  group_by(LopNr) %>%
  distinct(EDATUM) %>%
  as_tibble()

opic.study_pop <- opic.sir %>% 
  lazy_dt() %>%
  left_join(opic.antidepressants_dispensed) %>% 
  mutate(prev_exposure = fifelse(EDATUM %within% interval(sir_adm_date - days(180), sir_adm_date), TRUE, FALSE)) %>% 
  group_by(CONT_ICU_ID) %>%
  filter(!any(prev_exposure, na.rm = TRUE)) %>%
  filter(!mort365) %>%
  mutate(ad_treatment = fifelse(EDATUM %within% interval(sir_adm_date, sir_adm_date + days(365)), TRUE, FALSE)) %>%
  select(-EDATUM) %>%
  distinct() %>%
  group_by(CONT_ICU_ID) %>%
  summarise(ad_treatment = sum(ad_treatment, na.rm = TRUE),
            across(!starts_with("ad_treatment"), first)) %>%
  as_tibble()

df <- NULL
df$opic <- opic.study_pop %>%
  left_join(semi_join(opic.comorbs, opic.study_pop)) %>%
  left_join(opic.sir) %>%
  left_join(opic.lisa %>% select(-yr)) %>%
  mutate(edu = factor(case_when(between(as.numeric(edu), 1, 2) ~ "Primary & Lower Secondary",
                                between(as.numeric(edu), 3, 4) ~ "Upper Secondary",
                                between(as.numeric(edu), 5, 6) ~ "Tertiary",
                                edu == 7 ~ 'Research Degree'),
                      levels = c('Primary & Lower Secondary',
                                 'Upper Secondary',
                                 'Tertiary',
                                 'Research Degree'),
                      ordered = TRUE),
         yr = year(sir_adm_date),
         sex_female = if_else(str_detect(sex, "K"), TRUE, FALSE)) %>%
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
         CCIw,
         
         # ICU-variables
         SAPS_consciousness_level, 
         SAPS_total_score,
         icu_los,
         yr
  ) %>%
  mutate(CONT_ICU_LOS_DAYS = icu_los/(60*24),
         sex_female = as.logical(sex_female),
         ad_treatment = as.logical(ad_treatment),
         SAPS_consciousness_level = factor(SAPS_consciousness_level, levels = c("I (GCS ≥13)", 
                                                                                "II (GCS 7-12)",
                                                                                "III (GCS 6)",
                                                                                "IV (GCS 5)",
                                                                                "V (GCS ≤4)"),
                                           ordered = TRUE),
         .keep = 'unused')

df$opic <- df$opic %>%
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
         CCIw = relevel(factor(case_when(CCIw == 0 ~ "0",
                                         CCIw == 1 ~ "1",
                                         CCIw >1 ~ ">1"), ordered = FALSE), ref = "0"),
         CONT_ICU_LOS_DAYS = cut(CONT_ICU_LOS_DAYS, 
                                 breaks = c(0, 1, 4, 11, max(CONT_ICU_LOS_DAYS, na.rm = T)),
                                 include.lowest = TRUE)) %>%
  ungroup() %>%
  distinct()

readr::write_rds(df$opic %>% select(-CONT_ICU_ID, -SAPS_total_score), "data/df_opic.rds")
readr::write_rds(df$opic %>% select(-CONT_ICU_ID), "data/df_opic_tab.rds")
readr::write_rds(df$opic %>% select(-SAPS_total_score), "data/df_opic_incl_id.rds")
