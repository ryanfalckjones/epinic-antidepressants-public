# findDiagnosis() ----

# findDiagnosis() takes two parameters (data = a dataframe, icd = a vector of icd-codes to search for) 
# and creates a column for each ICD-code containing a boolean variable T/F of the search result

findDiagnosis <- function(data, icd){
  data %>%
    transmute(!!paste0(icd) := str_detect(DIAGNOS, icd))
}

# createMorbVars() ----

# createMorbVars() takes two parameters (data = a dataframe, icd = a vector of icd-codes to search for) 
# and creates a column with the name 'ICD-code'_morbX where X is:
  
# 1 for <= 30 days
# 2 for <= 60 days
# 3 for <= 90 days
# 4 for <= 1 year
# 5 for <= 2 years
# 6 for <= 5 years


createMorbVars <- function(data, icd){
  data %>%
    transmute(!!paste0(icd, '_morb1') := .[[!!icd]] & d30,
              !!paste0(icd, '_morb2') := .[[!!icd]] & d60,
              !!paste0(icd, '_morb3') := .[[!!icd]] & d90,
              !!paste0(icd, '_morb4') := .[[!!icd]] & d365,
              !!paste0(icd, '_morb5') := .[[!!icd]] & y2,
              !!paste0(icd, '_morb6') := .[[!!icd]] & y5,)
}

# findRx() ----
# Identically structured search functions are also created for prescribed drugs

findRx <- function(data, atc.list){
  data %>%
    transmute(!!paste0(atc.list) := str_detect(ATC, atc.list))
}

# createRxVars() ----
createRxVars <- function(data, atc.list){
  data %>%
    transmute(!!paste0(atc.list, '_rx1') := .[[!!atc.list]] & d30,
              !!paste0(atc.list, '_rx2') := .[[!!atc.list]] & d60,
              !!paste0(atc.list, '_rx3') := .[[!!atc.list]] & d90,
              !!paste0(atc.list, '_rx4') := .[[!!atc.list]] & d365,
              !!paste0(atc.list, '_rx5') := .[[!!atc.list]] & y2,
              !!paste0(atc.list, '_rx6') := .[[!!atc.list]] & y5,)
}

# sqlTbl() ----
# A quick function to return all tables in database
sqlTbl <- function(){
  dbListTables(db)
}

# sqlFields() ----
# A quick function for returning fields in a given table
sqlFields <- function(tbl){
  dbListFields(db, tbl)
}

# sqlQuery() ----
# A quick function for querying db
sqlQuery <- function(query){
  dbGetQuery(db, query)
}

# conDb ----
# A quick function for connecting to the DB
conDb <- function(path = "db.sqlite"){
  dbConnect(SQLite(), path, extended_types = TRUE) # Connect to DB
}

build_query <- function(paths, final_sql_query){
  paste(paste(purrr::map(paths, ~readr::read_file(.)), collapse = ", "), final_sql_query)
}

# Local functions

# Helper function to calculate mortality
calculate_mort <- function(DODSDAT_ROUND_MID, InskrTidpunkt, end_of_study, duration) {
  case_when(
    is.na(DODSDAT_ROUND_MID) ~ 0,
    DODSDAT_ROUND_MID > end_of_study ~ NA_integer_,
    as.duration(InskrTidpunkt %--% DODSDAT_ROUND_MID) <= duration ~ 1,
    as.duration(InskrTidpunkt %--% DODSDAT_ROUND_MID) > duration ~ 0
  )
}

calculate_mortality <- function(data, end_of_study) {
  durations <- list(
    mort7 = days(7),
    mort30 = days(30),
    mort90 = days(90),
    mort180 = days(180),
    mort365 = days(365),
    mort5y = years(5)
  )
  
  # Ensure date columns are properly formatted
  data <- data %>%
    mutate(across(c(InskrTidpunkt, DODSDAT_ROUND_MID), as.Date))
  
  # Apply the helper function to each duration and bind the results
  results <- map2(names(durations), durations, ~ {
    data %>%
      mutate(!!sym(.x) := calculate_mort(DODSDAT_ROUND_MID, InskrTidpunkt, end_of_study, .y), .keep = "none")
  })
  
  bind_cols(data, list_cbind(results))
}

# saps_less_age_gcs() ----
# Create a function for calculating saps without age and GCS

saps_less_age_gcs <- function(saps, age, gcs_cat){
  
  age_points <- case_when(age < 40 ~ 0,
                          between(age, 40, 59) ~ 5,
                          between(age, 60, 69) ~ 9,
                          between(age, 70, 74) ~ 13,
                          between(age, 75, 79) ~ 15,
                          age > 79 ~ 18)
  
  gcs_cat <- as_factor(gcs_cat) %>% as.numeric
  
  gcs_points <- case_when(gcs_cat == 1 ~ 0,
                          gcs_cat == 2 ~ 2,
                          gcs_cat == 3 ~ 7,
                          gcs_cat == 4 ~ 10, 
                          gcs_cat == 5 ~ 15)
  
  return(saps - age_points - gcs_points)
}