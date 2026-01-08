setwd("~/Local Documents/GitHub/epinic_asah_antidepressants/")
# Load dependencies
if(!require('rcmswe')) devtools::install_github('ryanfalckjones/rcmswe') # Contains function to get pt history
library(data.table)
library(tidyverse)
library(DBI)
library(RSQLite)
library(dtplyr)
library(gtsummary)
library(gt)
source('source/functions.R')

# Import data, requires the VPN to be logged in for access to server
whole_cohort <- read_rds("data/epinic_master_0_0_9_250515.rds") %>% 
  distinct() %>% 
  filter(between(sir_adm_date, ymd("2013-01-01"), ymd("2023-12-31")))
cohort <- whole_cohort %>% filter(str_detect(DX_GROUP, "ASAH")) %>% distinct()