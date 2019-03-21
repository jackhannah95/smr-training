################################################################
### SMR Training - Code
### LIST Team
###
### Original Author: Jack Hannah
### Original Date: 09 April 2018
### Last edited by: Jack Hannah
### Last edited on: 21 March 2019
###
### Written to be run on RStudio Server
###
### Packages required:
### odbc (for SMRA extraction);
### haven (for reading SPSS files);
### here (for defining filepaths);
### dplyr, tidylog, tidyr, purrr & janitor (for data manipulation);
### magrittr (for compound assignment pipe-operator %<>%);
### lubridate (for dates);
### openxlsx (for writing Excel files)
###
### This code is designed to provide an R equivalent to the existing SPSS 
### script used to train LIST analysts in SMR extraction and analysis



### Section 1: SPSS Equivalent functions ----


# Below is an approximate and non-exhaustive list of equivalent functions in R 
# and SPSS which are commonly used in analysis of SMR data
#
# 
# (R) arrange(x)              ==          (SPSS)  SORT CASES BY X (A)
# (R) arrange(desc(x))        ==          (SPSS)  SORT CASES BY X (D)
# (R) filter(x == 2)          ==          (SPSS)  SELECT IF X = 2
# (R) filter(x != 2)          ==          (SPSS)  SELECT IF NOT (X = 2)
# (R) select(x)               ==          (SPSS)  /KEEP X
# (R) select(-x)              ==          (SPSS)  /DROP X
# (R) mutate(x = 2)           ==          (SPSS)  COMPUTE X = 2
# (R) drop_na(x)              ==          (SPSS)  SELECT IF NOT (SYSMIS(X))
# (R) left_join(x, y)         ==          (SPSS)  MATCH FILES FILE = X
#                                                   /TABLE = Y
#                                                   /BY COMMON_VARIABLES
#
# (R) data %<>%               ==          (SPSS)  AGGREGATE OUTFILE = *
#       group_by(x) %>%                             /BREAK X
#       summarise(y = sum(y)) %>%                   /Y = SUM(Y)
#       ungroup()                         
#
# Several functions, such as first, last and substr are similar in both R and 
# SPSS



### Section 2: Housekeeping ----


# 2.1 - RStudio Projects

# This code uses RStudio Projects, which are a way of bundling together related 
# files and scripts
#
# RStudio Projects come with a .RProj file, and wherever this file is saved is 
# where RStudio sets the working directory, from which other filepaths can 
# be defined relatively using the here package
#
# Type 'getwd()' into the console to get the working directory for this project
#
# For more information on RStudio Projects, please visit
# https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects


# 2.2 - Install packages

# To install the odbc, here, tidylog and janitor packages, uncomment the four  
# lines of code below

# install.packages("odbc")
# install.packages("here")
# install.packages("tidylog")
# install.packages("janitor")

# Re-comment these lines after running, as packages only need to be installed 
# once


# 2.3 - Load packages
library(odbc)
library(haven)
library(here)
library(dplyr)
library(tidylog)
library(tidyr)
library(janitor)
library(magrittr)
library(lubridate)
library(openxlsx)



### Section 3: SMRA extraction ----


# 3.1 - Source SQL queries
source(here::here("code", "sql_queries.R"))


# 3.2 - Connect to SMRA tables using odbc connection
# The suppressWarnings function prevents your password from appearing in the 
# console if the connection is unsuccessful
channel <- suppressWarnings(
  dbConnect(odbc(),
            dsn = "SMRA",
            uid = .rs.askForPassword("What is your user ID?"),
            pwd = .rs.askForPassword("What is your LDAP password?")))


# 3.3 - Extract SMR01 data for D&G council area
# Required for all three exercises
smr1_extract <- as_tibble(dbGetQuery(channel, statement = query_smr1)) %>%
  
  # 'Clean' the variable names for a consistent naming style
  clean_names()


# 3.4 - Extract death data in 2015/16
# Required for exercise 3
deaths_extract <- as_tibble(dbGetQuery(channel, statement = query_deaths)) %>%
  clean_names()


# 3.5 - Close odbc connection
dbDisconnect(channel)



### Section 4: Reading in lookup files ----


# 4.1 - Read in D&G locality lookup file
dg_localities <- read_spss(here::here("data", "D&G_Localities.sav")) %>%
  clean_names()


# 4.2 - Read in D&G locality populations file
dg_pop <- read_spss(here::here("data", 
                               "201415_D&G_locality_populations.sav")) %>%
  clean_names() %>%
  
  # Financial year is incorrect - should be 2015/16
  # Remove this column - year will be added later
  select(-financial_year)


# 4.3 - Join locality lookup file to SMR1 extract
smr1_extract %<>%
  left_join(dg_localities, by = "datazone_2011")



### Section 5: Workshop Exercises ----


# Question 1:	Which locality has the highest rate of emergency admissions for 
# MI (I21-I22) in 2015/16?
# (Look for the diagnosis in main position of first episode only)

mi <- smr1_extract %>%
  
  # Arrange episodes into chronolgical order for each stay
  arrange(link_no, admission_date, record_type, sort_marker,
          discharge_date, admission, discharge, uri) %>%
  
  # Aggregate to stay level, taking the first admission type and first main 
  # diagnosis
  group_by(link_no, cis_marker) %>%
  summarise(locality = first(locality),
            cis_admission_date = first(admission_date),
            cis_discharge_date = last(discharge_date),
            cis_admission_type = first(admission_type),
            diagnosis_on_admission = first(main_condition)) %>%
  ungroup() %>%
  
  # Remove episodes with no locality
  # These would be included in a region total but cannot be attributed to a 
  # specific locality
  drop_na(locality) %>%
  
  # Filter only financial year of interest, 
  # stays with relevant main diagnosis and
  # only stays which begin as an emergency admission
  filter(cis_discharge_date %within% interval(dmy(01042015), dmy(31032016)),
         substr(diagnosis_on_admission, 1, 3) %in% c("I21", "I22"),
         substr(cis_admission_type, 1, 1) == "3") %>%
  mutate(fyear = "2015/16") %>%
  
  # Aggregate to find locality totals
  group_by(fyear, locality) %>%
  summarise(emergency_mi_admissions = n()) %>%
  ungroup() %>%
  
  # Join with locality populations file
  left_join(dg_pop, by = "locality") %>%
  
  # Calculate emergency admission rate per 1,000 population
  mutate(emergency_admission_rate = round(emergency_mi_admissions /
                                            population * 1000,
                                          digits = 2))


# Save output to Excel
# Commented out to avoid overwriting existing file every time this code is run
# mi %>%
#   write.xlsx(here::here("output", "SMR_Q1_output.xlsx"))



# Question 2: Which locality has the highest rate of multiple emergency 
# admissions for COPD (J40-J44) in 2015/16?
# (Look for diagnosis in all positions of all episodes)

copd <- smr1_extract %>%
  
  # Flag episodes where COPD diagnosis is present in any condition variable
  # Multiplying by 1 changes flag from true/false to 1/0
  mutate(copd_flag = purrr::pmap_dbl(select(., contains("condition")), 
                                     ~any(substr(c(...), 1, 1) == "J" &
                                            between(as.numeric(
                                              substr(c(...), 2, 3)), 40, 44), 
                                          na.rm = TRUE) * 1)) %>%
  
  # Arrange episodes into chronological order for each stay
  arrange(link_no, admission_date, record_type, sort_marker,
          discharge_date, admission, discharge, uri) %>%
  
  # Aggregate to stay level, taking the first admission type and including the 
  # COPD flag
  group_by(link_no, cis_marker) %>%
  summarise(locality = first(locality),
            cis_admission_date = first(admission_date),
            cis_discharge_date = last(discharge_date),
            cis_admission_type = first(admission_type),
            copd_flag = max(copd_flag)) %>%
  ungroup() %>%
  
  # Remove episodes with no locality
  # These would be included in a region total, but cannot be attributed to a 
  # specific locality
  drop_na(locality) %>%
  
  # Filter only financial year of interest,
  # only COPD admissions and
  # only emergency admissions
  filter(cis_discharge_date %within% interval(dmy(01042015), dmy(31032016)),
         copd_flag == 1,
         substr(cis_admission_type, 1, 1) == "3") %>%
  mutate(fyear = "2015/16") %>%
  
  # Aggregate to patient level, adding up the number of COPD emergency 
  # admissions for each patient
  group_by(fyear, locality, link_no) %>%
  summarise(emergency_admissions = n()) %>%
  ungroup() %>%
  
  # Categorise emergency admissions into bands
  mutate(emergency_admission_bands = case_when(
    emergency_admissions == 1 ~ "admissions_1",
    emergency_admissions == 2 ~ "admissions_2",
    emergency_admissions > 2 ~ "admissions_3_or_more")) %>%
  
  # Aggregate to get total number of patients within each band by locality
  group_by(fyear, locality, emergency_admission_bands) %>%
  summarise(patients = n()) %>%
  ungroup() %>%
  
  # Join with locality populations file
  left_join(dg_pop, by = "locality") %>%
  
  # Calculate multiple emergency admission rate per 1,000 population
  mutate(multiple_emergency_admission_rate = round(patients /
                                                     population * 1000,
                                                   digits = 2)) %>%
  
  # Restructure dataset by dropping patients and population variables and 
  # converting each emergency admission band into a variable with a 
  # corresponding multiple emergency admission rate
  select(-patients, -population) %>%
  spread(emergency_admission_bands, multiple_emergency_admission_rate)


# Save output to Excel
# Commented out to avoid overwriting existing file every time this code is run
# copd %>%
#   write.xlsx(here::here("output", "SMR_Q2_output.xlsx"))



# Question 3: Which locality has the highest 30-day mortality rate / 28-day 
# emergency readmission rate in 2015/16?

# Ensure deaths extract has no one dying on multiple occasions
deaths_extract %<>%
  group_by(link_no) %>%
  summarise(date_of_death = min(date_of_death)) %>%
  ungroup()


mort <- smr1_extract %>%
  
  # Arrange episodes into descending chronological order for each stay
  arrange(link_no, desc(admission_date), desc(record_type),
          desc(sort_marker), desc(discharge_date), desc(admission),
          desc(discharge), desc(uri)) %>%
  
  # Aggregate to stay level, taking the first admission type
  group_by(link_no, cis_marker) %>%
  summarise(locality = first(locality),
            cis_admission_date = first(admission_date),
            cis_discharge_date = last(discharge_date),
            cis_admission_type = first(admission_type)) %>%
  ungroup() %>%
  
  # Remove episodes with no locality
  # These would be included in a region total but cannot be attributed to a 
  # specific locality
  drop_na(locality) %>%
  
  # Filter only financial year of interest
  filter(cis_discharge_date %within% interval(dmy(01042015), dmy(31032016))) %>%
  mutate(fyear = "2015/16") %>%
  
  # Join with deaths extract
  left_join(deaths_extract, by = "link_no") %>%
  
  # Flag stays where the patient died within 30 days of discharge
  mutate(mortality_30_days = if_else(
    time_length(interval(cis_discharge_date, date_of_death), 
                unit = "day") <= 30,
    1, 0)) %>%
  
  # Patients with no date of death have NA for 30 day mortality flag so change 
  # these to 0
  replace_na(list(mortality_30_days = 0)) %>%
  
  # Arrange into descending stay order by patient for emergency readmission 
  # flagging
  arrange(link_no, desc(cis_marker)) %>%
  
  # Flag 28-day emergency readmissions
  mutate(readmission_28_days = if_else(
    link_no == c(0, head(link_no, -1)) &
      substr(lag(cis_admission_type), 1, 1) == 3 &
      time_length(interval(cis_discharge_date, lag(cis_admission_date)),
                  unit = "day") <= 28,
    1, 0)) %>%
  
  # Aggregate to find locality totals
  group_by(fyear, locality) %>%
  summarise(mortality_30_days = sum(mortality_30_days),
            readmission_28_days = sum(readmission_28_days)) %>%
  ungroup() %>%
  
  # Join with locality populations file
  left_join(dg_pop, by = "locality") %>%
  
  # Calculate 28-day emergency readmission rate and 30-day mortality rate
  mutate(mortality_rate = round(mortality_30_days /
                                  population * 1000,
                                digits = 2),
         emerg_readm_rate = round(readmission_28_days /
                                    population * 1000,
                                  digits = 2))

# Save output to Excel, dropping population, mortality rate and emergency 
# readmission rate variables
# Commented out to avoid overwriting existing file every time this code is run
# mort %>%
#   select(-(population:emerg_readm_rate)) %>%
#   write.xlsx(here::here("output", "SMR_Q3_output.xlsx"))



### END OF SCRIPT ###
