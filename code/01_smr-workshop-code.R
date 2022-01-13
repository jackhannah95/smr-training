################################################################
### SMR Training Code
###
### Original Author: Jack Hannah
### Original Date: 09 April 2018
### Last edited by: Jack Hannah
### Last edited on: 20 March 2019
###
### Written to be run on RStudio Server
###
### Packages required:
### odbc (for SMRA extraction);
### haven (for reading SPSS files);
### here (for defining filepaths);
### dplyr, tidyr, tidylog, purrr & janitor (for data manipulation);
### magrittr (for compound assignment pipe-operator %<>%);
### lubridate (for dates);
### writexl (for writing Excel files)



### Section 1: Housekeeping ----


# 1.1 - Install packages

# In the 'Packages' pane in the bottom right of the screen, there is a 'User 
# Library' and a 'System Library'

# If any of the odbc, haven, here, dplyr, tidyr, tidylog, purrr, janitor, 
# magrittr, lubridate or writexl packages are not contained within your User 
# Library, uncomment the relevant lines of code below to install them

# Packages need to be re-loaded every time you re-start R, but they only need 
# to be installed once
# Please re-comment or delete the relevant lines below once you've installed  
# the necessary packages and ignore this section in future

# install.packages("odbc")
# install.packages("haven")
# install.packages("here")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("tidylog")
# install.packages("purrr")
# install.packages("janitor")
# install.packages("magrittr")
# install.packages("lubridate")
# install.packages("writexl")


# 1.2 - Load packages
library(odbc)
library(haven)
library(here)
library(dplyr)
library(tidyr) # Ensure tidyr version is >= 1.0.0
library(tidylog)
library(janitor)
library(magrittr)
library(lubridate)
library(writexl)



### Section 2: SMRA extraction ----


# 2.1 - Source SQL queries
# Both the `here` and `lubridate` packages have a function called `here`, and 
# because `lubridate` was loaded later, `lubridate::here` masks `here::here`
# To circumvent issues with masking, either edit the order in which packages  
# are loaded, or explicitly declare which package a function comes from, e.g. 
# use `lubridate::here` or `here::here` instead of `here`
source(here::here("code", "00_sql-queries.R"))


# 2.2 - Connect to SMRA tables using odbc connection
# The suppressWarnings function prevents your password from appearing in the 
# console if the connection is unsuccessful
channel <- suppressWarnings(
  dbConnect(odbc(),
            dsn = "SMRA",
            uid = .rs.askForPassword("What is your user ID?"),
            pwd = .rs.askForPassword("What is your LDAP password?")))


# 2.3 - Extract SMR01 data for D&G council area
# Required for all exercises
smr1_extract <- as_tibble(dbGetQuery(channel, statement = query_smr1)) %>%
  
  # 'Clean' the variable names for a consistent naming style
  clean_names()


# 2.4 - Extract death data in 2015/16
# Required for exercise 3
deaths_extract <- as_tibble(dbGetQuery(channel, statement = query_deaths)) %>%
  clean_names()


# 2.5 - Close odbc connection
dbDisconnect(channel)



### Section 3: Reading in lookup files ----


# 3.1 - Read in D&G locality lookup file
dg_localities <- read_spss(here::here("data", "D&G_Localities.sav")) %>%
  clean_names()


# 3.2 - Read in D&G locality populations file
dg_pop <- read_spss(here::here("data",
                               "D&G_locality_populations.sav")) %>%
  clean_names()


# 3.3 - Join locality lookup file to SMR1 extract
smr1_extract %<>%
  left_join(dg_localities, by = "datazone_2011")



### Section 4: Workshop Exercises ----


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
  mutate(financial_year = "2015/16") %>%
  
  # Aggregate to find locality totals
  group_by(financial_year, locality) %>%
  summarise(emergency_mi_admissions = n()) %>%
  ungroup() %>%
  
  # Join with locality populations file
  left_join(dg_pop, by = "locality") %>%
  
  # Calculate emergency admission rate per 1,000 population
  # Type `?round_half_up` into the console for explanation of the difference 
  # between `janitor::round_half_up` and `base::round`
  mutate(emergency_admission_rate = round_half_up(emergency_mi_admissions /
                                                    population * 1000,
                                                  digits = 2))


# Save output to Excel
mi %>%
  write_xlsx(here::here("output", "q1_smr-output.xlsx"), 
             format_headers = FALSE)


# Question 2: Which locality has the highest rate of multiple emergency 
# admissions for COPD (J40-J44) in 2015/16?
# (Look for diagnosis in all positions of all episodes)

copd <- smr1_extract %>%
  
  # Flag episodes where COPD diagnosis is present in any condition variable
  # Multiplying by 1 changes flag from true/false to 1/0
  mutate(copd_flag = purrr::pmap_dbl(select(., contains("condition")),
                                     ~any(grepl("^J4[0-4]", .x),
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
  mutate(financial_year = "2015/16") %>%
  
  # Aggregate to patient level, adding up the number of COPD emergency 
  # admissions for each patient
  group_by(financial_year, locality, link_no) %>%
  summarise(emergency_admissions = n()) %>%
  ungroup() %>%
  
  # Categorise emergency admissions into bands
  mutate(emergency_admission_bands = case_when(
    emergency_admissions == 1 ~ "admissions_1",
    emergency_admissions == 2 ~ "admissions_2",
    emergency_admissions > 2 ~ "admissions_3_or_more")) %>%
  
  # Aggregate to get total number of patients within each band by locality
  group_by(financial_year, locality, emergency_admission_bands) %>%
  summarise(patients = n()) %>%
  ungroup() %>%
  
  # Join with locality populations file
  left_join(dg_pop, by = "locality") %>%
  
  # Calculate multiple emergency admission rate per 1,000 population
  mutate(multiple_emergency_admission_rate = round_half_up(patients /
                                                             population * 1000,
                                                           digits = 2)) %>%
  
  # Restructure dataset by dropping the patients and population variables and 
  # converting each emergency admission band into a variable with a 
  # corresponding multiple emergency admission rate
  pivot_wider(
    id_cols = c(-patients, -population),
    names_from = emergency_admission_bands,
    values_from = multiple_emergency_admission_rate
  )


# Save output to Excel
copd %>%
  write_xlsx(here::here("output", "q2_smr-output.xlsx"), 
             format_headers = FALSE)



# Question 3: Which locality has the highest 30-day mortality rate / 28-day 
# emergency readmission rate in 2015/16?

# Ensure deaths extract has no one dying on multiple occasions
deaths_extract %<>%
  group_by(link_no) %>%
  summarise(date_of_death = min(date_of_death)) %>%
  ungroup()


mort <- smr1_extract %>%
  
  # Arrange episodes into ascending chronological order for each stay
  arrange(link_no, admission_date, record_type, sort_marker, discharge_date, 
          admission, discharge, uri) %>%
  
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
  mutate(financial_year = "2015/16") %>%
  
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
  
  # Arrange into ascending stay order by patient for emergency readmission 
  # flagging
  arrange(link_no, cis_marker) %>%
  
  # Flag 28-day emergency readmissions
  mutate(readmission_28_days = if_else(
    link_no == c(tail(link_no, -1), 0) &
      substr(lead(cis_admission_type), 1, 1) == 3 &
      time_length(interval(cis_discharge_date, lead(cis_admission_date)),
                  unit = "day") <= 28,
    1, 0)) %>%
  
  # Aggregate to find locality totals
  group_by(financial_year, locality) %>%
  summarise(mortality_30_days = sum(mortality_30_days),
            readmission_28_days = sum(readmission_28_days)) %>%
  ungroup() %>%
  
  # Join with locality populations file
  left_join(dg_pop, by = "locality") %>%
  
  # Calculate 28-day emergency readmission rate and 30-day mortality rate
  mutate(mortality_rate = round_half_up(mortality_30_days /
                                          population * 1000,
                                        digits = 2),
         emerg_readm_rate = round_half_up(readmission_28_days /
                                            population * 1000,
                                          digits = 2))

# Save output to Excel, dropping population, mortality rate and emergency 
# readmission rate variables
mort %>%
  select(-(population:emerg_readm_rate)) %>%
  write_xlsx(here::here("output", "q3_smr-output.xlsx"),
             format_headers = FALSE)



### END OF SCRIPT ###
