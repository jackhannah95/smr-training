# Question 4: Which locality in D&G has the highest number of emergency 
# admissions for under 18s during 2015/16?

under_18 <- smr1_extract %>%
  
  # Arrange episodes into chronolgical order for each stay
  arrange(link_no, admission_date, record_type, sort_marker,
          discharge_date, admission, discharge, uri) %>%
  
  # Aggregate to stay level, taking the first admission type
  group_by(link_no, cis_marker) %>%
  summarise(dob = first(dob),
            locality = first(locality),
            cis_admission_date = first(admission_date),
            cis_discharge_date = last(discharge_date),
            cis_admission_type = first(admission_type)) %>%
  ungroup() %>%
  
  # Remove episodes with no locality
  # These would be included in a region total, but cannot be attributed to a 
  # specific locality
  drop_na(locality) %>%
  
  # Calculate a person's age as at their admission date
  # Note - use 'dob' variable
  mutate(admission_age = floor(time_length(interval(dob, cis_admission_date), 
                                           unit = "years"))) %>%
  
  # Filter only financial year of interest and
  # only stays which begin as an emergency admission
  # for people aged < 18 when they were admitted
  filter(cis_discharge_date %within% interval(dmy(01042015), dmy(31032016)),
         substr(cis_admission_type, 1, 1) == "3",
         admission_age < 18) %>%
  mutate(financial_year = "2015/16") %>%
  
  # Aggregate to find locality totals
  group_by(financial_year, locality) %>%
  summarise(emergency_admissions = n()) %>%
  ungroup()



# Question 5: Which age band (<18, 18-34, 35-49, 50-64, 65+) has the highest 
# number of emergency admissions for falls (W00-W19) in D&G as a whole during 
# 2015/16?
# (Look for diagnosis in all positions of all episodes)
#
# Note that number of admissions by patient is not of interest in this instance

falls <- smr1_extract %>%
  
  # Flag episodes where falls diagnosis is present in any condition variable
  mutate(falls_flag = purrr::pmap_dbl(select(., contains("condition")),
                                      ~any(grepl("^W[0-1][0-9]", c(...)),
                                           na.rm = TRUE) * 1)) %>%
  
  # Arrange episodes into chronolgical order for each stay
  arrange(link_no, admission_date, record_type, sort_marker,
          discharge_date, admission, discharge, uri) %>%
  
  # Aggregate to stay level, taking the first admission type and including the 
  # falls flag
  group_by(link_no, cis_marker, council_area) %>%
  summarise(dob = first(dob),
            cis_admission_date = first(admission_date),
            cis_discharge_date = last(discharge_date),
            cis_admission_type = first(admission_type),
            falls_flag = max(falls_flag)) %>%
  ungroup() %>%
  
  # Filter only financial year of interest,
  # only falls admissions and
  # only stays which begin as an emergency admission
  filter(cis_discharge_date %within% interval(dmy(01042015), dmy(31032016)),
         falls_flag == 1,
         substr(cis_admission_type, 1, 1) == "3") %>%
  mutate(financial_year = "2015/16") %>%
  
  # Calculate a person's age as at their admission date and classify this into
  # the relevant age bands
  # Note - use 'dob' variable
  mutate(admission_age = floor(time_length(interval(dob, cis_admission_date), 
                                           unit = "years")),
         age_band = case_when(
           admission_age < 18 ~ "Under 18",
           between(admission_age, 18, 34) ~ "18-34",
           between(admission_age, 35, 49) ~ "35-49",
           between(admission_age, 50, 64) ~ "50-64",
           admission_age >= 65 ~ "65+")) %>%
  
  # Aggregate to find totals by age band
  group_by(financial_year, council_area, 
           age_band = forcats::fct_relevel(age_band, "Under 18")) %>%
  summarise(emergency_admissions = n()) %>%
  ungroup()
