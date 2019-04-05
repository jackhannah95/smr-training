# Question 4: Which locality in D&G had the highest number of emergency 
# admissions for under 18s during 2015/16?

under_18 <- smr1_extract %>%
  
  # Arrange episodes into chronolgical order for each stay
  
  # Aggregate to stay level, taking the first admission type
  
  # Remove episodes with no locality
  # These would be included in a region total, but cannot be attributed to a 
  # specific locality
  
  # Calculate a person's age as at their admission date
  
  # Filter only financial year of interest and
  # only stays which begin as an emergency admission
  # for people aged < 18 when they were admitted

  # Aggregate to find locality totals


# Question 5: Which age band (<18, 18-34, 35-49, 50-64, 65+) had the highest 
# number of emergency admissions for falls (W00-W19) in D&G as a whole during 
# 2015/16?
# (Look for diagnosis in all positions of all episodes)
#
# Note that number of admissions by patient is not of interest in this instance

falls <- smr1_extract %>%