################################################################
### SMR Training - SQL queries
###
### Original Author: Jack Hannah
### Original Date: 09 April 2018
###
### Written to be run on RStudio Server
###
### This script defines the SQL queries sourced and used in the main
### training script



# Query 1: Extract SMR01 data for D&G council area ----
query_smr1 <- paste("SELECT ADMISSION_DATE, ADMISSION_TYPE, ADMISSION,",
                    "CIS_MARKER, COUNCIL_AREA, DATAZONE_2011, DISCHARGE,",
                    "DISCHARGE_DATE, DOB, MAIN_CONDITION, OTHER_CONDITION_1,",
                    "OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4,",
                    "OTHER_CONDITION_5, LINK_NO, RECORD_TYPE, SORT_MARKER, URI",
                    "FROM ANALYSIS.SMR01_PI",
                    "WHERE COUNCIL_AREA = '08' AND",
                    "DISCHARGE_DATE >= TO_DATE('2014-01-01', 'yyyy-mm-dd')")



# Query 2: Extract death data in 2015/16 ----
query_deaths <- paste("SELECT LINK_NO, DATE_OF_DEATH",
                      "FROM ANALYSIS.GRO_DEATHS_C",
                      "WHERE", 
                      "DATE_OF_DEATH >= TO_DATE('2015-04-01', 'yyyy-mm-dd')")
