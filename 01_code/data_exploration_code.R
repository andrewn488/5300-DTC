# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Date: 2/19/2021
# Data Exploration Project


# DATA: 
# 'trends_up_to_....csv': These are files generated using Google Trends. 
  # They are the Google Trends index for each keyword for the given 'monthorweek'. 
  # Each keyword (indexed with 'keynum') is selected to be reflective of a university in the United States, 
  # given by 'schname'. Multiple files. 
# 'Most+Recent+Cohorts+(Scorecard+Elements).csv': 
  # This is data from the College Scorecard, a simple dataset that contains lots of information about 
  # United States colleges and the students that graduate from them. The variable names aren’t super helpful 
  # but they are documented in CollegeScorecardDataDictionary-09-08-2015.csv
# CollegeScorecardDataDictionary-09-08-2015.csv: variable definition guide for
  # 'Most+Recent+Cohorts+(Scorecard+Elements).csv'
# 'id_name_link.csv': which can be used to match colleges as identified in the Scorecard data 
  # (by 'unitid' and 'opeid' / 'UNITID' and 'OPEID') with colleges as identified in the Google Trends data 
  # (by 'schname'). The join functions will be helpful (see help(join) after loading the tidyverse)

# IMPORTANT: subtract the mean and divide by standard deviation
  # drop all dups on 'schname'

# DELIVERABLES:
  # produce at least 1 regression
  # produce at least 1 graph
  # conclusion based on results

# Things to think about: 
  # where is the line drawn between "high-earning" vs. "low-earning" colleges?
  # what level should the analysis be at? use group_by() and summarize() to change levels
  # type of regression model, how to interpret, and WHY!????


# load libraries: 
library(tidyverse)
library(data.table)
library(jtools)
library(vtable)
library(purrr)

file_name_pattern <- str_sub('trends_up_', 1, 10)
trends_up_files <- list.files(path = '../Lab3_Rawdata/', pattern = file_name_pattern, full.names = TRUE) %>% 
  map(fread)

# %>% rbindlist()
# fread() - used for csv files

process_file <- function(df) {
  
}

## NHK code below: ##

filelist <- list.files(path = '../Monthly_reports/', pattern = 'sales', full.names = TRUE)

process_file <- function(df) {
  sales <- df[1,3]
  employee <- df[42,2]
  return(data.table(sales = sales, employee = employee))
}

compiled_data <- filelist %>%
  map(read_excel) %>%
  map(process_file) %>%
  rbindlist()
