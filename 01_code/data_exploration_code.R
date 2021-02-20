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
library(readr)
library(purrr)
library(lubridate)

# read in trends_up_to data
trends_up_files <- list.files(path = '02_raw_data/Data_Exploration_Rawdata/Lab3_Rawdata', 
                              pattern = 'trends_up_', full.names = TRUE)

# compile data into 1 df
trends_data <- trends_up_files %>% 
  map(read_csv) %>% 
  rbindlist()

# read in Most+Recent+Cohorts file and id_name_link
# drop all universities that share the same name
score_card <- read_csv('02_raw_data/Data_Exploration_Rawdata/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv')

id_name_link <- read_csv('02_raw_data/Data_Exploration_Rawdata/Lab3_Rawdata/id_name_link.csv')
id_name_link <- id_name_link %>% 
  rename(UNITID = unitid, OPEID = opeid) %>% 
  distinct(schname, .keep_all = TRUE)

# join id_name_link with score_card
id_sc_merged <- merge(x = id_name_link, y = score_card, by = c('UNITID', 'OPEID'), all.x = TRUE)

# join sc_id_merged with trends_data
id_sc_trends_merged <- merge(x = id_sc_merged, y = trends_data, by = 'schname', all.x = TRUE) %>% 
  na.omit(id_sc_trends_merged)

# determine low-earning vs. high-earning colleges
# select data to work with, filter for BS, rename variables to something understandable
# filter out Nulls and Privacy Suppressed median earnings and convert to numeric
working_data <- id_sc_trends_merged %>% 
  select('UNITID', 'OPEID', 'INSTNM', 'PREDDEG', 'keyword', 'monthorweek', 'keynum', 'index',
         'md_earn_wne_p10-REPORTED-EARNINGS') %>% 
  rename(inst_name = INSTNM, pred_degree = PREDDEG, 
         median_earnings = 'md_earn_wne_p10-REPORTED-EARNINGS') %>% 
  filter(pred_degree == 3) %>% 
  filter(median_earnings != 'NULL') %>% 
  filter(median_earnings != 'PrivacySuppressed') %>% 
  mutate(median_earnings = as.numeric(median_earnings))

# median salary == 41800. This is the line dividing high earning vs. low earning
median_earnings_threshold <- median(working_data$median_earnings)

# standardize trends data Index by keynum:
# 
standardized_trends_index <- working_data %>%
  group_by(keynum) %>%
  mutate(standardized_index = (index - mean(index, na.rm = TRUE)) / sd(index)) %>% 
  summarize(inst_name, keyword, monthorweek, keynum, median_earnings, standardized_index)


# get 'prior to Sept 2015' and 'post Sept 2015'
pre_sept_2015 <- standardized_trends_index %>%
  mutate(by_year = substr(monthorweek, 1, 10)) %>% 
  mutate(by_year = as.Date(by_year)) %>% 
  filter(by_year <= '2015-08-31')

post_sept_2015 <- standardized_trends_index %>%
  mutate(by_year = substr(monthorweek, 1, 10)) %>% 
  mutate(by_year = as.Date(by_year)) %>% 
  filter(by_year > '2015-08-31')




 
  filter(by_month >= 2015-09)
  group_by(by_month) %>% 
  summarize(UNITID, OPEID, inst_name, keyword, median_earnings, standardized_trends)

# should "start of September 2015" be accounted for in regression???
  


  
  



