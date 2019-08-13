#############################################
# Overall Summary of User Activity
#
#
#############################################

library(readxl)
library(janitor)
library(tidyverse)
# Access / Application
# the only items accessed are Journal and content																	


# read in list of all students
# downloaded from biology department on 08-08-19

students <- read_excel("../data/students.xlsx") %>% 
  clean_names()

# read in list of VLE use
# download from VLE 05-08-19 and covered period 01-09-18 to 05-08-19
file <-  "../data/Overall Summary of User Activity_MG.xlsx"
overall_summary <- read_excel(file, 
                              sheet = "Activity" ) %>% 
  clean_names()

# read in list of excludes
exclude <- read.table("../data/exclude_users.txt",
                      header = TRUE,
                      stringsAsFactors = FALSE)


# exclude the usernames in exclude from all overall_summary
overall_summary <- overall_summary %>% subset(!(username %in% exclude$username))

# exclude the usernames in exclude from all students
students <- students %>% subset(!(username %in% exclude$username))

# merge dataframes keeping all rows in overall_summary
# some student data will be missing for vle users since they have been removed
# but should be possible to get their information - has been requested from the
# office
# students in the list of documented students that were not in the list of registered user
# will not be include

overall_summary_user <-  overall_summary %>% 
  merge(students, by = "username", 
        all.x = TRUE) 

# Get list of usernames for people in VLE but not in student list 
# these are students who graduated or staff

undoc_user <- overall_summary_user %>% 
  filter(is.na(student_code)) %>% 
  select(name, username)

# work with the documented users
# these are students in years 2, 3 and 4, (1, 2, 3 as was in 2018-19 academic year)
overall_summary_doc_user <- overall_summary_user %>% 
  filter(!username %in% undoc_user$username)


# intended award all user

overall_summary_doc_user %>% 
  group_by(intended_award) %>% 
  summarize(number = length(content),
            mean_content = mean(content),
            sd_content = sd(content),
            median_content = median(content),
            max_content = max(content),
            min_content = min(content))
            
# intended award active-user use

overall_summary_doc_user %>% 
  filter(content > 0) %>% 
  group_by(intended_award) %>% 
  summarize(number = length(content),
            mean_content = mean(content),
            sd_content = sd(content),
            median_content = median(content),
            max_content = max(content),
            min_content = min(content))
