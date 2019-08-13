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

###########################################
# content use by intended award all user
###########################################


content_use_all <- overall_summary_doc_user %>% 
  group_by(intended_award) %>% 
  summarize(total_n = length(content),
            total_mean_content = mean(content),
            total_sd_content = sd(content),
            total_median_content = median(content),
            total_max_content = max(content),
            total_min_content = min(content))
            
# content use by intended award active-user use

content_use_active <- overall_summary_doc_user %>% 
  filter(content > 0) %>% 
  group_by(intended_award) %>% 
  summarize(active_n = length(content),
            active_mean_content = mean(content),
            active_sd_content = sd(content),
            active_median_content = median(content),
            active_max_content = max(content),
            active_min_content = min(content))

# content non use by intended award
content_use_nonuser <- overall_summary_doc_user %>% 
  filter(content == 0) %>% 
  group_by(intended_award) %>% 
  summarize(non_user_n = length(content))

content_use_all <- content_use_nonuser %>% 
  select(non_user_n, intended_award) %>% 
  merge(content_use_all, by = "intended_award")

content_use_all$percentuse <- 100 * (content_use_all$total_n - content_use_all$non_user_n) / content_use_all$total_n

content_use_all %>% select(intended_award,percentuse)

#################################
# content use by tier 4 status
#################################

content_use_all <- overall_summary_doc_user %>% 
  group_by(tier4) %>% 
  summarize(total_n = length(content),
            total_mean_content = mean(content),
            total_sd_content = sd(content),
            total_median_content = median(content),
            total_max_content = max(content),
            total_min_content = min(content))

# content use by intended award active-user use

content_use_active <- overall_summary_doc_user %>% 
  filter(content > 0) %>% 
  group_by(tier4) %>% 
  summarize(active_n = length(content),
            active_mean_content = mean(content),
            active_sd_content = sd(content),
            active_median_content = median(content),
            active_max_content = max(content),
            active_min_content = min(content))

# content non use by intended award
content_use_nonuser <- overall_summary_doc_user %>% 
  filter(content == 0) %>% 
  group_by(tier4) %>% 
  summarize(non_user_n = length(content))

content_use_all <- content_use_nonuser %>% 
  select(non_user_n, tier4) %>% 
  merge(content_use_all, by = "tier4")

content_use_all$percentuse <- 100 * (content_use_all$total_n - content_use_all$non_user_n) / content_use_all$total_n

content_use_all %>% select(tier4, percentuse)

