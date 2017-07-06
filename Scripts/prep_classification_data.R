rm(list = ls())


library(data.table)
library(dplyr)
library(dtplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(reldist)
library(ineq)

source("Scripts/functions.R")

# # This requires data created by grab_classification_metadata.R
# # so, source("Scripts/grab_classification_metadata.R") # THIS TAKES ABOUT 30 Min to run.
# 
# # Read Data
# q1 <- fread("data/yes-no-q1-classifications.csv")
# q2 <- fread("data/yes-no-q2-classifications.csv")
# q3 <- fread("data/yes-no-q3-classifications.csv")
# q4 <- fread("data/yes-no-q4-classifications.csv")
# survey <- fread("data/survey-classifications.csv")
# 
# # Filter to active workflows and classify as pre-post experiment.
# # Each workflow is a bit different, so read these in and clean accordingly.
# experiment_start_times <- read.csv("data/workflow_starts.csv")
# 
# survey_cleaned <- survey %>% trim_cols %>%
#      filter(., workflow_version %in% c(27.30, 29.30)) %>% #clean to real workflows, specify before and after experiment
#      mutate(., experiment_status = ifelse(ymd_hms(created_at) > ymd_hms("2017-06-07 14:58:00"), "experiment", "non-experiment"))
# 
# q1_cleaned <- q1 %>% trim_cols %>%
#      filter(., workflow_version == 6.20) %>% #clean to real workflows, specify before and after experiment
#      mutate(., experiment_status = ifelse(ymd_hms(created_at) >= ymd_hms("2017-06-07 14:58:00"), "experiment", "non-experiment"))
# 
# q2_cleaned <- q2 %>% trim_cols %>%
#      filter(., workflow_version == 3.24) %>% #clean to real workflows, specify before and after experiment
#      mutate(., experiment_status = ifelse(ymd_hms(created_at) >= ymd_hms("2017-06-12 08:46:11"), "experiment", "non-experiment"))
# 
# q3_cleaned <- q3 %>% trim_cols %>%
#      filter(., workflow_version == 4.16) %>% #clean to real workflows, specify before and after experiment
#      mutate(., experiment_status = ifelse(ymd_hms(created_at) >= ymd_hms("2017-06-16 15:38:00"), "experiment", "non-experiment"))
# 
# q4_cleaned <- q4 %>% trim_cols %>%
#      filter(., workflow_version == 4.11) %>% #clean to real workflows, specify before and after experiment
#      mutate(., experiment_status = ifelse(ymd_hms(created_at) >= ymd_hms("2017-06-21 09:32:09"), "experiment", "non-experiment"))
# 
# # Combine datasets and specify experiment branch
# combined <- rbind(survey_cleaned, q1_cleaned, q2_cleaned, q3_cleaned, q4_cleaned) %>%
#      mutate(., project = "snapshots", experiment = ifelse(workflow_name == "Survey", "survey", "yesno"))
# 
# # Group non-logged-in users and specify user-status
# old_users <- combined %>%
#      filter(., ymd_hms(created_at) < ymd("2017-06-06")) %>%
#      distinct(user_name) %>%
#      group_non_logged()
# 
# combined_SAS_dat <- combined %>% group_non_logged %>%
#      mutate(., user_status = ifelse(user == "not-logged-in", "not-logged-in",
#           ifelse(user_name %in% old_users$user_name, "old", "new")))
# 
# 
# # Grab metadata that was extracted from the JSON
# classification_metadata <- fread("outputs/SAS_dat_with_metadata.csv")
# sas_dat <- left_join(combined_SAS_dat, classification_metadata)
# write.csv(sas_dat, "outputs/SAS_data_all.csv", row.names = F)
# 

# # Grab and compare to some other projects
# ele <- fread(input = "data/elephant-expedition-classifications.csv")
# check_workflow(ele)
# ele %<>% filter(., workflow_version == 30.30) %>%
#      trim_cols %>%
#      filter(., ymd_hms(created_at) > ymd("2017-02-27")) %>%
#      mutate(., project = "elephant")
# 
# 
# wisc <- fread("data/snapshot-wisconsin-classifications.csv")
# check_workflow(wisc)
# wisc %<>% filter(., workflow_version %in% c(337.98, 552.99, 197.40, 189.40)) %>%
#      trim_cols %>%
#      filter(., ymd_hms(created_at) > ymd("2016-05-16")) %>%
#      mutate(., project = "wisconsin") 
# 
# whales <- fread("data/whales-as-individuals-classifications.csv")
# check_workflow(whales) %>% View
# whales %<>% 
#      trim_cols %>%
#      filter(., ymd_hms(created_at) > ymd("2015-06-30")) %>%
#      mutate(., project = "whales")
# 
# kenya <- fread("data/wildwatch-kenya-classifications.csv")
# check_workflow(kenya) %>% View
# kenya %<>% 
#      trim_cols %>%
#      filter(., ymd_hms(created_at) > ymd("2017-06-19")) %>%
#      mutate(., project = "kenya")
# 
# other_proj <- do.call(rbind, list(ele, kenya, wisc, whales))
# 
# other_proj <- other_proj %>%
#   group_non_logged %>%
#   mutate(experiment = project, user_status = ifelse(user == "not-logged-in", "not-logged-in", "registered")) %>%
#   mutate(., experiment_status = "non-experiment") %>% 
#   select(subject_ids:created_at, experiment_status, project:device)
# 
# # pull in metadata from other projects, just to compare!
# write.csv(other_proj, file = "outputs/other_projects_combined.csv", row.names = F)


# # Now combine other projects with classification metadata
# other_proj <- fread("outputs/other_projects_combined.csv")
# 
# # Grab metadata that was extracted from the JSON
# ele_metadata <- fread("outputs/ele_dat_with_metadata.csv") 
# whales_metadata <- fread("outputs/whale_dat_with_metadata.csv") 
# wisc_metadata <- fread("outputs/wisc_dat_with_metadata.csv") 
# kenya_metadata <- fread("outputs/kenya_dat_with_metadata.csv") 
# 
# other_meta <- rbind(ele_metadata, whales_metadata, wisc_metadata, kenya_metadata)
# all_other_dat <- left_join(other_proj, other_meta) 
# write.csv(all_other_dat, "outputs/other_proj_all.csv", row.names = F)


