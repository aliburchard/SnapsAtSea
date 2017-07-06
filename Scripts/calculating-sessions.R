library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(jsonlite)
library(tidyjson)
library(ggplot2)

source("scripts/functions.R")

sas <- read.csv("outputs/SAS_data_all.csv")
sas %<>% tbl_df %>% filter(., experiment_status == "experiment")

# calculate session
user_sesh <- sas %>% 
     mutate(created_at = ymd_hms(created_at),
            started_at = ymd_hms(started_at),
            finished_at = ymd_hms(finished_at)) %>%
     arrange(user_status, user_name, created_at) %>%
     group_by(user_status, user_name) %>%
     mutate(duration = as.numeric(difftime(finished_at, started_at, units = "secs"))) %>%
     mutate(lag = difftime(created_at, lag(created_at), units = "mins")) %>%
     mutate(new_session = ifelse(lag > 30 | is.na(lag), 1, 0)) %>% 
     mutate(session = cumsum(new_session)) %>%
     select(-lag, -new_session)

head(user_sesh)

sessions <- user_sesh %>% group_by(user_status, user_name, session) %>% 
     summarise(classifications_per_session = n_distinct(classification_id),
               classification_duration = median(duration),
               first = min(created_at), 
               last = max(created_at),
               #session_length = max(created_at)-min(created_at),
               workflows = as.factor(ifelse(n_distinct(experiment) == 1, as.character(first(experiment)), "both"))) %>%
     mutate(session_length = last - first, user_lifetime = max(last) - min(first))
glimpse(sessions)



ggplot(sessions, aes(classifications_per_session)) + geom_density(aes(fill = user_status), alpha = .4)

ggplot(sessions, aes(x = workflows, y = classifications_per_session)) + geom_boxplot()
ggplot(sessions, aes(x = workflows, y = session_length)) + geom_boxplot()
ggplot(sessions, aes(x = workflows, y = user_lifetime)) + geom_boxplot()

## In a given session, if you classify on both workflows, which do you spend the most time on?
sessions_splits <- user_sesh %>% group_by(user_status, user_name, session, experiment) %>% 
     summarise(classifications_per_workflow_per_session = n_distinct(classification_id)) %>%
     mutate(., total_class = sum(classifications_per_workflow_per_session)) %>%
     summarise(percent_survey_per_session = ifelse(n_distinct(experiment) == 2, min(classifications_per_workflow_per_session[experiment == "survey"]/total_class), NA))
glimpse(sessions_splits)

x <- sessions %>% filter(., user_status != "not-logged-in") %>%
     left_join(., sessions_splits) %>%
     mutate(., percent_survey = ifelse(workflows == "yesno", 0, ifelse(workflows == "survey", 100, percent_survey)))

# If users classified on both workflows in a session, NEW users split their time equally, whereas old users favored the yes/no workflow
ggplot(filter(x, workflows == "both"), aes(x = user_status, y = percent_survey)) + geom_boxplot()

