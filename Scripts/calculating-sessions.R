library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(jsonlite)
library(tidyjson)
library(ggplot2)

source("scripts/functions.R")

sas <- read.csv("outputs/SAS_data_all.csv")
sas_dat <- sas %>% tbl_df %>% filter(., experiment_status == "experiment") %>%
     filter(user_status != "not-logged-in") %>%
     mutate(duration = as.numeric(difftime(finished_at, started_at, units = "secs"))) %>% 
     filter(duration < 120)

# calculate session
user_sesh <- sas_dat %>% 
     mutate(created_at = ymd_hms(created_at),
            started_at = ymd_hms(started_at),
            finished_at = ymd_hms(finished_at)) %>%
     arrange(user_status, user_name, created_at) %>%
     group_by(user_status, user_name) %>%
     mutate(duration = as.numeric(difftime(finished_at, started_at, units = "secs"))) %>%
     mutate(lag = difftime(created_at, lag(created_at), units = "mins")) %>%
     mutate(new_session = ifelse(lag > 30 | is.na(lag), 1, 0)) %>% 
     mutate(session = cumsum(new_session)) %>%
     select(-new_session)

head(user_sesh)

sessions <- user_sesh %>% group_by(user_status, user_name, session) %>% 
     summarise(classifications_per_session = n_distinct(classification_id),
               classification_duration = median(duration),
               first = min(created_at), 
               last = max(created_at),
               n_device = n_distinct(device),
               device = first(device),
               workflows = as.factor(ifelse(n_distinct(experiment) == 1, as.character(first(experiment)), "both"))) %>%
     ungroup %>%
     mutate(session_length = difftime(last, first, units = "mins"))
glimpse(sessions)


sessions %>% group_by(workflows) %>% 
     summarise(sl = mean(session_length), sl.med = median(session_length), class = mean(classifications_per_session), class.med =median(classifications_per_session), max(session_length))

sas %>% filter(experiment_status == "experiment") %>% group_by(experiment, user_status) %>% summarise(n_distinct(user_name), n())

write.csv(sessions, "outputs/session_data.csv")

pdf("figures/classifications_per_session.pdf")
ggplot(sessions, aes(x = workflows, y = classifications_per_session, color = device)) + geom_boxplot() + 
     theme_bw(base_size = 16) + scale_y_log10()
dev.off()

pdf("figures/session_length.pdf")
ggplot(sessions, aes(x = workflows, y = as.numeric(session_length), color = device)) + geom_boxplot() +
     theme_bw(base_size = 16) + scale_y_log10()
dev.off()


ggplot(sessions, aes(x = workflows, y = classifications_per_session))

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

pdf("figures/percentage_survey_per_workflow.pdf")
ggplot(filter(x, workflows == "both"), aes(x = user_status, y = percent_survey)) + geom_boxplot()
dev.off()
