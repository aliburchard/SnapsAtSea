rm(list = ls())

library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(tidyr)

library(ggplot2)
library(scales)
library(reldist)
library(ineq)

source("Scripts/functions.R")

sas <- read.csv("outputs/SAS_data_all.csv") %>% tbl_df %>% filter(experiment_status == "experiment")



# set sas timestamps to lubridate timestamps
# started_at = ymd_hms(gsub(x=started_at, pattern = "T|.[0-9]{3}Z", replacement = " ")), # you don't need to parse this because lubridate is FUCKING AMAZING
sas %<>% 
  mutate(., created_at = ymd_hms(created_at),
       started_at = ymd_hms(started_at),
       finished_at = ymd_hms(finished_at)) %>%
  mutate(., duration = as.numeric(difftime(finished_at, started_at, units = "secs")))



# Calculate Summary Statistics

# Grab coefficients - results depend a bit on how you treat non-logged-in users. Do you discard them altogether?
sas %>% 
  group_by(., project, experiment) %>%
  summarise(., total_class = n_distinct(classification_id), 
            total_users = n_distinct(user), # counts only registered volunteers
            duration = median(duration))

# Gini has to be calculated for only new vs old users
sas %>% filter(., user_status %in% c("new", "old")) %>%
  group_by(., project, experiment, user_name) %>%
  summarise(., num_classifications = n()) %>% #count the number of classifications per user
  summarise(., gini = gini(num_classifications), 
            total_class = sum(num_classifications), 
            total_users = n_distinct(user_name)) #grouped to experiment, so gini is per experiment

sas %>% filter(., experiment_status == "experiment", user_status %in% c("new", "old")) %>%
     group_by(., project, experiment, user_status, user_name) %>%
     summarise(., num_classifications = n()) %>% #count the number of classifications per user
     summarise(., gini = gini(num_classifications), 
               total_class = sum(num_classifications), 
               total_users = n_distinct(user_name)) #grouped to experiment, so gini is per experiment



#Grab Lorenz data
grab_lorenz <- function(data, class_per_user = "num_classifications") {
     class_per_user <- data[[class_per_user]]
     temp <- Lc(class_per_user)
     out <- data.frame(prop_user = temp$p, prop_class = temp$L)
     return(out)
}


# lorenz_by_wf <- sas %>% 
#      group_by(., workflow_name, user_name) %>%
#      summarise(., num_classifications = n()) %>%
#      do(grab_lorenz(., class_per_user = "num_classifications"))
# 
# pdf(file = "Figures/lorenz_within_SAS.pdf", width = 12, height = 7)
# ggplot(lorenz_by_wf, aes(x = prop_user, y = prop_class, color = workflow_name)) + 
#      geom_line(size = 1.5) + 
#      labs(x = "proportion of volunteers", y = "proportion of classifications") +
#      geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
# dev.off()

# # plot against other projects
# lorenz_all_proj <- all_dat %>% 
#      group_by(., experiment, user_name) %>%
#      summarise(., num_classifications = n()) %>%
#      do(grab_lorenz(., class_per_user = "num_classifications"))
# 
# pdf(file = "Figures/lorenz_all_projects.pdf", width = 12, height = 7)
# ggplot(lorenz_all_proj, aes(x = prop_user, y = prop_class, color = experiment)) + 
#      geom_line(size = 1.5) + 
#      labs(x = "proportion of volunteers", y = "proportion of classifications") +
#      geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
# dev.off()
# 
# 
# pdf(file = "Figures/lorenz_SAS_only.pdf", width = 12, height = 7)
# ggplot(filter(lorenz_all_proj, experiment %in% c("survey", "yesno")), aes(x = prop_user, y = prop_class, color = experiment)) + 
#      geom_line(size = 1.5) + 
#      labs(x = "proportion of volunteers", y = "proportion of classifications") +
#      geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
# dev.off()
# 
# 
### lorenz by user type
# lorenz_by_user <- sas %>% filter(., user_status != "not-logged-in") %>%
#      group_by(., experiment, user_status, user_name) %>%
#      summarise(., num_classifications = n()) %>%
#      do(grab_lorenz(., class_per_user = "num_classifications"))
# 
# pdf(file = "figures/lorenz_new_v_old.pdf", width = 12, height = 7)
# ggplot(filter(lorenz_by_user), aes(x = prop_user, y = prop_class, color = experiment, linetype = user_status)) +
#      geom_line(size = 1.5) +
#      labs(x = "proportion of volunteers", y = "proportion of classifications") +
#      geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
# dev.off()
# 
# # pdf(file = "Figures/lorenz_new_v_old_all.pdf", width = 12, height = 7)
# # ggplot(lorenz_by_user, aes(x = prop_user, y = prop_class, color = experiment, linetype = user_status)) + 
# #      geom_line(size = 1.5) + 
# #      labs(x = "proportion of volunteers", y = "proportion of classifications") +
# #      geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
# # dev.off()
# # 

lorenz <- sas %>% filter(user_status != "not-logged-in") %>%
  group_by(., project, experiment, user_status, user_name) %>%
  summarise(., num_classifications = n()) %>%
  do(grab_lorenz(., class_per_user = "num_classifications"))

pdf(file = "figures/Figure5.pdf", width = 12, height = 7)
jpeg("figures/Figure5.jpg")
ggplot(lorenz, aes(x = prop_user, y = prop_class, color = experiment, linetype = user_status)) + 
  geom_line(size = 1.5) + 
  labs(x = "proportion of volunteers", y = "proportion of classifications") +
  geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
dev.off()

######## Cumulative classifications

# too hard to compare when durations are so different
# cum_class_all_proj <- all_dat %>%
#      mutate(created_at = ymd_hms(created_at)) %>%
#      group_by(., project) %>%
#      arrange(., created_at) %>%
#      mutate(., project_start =  min(created_at), project_counter = row_number()) %>%
#      mutate(days_since_project_start = round(as.numeric(created_at - project_start, units = "days"), digits = 1))
# 
# ggplot(data = cum_class_all_proj, 
#        mapping = aes(x = days_since_project_start, colour = project)) +
#      geom_line(aes(y = project_counter))

# # Jumped through a lot of damn hoops to compare across projects, but that's just silly
# cum_class <- sas %>%
#      mutate(created_at = ymd_hms(created_at)) %>%
#      group_by(., experiment) %>%
#      arrange(., created_at) %>%
#      mutate(., experiment_start =  min(created_at), experiment_counter = row_number()) %>%
#      group_by(experiment, workflow_name) %>%
#      arrange(., created_at) %>%
#      mutate(workflow_start = min(created_at), workflow_counter = row_number()) %>%
#      mutate(days_since_workflow_start = round(as.numeric(created_at - workflow_start, units = "days"), digits = 1),
#             days_since_experiment_start = round(as.numeric(created_at - experiment_start, units = "days"), digits = 1))
# 
# workflow_launches <- cum_class %>%
#      mutate(created_at = ymd_hms(created_at)) %>%
#      group_by(., experiment, workflow_name) %>%
#      summarise(date = ymd_hms(min(created_at)), launch = min(days_since_experiment_start)) 

# # each workflow = own line, all start same date
# ggplot(data = filter(cum_class, experiment %in% c("survey", "yesno")), 
#        mapping = aes(x = days_since_workflow_start, colour = experiment, linetype = workflow_name)) +
#      geom_line(aes(y = workflow_counter))
# 
# # split by workflow, dates and lines
# ggplot(data = filter(cum_class, experiment %in% c("survey", "yesno")), 
#        mapping = aes(x = days_since_experiment_start, colour = experiment, linetype = workflow_name)) +
#      geom_line(aes(y = workflow_counter))
# 
# 
# ggplot(data = filter(cum_class, experiment %in% c("survey", "yesno")),
#        mapping = aes(x = days_since_experiment_start, colour = experiment)) +
#      geom_vline(data = workflow_launches, aes(xintercept = launch, linetype = workflow_name), colour = "light gray") +
#      geom_line(aes(y = experiment_counter)) + theme_bw()


launches <- read.csv("data/launch_times.csv", nrows = 10)[, 1:2]
launches %<>% mutate(., launch_time = ymd_hms(Date), Event = str_trim(Event, side = "both"))

# 
# pdf(file = "Figures/cumulative_classifications.pdf", width = 12, height = 7)
# ggplot(data = filter(cum_class, experiment %in% c("survey", "yesno")), 
#        mapping = aes(x = created_at, colour = experiment)) +
#      geom_vline(data = filter(launches, Event == "Newsletter"), aes(xintercept = as.numeric(launch_time)), colour = "dark blue", linetype = "dashed", size = 1.2) +
#      geom_vline(data = filter(launches, Event == "New Data"), aes(xintercept = as.numeric(launch_time)), colour = "dark gray", linetype = "dashed", size = 1.2) +
#      geom_line(aes(y = experiment_counter), size = 1.5) + theme_bw(base_size = 16) +
#      scale_x_datetime(name="",labels = date_format("%b %d")) +
#      labs(y = "cumulative classifications")
# dev.off()
# 
# 
# ### Instantaneous classification rates
# 
# head(sas)
# 
# 
# # group by day 
# daily_class <- sas %>% 
#      filter(., experiment_status == "experiment") %>%
#      mutate(hour = round_date(created_at, unit = "hour")) %>%
#      group_by(experiment, hour) %>%
#      summarise(hourly_classifications = n_distinct(classification_id), hourly_users = n_distinct(user_name))
# 
# 
# pdf("Figures/instantaneous_classifications.pdf", width = 12, height = 6)
# ggplot(daily_class, aes(x = hour, y = hourly_classifications)) + 
#      geom_line(aes(colour = experiment), size = 1.2) +
#      geom_vline(data = filter(launches, Event == "Newsletter"), aes(xintercept = as.numeric(launch_time)), colour = "dark blue", linetype = "dashed", size = 1.2) +
#      geom_vline(data = filter(launches, Event == "New Data"), aes(xintercept = as.numeric(launch_time)), colour = "dark gray", linetype = "dashed", size = 1.2) +
#      theme_bw(base_size = 16) + scale_x_datetime(name="",labels = date_format("%b %d")) 
# dev.off()




