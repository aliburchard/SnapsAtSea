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
# 
# # Read Data
# q1 <- fread("data/yes-no-q1-classifications.csv") 
# q2 <- fread("data/yes-no-q2-classifications.csv") 
# q3 <- fread("data/yes-no-q3-classifications.csv") 
# q4 <- fread("data/yes-no-q4-classifications.csv")
# survey <- fread("data/survey-classifications.csv") 
# 
# 
# 
# ## Create list of old users
# user_dat <- do.call(rbind, list(q1, q2, q3, q4))
# 
# old_users <- user_dat %>% 
#      filter(., ymd_hms(created_at) < ymd("2017-06-06")) %>% 
#      distinct(user_name) %>%
#      group_non_logged()
# 
# write.csv(old_users, "outputs/previous_users.csv")




# #start date: 06-06-2017 - only keep records after that, well, sort of.
# lapply(list(q1, q2, q3, q4, survey), check_workflow)
# 
# # Since Q4 was active, need to drop extra classifications.
# x <- q4 %>% mutate(., created_at = ymd_hms(created_at)) %>%
#      filter(., created_at > ymd("2017-06-20")) %>%
#      group_by(., workflow_id, workflow_version) %>% 
#      arrange(ymd_hms(created_at)) %>% 
#      mutate(., counter = row_number(created_at))
# 
# ggplot(data = x, aes(created_at, y = counter)) + geom_line()
# 
# q4 %<>% filter(., ymd_hms(created_at) > ymd("2017-06-20")) 
# 
# dat <- lapply(X = list(q1, q2, q3, q4, survey), prep_data)
# combined <- do.call(what = rbind, args = dat)

# specify workflow-style
# combined %<>% mutate(., project = "snapshots", experiment = ifelse(workflow_name == "Survey", "survey", "yesno"))
# 
# # specify user-status
# dat <- lapply(X = list(q1, q2, q3, q4), keep_before)
# all_classifications <- do.call(what = rbind, args = dat)
# old_users <- all_classifications %>% distinct(user_name)
# write.csv(previous_users, "outputs/previous_users.csv")
# new_users <- combined %>% filter(., !(as.character(user_name) %in% as.character(old_users$user_name))) %>% distinct(user_name)
# old_user_list <- as.character(old_users$user_name)
# new_user_list <- as.character(new_users$user_name)
# 
# 
# combined_SAS <- combined %>% mutate( user_name = as.character(user_name)) %>%
#      mutate(user_status = ifelse(user_name == "not-logged-in", "not-logged-in",
#                                  ifelse(user_name %in% new_user_list, "new",  
#                                         ifelse(user_name %in% old_user_list, "old", "WTF"))))
# 
# write.csv(combined_SAS, "outputs/SAS_combined_classifications.csv", row.names = F)



# # Grab and compare to some other projects
# ele <- fread(input = "data/elephant-expedition-classifications.csv")
# check_workflow(ele)
# ele %<>% filter(., workflow_version == 30.30) %>%
#      prep_data(., start = "2017-02-27") %>%
#      mutate(., project = "elephant")
#
# wisc <- fread("data/snapshot-wisconsin-classifications.csv")
# check_workflow(wisc)
# wisc %<>% filter(., workflow_version %in% c(337.98, 552.99, 197.40, 189.40)) %>%
#      prep_data(., start = "2016-05-16") %>%
#      mutate(., project = "wisconsin")
#
# whales <- fread("data/whales-as-individuals-classifications.csv")
# check_workflow(whales) %>% View
# whales %<>% prep_data(., start = "2015-06-30") %>%
#      mutate(., project = "whales")
#
# kenya <- fread("data/wildwatch-kenya-classifications.csv")
# check_workflow(kenya) %>% View
# kenya %<>% prep_data(., start = "2017-06-19") %>%
#      mutate(., project = "kenya")
#
# other_proj <- do.call(rbind, list(ele, kenya, wisc, whales))
# other_proj <- other_proj %>% 
#      mutate(experiment = project, user_status = "NA")
# write.csv(other_proj, file = "outputs/other_projects_combined.csv", row.names = F)




combined <- read.csv("outputs/SAS_combined_classifications.csv")
other_proj <- read.csv("outputs/other_projects_combined.csv")
all_dat <- rbind(combined, other_proj)

combined %<>% tbl_df(.)
all_dat %<>% tbl_df(.)


# Grab coefficients
gini_dat_workflow <- combined %>% 
     group_by(., workflow_name, user_name) %>%
     summarise(., num_classifications = n()) %>% #count the number of classifications per user
     summarise(., gini = gini(num_classifications)) #grouped to experiment, so gini is per experiment

gini_dat <- all_dat %>% 
     group_by(., experiment, user_name) %>%
     summarise(., num_classifications = n()) %>% #count the number of classifications per user
     summarise(., gini = gini(num_classifications)) #grouped to experiment, so gini is per experiment

all_dat %>% 
     group_by(., experiment, user_status, user_name) %>%
     summarise(., num_classifications = n()) %>% #count the number of classifications per user
     summarise(., gini = gini(num_classifications)) #grouped to experiment, so gini is per experiment

# Summarize
workflow_summary <- combined %>% group_by(., workflow_name) %>% 
     summarise(., total_classifications = n(), total_users = n_distinct(user_name)) %>%
     left_join(., gini_dat_workflow)

overall_summary <- all_dat %>% group_by(., experiment) %>% 
     summarise(., total_classifications = n(), total_users = n_distinct(user_name)) %>%
     left_join(., gini_dat)

write.csv(overall_summary, "outputs/overall_summary.csv")
write.csv(workflow_summary, "outputs/workflow_summary.csv")
# Count up total amount of time spent on the workflow
# 
# data_out <- combined %>% 
#      group_by(., experiment, user_name) %>%
#      summarise(., num_classifications = n()) %>% #count the number of classifications per user
#      arrange(., experiment, num_classifications) %>% 
#      group_by(., experiment) %>% 
#      mutate(., id = row_number()) 

#Grab Lorenz data
grab_lorenz <- function(data, class_per_user = "num_classifications") {
     class_per_user <- data[[class_per_user]]
     temp <- Lc(class_per_user)
     out <- data.frame(prop_user = temp$p, prop_class = temp$L)
     return(out)
}


lorenz_by_wf <- combined %>% 
     group_by(., workflow_name, user_name) %>%
     summarise(., num_classifications = n()) %>%
     do(grab_lorenz(., class_per_user = "num_classifications"))

pdf(file = "Figures/lorenz_within_SAS.pdf", width = 12, height = 7)
ggplot(lorenz_by_wf, aes(x = prop_user, y = prop_class, color = workflow_name)) + 
     geom_line(size = 1.5) + 
     labs(x = "proportion of volunteers", y = "proportion of classifications") +
     geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
dev.off()

# plot against other projects
lorenz_all_proj <- all_dat %>% 
     group_by(., experiment, user_name) %>%
     summarise(., num_classifications = n()) %>%
     do(grab_lorenz(., class_per_user = "num_classifications"))

pdf(file = "Figures/lorenz_all_projects.pdf", width = 12, height = 7)
ggplot(lorenz_all_proj, aes(x = prop_user, y = prop_class, color = experiment)) + 
     geom_line(size = 1.5) + 
     labs(x = "proportion of volunteers", y = "proportion of classifications") +
     geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
dev.off()



pdf(file = "Figures/lorenz_SAS_only.pdf", width = 12, height = 7)
ggplot(filter(lorenz_all_proj, experiment %in% c("survey", "yesno")), aes(x = prop_user, y = prop_class, color = experiment)) + 
     geom_line(size = 1.5) + 
     labs(x = "proportion of volunteers", y = "proportion of classifications") +
     geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
dev.off()


### lorenz by user type
lorenz_by_user <- all_dat %>% filter(., user_status != "not-logged-in") %>%
     group_by(., experiment, user_status, user_name) %>%
     summarise(., num_classifications = n()) %>%
     do(grab_lorenz(., class_per_user = "num_classifications"))

pdf(file = "Figures/lorenz_new_v_old.pdf", width = 12, height = 7)
ggplot(filter(lorenz_by_user, project == "snapshots"), aes(x = prop_user, y = prop_class, color = experiment, linetype = user_status)) + 
     geom_line(size = 1.5) + 
     labs(x = "proportion of volunteers", y = "proportion of classifications") +
     geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
dev.off()

pdf(file = "Figures/lorenz_new_v_old_all.pdf", width = 12, height = 7)
ggplot(lorenz_by_user, aes(x = prop_user, y = prop_class, color = experiment, linetype = user_status)) + 
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

# Jumped through a lot of damn hoops to compare across projects, but that's just silly
cum_class <- combined %>%
     mutate(created_at = ymd_hms(created_at)) %>%
     group_by(., experiment) %>%
     arrange(., created_at) %>%
     mutate(., experiment_start =  min(created_at), experiment_counter = row_number()) %>%
     group_by(experiment, workflow_name) %>%
     arrange(., created_at) %>%
     mutate(workflow_start = min(created_at), workflow_counter = row_number()) %>%
     mutate(days_since_workflow_start = round(as.numeric(created_at - workflow_start, units = "days"), digits = 1),
            days_since_experiment_start = round(as.numeric(created_at - experiment_start, units = "days"), digits = 1))

workflow_launches <- cum_class %>%
     mutate(created_at = ymd_hms(created_at)) %>%
     group_by(., experiment, workflow_name) %>%
     summarise(date = ymd_hms(min(created_at)), launch = min(days_since_experiment_start)) 

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

pdf(file = "Figures/cumulative_classifications.pdf", width = 12, height = 7)
ggplot(data = filter(cum_class, experiment %in% c("survey", "yesno")), 
       mapping = aes(x = created_at, colour = experiment)) +
     geom_vline(data = workflow_launches, aes(xintercept = as.numeric(date)), colour = "dark gray", linetype = "dashed", size = 1.5) +
     geom_line(aes(y = experiment_counter), size = 1.5) + theme_bw(base_size = 16) +
     scale_x_datetime(name="",labels = date_format("%b %d")) +
     labs(y = "cumulative classifications")
dev.off()

