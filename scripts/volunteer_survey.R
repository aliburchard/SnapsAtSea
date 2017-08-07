# flow chart for movement bewteen different worklow versions
# Plot normalized instantaneous 
# circos plots for user movement




# Evaluate projects
# media attention, task difficulty, stargazing or not
# inequality, classification rates, talk comments, users, unique users active within first hundred days
# is more unequal "bad"?
# really challenging projects might benefit from some superusers who are really good?
# brooke's paper w/ gini coefficients

library(tidyverse)

vol_survey <- read.csv("data/survey_responses.csv", na.strings = "")
names <- c("Timestamp", "id", "other_projects", "type_projects", "age", "gender", "education", "workflow", "prefer_workflow", "why", "device")
names(vol_survey) <- names

vol_survey %>% filter(prefer_workflow == "Survey Workflow") %>% summary
vol_survey %>% filter(prefer_workflow == "Yes/No Workflow") %>% summary

prefer_workflow_levels <- c("Survey Workflow", "Prefer not to say", "Yes/No Workflow", "Both")
color_levels <- c()
ed_levels <- c("Prefer not to say", "High school or lower", "Some university", "University degree", "Graduate degree")

data <- vol_survey %>% filter(workflow == "Both") %>% 
     select(., id, age, gender, education, prefer_workflow) %>%
     mutate(education = factor(education, ed_levels), prefer_workflow = factor(prefer_workflow, prefer_workflow_levels)) 


age_plot <- ggplot(filter(data, age != "Prefer not to say"), aes(x = age)) + geom_bar(aes(fill = prefer_workflow), position = "fill") + theme_bw() + theme(legend.position = "none")
gender_plot <- ggplot(filter(data, gender != "Prefer not to say"), aes(x = gender)) + geom_bar(aes(fill = prefer_workflow), position = "fill") + theme_bw() + theme(legend.position = "none")
ed_plot <- ggplot(filter(data, education != "Prefer not to say"), aes(x = education)) + geom_bar(aes(fill = prefer_workflow), position = "fill") + 
     theme_bw() + theme(legend.position = "bottom")

legend <- ggplot(data, aes(x = education)) + geom_bar(aes(fill = prefer_workflow)) + theme_bw() + theme(legend.position = "bottom")

quartz()
grid.arrange(age_plot, gender_plot, ed_plot, nrow = 3)
legend
data %>% gr
