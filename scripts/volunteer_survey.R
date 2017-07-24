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

survey <- read.csv("data/survey_responses.csv")
names <- c("Timestamp", "id", "other_projects", "type_projects", "age", "gender", "education", "workflow", "prefer_workflow", "why", "device")
names(survey) <- names

survey %>% filter(prefer_workflow == "Survey Workflow") %>% summary
survey %>% filter(prefer_workflow == "Yes/No Workflow") %>% summary

ed_levels <- c("Prefer not to say", "High school or lower", "Some university", "University degree", "Graduate degree")

data <- survey %>% filter(workflow == "Both") %>% 
     select(., id, age, gender, education, prefer_workflow) %>%
     mutate(education = factor(education, ed_levels)) 

age_plot <- ggplot(data, aes(x = age)) + geom_bar(aes(fill = prefer_workflow)) + theme_bw() + theme(legend.position = "none")
gender_plot <- ggplot(data, aes(x = gender)) + geom_bar(aes(fill = prefer_workflow)) + theme_bw() + theme(legend.position = "none")
ed_plot <- ggplot(data, aes(x = education)) + geom_bar(aes(fill = prefer_workflow)) + theme_bw() + theme(legend.position = "none")

quartz()
grid.arrange(age_plot, gender_plot, ed_plot, ncol = 3)
ggplot(data, aes(x = workflow_preference, fill = prefer_workflow)) + geom_bar() + facet_grid(. ~ variable, scale = "free_x")

