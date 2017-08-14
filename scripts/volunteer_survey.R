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
library(gridExtra)
library(forcats)

vol_survey <- read.csv("data/survey_responses.csv", na.strings = "")
names <- c("Timestamp", "id", "other_projects", "type_projects", "age", "gender", "education", "workflow", "prefer_workflow", "why", "device")
names(vol_survey) <- names

vol_survey %>% filter(prefer_workflow == "Survey Workflow") %>% summary
vol_survey %>% filter(prefer_workflow == "Yes/No Workflow") %>% summary

prefer_workflow_levels <- c("Survey Workflow", "Prefer not to say", "Yes/No Workflow", "Both")
ed_levels <- c("Prefer not to say", "High school or lower", "Some university", "University degree", "Graduate degree")
age_levels <- c("<18", "18-24", "24-34", "35-44", "45-54", "55-64", "65+")

data <- vol_survey %>% #filter(workflow == "Both") %>% 
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

ggplot(data, aes(x=prefer_workflow)) + geom_bar(aes(fill = age), position = "stack")



age_summary <- data %>% rename(Age = age) %>%
     group_by(Age, prefer_workflow) %>% 
     summarise(n = n()) %>% 
     filter(prefer_workflow != "Prefer not to say", Age != "Prefer not to say") %>% 
     mutate(percent = n/sum(n), total = sum(n)) %>%
     gather(key = question, value = category, -prefer_workflow, -n, -percent, -total)

edu_summary <- data %>% rename(Education = education) %>%
     group_by(Education, prefer_workflow) %>% 
     summarise(n = n()) %>% 
     filter(prefer_workflow != "Prefer not to say", Education != "Prefer not to say") %>% 
     mutate(percent = n/sum(n), total = sum(n)) %>%
     gather(key = question, value = category, -prefer_workflow, -n, -percent, -total)

gender_summary <- data %>% rename(Gender = gender) %>%
     group_by(Gender, prefer_workflow) %>% 
     summarise(n = n()) %>% 
     filter(prefer_workflow != "Prefer not to say", Gender != "Prefer not to say") %>% 
     mutate(percent = n/sum(n), total = sum(n)) %>%
     gather(key = question, value = category, -prefer_workflow, -n, -percent, -total) %>%
     mutate()

summary_dat <- rbind(age_summary, edu_summary, gender_summary) %>% 
     mutate(category = as.factor(category)) %>% 
     mutate(category = fct_relevel(f = category, "Graduate degree", after = Inf)) %>%
     mutate(category = fct_recode(f = category,   "<18" = "17 or younger", 
                                                  "High school \n or lower" = "High school or lower", 
                                                  "Graduate \n degree" = "Graduate degree",
                                                  "Some \n university" = "Some university",
                                                  "University \n degree" = "University degree")) %>%
     mutate(preference = fct_relevel(f = prefer_workflow, c("Both", "Survey Workflow", "Yes/No Workflow"))) %>%
     mutate(preference = fct_recode(f = preference, "Survey" = "Survey Workflow", "Yes/No" = "Yes/No Workflow"))

samples <- summary_dat %>% distinct(question, category, total) 
sample_size <- geom_text(data=samples,aes(x=category,y=.95,label=total),
          inherit.aes=FALSE)
color_levels <- c("#C77CFF","#F8766D", "#00BDC2") 

ggplot(data = summary_dat, aes(x = category, y = n, fill = preference)) + 
     geom_bar(stat = "identity") + facet_grid(~question,scales="free",space="free") + scale_fill_manual(values = color_levels)
ggplot(data = summary_dat, aes(x = category, y = percent, fill = preference)) + geom_bar(stat = "identity") + 
     facet_grid(~question,scales="free",space="free") + sample_size + scale_fill_manual(values = color_levels)



ggplot(data = summary_dat, aes(x = category, y = n, fill = preference)) + geom_bar(stat = "identity", position = "dodge") + 
     facet_grid(preference~question,scales="free",space="free") + scale_fill_manual(values = color_levels)


jpeg(file = "figures/Figure6.jpg", width = 9, height = 3.5, units = "in", res = 600)
ggplot(data = summary_dat, aes(x = category, y = n, fill = preference)) + theme_bw(base_size = 10) +
     geom_bar(stat = "identity") + facet_grid(~question, scales="free",space="free") + 
     scale_fill_manual(name = "Preference", values = color_levels) +
     labs(x = "Category", y = "Number of Volunteers")
dev.off()

# alternatives
# 
# pdf(file = "figures/Figure6_1.pdf", width = 12.5, height = 4)
# ggplot(data = summary_dat, aes(x = category, y = percent, fill = preference)) + theme_bw() +
#      geom_bar(stat = "identity") + facet_grid(~question, scales="free",space="free") + 
#      sample_size + scale_fill_manual(name = "Preference", values = color_levels)
# dev.off()
# 
# #pdf(file = "figures/Figure6.pdf", width = 9, height = 3.5)
# 
# pdf(file = "figures/Figure6_3.pdf", width = 12.5, height = 4)
# ggplot(data = summary_dat, aes(x = category, y = n, fill = preference)) + geom_bar(stat = "identity", position = "dodge") + 
#      facet_grid(~question, scales ="free", space="free", shrink = T,drop = T) + scale_fill_manual(values = color_levels) + theme_bw()
# dev.off()
# 
# pdf(file = "figures/Figure6_4.pdf", width = 12.5, height = 4)
# ggplot(data = summary_dat, aes(x = category, y = n, fill = preference)) + geom_bar(stat = "identity", position = "dodge") + 
#      facet_wrap(~question, nrow = 3, scales="free")  + scale_fill_manual(values = color_levels) + theme_bw()
# dev.off()
