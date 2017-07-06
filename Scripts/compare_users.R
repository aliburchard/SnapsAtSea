library(tidyverse)
library(forcats)
library(tidyjson)
library(printr)

sas <- read.csv("outputs/SAS_data_all.csv")

with_non_loggeds <- sas %>% filter(., experiment_status == "experiment") %>%
  group_by(user, user_name, experiment, user_status) %>% 
  summarise(classifications = n_distinct(classification_id)) %>%
  mutate(., user_status = fct_relevel(user_status, "not-logged-in", after = 0)) %>%
  spread(., key = experiment, value = classifications, fill = 0) %>% 
  mutate(., workflows = ifelse(survey > 0 & yesno == 0, "survey_only", 
                          ifelse(survey == 0 & yesno > 0, "yesno_only", 
                            ifelse(survey > 0 & yesno > 0, "both", "wtf")))) %>%
  gather(key = workflow_type, value = n_classifications, -user_status, -workflows, -user, -user_name) %>% 
  arrange(user)


# How many people classified on only one workflow or the other, vs both.

jpeg("Figures/volunteers_per_workflow.jpg")
ggplot(with_non_loggeds, aes(workflows)) + geom_bar(aes(fill = user_status)) + 
  labs(y = "number of volunteers") + scale_fill_manual(values = c("#DADADA", "#00BDC2", "#F8766D")) +
  theme_bw(base_size = 16)
dev.off()

#drop_non_loggeds
users <- sas %>% filter(., experiment_status == "experiment") %>%
  group_by(user, experiment, user_status) %>% 
  summarise(classifications = n_distinct(classification_id)) %>% 
  spread(., key = experiment, value = classifications, fill = 0) %>% 
  mutate(., workflows = ifelse(survey > 0 & yesno == 0, "survey_only", 
                   ifelse(survey == 0 & yesno > 0, "yesno_only", 
                          ifelse(survey > 0 & yesno > 0, "both", "wtf")))) %>%
  gather(key = workflow_type, value = n_classifications, -user_status, -workflows, -user) %>% 
  arrange(user)

# merge user_data back to classification data
sas_with_user  <- users %>% 
  ungroup %>% 
  select(user, workflows) %>% 
  distinct() %>%
  left_join(sas, .) %>%
  filter(., experiment_status == "experiment")


# look at classifications by these different types.
jpeg(file = "Figures/classifications_per_workflow.jpg")
ggplot(sas_with_user, aes(experiment)) + 
  geom_bar(aes(fill=workflows), position = "dodge") +
  labs(y = "number of classifications", x = "workflow style") + theme_bw(base_size = 16)
dev.off()

ggplot(sas_with_user, aes(user_status)) + 
  geom_bar(aes(fill = workflows), position = "dodge") +
  labs(y = "n_classifications")

ggplot(filter(sas_with_user, experiment_status == "experiment"), aes(workflows)) + 
  geom_bar(aes(fill=experiment), position = "dodge") +
  labs(y = "n_classifications")

ggplot(sas_with_user, aes(workflows)) + 
  geom_bar(aes(fill=workflow_name), position = "dodge") +
  labs(y = "n_classifications")

ggplot(sas_with_user, aes(workflows)) + 
  geom_histogram(aes(fill=workflow_name), position = "dodge") +
  labs(y = "n_classifications")



# COMPARE TALK ACTIVITY
talk_comments <- fromJSON(txt = "data/project-14-comments_2017-06-29.json", flatten = T)
talk_tags <- fromJSON(txt = "data/project-14-tags_2017-06-29.json", flatten = T)

user_talk <- talk_comments %>% filter(., ymd_hms(comment_created_at) > ymd("2017-06-07")) %>%
  group_by(user = comment_user_login) %>%
  summarise(., user_talk_comments =  n())

user_summaries_by_workflow_preference <- sas_with_user %>% left_join(., user_talk, by = "user") %>%
  filter(experiment_status == "experiment") %>%
  group_by(., workflows, user) %>%
  summarise(., comments = first(user_talk_comments), classifications = n_distinct(classification_id)) %>%
  mutate(., comments = ifelse(is.na(comments), 0, comments)) %>%
  summarise(., volunteers = n_distinct(user), 
            total_classifications = sum(classifications), 
            prop_users_commenting = length(comments[comments > 0])/n_distinct(user),
            commenting_users = length(comments[comments > 0]), total_comments = sum(comments))


write.csv(user_summaries_by_workflow_preference, "outputs/user_summaries.csv")

library(gridExtra)
pdf("Figures/user_summaries.pdf", height=3, width=11)
grid.table(user_summaries_by_workflow_preference)
dev.off()
