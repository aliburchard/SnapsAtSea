# # Read Data
rm(list = ls())
library(jsonlite)
library(tidyjson)
source("Scripts/functions.R")



q1 <- read.csv("data/yes-no-q1-classifications.csv", stringsAsFactors = F)
q2 <- read.csv("data/yes-no-q2-classifications.csv", stringsAsFactors = F)
q3 <- read.csv("data/yes-no-q3-classifications.csv", stringsAsFactors = F)
q4 <- read.csv("data/yes-no-q4-classifications.csv", stringsAsFactors = F)
survey <- read.csv("data/survey-classifications.csv", stringsAsFactors = F) 

all_dat_raw <- rbind(q1, q2, q3, q4, survey)
# Extract needed info from json

for (i in 100000:100010) {
     prettify(survey$annotations[i]) %>% print
}

# grab all classification data
yesno <- all_dat_raw %>% filter(., workflow_name != "Survey") %>%
     select(., subject_ids, classification_id, user_name, created_at, workflow_name, gold_standard, expert, annotations) %>%
     as.tbl_json("annotations") %>%
     gather_array %>%
     spread_values(task_label = jstring("task_label"), value = jstring("value"))


survey_dat <- survey %>% # survey classifications are going to have > 1 row per classification if > 1 species per image
     select(., subject_ids, classification_id, user_name, created_at, workflow_name, gold_standard, expert, annotations) %>%
     as.tbl_json("annotations") %>%
     gather_array %>%
     enter_object("value") %>%
     json_types() %>% 
     filter(type == "array") %>%
     gather_array() %>%
     spread_values(choice = jstring("choice")) %>%
     enter_object("answers") %>%
     spread_values(tail = jstring("ISTHEUNDERSIDEOFTHETAILVISIBLE"), humpback = jstring("IFYESDOESTHETAILBELONGTOAHUMPBACKWHALE"))



# grab gold-standard data per subject, and then left_join with the actual classification data. 
# AUGH, except we can't just add to the SAS data because there is more than one record per classification.
# So all accuracy analyses have to be done separately.

gold_yesno <- yesno %>% filter(., gold_standard == "true") %>%
     mutate(expert_answer = value) %>%
     distinct(., subject_ids, workflow_name, expert_answer)


gold_yesno <- yesno %>% filter(., user_name == "tedcheese") %>%
     mutate(expert_answer = value) %>%
     distinct(., subject_ids, workflow_name, expert_answer)

yesno_data <- inner_join(yesno, gold_yesno) %>% 
     mutate(value = as.factor(value), expert_answer = as.factor(expert_answer)) %>%
     mutate(correct = ifelse(value == expert_answer, 1, 0)) %>% 
     filter(., user_name != "tedcheese" & !is.na(value))

# raw accuracy is the individual accuracy
yesno_data %<>% filter(., ymd_hms(created_at) > ymd("2017-06-07")) 
yesno_data %>% group_by(workflow_name) %>% summarise(sum(correct), n(), sum(correct)/n())

# aggregated accuracy: at 5 votes, using Ted's rules
agg_data <- yesno_data %>% group_by(workflow_name, subject_ids) %>% 
     summarise(votes = n_distinct(classification_id), test = n(),
          count_yes = length(value[value == "Yes"]),
          expert_answer = first(expert_answer)) %>%
     mutate(prop_yes = count_yes/votes) %>%
     mutate(agg_answer = ifelse(workflow_name %in% c("Yes/No (Q1)", "Yes/No (Q2)") & prop_yes >= 0.75, "Yes", 
                                ifelse(workflow_name == "Yes/No (Q3)" & prop_yes > .45, "Yes",
                                       ifelse(workflow_name == "Yes/No (Q4)" & prop_yes >= 0.5, "Yes", "No")))) %>%
     mutate(correct = ifelse(agg_answer == expert_answer, 1, 0))

agg_data %>% filter(votes > 4) %>% 
     group_by(workflow_name) %>% summarise(sum(correct), n(), sum(correct)/n())

agg_data %>% filter(votes > 4) %>% View
agg_data %>% filter(correct == 0) %>% View



### survey

#augh, manual consensus running on the next scrip over.
consensus_dat <- read.csv("data/survey_aggregation.csv")

gold_survey <- survey_dat %>% filter(., user_name == "tedcheese") %>% 
     mutate(gold_choice = choice, gold_tail = tail, gold_humpback = humpback) %>%
     select(., subject_ids, gold_choice, gold_tail, gold_humpback)

survey_with_gold <- inner_join(consensus_dat, gold_survey) %>%
     mutate(agg_tail = ifelse(tail > 0.45, "YES", "NO"), agg_humpback = ifelse(humpback >= 0.5, "YES", "NO")) %>%
     mutate(correct_choice = ifelse(choice == gold_choice, 1, 0), 
            correct_tail = ifelse(agg_tail == gold_tail, 1, 0)) %>%
     filter(., num_class >= 5)


survey_with_gold %>% group_by(gold_choice) %>%
     summarise(sum(correct_choice), n(), sum(correct_choice)/n())

good_whales <- survey_with_gold %>% filter(gold_choice == "WHALEORDOLPHIN" & correct_choice == 1)
good_whales %>% summarise(sum(correct_tail), n(), sum(correct_tail)/n())

good_tails <- good_whales %>% filter(., !is.na(gold_humpback)) %>%
     mutate(correct_humpback = ifelse(gold_humpback == "YES" & agg_humpback == "YES", 1, 
                                      ifelse(gold_humpback == "NO" & agg_humpback == "NO", 1, 0)))

good_tails %>% summarise(sum(correct_humpback), n(), sum(correct_humpback)/n())

