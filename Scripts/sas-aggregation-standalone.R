library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(jsonlite)
library(tidyjson)
library(stringr)
library(ggplot2)

classifications <- read.csv("Data/snapshots-at-sea-classifications.csv", stringsAsFactors = F)

classifications %<>% mutate(., created_at = ymd_hms(created_at)) #set created_at to a date field
dim(classifications)

classification_summary <- classifications %>% 
     group_by(., workflow_id, workflow_version) %>% 
     summarise(., count = n(), text = first(workflow_name), first = min(created_at), last = max(created_at)) %>% 
     arrange(., workflow_version)

print(classification_summary)

# filter to relevant workflow versions & workflow ID
workflow_id_set <- 505
workflow_version_set <- 4.16
created_at_set <- ymd("2017-01-01")

filtered_dat <- classifications %>% 
     filter(., workflow_id == workflow_id_set, workflow_version == workflow_version_set, created_at > created_at_set ) %>%
     arrange(., created_at) %>%
     mutate(., counter = row_number())

summary(filtered_dat)

## Flatten the JSON

flattened <- filtered_dat %>% 
     select(., -subject_data, -metadata) %>%
     as.tbl_json(json.column = "annotations") %>%
     gather_array %>%
     spread_values(
          task = jstring("task"),
          task_label = jstring("task_label"),
          value = jstring("value")
     )

head(flattened)
dim(flattened)

### Now run aggregation
threshold <- 0.45

aggregated <- flattened %>% 
     mutate(., answer = ifelse(value == "No", 0, 1)) %>%
     group_by(., subject_ids, workflow_id, workflow_name, workflow_version) %>%
     summarise(., total_votes = n(), total_yes = sum(answer), prop_yes = sum(answer)/n()) %>%
     mutate(., keep = as.factor(ifelse(prop_yes > threshold, "T", "F")))

glimpse(aggregated)

# select subjects to pass onto next workflow
subjects_Q4 <- aggregated %>% filter(., keep == "T")
summary(subjects_Q4)

write.csv(x = subjects_Q4, file = "Data/subjects_for_Q4.csv")
