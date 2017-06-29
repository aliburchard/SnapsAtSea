library(tidyjson)
library(dplyr)
library(jsonlite)
library(magrittr)

# full set 
manifest_orig <- read.csv("~/Desktop/ZooniverseConsulting/SAS-Experiment/subjects/original/original_manifest.csv", stringsAsFactors = F)
manifest_alt <- read.csv("~/Desktop/ZooniverseConsulting/SAS-Experiment/subjects/alternative/alternative_manifest.csv", stringsAsFactors = F)

alternative_id <- 12618
original_id <- 12613


#download subject export
subjects <- read.csv("~/Downloads/snapshots-at-sea-subjects.csv", stringsAsFactors = F)
View(subjects)

subjects$locations[1] %>% prettify
subjects$metadata[1] %>% prettify

flat <- subjects %>% 
     select(., subject_id, subject_set_id, metadata) %>%
     as.tbl_json(json.column = "metadata") %>% #parse JSON
     spread_values(filename = jstring("filename")) %>%
     unique(.)

original_list <- flat %>% filter(., subject_set_id == original_id)
alternative_list <- flat %>% filter(., subject_set_id == alternative_id)
     

# compare the two
# you want to know how many the manifest has that ARE NOT in the subject export

missing_orig <- anti_join(manifest_orig, original_list)
missing_alt <- anti_join(manifest_alt, alternative_list)

write.csv(missing_orig, file = "~/Desktop/ZooniverseConsulting/SAS-Experiment/subjects/original/missing_original_manifest.csv", row.names = F)

write.csv(missing_alt, file = "~/Desktop/ZooniverseConsulting/SAS-Experiment/subjects/alternative/missing_alternative_manifest.csv", row.names = F)
