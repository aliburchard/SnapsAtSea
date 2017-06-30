# # Read Data
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
     prettify(q1$metadata[i]) %>% print
}
     
#grab started at, finished at, viewport, and subject_diension...maybe user_agent

SAS_dat <- all_dat_raw %>%
     grab_classification_metadata() %>%
     grab_mobile_info() %>%
     keep_metadata_rows()


write.csv(SAS_dat, "outputs/SAS_dat_with_metadata.csv", row.names = F)


### Grab metadata for other projects


ele <- read.csv("data/elephant-expedition-classifications.csv", stringsAsFactors = F)
ele_dat <- ele %>%
     grab_classification_metadata() %>%
     grab_mobile_info() %>%
     keep_metadata_rows()
write.csv(ele_dat, "outputs/ele_dat_with_metadata.csv", row.names = F)

whales <- read.csv("data/whales-as-individuals-classifications.csv", stringsAsFactors = F)
whale_dat <- whales %>%
     grab_classification_metadata() %>%
     grab_mobile_info() %>%
     keep_metadata_rows()
write.csv(whale_dat, "outputs/whale_dat_with_metadata.csv", row.names = F)

wisc <- read.csv("data/snapshot-wisconsin-classifications.csv", stringsAsFactors = F)
wisc_dat <- wisc %>%
     grab_classification_metadata() %>%
     grab_mobile_info() %>%
     keep_metadata_rows()
write.csv(wisc_dat, "outputs/wisc_dat_with_metadata.csv", row.names = F)

kenya <- read.csv("data/wildwatch-kenya-classifications.csv", stringsAsFactors = F)
kenya_dat <- kenya %>%
     grab_classification_metadata() %>%
     grab_mobile_info() %>%
     keep_metadata_rows()
write.csv(kenya_dat, "outputs/kenya_dat_with_metadata.csv", row.names = F)







