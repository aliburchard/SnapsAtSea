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

flattened <- grab_classification_metadata(all_dat_raw)
flat_with_mobile <- grab_mobile_info(flattened)

data_out <- flat_with_mobile %>% select(., subject_ids, 
                                        classification_id, 
                                        user_name, 
                                        user_id, 
                                        user_ip, 
                                        workflow_id, 
                                        workflow_name, 
                                        workflow_version,
                                        created_at,
                                        annotations,
                                        started_at,
                                        finished_at,
                                        device)

flattened <- grab_classification_metadata(all_dat_raw)
flat_with_mobile <- grab_mobile_info(flattened)
write.csv(data_out, "data/SAS_dat_with_metadata.csv", row.names = F)





### Grab metadata for other projects

ele <- read.csv(input = "data/elephant-expedition-classifications.csv")
flattened <- grab_classification_metadata(ele)
flat_with_mobile <- grab_mobile_info(flattened)

