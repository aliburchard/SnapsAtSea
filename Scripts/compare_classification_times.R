library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)
library(ineq)

source("Scripts/functions.R")

sas <- read.csv("outputs/SAS_data_all.csv") %>% tbl_df 

# set sas timestamps to lubridate timestamps
# started_at = ymd_hms(gsub(x=started_at, pattern = "T|.[0-9]{3}Z", replacement = " ")), # you don't need to parse this because lubridate is FUCKING AMAZING
sas %<>% 
  mutate(., created_at = ymd_hms(created_at),
         started_at = ymd_hms(started_at),
         finished_at = ymd_hms(finished_at)) %>%
  mutate(., duration = as.numeric(difftime(finished_at, started_at, units = "secs")))



# Count up total amount of time spent on the workflow
class_times <- sas %>% 
  filter(., duration < 60*5)

ggplot(data = class_times, aes(duration, fill = user_status, colour = user_status)) + 
  geom_density(alpha = .2) + scale_x_log10() +
  facet_grid(workflow_name ~ .)


ggplot(data = class_times, aes(duration, fill = workflow_name, colour = workflow_name)) + 
  geom_density(alpha = .2) + scale_x_log10() +
  facet_grid( device ~ user_status)


ggplot(data = class_times, aes(duration, fill = workflow_name, colour = workflow_name)) + 
  geom_density(alpha = .2) + scale_x_log10() +
  facet_grid( device ~ .)

ggplot(data = class_times, aes(duration, fill = device, colour = device)) + 
  geom_density(alpha = .2) + scale_x_log10() +
  facet_grid( workflow_name ~ .)

