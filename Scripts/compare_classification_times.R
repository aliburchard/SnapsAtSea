library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(tidyr)

library(ggplot2)
library(scales)
library(reldist)
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
