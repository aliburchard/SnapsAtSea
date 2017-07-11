library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(jsonlite)
library(tidyjson)
library(ggplot2)

source("scripts/functions.R")

sas <- read.csv("outputs/SAS_data_all.csv")
sas %>% tbl_df
sas2 <- sas %>% filter(., experiment_status == "experiment", user != "not-logged-in") %>% 
  mutate(created_at = ymd_hms(created_at)) %>%
  arrange(created_at) %>%
  group_by(user) %>%
  mutate(counter = row_number())
  
test <- sas2 %>% filter(user_name %in% c("tedcheese", "aliburchard", "elisabethB", "Zoonicat"))

ggplot(data = sas2, aes(x = created_at, y = counter, group = user)) + 
  geom_line(alpha = .5, aes(color = user)) + theme_bw() + theme(legend.position = "none")

quartz()
ggplot(data = sas2, aes(x = counter, y = experiment, group = user)) + 
  geom_line(position=position_jitter(w=0.1, h=0.1), aes(color = user), alpha = .5) + theme_bw() + theme(legend.position = "none")

ggplot(data = sas2, aes(x = created_at, y = experiment, group = user)) + 
  geom_line(position=position_jitter(w=0.0, h=0.0), aes(color = user), alpha = .5) + theme_bw() + theme(legend.position = "none")

ggplot(data = sas2, aes(x = created_at, y = experiment, group = user)) + 
  geom_line(position=position_dodge(w=1), aes(color = user), alpha = .5) + theme_bw() + theme(legend.position = "none")
