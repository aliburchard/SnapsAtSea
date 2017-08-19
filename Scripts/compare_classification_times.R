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
# other <- read.csv("outputs/other_proj_all.csv") %>% tbl_df
# 
# whales <- other %>% filter(., project == "whales")
# # set sas timestamps to lubridate timestamps
# # started_at = ymd_hms(gsub(x=started_at, pattern = "T|.[0-9]{3}Z", replacement = " ")), # you don't need to parse this because lubridate is FUCKING AMAZING

class_times <- sas %>% filter(experiment_status == "experiment") %>%
  mutate(., created_at = ymd_hms(created_at),
         started_at = ymd_hms(started_at),
         finished_at = ymd_hms(finished_at)) %>%
  mutate(., duration = as.numeric(difftime(finished_at, started_at, units = "secs"))) %>% 
     filter(., duration < 60*2)

class_times %>% 
     group_by(experiment) %>%
     summarise(total_time = sum(duration))

sample_sizes <- class_times %>% 
     group_by(experiment, device) %>% 
     summarise(time = median(duration), n = n_distinct(classification_id))


#pdf("figures/Figure4.pdf", width = 10, height = 7)
jpeg("figures/Figure4.jpeg", width = 8, height = 6, units = "in", res = 600)
ggplot(data = class_times, aes(x = experiment, y = duration)) + 
     geom_boxplot(aes(colour = device), position = position_dodge(.8)) + scale_y_log10(breaks = c(1, 10, 100)) + 
     guides(colour=guide_legend(title="Device", title.position = "top")) +
     scale_color_hue(labels = c("Computer", "Tablet", "Mobile", "Swipe App")) +
     scale_x_discrete(labels = c("Survey", "Yes/No")) +
     theme_bw(base_size = 16) + labs(x = "Workflow", y = "Classification Duration")
     #stat_summary(aes(x=experiment, color = device), fun.data = fun_length, geom = "text", 
     #              position=position_dodge(width=0.9), vjust=2)

dev.off()

sample_sizes_byQ <- class_times %>% 
     group_by(workflow_name, device) %>% 
     summarise(time = median(duration), n = n_distinct(classification_id))

pdf("figures/Figure4.pdf", width = 10, height = 7)
ggplot(data = class_times, aes(x = workflow_name, y = duration)) + 
     geom_boxplot(aes(colour = device)) + scale_y_log10(breaks = c(1, 10, 100)) + 
     theme_bw(base_size = 16) + labs(x = "workflow", y = "classification duration")
dev.off()


class_times %>% filter(., experiment_status == "experiment") %>%
     group_by(experiment) %>% 
     summarise(time = median(duration), n = n_distinct(classification_id))


pdf("figures/duration_v_device_boxplot.pdf", width = 10, height = 7)
ggplot(data = class_times, aes(x = experiment, y = duration)) + 
     geom_boxplot(aes(colour = device)) + scale_y_log10(breaks = c(1, 10, 100)) + 
     theme_bw(base_size = 16) + labs(x = "Question", y = "Classification Duration") 


dev.off()

