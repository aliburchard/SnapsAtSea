rm(list = ls())

library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

source("Scripts/functions.R")

launches <- read.csv("data/launch_times.csv", nrows = 10)[, 1:2]
launches %<>% mutate(., launch_time = ymd_hms(Date), Event = str_trim(Event, side = "both"))

sas <- read.csv("outputs/SAS_data_all.csv") %>% tbl_df 

data <- sas %>% filter(experiment_status == "experiment") %>%
     mutate(., created_at = ymd_hms(created_at),
            started_at = ymd_hms(started_at),
            finished_at = ymd_hms(finished_at)) %>%
     mutate(., duration = as.numeric(difftime(finished_at, started_at, units = "secs"))) %>%
     rename(workflow_type = experiment)



# Fig 3: cumulative and instantaneous classification rates
hourly_class <- data %>% 
     mutate(hour = round_date(created_at, unit = "hour")) %>%
     group_by(workflow_type, hour) %>%
     summarise(hourly_classifications = n_distinct(classification_id)) %>%
     mutate(required_classifications = ifelse(workflow_type == "yesno", 95195, 66320)) %>%
     mutate(cumulative_classifications = cumsum(hourly_classifications)) %>%
     mutate(percent_complete = 100*cumulative_classifications/required_classifications) %>%
     gather(key = classification_type, value = classifications, -workflow_type, -hour, -required_classifications, -cumulative_classifications)

legend_title <- "Workflow"

pdf("figures/Figure3.pdf", width = 12, height = 6)
jpeg("figures/Figure3.jpeg", width = 8, height = 4, units = "in", res = 600)
ggplot(hourly_class, aes(x = hour, y = classifications)) + 
     geom_line(aes(colour = workflow_type), size = 1) +
     guides(colour=guide_legend(title="Workflow", title.position = "top")) +
     facet_grid(classification_type ~ ., scales = "free_y") +
     geom_vline(data = filter(launches, Event == "Newsletter"), aes(xintercept = as.numeric(launch_time)), colour = "dark blue", linetype = "dashed", size = 1) +
     geom_vline(data = filter(launches, Event == "New Data"), aes(xintercept = as.numeric(launch_time)), colour = "dark gray", linetype = "dashed", size = 1) +
     theme_bw(base_size = 11) + 
     scale_color_hue(labels = c("Survey", "Yes/No")) +
     theme(legend.position = c(0.9, 0.88), legend.background = element_blank(), strip.background = element_blank(),strip.text.y = element_blank()) + 
     scale_x_datetime(name="Date", date_breaks = "1 week", labels = date_format("%B %d")) +
     labs(y = "Percent Complete             Hourly Classifications") 
dev.off()
