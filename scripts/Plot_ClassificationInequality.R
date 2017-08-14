rm(list = ls())

library(dplyr)
library(magrittr)
library(lubridate)
library(tidyr)

library(ggplot2)
library(scales)
library(reldist)
library(ineq)
library(gridExtra)

source("Scripts/functions.R")

sas <- read.csv("outputs/SAS_data_all.csv") %>% tbl_df 

data <- sas %>% filter(experiment_status == "experiment", user_status != "not-logged-in") %>%
     mutate(., created_at = ymd_hms(created_at),
            started_at = ymd_hms(started_at),
            finished_at = ymd_hms(finished_at), 
            user_status = ifelse(user_status == "old", "existing", "new")) %>%
     mutate(., duration = as.numeric(difftime(finished_at, started_at, units = "secs"))) %>%
     mutate(workflow_type = experiment)


#Grab Lorenz data
grab_lorenz <- function(data, class_per_user = "num_classifications") {
     class_per_user <- data[[class_per_user]]
     temp <- Lc(class_per_user)
     out <- data.frame(prop_user = temp$p, prop_class = temp$L)
     return(out)
}


# plot lorenz curve - is there skew?
lorenz <- data %>%
     group_by(., project, experiment, user_status, user_name) %>%
     summarise(., num_classifications = n()) %>%
     do(grab_lorenz(., class_per_user = "num_classifications"))
     

lorenz_plot <- ggplot(lorenz, aes(x = prop_user, y = prop_class, color = experiment, linetype = user_status)) + 
     geom_line(size = .9) + 
     labs(x = "Proportion of Volunteers", y = "Proportion of Classifications") +
     geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 11) + 
     theme(legend.position = c(0.2, 0.75), legend.background = element_blank()) +
     scale_color_hue(name = "Workflow", labels = c("Survey", "Yes/No")) +
     scale_linetype_manual(name = "User Status", labels = c("Existing", "New"), values = c("solid", "dotted"))

# plot distributions - what drives the skew?
user_class <- data %>% 
     group_by(experiment, user_name) %>%
     summarise(total = n_distinct(classification_id))


density_plot <- ggplot(user_class, aes(total)) +
     geom_density(alpha = 0.4, aes(colour = experiment), position = "identity") + 
     theme_bw(base_size = 11) + 
     scale_x_log10(breaks = c(1, 10, 100, 1000, 10000)) +
     labs(x = "Classifications per Volunteer", y = "Probability Density") + 
     theme(legend.position = c(0.8, 0.85), legend.background = element_blank()) +
     scale_color_hue(name = "Workflow", labels = c("Survey", "Yes/No"))

require(gridExtra)

pdf(file = "figures/Figure5_combined.pdf", width = 8, height = 4)
jpeg(file = "figures/Figure5.jpeg", width = 8, height = 4, units = "in", res = 600)
grid.arrange(lorenz_plot, density_plot, ncol=2)
dev.off()




bins = c(1, 10, 100, 1000, 10000)

user_class %>% group_by(experiment) %>%
     filter(total == 1) %>% summarise(n_distinct(user_name)) 

user_class %>% group_by(experiment) %>%
     filter(total < 10) %>% summarise(n_distinct(user_name)) 

user_class %>% group_by(experiment) %>%
     filter(total > 100) %>% summarise(n_distinct(user_name)) 

user_class %>% group_by(experiment) %>%
     filter(total > 2000) %>% summarise(n_distinct(user_name)) 

user_class %>% group_by(experiment) %>%
     filter(total > 6000) %>% summarise(n_distinct(user_name)) 
