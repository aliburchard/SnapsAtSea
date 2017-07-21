rm(list = ls())

library(dplyr)
library(magrittr)
library(lubridate)
library(tidyr)

library(ggplot2)
library(scales)
library(reldist)
library(ineq)

source("Scripts/functions.R")

sas <- read.csv("outputs/SAS_data_all.csv") %>% tbl_df 

data <- sas %>% filter(experiment_status == "experiment", user_status != "not-logged-in")


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
     geom_line(size = 1.5) + 
     labs(x = "proportion of volunteers", y = "proportion of classifications") +
     geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16) + theme(legend.position = c(0.2, 0.7))

# plot distributions - what drives the skew?
user_class <- data %>% 
     group_by(experiment, user_name) %>%
     summarise(total = n_distinct(classification_id))


density_plot <- ggplot(user_class, aes(total)) + 
     geom_density(alpha = 0.4, aes(fill = experiment, colour = experiment), position = "identity") + 
     scale_x_log10() +
     theme_bw(base_size = 16) + 
     labs(x = "classifications per volunteer", y = "probability density") + 
     theme(legend.position = c(0.8, 0.8))

require(gridExtra)

pdf(file = "figures/Figure5_combined.pdf", width = 14, height = 7)
grid.arrange(lorenz_plot, density_plot, ncol=2)
dev.off()
