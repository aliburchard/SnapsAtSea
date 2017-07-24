library(magrittr)
library(ggplot2)
library(dplyr)
library(lubridate)

launches <- read.csv("Data/project_launches.csv")
launch_dates <- launches %>% mutate(., date = dmy(as.character(date)), counter = row_number(), discipline = type)

project_builder <- ymd("2015-06-01")

launch_plot <- ggplot(data = launch_dates, aes(x = date, y = counter)) + 
     geom_line(size = 1.2) +
     geom_vline(aes(xintercept = as.numeric(as.Date(date)), colour = type)) +
     geom_point(aes(colour = type), size = 2) 


pdf("figures/Figure1.pdf", width = 8, height = 4)
ggplot(data = launch_dates, aes(x = date, y = counter)) + 
     geom_line(size = 1.2) +
     geom_vline(aes(xintercept = as.numeric(as.Date(date)), colour = discipline)) +
     geom_vline(aes(xintercept = as.numeric(as.Date(project_builder))), colour = "dark gray", size = 1.2) +
     geom_vline(aes(xintercept = as.numeric(as.Date(project_builder))), colour = "black", linetype = "dotted", size = 1.2) +
     geom_point(aes(colour = discipline)) + 
     theme_bw(base_size = 12) +
     theme(panel.border=element_blank(), legend.position = "bottom") +
     labs(y = "Number of Projects") + 
     scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()
# add vertical lines for every project launch & hide the legend


annual_data <- launch_dates %>% group_by(year = year(date)) %>%
     summarise(total = n()) %>%
     mutate(., year_color = as.factor(year))

annual_data_ecol <- launch_dates %>% mutate(., ecology = ifelse(type == "Ecology", "ecology", "other")) %>%
     group_by(year = year(date), ecology) %>%
     summarise(total = n()) %>%
     mutate(., year_color = as.factor(year))


jpeg(filename = "ecology_v_other.jpg")
ggplot(annual_data_ecol, aes(x = year, y = total, fill = ecology)) + 
     geom_bar(stat = "identity") + scale_x_continuous(breaks = annual_data$year, labels = annual_data$year) +
     theme_bw() + 
     theme(panel.border=element_blank(), panel.grid.major.x = element_blank()) +
     labs(y = "Number of Projects") +
     theme(text = element_text(size = 16), legend.position = c(.1, .9)) + 
     scale_fill_manual(values = c( "#CFBF2D", "#9D9D9D"))
dev.off()

quartz()
annual_plot <- ggplot(annual_data, aes(x = year, y = total, fill = year_color)) + 
     geom_bar(stat = "identity") + scale_x_continuous(breaks = annual_data$year, labels = annual_data$year) +
     theme_bw() + 
     theme(panel.border=element_blank()) +
     labs(y = "Number of Projects") +
     theme(text = element_text(size = 16), legend.position = "none")
annual_plot

# barplot projects by type
quartz()
ggplot(launch_dates, aes(x = type, fill = type)) + 
     geom_bar() +
     theme_bw() + 
     theme(panel.border=element_blank()) +
     labs(y = "Number of Projects", x = "") +
     theme(text = element_text(size = 16), legend.position = "none")

quartz()
types <- launch_dates %>% group_by(., type) %>% summarize(., count = n())
type_plot <- ggplot(types, aes(x = "", y = count, fill = type)) + 
     geom_bar(width = 1, stat = "identity") + 
     #coord_polar("y", start = 0) + theme_minimal() + 
     theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank())

type_plot  
