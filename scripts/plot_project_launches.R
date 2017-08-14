library(magrittr)
library(ggplot2)
library(dplyr)
library(lubridate)

launches <- read.csv("Data/project_launches.csv")
launch_dates <- launches %>% mutate(., date = dmy(as.character(date)), counter = row_number(), Discipline = type)

project_builder <- ymd("2015-06-01")


#pdf("figures/Figure1.pdf", width = 8, height = 4)
jpeg(filename = "figures/Figure1.jpg", width = 8, height = 4, units = "in", res = 600, bg = "transparent")
ggplot(data = launch_dates, aes(x = date, y = counter)) + 
     geom_line(size = 1.2) +
     geom_vline(aes(xintercept = as.numeric(as.Date(date)), colour = Discipline)) +
     geom_vline(aes(xintercept = as.numeric(as.Date(project_builder))), colour = "dark gray", size = 1.2) +
     geom_vline(aes(xintercept = as.numeric(as.Date(project_builder))), colour = "black", linetype = "dotted", size = 1.2) +
     geom_point(aes(colour = Discipline)) + 
     theme_bw(base_size = 12) +
     theme(panel.border=element_blank(), legend.position = "bottom") +
     labs(y = "Number of Projects") + 
     scale_x_date(name = "Date", date_breaks = "1 year", date_labels = "%Y") + theme(plot.margin=unit(c(.5,.5,.5,.75),"cm"))
dev.off()


