library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

load(file = "Data/stats_by_week.rda")

head(stats_by_week)

plot_dat <- stats_by_week %>% 
  select(date, cumulative_users, cumulative_projects) %>% 
  gather(., key = type, value = growth, -date)

quartz()
ggplot(data = plot_dat, aes(x = date, y = growth, colour = type)) + 
  geom_line() + facet_grid(type ~ ., scale = "free_y") + theme_bw(base_size = 16) + theme(legend.position = "none")

growth_dat <- stats_by_week %>% mutate(users_per_proj = cumulative_users/cumulative_projects)

quartz()
ggplot(data = growth_dat, aes(x = date, y = users_per_proj)) + geom_line() + theme_bw(base_size = 16) + theme(legend.position = "none")
ggplot(data = growth_dat, aes(x = date, y = cumulative_projects/cumulative_users)) + geom_line() + theme_bw(base_size = 16) + theme(legend.position = "none")
