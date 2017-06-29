library(data.table)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(treemap)
library(stringr)

classifications <- fread("Data/snapshots-at-sea-classifications.csv", select = 1:8)
View(classifications) 


dim(classifications)
summary(classifications)

#lump all the not-logged-in users together into one username
#if username begins with "not-logged-in", then assign all to just that 
class_dat <- classifications %>% 
     mutate(., name = str_match(user_name, pattern = "not-logged-in")) %>%
     mutate(., user_name = ifelse(is.na(name), user_name, name))

# Workflows have changed since launching the project. The workflow was streamlined when the challenge was launched. 
dat <- class_dat %>% group_by(., user_name) %>% 
     summarise(., num_classifications = n()) %>% 
     arrange(., -num_classifications) %>% add_rownames()

# raw density plot 

# # so skewed by users > 10K! Remove the top 5 contributors
# top5 <- dat[1:5,]
# rest <- dat[6:length(user_name)]
# ggplot(data = rest, aes(x=num_classifications)) + geom_density()
# ggplot(data = filter(dat, num_classifications < 10000), aes(x=log(num_classifications))) + geom_density() 
# ggplot(data = filter(dat, num_classifications > 1), aes(x=num_classifications)) + geom_density() 
# ggplot(data = filter(dat, num_classifications > 10), aes(x=num_classifications)) + geom_density() 
# ggplot(data = filter(dat, num_classifications > 100), aes(x=num_classifications)) + geom_density() 
# ggplot(data = rest, aes(x=log10(num_classifications))) + geom_histogram()

ggplot(data = dat, aes(x=num_classifications)) + geom_density()
ggplot(data = dat, aes(x=num_classifications)) + geom_density() + scale_x_log10()

pdf(file = "Figures/histograms.pdf")
ggplot(data = dat, aes(x=num_classifications)) + geom_histogram(bins = 100) + scale_x_log10()
ggplot(data = filter(dat, num_classifications <= 100), aes(x=num_classifications)) + geom_bar() 
ggplot(data = filter(dat, num_classifications <= 25), aes(x=num_classifications)) + geom_bar() 
dev.off()

# treemaps take forever

# quartz()
# treemap(dtf = head(dat, n=5000), index = "rowname", vSize = "num_classifications")
# quartz()
# treemap(dtf = dat, index = "rowname", vSize = "num_classifications", border.lwds = .1, fontsize.labels = 0)



sAs <- fread("Data/snapshots-at-sea-classifications.csv", select = 1:8, nrows = 1000)


ggplot(data = dat, aes(x=num_classifications)) + geom_density()
ggplot(data = dat, aes(x=num_classifications)) + geom_density() + scale_x_log10()

pdf(file = "Figures/histograms.pdf")
ggplot(data = dat, aes(x=num_classifications)) + geom_histogram(bins = 100) + scale_x_log10()
ggplot(data = filter(dat, num_classifications <= 100), aes(x=num_classifications)) + geom_bar() 
ggplot(data = filter(dat, num_classifications <= 25), aes(x=num_classifications)) + geom_bar() 
dev.off()

