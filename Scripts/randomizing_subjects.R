library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)

subject_list <- list.files(path = "~/Desktop/ZooniverseConsulting/SAS-Experiment/sas-experiment-subjects/") %>% as.data.frame(., stringsAsFactors = F)
names(subject_list) <- "filename"
subject_list %<>% rownames_to_column

dim(subject_list)

original <- subject_list %>% sample_frac(., size = .5) %>% arrange(., as.numeric(rowname))
alternative <- anti_join(subject_list, original) %>% arrange(., as.numeric(rowname))


# check to ensure no overlap
inner_join(original, alternative) # should be empty

#move into two different directories for ease of uploading.
#create move scripts

move_orig <- original %>% mutate(., move = paste("mv ", filename, " ./original/", filename, sep = ""))
move_alt <- alternative %>% mutate(., move = paste("mv ", filename, " ./alternative/", filename, sep = ""))

write.table(move_orig$filename, "~/Desktop/ZooniverseConsulting/SAS-Experiment/subjects/original_list.csv", sep = ",", col.names = F, row.names = F, quote = F)

write.csv(original, "~/Desktop/ZooniverseConsulting/SAS-Experiment/subjects/original_manifest.csv", row.names = F)
write.csv(alternative, "~/Desktop/ZooniverseConsulting/SAS-Experiment/subjects/alternative_manifest.csv", row.names = F, quote = F)
# setwd(dir = "~/Desktop/ZooniverseConsulting/SAS-Experiment/sas-experiment-subjects/")
# for (i in 1:nrow(original)) {
#      system(move_orig$move[i])
# }
# 
# for (i in 1:nrow(alternative)) {
#      system(move_alt$move[i])
# }
