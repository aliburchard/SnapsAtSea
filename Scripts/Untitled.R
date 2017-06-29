library(dpl)
ted <- read.csv("Data/Ted_subjects_for_Q4.csv", stringsAsFactors = F)
subjects_Q4 <- read.csv("Data/subjects_for_Q4.csv", stringsAsFactors = F)

subjects_only <- subjects_Q4$subject_ids 
     write.table(subjects_only, file = "Data/Q4_subject_list.csv", sep = ",", row.names = F, col.names = F)


summary(subjects_Q4)

anti_join(ted, subjects_Q4, by = c("SUBJECT_IDS" = "subject_ids"))
anti_join(subjects_Q4, ted, by = c("subject_ids" = "SUBJECT_IDS"))


x <- read.csv("~/Desktop/ZooniverseConsulting/CLI-testing/testset2.csv", header = F)

write.table(x, file = "~/Desktop/ZooniverseConsulting/CLI-testing/testing3.csv", sep = ",", row.names = F, col.names = F)

