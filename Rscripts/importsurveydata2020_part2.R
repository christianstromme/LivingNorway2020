#### IMPORT DATA FROM PART 2 ####
# load libraries

library("tidyverse")
library("wesanderson")

# read in files
data_raw2 <- read_csv2(file = "data/2dataset_clean.csv", skip_empty_rows = TRUE)
questions2 <- read_csv2(file = "data/2complete_clean.csv", n_max = 1, col_names = FALSE)




# clean and 
data2 <- data_raw2 %>%
  # make long table
  pivot_longer(cols = c(s_2, s_3, s_8, s_12, s_9:s_10_12), names_to = "Question2", values_to = "Value2") %>% 
  
  # make categories
  mutate(Category = case_when(Question2 %in% c("s_2", "s_3") ~ "Teaching2",
                              Question2 %in% c("s_8", "s_12") ~ "Supervision2",
                              Question2 %in% c("s_9", "s_10_1", "s_10_2","s_10_3","s_10_4", "s_10_5", "s_10_6", "s_10_7", "s_10_8", "s_10_9", "s_10_10","s_10_11", "s_10_12") ~ "Own_education")) %>% 
  
  # rename questions
  mutate(Question2 = case_when(Question2 == "s_2" ~ "Teaches",
                              Question2 == "s_3" ~ "OS_in_teaching",
                              Question2 == "s_8" ~ "Supervises",
                              Question2 == "s_12" ~ "OS_in_supervision",
                              
                              Question2 == "s_9" ~ "OS_own_edu",
                              Question2 == "s_10_1" ~ "Edu_tools2",
                              Question2 == "s_10_2" ~ "OS_literature",
                              Question2 == "s_10_3" ~ "Used_open_data2",
                              Question2 == "s_10_4" ~ "Used_codes2",
                              Question2 == "s_10_5" ~ "Shared_data2",
                              Question2 == "s_10_6" ~ "Shared_code2",
                              Question2 == "s_10_7" ~ "Published_open2",
                              Question2 == "s_10_8" ~ "Transparency2",
                              Question2 == "s_10_9" ~ "Reproducib2",
                              Question2 == "s_10_10" ~ "Open_review2",
                              Question2 == "s_10_11" ~ "Outreach2",
                              Question2 == "s_10_12" ~ "None2")) %>% 
  #reorder questions
  mutate(Question2 = factor( Question2, levels = c("OS_own_edu", "Shared_data2","Shared_code2","Used_open_data2","Used_codes2","Published_open2","Edu_tools2", "OS_literature", "Open_review2","Outreach2", "Transparency2", "Reproducib2", "None2", "Teaches","Supervises","OS_in_teaching","OS_in_supervision")))


