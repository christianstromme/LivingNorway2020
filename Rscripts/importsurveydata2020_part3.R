#### IMPORT DATA FROM PART 2 ####
# load libraries

library("tidyverse")
library("wesanderson")

# read in files
data_raw3 <- read_csv2(file = "data/3dataset_clean.csv", skip_empty_rows = TRUE)
questions3 <- read_csv2(file = "data/3complete_clean.csv", n_max = 1, col_names = FALSE)




# clean and 
data3 <- data_raw3 %>%
  # make long table
  pivot_longer(cols = c(s_7:s_16), names_to = "Question3", values_to = "Value3") %>% 
  
  # make categories
  mutate(Category = case_when(Question3 %in% c("s_7", "s_8", "s_9", "s_10", "s_11", "s_12") ~ "Use_research",
                              Question3 %in% c("s_13", "s_14", "s_15", "s_16") ~ "Overall_use")) %>% 
                              
  
  # rename questions
  mutate(Question2 = case_when(Question3 == "s_7" ~ "Lectures",
                               Question3 == "s_8" ~ "1_discuss",
                               Question3 == "s_9" ~ "Model_workshop",
                               Question3 == "s_10" ~ "Edu_workshop",
                               Question3 == "s_11" ~ "2_discuss",
                               Question3 == "s_12" ~ "Surveys",
                               
                               Question3 == "s_13" ~ "Use_research",
                               Question3 == "s_14" ~ "Use_teaching",
                               Question3 == "s_15" ~ "Use_supervision",
                               Question3 == "s_16" ~ "Use_edu"))

