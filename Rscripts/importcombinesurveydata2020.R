####MOTHER SCRIPT JOINING ALL PARTS####

library("tidyverse")

data_raw1 <- read_csv2(file = "data/1dataset_clean.csv", skip_empty_rows = TRUE)
data_raw2 <- read_csv2(file = "data/2dataset_clean.csv", skip_empty_rows = TRUE)
data_raw3 <- read_csv2(file = "data/3dataset_clean.csv", skip_empty_rows = TRUE)

dim(data_raw1)
dim(data_raw2)
dim(data_raw3)

data_raw12<-full_join(data_raw1, data_raw2, by="NO")
data_raw_full<-full_join(data_raw12, data_raw3, by="NO")

dim(data_raw_full)


questions1 <- read_csv2(file = "data/1complete_clean.csv", n_max = 1, col_names = FALSE)
questions2 <- read_csv2(file = "data/2complete_clean.csv", n_max = 1, col_names = FALSE)
questions3 <- read_csv2(file = "data/3complete_clean.csv", n_max = 1, col_names = FALSE)



# clean and 
data <- data_raw_full %>%
  # make long table for part 1
  pivot_longer(cols = c(s_3.x:s_11.x, s_46_1:s_46_4, s_16.x:s_21, s_22:s_27, s_28:s_33), names_to = "Question", values_to = "Value") %>% 
  
  # make categories for part 1
  mutate(Category = case_when(Question %in% c("s_3.x", "s_4.x", "s_45", "s_5.x", "s_6.x", "s_7.x", "s_8.x", "s_9.x", "s_10.x","s_11.x") ~ "OS_activity",
                              Question %in% c("s_46_1", "s_46_2", "s_46_3", "s_46_4") ~ "OS_Engagement",
                              Question %in% c("s_16.x", "s_17", "s_50", "s_18", "s_19", "s_20", "s_21") ~ "Research",
                              Question %in% c("s_22", "s_23", "s_51", "s_24", "s_25", "s_26", "s_27") ~ "Teaching",
                              Question %in% c("s_28", "s_29", "s_52", "s_30", "s_31", "s_32", "s_33") ~ "Supervision")) %>% 
  
  # rename questions for part 1
  mutate(Question = case_when(Question == "s_3.x" ~ "Shared_data",
                              Question == "s_4.x" ~ "Shared_code",
                              Question == "s_45" ~ "Shared_methods",
                              Question == "s_5.x" ~ "Used_open_data",
                              Question == "s_6.x" ~ "Used_codes",
                              Question == "s_7.x" ~ "Published_open",
                              Question == "s_8.x" ~ "Edu_tools",
                              Question == "s_9.x" ~ "Read_papers",
                              Question == "s_10.x" ~ "Open_review",
                              Question == "s_11.x" ~ "Outreach",
                              
                              Question == "s_46_1" ~ "Research",
                              Question == "s_46_2" ~ "Teaching",
                              Question == "s_46_3" ~ "Supervision",
                              Question == "s_46_4" ~ "None",
                              
                              Question %in% c("s_16.x", "s_22", "s_28") ~ "Data_sharing",
                              Question %in% c("s_17", "s_23", "s_29") ~ "Code_sharing",
                              Question %in% c("s_50", "s_51", "s_52") ~ "Method_sharing",
                              Question %in% c("s_18", "s_24", "s_30") ~ "Publish_open",
                              Question %in% c("s_19", "s_25", "s_31") ~ "Comm_science",
                              Question %in% c("s_20", "s_26", "s_32") ~ "Reproducib",
                              Question %in% c("s_21", "s_27", "s_33") ~ "Transparency")) %>% 

  # make long table for part 2
  pivot_longer(cols = c(s_2.y, s_3.y, s_8.y, s_12.y, s_9.y:s_10_12), names_to = "Question2", values_to = "Value2") %>% 
  
  # make categories for part 2
  mutate(Category = case_when(Question2 %in% c("s_2.y", "s_3.y") ~ "Teaching2",
                              Question2 %in% c("s_8.y", "s_12.y") ~ "Supervision2",
                              Question2 %in% c("s_9.y", "s_10_1", "s_10_2","s_10_3","s_10_4", "s_10_5", "s_10_6", "s_10_7", "s_10_8", "s_10_9", "s_10_10","s_10_11", "s_10_12") ~ "Own_education")) %>% 
  
  # rename questions for part 2
  mutate(Question2 = case_when(Question2 == "s_2.y" ~ "Teaches",
                               Question2 == "s_3.y" ~ "OS_in_teaching",
                               Question2 == "s_8.y" ~ "Supervises",
                               Question2 == "s_12.y" ~ "OS_in_supervision",
                               
                               Question2 == "s_9.y" ~ "OS_own_edu",
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
  #reorder questions for part 2
  mutate(Question2 = factor( Question2, levels = c("OS_own_edu", "Shared_data2","Shared_code2","Used_open_data2","Used_codes2","Published_open2","Edu_tools2", "OS_literature", "Open_review2","Outreach2", "Transparency2", "Reproducib2", "None2", "Teaches","Supervises","OS_in_teaching","OS_in_supervision"))) %>% 
  
  # make long table for part 3
  pivot_longer(cols = c(s_7:s_16.y), names_to = "Question3", values_to = "Value3") %>% 
    
  # make categories for part 3
    mutate(Category = case_when(Question3 %in% c("s_7", "s_8", "s_9", "s_10.y", "s_11", "s_12") ~ "Use_research",
                                Question3 %in% c("s_13.y", "s_14.y", "s_15.y", "s_16.y") ~ "Overall_use")) %>% 
    
    
  # rename questions for part 3
    mutate(Question2 = case_when(Question3 == "s_7" ~ "Lectures",
                                 Question3 == "s_8" ~ "1_discuss",
                                 Question3 == "s_9" ~ "Model_workshop",
                                 Question3 == "s_10.y" ~ "Edu_workshop",
                                 Question3 == "s_11" ~ "2_discuss",
                                 Question3 == "s_12" ~ "Surveys",
                                 
                                 Question3 == "s_13.y" ~ "Use_research",
                                 Question3 == "s_14.y" ~ "Use_teaching",
                                 Question3 == "s_15.y" ~ "Use_supervision",
                                 Question3 == "s_16.y" ~ "Use_edu")) %>% 

  #reorder questions
  mutate(Question = factor( Question, levels = c("Shared_data","Shared_code", "Shared_methods","Used_open_data","Used_codes","Published_open","Edu_tools", "Read_papers","Open_review","Outreach","Research","Teaching","Supervision","None","Data_sharing","Code_sharing","Method_sharing","Publish_open","Comm_science","Reproducib","Transparency"))) %>% 
  select(Category, Question, Value, NO:s_44) 

  #rename scale levels
  scale1 <- tibble(numeric_scale = c(1, 2, 3, 4, 5, 6),
                 text_scale = c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week", "I don't know")) 

  scale2 <- tibble(numeric_scale = c(1, 2, 3, 4, 5),
                 text_scale = c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) 

  scale3 <- tibble(numeric_scale = c(1, 2, 3, 4, 5),
                 text_scale = c("Not applicable to my work", "Minimally useful", "Somewhat useful", "Very useful", "Extremely useful")) 

  # rename co-variables 
  mutate(Question = case_when(Question == "s_36_1" ~ "University",
                            "s_36_2" ~ "Institute",
                            "s_36_3" ~ "Gov_agency",
                            "s_36_4" ~ "Private_comp",
                            "s_36_5" ~ "Other",
                            "s_37_1" ~ "Norway",
                            "s_37_2" ~ "EU",
                            "s_37_3" ~ "Non_EU",
                            "s_38" ~ "Position",
                            "s_39" ~ "Degree",
                            "s_40" ~ "Year",
                            "s_41_1" ~ "Undergrad_classes",
                            "s_41_2" ~ "Grad_classes",
                            "s_41_3" ~ "Superv_undergrad",
                            "s_41_4" ~ "Superv_grad",
                            "s_41_5" ~ "Superv_postdoc",
                            "s_41_6" ~ "TPublic_outreach",
                            "s_41_7" ~ "TOther",
                            "s_49_1" ~ "Prim_research",
                            "s_49_2" ~ "Synthesis",
                            "s_49_3" ~ "Assessment",
                            "s_49_4" ~ "Policy_interf",
                            "s_49_5" ~ "APublic_outreach",
                            "s_49_6" ~ "AOther",
                            "s_42" ~ "Gender",
                            "s_43_1" ~ "Day1",
                            "s_43_2" ~ "An_worskshop",
                            "s_43_3" ~ "Ed_workshop",
                            "s_44" ~ "Attending"))
  