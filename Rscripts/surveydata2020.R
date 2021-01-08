library("tidyverse")
library("wesanderson")

#Load the three partial surveys
data_raw1 <- read_csv2( file = "data/1dataset_clean.csv", skip_empty_rows = TRUE )
data_raw2 <- read_csv2( file = "data/2dataset_clean.csv", skip_empty_rows = TRUE )
data_raw3 <- read_csv2( file = "data/3dataset_clean.csv", skip_empty_rows = TRUE )


#Join datasets
data_raw12 <- full_join( data_raw1, data_raw2, by="NO" )
data_raw_full <- full_join( data_raw12, data_raw3, by="NO" )

#Load complete questions
questions1 <- read_csv2( file = "data/1complete_clean.csv", n_max = 1, col_names = FALSE )
questions2 <- read_csv2( file = "data/2complete_clean.csv", n_max = 1, col_names = FALSE )
questions3 <- read_csv2( file = "data/3complete_clean.csv", n_max = 1, col_names = FALSE )



# clean and 
data <- data_raw_full %>%

  # rename questions for part 1
  rename( Define_1 = s_2.x, 
                              Shared_data = s_3.x,
                              Shared_code = s_4.x,
                              Shared_methods = s_45,
                              Used_open_data = s_5.x,
                              Used_codes = s_6.x,
                              Published_open = s_7.x,
                              Edu_tools = s_8.x,
                              Read_papers = s_9.x,
                              Open_review = s_10.x,
                              Outreach = s_11.x,
                              Other_act = s_12.x,
                              Help = s_13.x,
                              Hinder = s_14.x,
                              Motiv = s_15.x,
                              Explain_R = s_48,
                              Explain_T = s_47,
                              Explain_S = s_35,
                              Research = s_46_1,
                              Teaching = s_46_2,
                              Supervision = s_46_3,
                              None = s_46_4,
                              
                              Data_sharing_R = s_16.x, 
                              Data_sharing_T = s_22,
                              Data_sharing_S = s_28,
                              Code_sharing_R = s_17,
                              Code_sharing_T = s_23,
                              Code_sharing_S = s_29,
                              Method_sharing_R = s_50,
                              Method_sharing_T = s_51,
                              Method_sharing_S = s_52,
                              Publish_open_R = s_18,
                              Publish_open_T = s_24,
                              Publish_open_S = s_30,
                              Comm_science_R = s_19,
                              Comm_science_T = s_25,
                              Comm_science_S = s_31,
                              Reproducib_R = s_20,
                              Reproducib_T = s_26,
                              Reproducib_S = s_32,
                              Transparency_R = s_21,
                              Transparency_T = s_27,
                              Transparency_S = s_33) %>% 
  
  # rename co-variables 
  rename( University = s_36_1,
                              Institute = s_36_2,
                              Gov_agency = s_36_3,
                              Private_comp = s_36_4,
                              Other = s_36_5,
                              Norway = s_37_1,
                              EU = s_37_2,
                              Non_EU = s_37_3,
                              Position = s_38,
                              Degree = s_39,
                              Year = s_40,
                              Undergrad_classes = s_41_1,
                              Grad_classes = s_41_2,
                              Superv_undergrad = s_41_3,
                              Superv_grad = s_41_4,
                              Superv_postdoc = s_41_5,
                              TPublic_outreach = s_41_6,
                              TOther = s_41_7,
                              Prim_research = s_49_1,
                              Synthesis = s_49_2,
                              Assessment = s_49_3,
                              Policy_interf = s_49_4,
                              APublic_outreach = s_49_5,
                              AOther = s_49_6,
                              Gender = s_42,
                              Day1 = s_43_1,
                              An_workshop = s_43_2,
                              Ed_workshop = s_43_3,
                              Attending = s_44) %>% 
  
  
  # rename questions for part 2
  rename( Teaches = s_2.y,
                               OS_in_teaching = s_3.y,
                               Inc_teach = s_4.y,
                               Inc_teach_future = s_5.y,
                               Other_edu = s_11.y,
                               Supervises = s_8.y,
                               OS_in_supervision = s_12.y,
                               
                               OS_own_edu = s_9.y,
                               Edu_tools2 = s_10_1,
                               OS_literature = s_10_2,
                               Used_open_data2 = s_10_3,
                               Used_codes2 = s_10_4,
                               Shared_data2 = s_10_5,
                               Shared_code2 = s_10_6,
                               Published_open2 = s_10_7,
                               Transparency2 = s_10_8,
                               Reproducib2 = s_10_9,
                               Open_review2 = s_10_10,
                               Outreach2 = s_10_11,
                               None2 = s_10_12) %>% 
  
 
  
  
  
  # rename questions for part 3
  rename( Define_2 = s_2, 
                               Change = s_3,
                               Change_how = s_4,
                               Future = s_5,
                               Comments = s_6, 
                               Lectures = s_7,
                               Discuss_1 = s_8,
                               Model_workshop = s_9,
                               Edu_workshop = s_10.y,
                               Discuss_2 = s_11,
                               Surveys = s_12,
                               
                               Use_research = s_13.y,
                               Use_teaching = s_14.y,
                               Use_supervision = s_15.y,
                               Use_edu = s_16.y) 


plotdata <- data %>%
  mutate_if(is.numeric,as.character, is.factor, as.character) %>% 
  pivot_longer(cols = c(Shared_data:Outreach, Research:Transparency_R, Data_sharing_T:Transparency_T, Data_sharing_S:Transparency_S, University:Attending, Teaches, OS_in_teaching, Supervises, OS_in_supervision, OS_own_edu:None2, Lectures:Use_edu ), names_to = "Question", values_drop_na = TRUE, values_to = "Value") %>%
  
  # make categories
  mutate(Category = case_when(Question %in% c("Shared_data", "Shared_code", "Shared_methods", "Used_open_data","Used_codes", "Published_open", "Edu_tools", "Read_papers", "Open_review", "Outreach", "Other_act") ~ "OS_activity",
                              
          
                              Question %in% c( "Research", "Teaching", "Supervision") ~ "Academic_activities",

                              Question %in% c("Data_sharing_R", "Code_sharing_R", "Method_sharing_R", "Publish_open_R", "Comm_science_R", "Reproducib_R", "Transparency_R") ~ "Importance_research",
                              
                              Question %in% c("Data_sharing_T", "Code_sharing_T", "Method_sharing_T", "Publish_open_T", "Comm_science_T", "Reproducib_T", "Transparency_T") ~ "Importance_teaching",
                              
                              Question %in% c("Data_sharing_S", "Code_sharing_S", "Method_sharing_S", "Publish_open_S", "Comm_science_S", "Reproducib_S", "Transparency_S") ~ "Importance_supervision",
                              
                              Question %in% c("University", "Institute", "Gov_agency", "Private_comp", "Other", "Norway", "EU", "Non_EU", "Position", "Degree", "Prim_research", "Synthesis", "Assessment", "Policy_interf", "APublic_outreach", "AOther") ~"Background",
                              
                              Question %in% c( "Day1", "An_workshop", "Ed_workshop", "Attending") ~ "Participation",
                              
                              
                              Question %in% c( "OS_in_teaching", "OS_in_supervision", "OS_own_edu") ~ "OS_in_education",
                              
                              Question %in% c( "Teaches", "Supervises") ~ "Duties",
                              
                              Question %in% c( "Edu_tools2", "OS_literature", "Used_open_data2", "Used_codes2", "Shared_data2", "Shared_code2", "Published_open2", "Transparency2", "Reproducib2", "Open_review2", "Outreach2", "None2") ~ "Practices_personal_education",
                              
                              Question %in% c( "Lectures", "Discuss_1", "Model_workshop", "Edu_workshop", "Discuss_2", "Surveys", "Use_research", "Use_teaching", "Use_supervision", "Use_edu") ~ "Workshop_utility",
         
                              Question %in% c("Undergrad_classes", "Grad_classes", "Superv_undergrad", "Superv_grad", "Superv_postdoc", "TPublic_outreach", "TOther") ~ "Teaching_types"))
                              
        


                      
#rename scale levels
scale1 <- tibble( numeric_scale = c( 1, 2, 3, 4, 5, 6 ),
                  text_scale = c( "Never", "Rarely", "Several times a year", "Several times a month", "Several times a week", "I don't know" ))

scale2 <- tibble( numeric_scale = c( 1, 2, 3, 4, 5 ),
                  text_scale = c( "Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important" )) 

scale3 <- tibble( numeric_scale = c( 1, 2, 3, 4, 5 ),
                  text_scale = c( "Not applicable to my work", "Minimally useful", "Somewhat useful", "Very useful", "Extremely useful" )) 

scale4 <- tibble (numeric_scale = c(0, 1),
                  text_scale= c("No", "Yes"))


scale5 <- tibble (numeric_scale = c(0, 1, 2),
                  text_scale= c("No", "Yes", "Don't_know"))

scale6 <- tibble (numeric_scale = c(1, 2),
                  text_scale= c("Yes", "No"))

scale7 <- tibble (numeric_scale = c(1,2,3,4),
                  text_scale = c ("High_school", "Bsc", "Msc", "PhD"))
  
scale8 <- tibble (numeric_scale = c(1,2,3,4,5,6,7),
                  text_scale = c ("Bachelor_student", "Master_student", "PhD_student", "Researcher(temporary)", "Researcher(permanent)", "Associate_professor/Professor", "Other" ))  



### Figures


plotdata %>% 
  filter(Category == "OS_activity") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5,6)]) +
  scale_x_discrete(breaks = 1:6, labels = scale1$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/OS_activity.png", device = "png", width = 9, height = 9, dpi = 300)

plotdata %>% 
  filter(Category == "Academic_activities") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2)]) +
  scale_x_discrete(breaks = 0:1, labels = scale4$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/Academic_activities.png", device = "png", width = 9, height = 9, dpi = 300)

plotdata %>% 
  filter(Category == "Importance_research") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5)]) +
  scale_x_discrete(breaks = 1:5, labels = scale2$text_scale) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/Importance_research.png", device = "png", width = 9, height = 9, dpi = 300)

plotdata %>% 
  filter(Category == "Importance_teaching") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5)]) +
  scale_x_discrete(breaks = 1:5, labels = scale2$text_scale) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/Importance_teaching.png", device = "png", width = 9, height = 9, dpi = 300)


plotdata %>% 
  filter(Category == "Importance_supervision") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5)]) +
  scale_x_discrete(breaks = 1:5, labels = scale2$text_scale) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/Importance_supervision.png", device = "png", width = 9, height = 9, dpi = 300)

plotdata %>% 
  filter(Category == "OS_in_education") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3)]) +
  scale_x_discrete(breaks = 1:3, labels = scale5$text_scale) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/OS_in_education.png", device = "png", width = 9, height = 9, dpi = 300)

plotdata %>% 
  filter(Category == "Duties") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2)]) +
  scale_x_discrete(breaks = 1:2, labels = scale6$text_scale) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/Duties.png", device = "png", width = 9, height = 9, dpi = 300)


plotdata %>% 
  filter(Category == "Practices_personal_education") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3)]) +
  scale_x_discrete(breaks = 0:2, labels = scale5$text_scale) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/Practices_personal_education.png", device = "png", width = 9, height = 9, dpi = 300)


#Background data
plotdata %>% 
  filter(Question == "Year") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

plotdata %>% 
  filter(Question == "Degree") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4)]) +
  scale_x_discrete(breaks = 1:4, labels = scale7$text_scale) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

plotdata %>% 
  filter(Question == "Position") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4, 5, 6, 7)]) +
  scale_x_discrete(breaks = 1:7, labels = scale8$text_scale) +
  scale_y_continuous(breaks = c(0, 10, 20)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()


plotdata %>% 
  filter(Question == "Gender") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3)]) +
  scale_y_continuous(breaks = c(0, 10, 20)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

plotdata %>% 
  filter(Category == "Teaching_types") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2)]) +
  scale_x_discrete(breaks = 0:1, labels = scale4$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Figures/Teaching_types.png", device = "png", width = 9, height = 9, dpi = 300)
  
