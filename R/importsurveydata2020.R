#### IMPORT DATA PREPARE FOR ANALYSIY ####

# load libraries
library("tidyverse")
library("wesanderson")

# read in files
data_raw <- read_csv2(file = "Data/dataset.csv", skip_empty_rows = TRUE)
questions <- read_csv2(file = "Data/complete.csv", n_max = 1, col_names = FALSE)




data_raw %>%
  rename(Shared_data=s_3, 
         Shared_code=s_4,
         Shared_methods=s_45,
         Used_open_data=s_5,
         Used_codes=s_6,
         Published_open=s_7,
         Edu_tools=s_8,
         Read_papers=s_9,
         Open_review=s_10,
         Outreach=s_11,
         OOther=s_12,
         Research=s_46_1,
         Teaching=s_46_2,
         Supervision=s_46_3,
         RData_sharing=s_16,
         RCode_sharing=s_17,
         RMethod_sharing=s_50,
         RPublish_open=s_18,
         RComm_science=s_19,
         RReproducib=s_20,
         RTransparency=s_21,
         TData_sharing=s_22,
         TCode_sharing=s_23,
         TMethod_sharing=s_51,
         TPublish_open=s_24,
         TComm_science=s_25,
         TReproducib=s_26,
         TTransparency=s_27,
         SData_sharing=s_28,
         SCode_sharing=s_29,
         SMethod_sharing=s_52,
         SPublish_open=s_30,
         SComm_science=s_31,
         SReproducib=s_32,
         STransparency=s_33,
         University=s_36_1,
         Institute=s_36_2,
         Gov_agency=s_36_3,
         Private_comp=s_36_4,
         Other=s_36_5,
         Norway=s_37_1,
         EU=s_37_2,
         Non_EU=s_37_3,
         Position=s_38,
         Degree=s_39,
         Year=s_40,
         Undergrad_classes=s_41_1,
         Grad_classes=s_41_2,
         Superv_undergrad=s_41_3,
         Superv_grad=s_41_4,
         Superv_postdoc=s_41_5,
         TPublic_outreach=s_41_6,
         TOther=s_41_7,
         Prim_research=s_49_1,
         Synthesis=s_49_2,
         Assessment=s_49_3,
         Policy_interf=s_49_4,
         APublic_outreach=s_49_5,
         AOther=s_49_6,
         Gender=s_42,
         Day1=s_43_1,
         An_worskshop=s_43_2,
         Ed_workshop=s_43_3,
         Attending=s_44) %>% 
  pivot_longer(cols = c(Shared_data:Outreach, RData_sharing:RTransparency, TData_sharing:TTransparency, SData_sharing:STransparency), names_to = "Question", values_to = "Value")
  

t(questions)  

data <- data_raw %>%
  # make long table
  pivot_longer(cols = c(s_3:s_11, s_46_1:s_46_4, s_16:s_21, s_22:s_27, s_28:s_33), names_to = "Question", values_to = "Value") %>% 
  
  # make categories
  mutate(Category = case_when(Question %in% c("s_3", "s_4", "s_45", "s_5", "s_6", "s_7", "s_8", "s_9", "s_10","s_11") ~ "OS_activity",
                              Question %in% c("s_46_1", "s_46_2", "s_46_3", "s_46_4") ~ "OS_Engagement",
                              Question %in% c("s_16", "s_17", "s_50", "s_18", "s_19", "s_20", "s_21") ~ "Research",
                              Question %in% c("s_22", "s_23", "s_51", "s_24", "s_25", "s_26", "s_27") ~ "Teaching",
         Question %in% c("s_28", "s_29", "s_52", "s_30", "s_31", "s_32", "s_33") ~ "Supervision")) %>% 
  
  # rename questions
  mutate(Question = case_when(Question == "s_3" ~ "Shared_data",
                              Question == "s_4" ~ "Shared_code",
                              Question == "s_45" ~ "Shared_methods",
                              Question == "s_5" ~ "Used_open_data",
                              Question == "s_6" ~ "Used_codes",
                              Question == "s_7" ~ "Published_open",
                              Question == "s_8" ~ "Edu_tools",
                              Question == "s_9" ~ "Read_papers",
                              Question == "s_10" ~ "Open_review",
                              Question == "s_11" ~ "Outreach",
                              
                              Question == "s_46_1" ~ "Research",
                              Question == "s_46_2" ~ "Teaching",
                              Question == "s_46_3" ~ "Supervision",
                              Question == "s_46_4" ~ "None",
                              
                              Question %in% c("s_16", "s_22", "s_28") ~ "Data_sharing",
                              Question %in% c("s_17", "s_23", "s_29") ~ "Code_sharing",
                              Question %in% c("s_50", "s_51", "s_52") ~ "Method_sharing",
                              Question %in% c("s_18", "s_24", "s_30") ~ "Publish_open",
                              Question %in% c("s_19", "s_25", "s_31") ~ "Comm_science",
                              Question %in% c("s_20", "s_26", "s_32") ~ "Reproducib",
                              Question %in% c("s_21", "s_27", "s_33") ~ "Transparency")) %>% 
  
  # rename co-variables
  rename(University=s_36_1,
         Institute=s_36_2,
         Gov_agency=s_36_3,
         Private_comp=s_36_4,
         Other=s_36_5,
         Norway=s_37_1,
         EU=s_37_2,
         Non_EU=s_37_3,
         Position=s_38,
         Degree=s_39,
         Year=s_40,
         Undergrad_classes=s_41_1,
         Grad_classes=s_41_2,
         Superv_undergrad=s_41_3,
         Superv_grad=s_41_4,
         Superv_postdoc=s_41_5,
         TPublic_outreach=s_41_6,
         TOther=s_41_7,
         Prim_research=s_49_1,
         Synthesis=s_49_2,
         Assessment=s_49_3,
         Policy_interf=s_49_4,
         APublic_outreach=s_49_5,
         AOther=s_49_6,
         Gender=s_42,
         Day1=s_43_1,
         An_worskshop=s_43_2,
         Ed_workshop=s_43_3,
         Attending=s_44) %>% 
  select(Category, Question, Value, s_2:stato_5)


scale <- tibble(numeric_scale = c(1, 2, 3, 4, 5, 6),
                 text_scale = c("Not at all", "Little", "Moderately", "Very much", "Completely", "Something"))



### Figures
data %>% 
  filter(Category == "OS_activity") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  #scale_fill_manual(values = wes_palette("Darjeeling1")[c(1,4,3,2,5, 6)]) +
  scale_x_continuous(breaks = 1:6, labels = scale$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "add Y legend") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

ggsave(filename = "General.png", device = "png", width = 6, height = 5, dpi = 300)
