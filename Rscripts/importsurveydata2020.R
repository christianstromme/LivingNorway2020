#### IMPORT DATA PREPARE FOR ANALYSIS ####

# load libraries
library("tidyverse")
library("wesanderson")

# read in files
data_raw <- read_csv2(file = "data/1dataset_clean.csv", skip_empty_rows = TRUE)
questions <- read_csv2(file = "data/1complete_clean.csv", n_max = 1, col_names = FALSE)




# clean and 
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
  
  #reorder questions
  mutate(Question = factor( Question, levels = c("Shared_data","Shared_code", "Shared_methods","Used_open_data","Used_codes","Published_open","Edu_tools", "Read_papers","Open_review","Outreach","Research","Teaching","Supervision","None","Data_sharing","Code_sharing","Method_sharing","Publish_open","Comm_science","Reproducib","Transparency"))) %>% 
  select(Category, Question, Value, NO:s_44) %>%
  
  # rename co-variables 
  mutate(Question = case_when(Question == "s_36_1" ~ "University",
                              Question == "s_36_2" ~ "Institute",
                              Question == "s_36_3" ~ "Gov_agency",
                              Question == "s_36_4" ~ "Private_comp",
                              Question == "s_36_5" ~ "Other",
                              Question == "s_37_1" ~ "Norway",
                              Question == "s_37_2" ~ "EU",
                              Question == "s_37_3" ~ "Non_EU",
                              Question == "s_38" ~ "Position",
                              Question == "s_39" ~ "Degree",
                              Question == "s_40" ~ "Year",
                              Question == "s_41_1" ~ "Undergrad_classes",
                              Question == "s_41_2" ~ "Grad_classes",
                              Question == "s_41_3" ~ "Superv_undergrad",
                              Question == "s_41_4" ~ "Superv_grad",
                              Question == "s_41_5" ~ "Superv_postdoc",
                              Question == "s_41_6" ~ "TPublic_outreach",
                              Question == "s_41_7" ~ "TOther",
                              Question == "s_49_1" ~ "Prim_research",
                              Question == "s_49_2" ~ "Synthesis",
                              Question == "s_49_3" ~ "Assessment",
                              Question == "s_49_4" ~ "Policy_interf",
                              Question == "s_49_5" ~ "APublic_outreach",
                              Question == "s_49_6" ~ "AOther",
                              Question == "s_42" ~ "Gender",
                              Question == "s_43_1" ~ "Day1",
                              Question == "s_43_2" ~ "An_worskshop",
                              Question == "s_43_3" ~ "Ed_workshop",
                              Question == "s_44" ~ "Attending"))


scale1 <- tibble(numeric_scale = c(1, 2, 3, 4, 5, 6),
                text_scale = c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week", "I don't know"))

scale2 <- tibble(numeric_scale = c(1, 2, 3, 4, 5),
                text_scale = c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important"))






### Figures
  ## ----Fig1
  data %>% 
    filter(Category == "OS_activity") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  #scale_fill_manual(values = wes_palette("Zissou1")[c(1,4,3,2,5, 6)]) +
  scale_x_continuous(breaks = 1:6, labels = scale1$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

## ----
#ggsave(filename = "General.png", device = "png", width = 9, height = 9, dpi = 300)

#Research

data %>% 
  filter(Category == "Research") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  #scale_fill_manual(values = wes_palette("Zissou1")[c(1,4,3,2,5, 6)]) +
  scale_x_continuous(breaks = 1:5, labels = scale2$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Research.png", device = "png", width = 9, height = 9, dpi = 300)

#Teaching
data %>% 
  filter(Category == "Teaching") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  #scale_fill_manual(values = wes_palette("Zissou1")[c(1,4,3,2,5, 6)]) +
  scale_x_continuous(breaks = 1:5, labels = scale2$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Teaching.png", device = "png", width = 9, height = 9, dpi = 300)

#Supervision
data %>% 
  filter(Category == "Supervision") %>% 
  ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
  geom_bar(show.legend = FALSE) +
  #scale_fill_manual(values = wes_palette("Zissou1")[c(1,4,3,2,5, 6)]) +
  scale_x_continuous(breaks = 1:5, labels = scale2$text_scale) +
  #scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of respondents") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()

#ggsave(filename = "Supervision.png", device = "png", width = 9, height = 9, dpi = 300)
