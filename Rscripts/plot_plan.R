plot_plan <- drake_plan(
  
  #make scales
  scale1 = tibble( numeric_scale = c( 1, 2, 3, 4, 5, 6 ),
                    text_scale = c( "Never", "Rarely", "Several times a year", "Several times a month", "Several times a week", "I don't know" )),
  scale2 = tibble( numeric_scale = c( 1, 2, 3, 4, 5 ),
                    text_scale = c( "Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important" )),
  scale3 = tibble( numeric_scale = c( 1, 2, 3, 4, 5 ),
                    text_scale = c( "Not applicable to my work", "Minimally useful", "Somewhat useful", "Very useful", "Extremely useful" )),
  scale4 = tibble (numeric_scale = c(0, 1),
                    text_scale= c("No", "Yes")),
  scale5 = tibble (numeric_scale = c(0, 1, 2),
                    text_scale= c("No", "Yes", "Don't_know")),
  scale6 = tibble (numeric_scale = c(1, 2),
                    text_scale= c("Yes", "No")),
  scale7 = tibble (numeric_scale = c(1,2,3,4),
                    text_scale = c ("High_school", "Bsc", "Msc", "PhD")),
  scale8 = tibble (numeric_scale = c(1,2,3,4,5,6,7),
                    text_scale = c ("Bachelor_student", "Master_student", "PhD_student", "Researcher(temporary)", "Researcher(permanent)", "Associate_professor/Professor", "Other" )),
  

  ###Figures
  os_activity_plot = plotdata %>% 
    filter(Category == "OS_activity") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5,6)]) +
    scale_x_discrete(breaks = 1:6, labels = scale1$text_scale) +
    #scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  #ggsave(filename = "Figures/OS_activity.png", device = "png", width = 9, height = 9, dpi = 300)
  
  academic_acplotdata %>% 
    filter(Category == "Academic_activities") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2)]) +
    scale_x_discrete(breaks = 0:1, labels = scale4$text_scale) +
    #scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  important_research_plot = plotdata %>% 
    filter(Category == "Importance_research") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5)]) +
    scale_x_discrete(breaks = 1:5, labels = scale2$text_scale) +
    scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),

  important_teaching_plot = plotdata %>% 
    filter(Category == "Importance_teaching") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5)]) +
    scale_x_discrete(breaks = 1:5, labels = scale2$text_scale) +
    scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  important_supervision_plot = plotdata %>% 
    filter(Category == "Importance_supervision") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4,5)]) +
    scale_x_discrete(breaks = 1:5, labels = scale2$text_scale) +
    scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  os_education_plot = plotdata %>% 
    filter(Category == "OS_in_education") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3)]) +
    scale_x_discrete(breaks = 1:3, labels = scale5$text_scale) +
    scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  dutie_plot = plotdata %>% 
    filter(Category == "Duties") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2)]) +
    scale_x_discrete(breaks = 1:2, labels = scale6$text_scale) +
    scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  practical_edu_plot = plotdata %>% 
    filter(Category == "Practices_personal_education") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3)]) +
    scale_x_discrete(breaks = 0:2, labels = scale5$text_scale) +
    scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  #Background data
  plotdata %>% 
    filter(Question == "Year") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  plotdata %>% 
    filter(Question == "Degree") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4)]) +
    scale_x_discrete(breaks = 1:4, labels = scale7$text_scale) +
    scale_y_continuous(breaks = c(0, 20, 40)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  plotdata %>% 
    filter(Question == "Position") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3,4, 5, 6, 7)]) +
    scale_x_discrete(breaks = 1:7, labels = scale8$text_scale) +
    scale_y_continuous(breaks = c(0, 10, 20)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
  
  plotdata %>% 
    filter(Question == "Gender") %>% 
    ggplot(aes(x = Value, fill = factor(Value), group = Value)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,3)]) +
    scale_y_continuous(breaks = c(0, 10, 20)) +
    labs(x = "", y = "Number of respondents") +
    facet_wrap(~ Question, ncol = 1) +
    theme_bw(),
  
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
  
)
