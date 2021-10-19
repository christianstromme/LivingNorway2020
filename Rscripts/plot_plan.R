### Make figures

plot_plan <- drake_plan(

  # viridis colours
  five_col = rev(viridis_pal()(5)),
  
  # Engagement plot
  os_activity_stackplot = ana.data %>%
    filter(Domain == "engage") %>%
    mutate(AspAct = paste (Aspect, Action, sep = '_'),
           AspAct = fct_relevel(AspAct, "Data_share", "Code_share", "Methods_share", "Data_use", "Code_use", "EduTool_use", "Publish_share",  "Publish_use", "Review_does", "Outreach_does"),
           AspAct = fct_recode(AspAct, "Shared data openly" = "Data_share", "Shared code openly" = "Code_share", "Shared methods/protocols openly" = "Methods_share", "Used open data" = "Data_use", "Used open code" = "Code_use",  "Published open access" = "Publish_share", "Used open educational tools" = "EduTool_use", "Read open access publications" = "Publish_use", "Engaged in open peer review" = "Review_does", "Engaged in outreach/\nscience communication" = "Outreach_does"),
           
           Values = fct_relevel(Values, "6", "1", "2", "3", "4", "5"),
           Values = fct_recode(Values, "Never" = "1", "Rarely" = "2", "Several times a year" = "3", "Several times a month" = "4", "Several times a week" = "5", "I don't know"  = "6")) %>%
    count(AspAct, Values) %>%
    group_by(AspAct) %>%
    mutate(Freq = prop.table(n)) %>%
    ggplot(aes(y = Freq, x = AspAct, fill = Values)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(direction = -1) +
    labs(y = "Proportion of respondents", x = "") +
    guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
    coord_flip() +
    theme_bw(base_size = 17) +
    theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.background = element_blank()),
  
  
  # Mosaic plots
  
  ##1.1
  mosaic_activity = ana.data.1.1 %>% 
  mutate(University = if_else(University == "1", "University", "Other"),
         University = factor(University, levels = c("University", "Other")),
         Gender = fct_recode(Gender, "Men" = "Male", "Women" = "Female")) %>% 
    ggplot() +
    geom_mosaic(aes(x = product(Gender, Values, University), fill = Values), offset = 0.02) + 
    scale_fill_viridis_d(direction = 1,
                         labels = c("Several times a week", "Several times a month", "Several times a year", "Rarely", "Never"),
                         name = "") +
    #scale_y_productlist(labels = c("Several times a week", "Several times a month", "Several times a year", "Rarely", "Never")) +
    labs(x = "", y = "") +
    coord_flip() +
    guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank()),
  
  
  #axis label frequency, text scale y axis, increase offset
  
  
  ##1.2a
  
  mosaic_importance = ana.data.1.2a %>% 
    mutate(Domain = if_else(Domain == "R", "Research", "Teaching")) %>% 
  ggplot() +
    geom_mosaic(aes(x = product(Gender, Values, Domain), fill = Values), offset = 0.02) + 
    scale_fill_viridis_d(direction = -1,
                         labels = c("Minimally important", "Somewhat important", "Very important", "Extremely important"),
                         name = "") +
    #scale_y_productlist(labels = c("Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    coord_flip() +
    guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank()),
  
  
  ##1.3
  mosaic_action = ggplot(data = ana.data.1.3) +
    geom_mosaic(aes(x = product(Gender), fill = Values), offset = 0.02) + 
    scale_fill_viridis_d(direction = -1,
                         labels = c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week"),
                         name = "") +
    #scale_y_productlist(labels = c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week")) +
    labs(x = "", y = "") +
    coord_flip() +
    guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank()),
  
  
  ##3.3
  mosaic_all = ana.data.3.3 %>% 
  mutate(Domain = fct_recode(Domain,
                             "Research" = "R",
                             "Supervision" = "S",
                             "Teaching" = "T")) %>% 
  mutate(Values = fct_relevel(Values, "5", "4", "3", "2")) %>% 
  #mutate(Domain = fct_relevel(Domain, "Research", "Teaching", "Supervision")) %>% 
  mutate(Aspect = fct_recode(Aspect, "Data sharing" = "Data", "Code sharing" = "Code" , "Methods/protocol sharing" = "Method", "Open access publishing" = "Publish", "Science communication \nthrough open channels" = "Communication", "Research reproducibility" = "Reproducibility", "Research transparency" = "Transparency")) %>% 
  mutate(Aspect = fct_relevel(Aspect, "Data sharing", "Code sharing", "Methods/protocol sharing", "Open access publishing", "Science communication \nthrough open channels", "Research reproducibility", "Research transparency")) %>%
    ggplot() +
    geom_mosaic(aes(x = product(Aspect), fill = Values), offset = 0.02) + 
    scale_fill_viridis_d(direction = 1, 
                         labels = c("Extremely important", "Very important", "Somewhat important", "Minimally important"),
                         name = "") +
    #scale_y_productlist(labels = c("Extremely important", "Very important", "Somewhat important", "Minimally important")) +
    labs(x = "", y = "", title="") +
    facet_grid(~ Domain) +
    guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
    coord_flip() +
    theme_minimal(base_size = 18) +
    theme(axis.text.x=element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank())
  
)


# #1
# plotdata3.3.1 = plotdata3.3 %>% 
#   filter(Aspect=="Data"),
# 
# 
# 
# mosaic_data = ggplot(data=plotdata3.3.1)+
#   geom_mosaic(aes( x=product(Domain), fill = Values), offset=0.02) + 
#   scale_fill_viridis_d() +
#   scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
#   labs(x = "", y = "") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank())+
#   labs(title="Data sharing"),
# 
# mosaic_data,
# 
# ##3.3
# #2
# plotdata3.3.2 = plotdata3.3 %>% 
#   filter(Aspect=="Code") %>% 
#   
#   
#   mosaic_code = ggplot(data=plotdata3.3.2)+
#   geom_mosaic(aes( x=product(Values,Domain), fill = Values), offset=0.02) + 
#   scale_fill_viridis_d() +
#   scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
#   labs(x = "", y = "") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank())+
#   labs(title="Code sharing"),
# 
# mosaic_code,
# 
# #3
# plotdata3.3.3 = plotdata3.3 %>% 
#   filter(Aspect=="Method"),
# 
# mosaic_method = ggplot(data=plotdata3.3.3)+
#   geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
#   scale_fill_viridis_d() +
#   scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
#   labs(x = "", y = "") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank())+
#   labs(title="Method sharing"),
# 
# mosaic_method,
# 
# 
# 
# #4
# plotdata3.3.4 = plotdata3.3 %>% 
#   filter(Aspect=="Publish"),
# 
# mosaic_publish = ggplot(data=plotdata3.3.4)+
#   geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
#   scale_fill_viridis_d() +
#   scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
#   labs(x = "", y = "") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank())+
#   labs(title="Publishing open access"),
# 
# mosaic_publish,
# 
# 
# #5
# plotdata3.3.5 = plotdata3.3 %>% 
#   filter(Aspect=="Communication"),
# 
# mosaic_communication = ggplot(data=plotdata3.3.5)+
#   geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
#   scale_fill_viridis_d() +
#   scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
#   labs(x = "", y = "") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank())+
#   labs(title="Communicating science through open channels"),
# 
# mosaic_communication,
# 
# #6
# plotdata3.3.6 = plotdata3.3 %>% 
#   filter(Aspect=="Reproducibility"),
# 
# mosaic_reproducibility = ggplot(data=plotdata3.3.6)+
#   geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
#   scale_fill_viridis_d() +
#   scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
#   labs(x = "", y = "") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank())+
#   labs(title="Research reproducibility"),
# 
# mosaic_reproducibility,
# 
# #7
# plotdata3.3.7 = plotdata3.3 %>% 
#   filter(Aspect=="Transparency"),
# 
# mosaic_transparency = ggplot(data=plotdata3.3.7)+
#   geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
#   scale_fill_viridis_d() +
#   scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
#   labs(x = "", y = "") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank())+
#   labs(title="Research transparency")+
#   
#   
#   mosaic_transparency,

# grid.newpage(),
# pushViewport(viewport(layout = grid.layout(4,2))),
# 
# print(mosaic_data, vp = viewport(layout.pos.row = 1, layout.pos.col = 1)),
# print(mosaic_code, vp = viewport(layout.pos.row = 1, layout.pos.col = 2)),
# print(mosaic_method, vp = viewport(layout.pos.row = 2, layout.pos.col = 1)),
# print(mosaic_publish, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)),
# print(mosaic_communication, vp = viewport(layout.pos.row = 3, layout.pos.col = 1)),
# print(mosaic_reproducibility, vp = viewport(layout.pos.row = 3, layout.pos.col = 2)),
# print(mosaic_transparency, vp = viewport(layout.pos.row = 4, layout.pos.col = 1)),

