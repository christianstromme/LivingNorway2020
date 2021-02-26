#data analysis plan

library(ordinal)
library(lme4)
library(nlme)
library(lmerTest)

analysis_plan <- drake_plan(
  # ...
  
  plotdata %>% 
    select(NO, Question, Category, Value) %>% 
    filter(Category %in% c("Importance_research", "Importance_teaching")) %>% 
    mutate(Question = str_sub(Question, 1, str_length(Question)-2)) %>% 
    pivot_wider(names_from = Category, values_from = Value)
  
)

#early_carreer<-filter(data, Degree == "3" | Position == "3"  )


#1.1 Researchers with academic affiliation have engaged more in open science-related practices compared to other researchers, and more so for early-career researchers

#Define new variable for dataframe, "Affiliation" with levels
#Define new variable for dataframe, number of OS practices engaged in                    

#Model development following protocol from Zuur et al. 2009
#n.practlm <- lm(n.pract ~ Affiliation * fGender, data = XX ,na.action=na.omit)
#n.practlme <- lme(n.pract ~ Affiliation * Gender, data = XX ,na.action=na.omit, method="REML", random=~1|fGender)

#anova(n.practlm, n.practlme)

#run global model using gls or lmer, depending on inclusion of random variable


#1.2 OS practices are perceived to be more important in data-, code-, methods-sharing and publishing than educational tools, depending on activities engaged in.

#Define new variable for dataframe, "Activity" with levels

#n.practclm <- clm(Importance_R ~ Activity * fGender, data = XX ,na.action=na.omit)
#n.practclmm <- clmm(Importance_R ~ Activity * fGender + (1|Activity*fGender:Individual), data = XX ,na.action=na.omit)

#anova(n.practclm, n.practclmm)


#run model using clm or clmm, depending on inclusion of random variable

#dredge(globalmodel)
                    

#dredge(globalmodel)


#1.3 BUT Most people have used open data and code, but have not contributed to open data and code.

#1.4 People more likely to use OS in supervision and teaching when they use it more in their ownresearch.

#2.1 Colloquium participants having less experience with open science-related practices are more likely to change or adapt their open science related practices in research, teaching and/or supervision after the colloquium.

#2.2 Motivation for doing the workshop - experience with OS practices

#2.3 Statements of use in the future vs used in the past -- predictor also: stated usefulness of the workshop ... (open ended answers available to qualify)
                                       
#3.1 Researchers who teach are more likely to include open science-related practices in their teaching if they also frequently engage in those practices as researchers.

#3.2 Colloquium participants having more experience with being taught open science-related practices are more likely to engage in those practices as researchers, and teachers.

#3.3 Colloquium participants "think" OS practices are more important in their research compared to teaching and supervision.


##Teaching



#Fragments
#early_carreer$fGender <- as.factor(early_carreer$Gender)

#ea1 <- clmm(fShared_data ~ Year + (1|fGender) , data=early_carreer)
#summary(ea1)

#data$fImportance_research <- as.factor(data$Importance_research)

#data$fImportance_teaching <- as.factor(data$Importance_teaching)