#data analysis plan

library(ordinal)
library(tidyverse)
library(nlme)
library(lme4)
library(MuMIn)

analysis_plan <- drake_plan(
  # ...
  
  ana.data <- bndata %>%
    select(NO:usein.S.prior, does.T.teaching,  does.S.supervision  ) %>%
    pivot_longer( cols = c(does.T.ug, does.T.pg, does.T.outreach, does.T.other, does.S.ug, does.S.pg, does.S.pd, does.R.primary, does.R.synthesis,  does.R.Assessment, does.R.policy, does.R.outreach, does.R.other, learnt.use.Data, learnt.use.Code, learnt.use.Publish, learnt.share.Data, learnt.share.Code, learnt.share.Publish, learnt.use.EduTool, imp.R.Data, imp.R.Code, imp.R.Method, imp.R.Publish, imp.S.Data, imp.S.Code, imp.S.Method, imp.S.Publish, imp.T.Data, imp.T.Code, imp.T.Method, imp.T.Publish, imp.R.Communication, imp.R.Communication, imp.R.Reproducibility, imp.R.Transparency, imp.S.Communication, imp.S.Reproducibility, imp.S.Transparency, imp.T.Communication, imp.T.Reproducibility, imp.T.Transparency, use.engage.Data, use.engage.Code, use.engage.Publsh, use.engage.EduTool, does.engage.Review, does.engage.Outreach, share.engage.Data, share.engage.Code, share.engage.Publish, share.engage.Methods) , names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    filter(Gender!= "Other") ,

  #Removed "Other" gender category due to singleton. Combine or removed?
  
  ana.data$fValues <- as.factor(ana.data$Values),
  ana.data$fUniversity <- as.factor(ana.data$University),

  options(na.action = "na.fail"),
  
#1.1 Researchers with academic affiliation have engaged more in open science-related practices compared to other researchers, and more so for early-career researchers



ana.data.1.1 <- ana.data %>% 
  filter(Action == "use" | Action == "share",!is.na(fValues)) %>% 
  mutate(Year = 2020 - Year),


eng.clm.g <- clm(fValues ~ fUniversity + Gender + Year, data = ana.data.1.1),

dredge(eng.clm.g),

eng.clm.f <- clm(fValues ~ fUniversity + Gender, data = ana.data.1.1),

summary(eng.clm.f),

               



#1.2 OS practices are perceived to be more important in data-, code-, methods-sharing and publishing than educational tools, depending on activities engaged in.

##1.2 OS practices are perceived to be more important in data-, code-, methods-sharing and publishing than educational tools, depending on activities engaged in.

#data sharing

ana.data.1.2a <- bndata %>% 
  select(NO, Gender, imp.R.Data, imp.T.Data, share.engage.Data) %>% 
  pivot_longer( cols = c(imp.R.Data, imp.T.Data), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
  mutate(Values = as.factor(Values)) %>% 
  filter(Gender!= "Other", !is.na(Values)),

imp.clm.a.g <- clm(Values ~ Domain + share.engage.Data + Gender, data = ana.data.1.2a)  ,

dredge(imp.clm.a.g),

imp.clm.a.f <- imp.clm.a.g,

summary(imp.clm.a.f),

#code sharing

ana.data.1.2b <- bndata %>% 
  select(NO, Gender, imp.R.Code, imp.T.Code, share.engage.Code) %>% 
  pivot_longer( cols = c(imp.R.Code, imp.T.Code), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
  mutate(Values = as.factor(Values)) %>% 
  filter(Gender!= "Other", !is.na(Values)),

imp.clm.b.g <- clm(Values ~ Domain + share.engage.Code + Gender, data = ana.data.1.2b) , 

dredge(imp.clm.b.g),

imp.clm.b.f <- clm(Values ~ Domain + share.engage.Code, data = ana.data.1.2b)  ,

summary(imp.clm.b.f),


#methods sharing

ana.data.1.2c <- bndata %>% 
  select(NO, Gender, imp.R.Method, imp.T.Method, share.engage.Methods) %>% 
  pivot_longer( cols = c(imp.R.Method, imp.T.Method), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
  mutate(Values = as.factor(Values)) %>% 
  filter(Gender!= "Other", !is.na(Values)),

imp.clm.c.g <- clm(Values ~ Domain + share.engage.Methods + Gender, data = ana.data.1.2c)  ,

dredge(imp.clm.c.g),

imp.clm.c.f <- clm(Values ~ Domain + Gender, data = ana.data.1.2c)  ,

summary(imp.clm.c.f),

#open publishing

ana.data.1.2d <- bndata %>% 
  select(NO, Gender, imp.R.Publish, imp.T.Publish, share.engage.Publish) %>% 
  pivot_longer( cols = c(imp.R.Publish, imp.T.Publish), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
  mutate(Values = as.factor(Values)) %>% 
  filter(Gender!= "Other", !is.na(Values)),

imp.clm.d.g <- clm(Values ~ Domain + share.engage.Publish + Gender, data = ana.data.1.2d)  ,

dredge(imp.clm.d.g),

imp.clm.d.f <- clm(Values ~ Domain, data = ana.data.1.2d)  ,

summary(imp.clm.d.f),


#1.3 BUT Most people have used open data and code, but have not contributed to open data and code.


ana.data.1.3 <- ana.data %>% 
  filter( Action %in% c("use", "share"), !is.na(fValues)),

use.clm.g <- clm(fValues ~ Aspect + Action * Gender, data = ana.data.1.3),

dredge(use.clm.g),

use.clm.f <- clm(fValues ~ Aspect + Action + Gender, data = ana.data.1.3),

summary(use.clm.f)
,


#1.4 People more likely to use OS in supervision and teaching when they use it more in their ownresearch.

#make average values for OS activity scores

ana.data.1.4 <- bndata %>%
  select( NO, Gender, useinTprior, useinSprior, use.engage.Data:does.engage.Outreach) %>% 
  pivot_longer( cols = c(use.engage.Data:does.engage.Outreach), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
  mutate(Values = as.numeric(Values)) %>% 
  filter(Gender!= "Other",!is.na(useinTprior), !is.na(useinSprior) ) %>% 
  group_by(NO, Gender, useinTprior, useinSprior) %>% 
  summarize(Values = mean(Values, na.rm = TRUE)),

ana.data.1.4$useinTprior <- as.numeric(ana.data.1.4$useinTprior) ,
ana.data.1.4$useinTprior[ana.data.1.4$useinTprior == "2"] <- "0",
ana.data.1.4$useinTprior <- as.numeric(ana.data.1.4$useinTprior) ,

ana.data.1.4$useinSprior <- as.numeric(ana.data.1.4$useinSprior) ,
ana.data.1.4$useinSprior[ana.data.1.4$useinSprior == "2"] <- "0",
ana.data.1.4$useinSprior <- as.numeric(ana.data.1.4$useinSprior) ,


teach.glm.g <- glm(useinTprior ~  Values + Gender, family = "binomial", data = ana.data.1.4),

dredge(teach.glm.g),

#no significant terms

supervise.glm.g <- glm(useinSprior ~  Values + Gender, family = "binomial", data = ana.data.1.4),

dredge(supervise.glm.g),

supervise.glm.f <- glm(useinSprior ~ Gender, family = "binomial", data = ana.data.1.4),

summary(supervise.glm.f),


#no significant terms



#2.1 Colloquium participants having less experience with open science-related practices are more likely to change or adapt their open science related practices in research, teaching and/or supervision after the colloquium.

#2.2 Motivation for doing the workshop - experience with OS practices

#2.3 Statements of use in the future vs used in the past -- predictor also: stated usefulness of the workshop ... (open ended answers available to qualify)
                                       
#3.1 Researchers who teach are more likely to include open science-related practices in their teaching if they also frequently engage in those practices as researchers.

#3.2 Colloquium participants having more experience with being taught open science-related practices are more likely to engage in those practices as researchers, and teachers.

ana.data.3.2 <- ana.data %>% 
  filter(Action == "imp",!is.na(fValues)),

imp.clm.g <- clm(fValues ~ Domain + Aspect + Gender, data = ana.data.3.2),

dredge(imp.clm.g),

imp.clm.f <- imp.clm.g,

summary(imp.clm.f)

#3.3 Colloquium participants "think" OS practices are more important in their research compared to teaching and supervision.


##Teaching




#Fragments
#early_carreer$fGender <- as.factor(early_carreer$Gender)

#ea1 <- clmm(fShared_data ~ Year + (1|fGender) , data=early_carreer)
#summary(ea1)

#data$fImportance_research <- as.factor(data$Importance_research)

#data$fImportance_teaching <- as.factor(data$Importance_teaching)





#Model development following protocol from Zuur et al. 2009

#n.practlm <- lm(n.pract ~ Affiliation * fGender, data = XX ,na.action=na.omit)
#n.practlme <- lme(n.pract ~ Affiliation * Gender, data = XX ,na.action=na.omit, method="REML", random=~1|fGender)

#anova(n.practlm, n.practlme)

#run global model using gls or lmer, depending on inclusion of random variable

)
