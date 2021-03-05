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

ana.data.1.2 <- ana.data %>% 
  filter(Action == "imp",!is.na(fValues)) ,

imp.clm.g <- clm(fValues ~ Domain + Aspect + Gender, data = ana.data.1.2),

dredge(imp.clm.g),

imp.clm.f <- imp.clm.g,


summary(imp.clm.f),


#1.3 BUT Most people have used open data and code, but have not contributed to open data and code.


ana.data.1.3 <- ana.data %>% 
  filter( Action %in% c("use", "share"), !is.na(fValues)),

use.clm.g <- clm(fValues ~ Aspect + Action * Gender, data = ana.data.1.3),

dredge(use.clm.g),

use.clm.f <- clm(fValues ~ Aspect + Action + Gender, data = ana.data.1.3),

summary(use.clm.f)
,


#1.4 People more likely to use OS in supervision and teaching when they use it more in their ownresearch.

ana.data.1.4 <- ana.data %>%
  filter(does.T.teaching == "1" | does.S.supervision == "1", Action %in% c("use", "share") ) %>% 
  group_by(NO, Domain) %>% 
  summarise(sum.aspects = sum(Aspect, na.rm = TRUE))


#teach.lme.g <- lme( ~  Aspect*Action + Gender, data = ana.data.1.3, family ="binomial", na.action = na.omit)



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





#Model development following protocol from Zuur et al. 2009

#n.practlm <- lm(n.pract ~ Affiliation * fGender, data = XX ,na.action=na.omit)
#n.practlme <- lme(n.pract ~ Affiliation * Gender, data = XX ,na.action=na.omit, method="REML", random=~1|fGender)

#anova(n.practlm, n.practlme)

#run global model using gls or lmer, depending on inclusion of random variable

)
