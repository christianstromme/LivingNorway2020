#data analysis plan

library(ordinal)
library(tidyverse)
library(nlme)
library(lme4)
library(MuMIn)

analysis_plan = drake_plan(
  # ...
  
  ana.data = bndata %>%
    select(NO:usein.S.prior, does.T.teaching,  does.S.supervision  ) %>%
    pivot_longer( cols = c(does.T.ug, does.T.pg, does.T.outreach, does.T.other, does.S.ug, does.S.pg, does.S.pd, does.R.primary, does.R.synthesis,  does.R.Assessment, does.R.policy, does.R.outreach, does.R.other, learnt.use.Data, learnt.use.Code, learnt.use.Publish, learnt.share.Data, learnt.share.Code, learnt.share.Publish, learnt.use.EduTool, imp.R.Data, imp.R.Code, imp.R.Method, imp.R.Publish, imp.S.Data, imp.S.Code, imp.S.Method, imp.S.Publish, imp.T.Data, imp.T.Code, imp.T.Method, imp.T.Publish, imp.R.Communication, imp.R.Communication, imp.R.Reproducibility, imp.R.Transparency, imp.S.Communication, imp.S.Reproducibility, imp.S.Transparency, imp.T.Communication, imp.T.Reproducibility, imp.T.Transparency, use.engage.Data, use.engage.Code, use.engage.Publish, use.engage.EduTool, does.engage.Review, does.engage.Outreach, share.engage.Data, share.engage.Code, share.engage.Publish, share.engage.Methods) , names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(NO = as.factor(NO), University = as.factor(University), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other"),
  
  #Removed "Other" gender category due to singleton. Combine or removed?
  
  
  options(na.action = "na.fail"),
  
  
  
  #1.1 Researchers with academic affiliation have engaged more in open science-related practices compared to other researchers, and more so for early-career researchers
  
  
  
  ana.data.1.1 = ana.data %>% 
    filter(Action == "use" | Action == "share",!is.na(Values)) %>% 
    mutate(Year = 2020 - Year),
  
  
  eng.clm.g = clm(Values ~ University + Gender + Year, data = ana.data.1.1),
  eng.clmm.g = clmm(Values ~ University + Gender + Year + (1|NO), data = ana.data.1.1),
  
  anova(eng.clm.g, eng.clmm.g),
  
  dredge(eng.clmm.g),
  
  eng.clmm.f = clmm(Values ~ Gender + (1|NO), data = ana.data.1.1),
  
  summary(eng.clmm.f),
  
  eng.clm.f = clm(Values ~ Gender, data = ana.data.1.1),
  
  nominal_test(eng.clm.f),
  scale_test(eng.clm.f),
  
  eng.clmm2.f = clmm2(Values ~ Gender, scale = ~Gender, random = NO, data = ana.data.1.1, Hess = TRUE),
  
  summary(eng.clmm2.f),
  
  ##1.2 OS practices are perceived to be more important in data-, code-, methods-sharing and publishing than educational tools, depending on activities engaged in.
  
  #data sharing
  
  ana.data.1.2a = bndata %>% 
    select(NO, Gender, imp.R.Data, imp.T.Data, share.engage.Data) %>% 
    pivot_longer( cols = c(imp.R.Data, imp.T.Data), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(NO = as.factor(NO), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other", !is.na(Values)),
  
  imp.clm.a.g = clm(Values ~ Domain + share.engage.Data + Gender, data = ana.data.1.2a)  ,
  imp.clmm.a.g = clmm(Values ~ Domain + share.engage.Data + Gender + (1|NO), data = ana.data.1.2a)  ,
  
  anova (imp.clm.a.g, imp.clmm.a.g),
  
  dredge(imp.clm.a.g),
  
  imp.clm.a.f = clm(Values ~ Domain + share.engage.Data, data = ana.data.1.2a),  
  
  summary(imp.clm.a.f),
  
  nominal_test(imp.clm.a.f),
  scale_test(imp.clm.a.f),
  
  
  #code sharing
  
  ana.data.1.2b = bndata %>% 
    select(NO, Gender, imp.R.Code, imp.T.Code, share.engage.Code) %>% 
    pivot_longer( cols = c(imp.R.Code, imp.T.Code), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(NO = as.factor(NO), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other", !is.na(Values)),
  
  imp.clm.b.g = clm(Values ~ Domain + share.engage.Code + Gender, data = ana.data.1.2b)  ,
  imp.clmm.b.g = clmm(Values ~ Domain + share.engage.Code + Gender + (1|NO), data = ana.data.1.2b)  ,
  
  anova(imp.clm.b.g, imp.clmm.b.g),
  
  dredge(imp.clmm.b.g),
  
  imp.clmm.b.f = clmm(Values ~ Domain + share.engage.Code + (1|NO), data = ana.data.1.2b)  ,
  
  summary(imp.clmm.b.f),
  
  imp.clmm.b.nom = clmm2(Values ~ Domain + share.engage.Code, random = NO, data = ana.data.1.2b),
  imp.clmm.b.nom2 = clmm2(Values ~ share.engage.Code, nominal = ~Domain, random = NO, data = ana.data.1.2b),
  imp.clmm.b.nom3 = clmm2(Values ~ Domain, nominal = ~share.engage.Code, random = NO, data = ana.data.1.2b),
  
  anova(imp.clmm.b.nom, imp.clmm.b.nom2),
  anova(imp.clmm.b.nom, imp.clmm.b.nom3),
  
  #methods sharing
  
  ana.data.1.2c = bndata %>% 
    select(NO, Gender, imp.R.Method, imp.T.Method, share.engage.Methods) %>% 
    pivot_longer( cols = c(imp.R.Method, imp.T.Method), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(NO = as.factor(NO), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other", !is.na(Values)),
  
  imp.clm.c.g = clm(Values ~ Domain + share.engage.Methods + Gender, data = ana.data.1.2c)  ,
  imp.clmm.c.g = clmm(Values ~ Domain + share.engage.Methods + Gender + (1|NO), data = ana.data.1.2c)  ,
  
  anova(imp.clm.c.g, imp.clmm.c.g),
  
  dredge(imp.clm.c.g),
  
  imp.clm.c.f = clm(Values ~ Domain, data = ana.data.1.2c) , 
  
  summary(imp.clm.c.f),
  
  nominal_test(imp.clm.c.f),
  scale_test(imp.clm.c.f),
  
  #open publishing
  
  ana.data.1.2d = bndata %>% 
    select(NO, Gender, imp.R.Publish, imp.T.Publish, share.engage.Publish) %>% 
    pivot_longer( cols = c(imp.R.Publish, imp.T.Publish), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(NO = as.factor(NO), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other", !is.na(Values)),
  
  imp.clm.d.g = clm(Values ~ Domain + share.engage.Publish + Gender, data = ana.data.1.2d)  ,
  imp.clmm.d.g = clmm(Values ~ Domain + share.engage.Publish + Gender + (1|NO), data = ana.data.1.2d)  ,
  
  anova(imp.clm.d.g, imp.clmm.d.g),
  
  dredge(imp.clmm.d.g),
  
  imp.clmm.d.f = clmm(Values ~ Domain + (1|NO), data = ana.data.1.2d),
  
  summary(imp.clmm.d.f),
  
  imp.clm.d.f = clm(Values ~ Domain, data = ana.data.1.2d),
  
  nominal_test(imp.clm.d.f),
  scale_test(imp.clm.d.f),
  
  #1.3 BUT Most people have used open data and code, but have not contributed to open data and code.
  
  
  
  ana.data.1.3 = ana.data %>% 
    filter( Action %in% c("use", "share"), Aspect %in% c("Code", "Data"), !is.na(Values)),
  
  use.clm.g = clm(Values ~ Aspect + Action + Gender, data = ana.data.1.3),
  use.clmm.g = clmm(Values ~ Aspect + Action + Gender + (1|NO), data = ana.data.1.3),
  
  anova (use.clm.g, use.clmm.g),
  
  dredge(use.clmm.g),
  
  
  use.clmm.f = clmm(Values ~ Action + Gender + (1|NO), data = ana.data.1.3),
  
  summary(use.clmm.f),
  
  use.clmm.nom = clmm2(Values ~ Action + Gender, random = NO, data = ana.data.1.3),
  use.clmm.nom2 = clmm2(Values ~ Action, nominal = ~Gender, random = NO, data = ana.data.1.3),
  use.clmm.nom3 = clmm2(Values ~ Gender, nominal = ~Action, random = NO, data = ana.data.1.3),
  
  anova(use.clmm.nom, use.clmm.nom2),
  #proportional odds assumption violated for Gender
  
  anova(use.clmm.nom, use.clmm.nom3),
  
  #scaling Gender to relax proportional odds assumption
  use.clmm2.f = clmm2(Values ~ Action + Gender, scale = ~Gender, random = NO, data = ana.data.1.3, Hess = TRUE),
  
  summary(use.clmm2.f),
  
  
  #1.4 People more likely to use OS in supervision and teaching when they use it more in their own research.
  
  #make average values for OS activity scores
  
  ana.data.1.4 = bndata %>%
    select( NO, Gender, useinTprior, useinSprior, use.engage.Data:does.engage.Outreach) %>% 
    pivot_longer( cols = c(use.engage.Data:does.engage.Outreach), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(Values = as.numeric(Values)) %>% 
    filter(Gender!= "Other",!is.na(useinTprior), !is.na(useinSprior) ) %>% 
    group_by(NO, Gender, useinTprior, useinSprior) %>% 
    summarize(Values = mean(Values, na.rm = TRUE)),
  
  #ana.data.1.4$useinTprior = as.numeric(ana.data.1.4$useinTprior),
  #ana.data.1.4$useinTprior[ana.data.1.4$useinTprior == "2"] = "0",
  #ana.data.1.4$useinTprior = as.numeric(ana.data.1.4$useinTprior),
  
  #ana.data.1.4$useinSprior = as.numeric(ana.data.1.4$useinSprior),
  #ana.data.1.4$useinSprior[ana.data.1.4$useinSprior == "2"] = "0",
  #ana.data.1.4$useinSprior = as.numeric(ana.data.1.4$useinSprior),
  
  
  #teach.glm.g = glm(useinTprior ~  Values + Gender, family = "binomial", data = ana.data.1.4),
  
  #dredge(teach.glm.g),
  
  #no significant terms
  
  #supervise.glm.g = glm(useinSprior ~  Values + Gender, family = "binomial", data = ana.data.1.4),
  
  #dredge(supervise.glm.g),
  
  #supervise.glm.f = glm(useinSprior ~ Gender, family = "binomial", data = ana.data.1.4),
  
  #summary(supervise.glm.f),
  
  
  #no significant terms
  
  #3.1 Researchers who teach are more likely to include open science-related practices in their teaching if they also frequently engage in those practices as researchers.
  #This prediction is addressed in 1.1
  
  #3.2 Colloquium participants having more experience with being taught open science-related practices are more likely to engage in those practices as researchers, and teachers.
  
  #3.2a Use open data
  ana.data.3.2a = bndata %>% 
    select(NO, Gender, use.engage.Data, learnt.use.Data) %>% 
    mutate(use.engage.Data = as.factor(use.engage.Data), learnt.use.Data = as.logical(learnt.use.Data)) %>% 
    filter(Gender!= "Other", !is.na(learnt.use.Data), !is.na(use.engage.Data)),
  
  learn.clm.a.g = clm(use.engage.Data ~ learnt.use.Data + Gender, data = ana.data.3.2a)  ,
  
  dredge(learn.clm.a.g),
  
  learn.clm.a.f = clm(use.engage.Data ~ Gender, data = ana.data.3.2a)  ,
  
  summary(learn.clm.a.f),
  
  #3.2b Use open code
  ana.data.3.2b = bndata %>% 
    select(NO, Gender, use.engage.Code, learnt.use.Code) %>% 
    mutate(use.engage.Code = as.factor(use.engage.Code), learnt.use.Code = as.logical(learnt.use.Code)) %>% 
    filter(Gender!= "Other", !is.na(learnt.use.Code), !is.na(use.engage.Code)),
  
  learn.clm.b.g = clm(use.engage.Code ~ learnt.use.Code + Gender, data = ana.data.3.2b) , 
  
  dredge(learn.clm.b.g),
  
  learn.clm.b.f = clm(use.engage.Code ~ learnt.use.Code, data = ana.data.3.2b)  ,
  
  summary(learn.clm.b.f),
  
  #3.2c Use open publications
  ana.data.3.2c = bndata %>% 
    select(NO, Gender, use.engage.Publish, learnt.use.Publish) %>% 
    mutate(use.engage.Publish = as.factor(use.engage.Publish), learnt.use.Publish = as.logical(learnt.use.Publish)) %>% 
    filter(Gender!= "Other", !is.na(learnt.use.Publish), !is.na(use.engage.Publish)),
  
  learn.clm.c.g = clm(use.engage.Publish ~ learnt.use.Publish + Gender, data = ana.data.3.2c)  ,
  
  dredge(learn.clm.c.g),
  
  learn.clm.c.f = clm(use.engage.Publish ~ learnt.use.Publish, data = ana.data.3.2c)  ,
  
  summary(learn.clm.c.f),
  
  #3.2d Share open data
  ana.data.3.2d = bndata %>% 
    select(NO, Gender, share.engage.Data, learnt.share.Data) %>% 
    mutate(share.engage.Data = as.factor(share.engage.Data), learnt.share.Data = as.logical(learnt.share.Data)) %>% 
    filter(Gender!= "Other", !is.na(learnt.share.Data), !is.na(share.engage.Data)),
  
  learn.clm.d.g = clm(share.engage.Data ~ learnt.share.Data + Gender, data = ana.data.3.2d)  ,
  
  dredge(learn.clm.d.g),
  
  learn.clm.d.f = clm(share.engage.Data ~ Gender, data = ana.data.3.2d) ,
  
  summary(learn.clm.d.f),
  
  nominal_test(learn.clm.d.f),
  scale_test(learn.clm.d.f),
  
  #3.2e Share open code
  ana.data.3.2e = bndata %>% 
    select(NO, Gender, share.engage.Code, learnt.share.Code) %>% 
    mutate(share.engage.Code = as.factor(share.engage.Code), learnt.share.Code = as.logical(learnt.share.Code)) %>% 
    filter(Gender!= "Other", !is.na(learnt.share.Code), !is.na(share.engage.Code)),
  
  learn.clm.e.g = clm(share.engage.Code ~ learnt.share.Code + Gender, data = ana.data.3.2e)  ,
  
  dredge(learn.clm.e.g),
  
  learn.clm.e.f = clm(share.engage.Code ~ Gender, data = ana.data.3.2e)  ,
  
  summary(learn.clm.e.f),
  
  nominal_test(learn.clm.e.f),
  scale_test(learn.clm.e.f),
  
  #3.2f Publish open access
  ana.data.3.2f = bndata %>% 
    select(NO, Gender, share.engage.Publish, learnt.share.Publish) %>% 
    mutate(share.engage.Publish = as.factor(share.engage.Publish), learnt.share.Publish = as.logical(learnt.share.Publish)) %>% 
    filter(Gender!= "Other", !is.na(learnt.share.Publish), !is.na(share.engage.Publish)),
  
  learn.clm.f.g = clm(share.engage.Publish ~ learnt.share.Publish + Gender, data = ana.data.3.2f)  ,
  
  dredge(learn.clm.f.g),
  
  learn.clm.f.f = clm(share.engage.Publish ~ learnt.share.Publish, data = ana.data.3.2f)  ,
  
  summary(learn.clm.f.f),
  
  #3.2g Use open education tools
  ana.data.3.2g = bndata %>% 
    select(NO, Gender, use.engage.EduTool, learnt.use.EduTool) %>% 
    mutate(use.engage.EduTool = as.factor(use.engage.EduTool), learnt.use.EduTool = as.logical(learnt.use.EduTool)) %>% 
    filter(Gender!= "Other", !is.na(learnt.use.EduTool), !is.na(use.engage.EduTool)),
  
  learn.clm.g.g = clm(use.engage.EduTool ~ learnt.use.EduTool + Gender, data = ana.data.3.2g) , 
  
  dredge(learn.clm.g.g),
  
  learn.clm.g.f = clm(use.engage.EduTool ~ learnt.use.EduTool, data = ana.data.3.2g)  ,
  
  summary(learn.clm.g.f),
  
  #3.2h Does open review
  ana.data.3.2h = bndata %>% 
    select(NO, Gender, does.engage.Review, learnt.do.Review) %>% 
    mutate(does.engage.Review = as.factor(does.engage.Review), learnt.do.Review = as.logical(learnt.do.Review)) %>% 
    filter(Gender!= "Other", !is.na(learnt.do.Review), !is.na(does.engage.Review)),
  
  learn.clm.h.g = clm(does.engage.Review ~ learnt.do.Review + Gender, data = ana.data.3.2h)  ,
  
  dredge(learn.clm.h.g),
  
  #No significant terms
  
  #3.2i Does outreach
  ana.data.3.2i = bndata %>% 
    select(NO, Gender, does.engage.Outreach, learnt.do.Outreach) %>% 
    mutate(does.engage.Outreach = as.factor(does.engage.Outreach), learnt.do.Outreach = as.logical(learnt.do.Outreach)) %>% 
    filter(Gender!= "Other", !is.na(learnt.do.Outreach), !is.na(does.engage.Outreach)),
  
  learn.clm.i.g = clm(does.engage.Outreach ~ learnt.do.Outreach + Gender, data = ana.data.3.2i)  ,
  
  dredge(learn.clm.i.g),
  
  #No significant terms
  
  
  #3.3 Colloquium participants "think" OS practices are more important in their research compared to teaching and supervision.
  ana.data.3.2 = ana.data %>% 
    filter(Action == "imp",!is.na(Values)) %>% 
    mutate(NO = as.factor(NO), Values = as.factor(Values)),
  
  imp.clm.g = clm(Values ~ Domain + Aspect + Gender, data = ana.data.3.2),
  imp.clmm.g = clmm(Values ~ Domain + Aspect + Gender + (1|NO), data = ana.data.3.2),
  
  anova(imp.clm.g, imp.clmm.g),
  
  dredge(imp.clmm.g),
  
  imp.clmm.f = clmm(Values ~ Domain + Aspect + (1|NO), data = ana.data.3.2),
  
  summary(imp.clmm.f),
  
  imp.clmm.nom = clmm2(Values ~ Domain + Aspect, random = NO, data = ana.data.3.2),
  imp.clmm.nom2 = clmm2(Values ~ Aspect, nominal = ~Domain, random = NO, data = ana.data.3.2),
  imp.clmm.nom3 = clmm2(Values ~ Domain, nominal = ~Aspect, random = NO, data = ana.data.3.2),
  
  anova(imp.clmm.nom, imp.clmm.nom2),
  #proportional odds assumption violated for Domain
  
  anova(imp.clmm.nom, imp.clmm.nom3),
  #proportional odds assumption violated for Aspect
  
  #scaling Domain to relax proportional odds assumption
  imp.clmm2.f = clmm2(Values ~ Domain + Aspect, scale = ~Domain, random = NO, data = ana.data.3.2, Hess = TRUE),
  
  summary(imp.clmm2.f)

)
