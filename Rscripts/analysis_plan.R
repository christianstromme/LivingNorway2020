#data analysis plan

library(ordinal)
library(tidyverse)
library(nlme)
library(lme4)
library(MuMIn)

analysis_plan = drake_plan(
  # ...
  
  ana.data = bndata %>%
    mutate(does.T.teaching = if_else(does.T.teaching == 2, 0, does.T.teaching)) %>%
    select(NO:usein.S.prior, Degree, does.T.teaching, does.S.supervision  ) %>%
    pivot_longer( cols = c(does.T.ug, does.T.pg, does.T.outreach, does.T.other, does.S.ug, does.S.pg, does.S.pd, does.R.synthesis,  does.R.Assessment, does.R.policy, does.R.outreach, does.R.other, learnt.use.Data, learnt.use.Code, learnt.use.Publish, learnt.share.Data, learnt.share.Code, learnt.share.Publish, learnt.use.EduTool, imp.R.Data, imp.R.Code, imp.R.Method, imp.R.Publish, imp.S.Data, imp.S.Code, imp.S.Method, imp.S.Publish, imp.T.Data, imp.T.Code, imp.T.Method, imp.T.Publish, imp.R.Communication, imp.R.Communication, imp.R.Reproducibility, imp.R.Transparency, imp.S.Communication, imp.S.Reproducibility, imp.S.Transparency, imp.T.Communication, imp.T.Reproducibility, imp.T.Transparency, use.engage.Data, use.engage.Code, use.engage.Publish, use.engage.EduTool, does.engage.Review, does.engage.Outreach, share.engage.Data, share.engage.Code, share.engage.Publish, share.engage.Methods) , names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(NO = as.factor(NO), University = as.factor(University), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other"),
  
  #Removed "Other" gender category due to singleton. Combine or remove?
  
  
  
  options(na.action = "na.fail"),
  
  
  
  #1.1 Researchers with academic affiliation have engaged more in open science-related practices compared to other researchers, and more so for early-career researchers
  
  
  
  ana.data.1.1 = bndata %>% 
    select(NO, Gender, Degree, University, does.R.primary, Year, 
           use.engage.Data:does.engage.Outreach, share.engage.Data:share.engage.Methods) %>% 
    pivot_longer( cols = c(use.engage.Data:does.engage.Outreach, share.engage.Data:share.engage.Methods), 
                  names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    filter(Degree == 3, does.R.primary ==1, Values != 6, !is.na(Values)) %>% 
    mutate(NO = as.factor(NO), 
           University = as.factor(University), 
           Values = as.factor(Values), Year = 2020 - Year),
  #mutate(NO = as.factor(NO), 
  #      University = as.factor(University), 
  #     Values = as.factor(Values), 
  #    Year = case_when(Year >= 2012 ~ "Early", 
  #                    Year < 2012 ~ "Late"))
  #defined early-carreer researchers as within eight years of phd defence
  #filtering out those without phd
  #filtering out Values = 6 ("I don't know")
  
  eng.clmm.g = clmm(Values ~ University * Year + Gender + (1|NO), data = ana.data.1.1),
  
  dredge(eng.clmm.g),
  
  eng.clmm.f = clmm(Values ~ University + Gender + (1|NO), data = ana.data.1.1),
  
  summary(eng.clmm.f),
  
  #checking for violation of proportional odds assumption
  
  eng.clmm2.f1 = clmm2(Values ~ University + Gender, random = NO, data = ana.data.1.1, Hess = TRUE),
  eng.clmm2.f2 = clmm2(Values ~ University, nominal = ~Gender, random = NO, data = ana.data.1.1, Hess = TRUE),
  eng.clmm2.f3 = clmm2(Values ~ Gender, nominal = ~University, random = NO, data = ana.data.1.1, Hess = TRUE),
  
  
  anova(eng.clmm2.f1, eng.clmm2.f2),
  anova(eng.clmm2.f1, eng.clmm2.f3),
  
  #assumption ok
  
  ##1.2 OS practices are perceived to be more important in data-, code-, methods-sharing and publishing than educational tools, depending on activities engaged in.
  
  #data sharing
  
  ana.data.1.2a = bndata %>% 
    select(NO, Gender, does.T.teaching, does.R.primary, imp.R.Data, imp.T.Data, share.engage.Data) %>% 
    pivot_longer( cols = c(imp.R.Data, imp.T.Data), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(T_R = paste (does.T.teaching, does.R.primary, sep = '_'), NO = as.factor(NO), Values = as.factor(Values)) %>%
    filter(Gender!= "Other", T_R == "1_1", share.engage.Data != 6, Values != 1, !is.na(Values)),
  #filtering out Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
  #filtering out share.engage.data = 6 ("I don't know")
  
  imp.clmm.a.g = clmm(Values ~ Domain + share.engage.Data + Gender + (1|NO), data = ana.data.1.2a)  ,
  
  dredge(imp.clmm.a.g),
  
  imp.clmm.a.f = clmm(Values ~ Domain + Gender + (1|NO), data = ana.data.1.2a),  
  
  summary(imp.clmm.a.f),
  
  #checking for violation of proportional odds assumption
  
  imp.clmm2.a.f1 = clmm2(Values ~ Domain + Gender, random = NO, data = ana.data.1.2a, Hess = TRUE)  ,
  imp.clmm2.a.f2 = clmm2(Values ~ Domain, nominal = ~Gender, random = NO, data = ana.data.1.2a, Hess = TRUE),
  imp.clmm2.a.f3 = clmm2(Values ~ Gender, nominal = ~Domain, random = NO, data = ana.data.1.2a, Hess = TRUE),
  
  anova(imp.clmm2.a.f1, imp.clmm2.a.f2),
  anova(imp.clmm2.a.f1, imp.clmm2.a.f3),
  
  #assumption ok
  
  
  #code sharing
  
  ana.data.1.2b = bndata %>% 
    select(NO, Gender, does.T.teaching, does.R.primary,imp.R.Code, imp.T.Code, share.engage.Code) %>% 
    pivot_longer( cols = c(imp.R.Code, imp.T.Code), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(T_R = paste (does.T.teaching, does.R.primary, sep = '_'), NO = as.factor(NO), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other", T_R == "1_1", share.engage.Code != 6, Values != 1, !is.na(Values)),
  #filtering Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
  #filtering out share.engage.data = 6 ("I don't know")
  
  imp.clmm.b.g = clmm(Values ~ Domain + share.engage.Code + Gender + (1|NO), data = ana.data.1.2b)  ,
  
  dredge(imp.clmm.b.g),
  
  imp.clmm.b.f = clmm(Values ~ share.engage.Code + (1|NO), data = ana.data.1.2b)  ,
  
  summary(imp.clmm.b.f),
  
  #checking for violation of proportional odds assumption
  
  imp.clm.b.f = clm(Values ~ share.engage.Code, data = ana.data.1.2b)  ,
  
  nominal_test(imp.clm.b.f),
  
  #check ok
  
  
  #methods sharing
  
  ana.data.1.2c = bndata %>% 
    select(NO, Gender, does.T.teaching, does.R.primary, imp.R.Method, imp.T.Method, share.engage.Methods) %>% 
    pivot_longer( cols = c(imp.R.Method, imp.T.Method), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(T_R = paste (does.T.teaching, does.R.primary, sep = '_'), NO = as.factor(NO), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other", T_R == "1_1", share.engage.Methods != 6, Values != 1, !is.na(Values)),
  #filtering Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
  #filtering out share.engage.data = 6 ("I don't know")
  
  imp.clmm.c.g = clmm(Values ~ Domain + share.engage.Methods + Gender + (1|NO), data = ana.data.1.2c)  ,
  
  dredge(imp.clmm.c.g),
  
  #model with no fixed terms most parsimonious withing delta2-criterion. Discuss
  
  
  #open publishing
  
  ana.data.1.2d = bndata %>% 
    select(NO, Gender, does.T.teaching, does.R.primary, imp.R.Publish, imp.T.Publish, share.engage.Publish) %>% 
    pivot_longer( cols = c(imp.R.Publish, imp.T.Publish), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(T_R = paste (does.T.teaching, does.R.primary, sep = '_'), NO = as.factor(NO), Values = as.factor(Values)) %>% 
    filter(Gender!= "Other", T_R == "1_1", share.engage.Publish != 6, Values != 1, !is.na(Values)),
  #filtering Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
  #filtering out share.engage.data = 6 ("I don't know")
  
  imp.clmm.d.g = clmm(Values ~ Domain + share.engage.Publish + Gender + (1|NO), data = ana.data.1.2d)  ,
  
  dredge(imp.clmm.d.g),
  
  
  #model with no fixed terms most parsimonious withing delta2-criterion. Discuss
  
  
  
  #1.3 BUT Most people have used open data and code, but have not contributed to open data and code.
  
  
  ana.data.1.3 = ana.data %>% 
    filter( Action %in% c("use", "share"), Aspect %in% c("Code", "Data"), Values != 6, !is.na(Values)),
  #filtering out values = 6 ("I don't know")
  
  use.clmm.g = clmm(Values ~ Action + Aspect + Gender + (1|NO), data = ana.data.1.3),
  
  dredge(use.clmm.g),
  
  
  use.clmm.f = clmm(Values ~ Action + Gender + (1|NO), data = ana.data.1.3),
  
  summary(use.clmm.f),
  
  #checking for violation of proportional odds assumption
  
  use.clmm2.f1 = clmm2(Values ~ Action + Gender, random = NO, data = ana.data.1.3, Hess = TRUE),
  use.clmm2.f2 = clmm2(Values ~ Action, nominal = ~Gender, random = NO, data = ana.data.1.3, Hess = TRUE),
  use.clmm2.f3 = clmm2(Values ~ Gender, nominal = ~Action, random = NO, data = ana.data.1.3, Hess = TRUE),
  
  anova(use.clmm2.f1, use.clmm2.f2),
  anova(use.clmm2.f1, use.clmm2.f3),
  
  #check ok
  
  
  #1.4 People more likely to use OS in supervision and teaching when they use it more in their own research.
  
  #make average values for OS activity scores
  
  ana.data.1.4 = bndata %>%
    select( NO, Gender, does.R.primary, useinTprior, useinSprior, use.engage.Data:does.engage.Outreach) %>% 
    pivot_longer( cols = c(use.engage.Data:does.engage.Outreach), 
                  names_to = c("Action", "Domain", "Aspect"), 
                  values_to = "Values", names_sep = "[.]") %>% 
    mutate(Values = as.numeric(Values)) %>% 
    filter(does.R.primary == "1", Gender!= "Other", !is.na(useinTprior), !is.na(useinSprior) ) %>% 
    group_by(NO, Gender, useinTprior, useinSprior) %>% 
    summarize(Values = mean(Values, na.rm = TRUE)) %>% 
    mutate(useinTprior = if_else(useinTprior == 2, 0, useinTprior),
           useinSprior = if_else(useinSprior == 2, 0, useinSprior)),
  
  teach.glm.g = glm(useinTprior ~  Values + Gender, family = "binomial", data = ana.data.1.4),
  
  dredge(teach.glm.g),
  
  #no significant terms
  
  supervise.glm.g = glm(useinSprior ~  Values + Gender, family = "binomial", data = ana.data.1.4),
  
  dredge(supervise.glm.g),
  
  supervise.glm.f = glm(useinSprior ~ Gender, family = "binomial", data = ana.data.1.4),
  
  summary(supervise.glm.f),
  
  
  #no significant terms
  
  #3.1 Researchers who teach are more likely to include open science-related practices in their teaching if they also frequently engage in those practices as researchers.
  #This prediction is addressed in 1.1
  
  #3.2 Colloquium participants having more experience with being taught open science-related practices are more likely to engage in those practices as researchers, and teachers.
  
  #3.2a Use open data
  ana.data.3.2a = bndata %>% 
    select(NO, Gender, use.engage.Data, learnt.use.Data) %>% 
    mutate(use.engage.Data = as.factor(use.engage.Data), learnt.use.Data = as.logical(learnt.use.Data)) %>% 
    filter(Gender!= "Other", use.engage.Data != 6, !is.na(learnt.use.Data), !is.na(use.engage.Data)),
  #filtering out use.engage.data = 6 ("I don't know"),
  
  learn.clm.a.g = clm(use.engage.Data ~ learnt.use.Data + Gender, data = ana.data.3.2a)  ,
  
  dredge(learn.clm.a.g),
  
  learn.clm.a.f = clm(use.engage.Data ~ Gender, data = ana.data.3.2a)  ,
  
  summary(learn.clm.a.f),
  
  #checking for violation of proportional odds assumption
  
  nominal_test(learn.clm.a.f),
  
  #check ok
  
  #3.2b Use open code
  ana.data.3.2b = bndata %>% 
    select(NO, Gender, use.engage.Code, learnt.use.Code) %>% 
    mutate(use.engage.Code = as.factor(use.engage.Code), learnt.use.Code = as.logical(learnt.use.Code)) %>% 
    filter(Gender!= "Other", use.engage.Code != 6, !is.na(learnt.use.Code), !is.na(use.engage.Code)),
  #filtering out use.engage.Code = 6 ("I don't know")
  
  learn.clm.b.g = clm(use.engage.Code ~ learnt.use.Code + Gender, data = ana.data.3.2b)  ,
  
  dredge(learn.clm.b.g),
  
  learn.clm.b.f = clm(use.engage.Code ~ learnt.use.Code, data = ana.data.3.2b)  ,
  
  summary(learn.clm.b.f),
  
  #checking for violation of proportional odds assumption
  
  nominal_test(learn.clm.b.f),
  
  #check ok
  
  
  #3.2c Use open publications
  ana.data.3.2c = bndata %>% 
    select(NO, Gender, use.engage.Publish, learnt.use.Publish) %>% 
    mutate(use.engage.Publish = as.factor(use.engage.Publish), learnt.use.Publish = as.logical(learnt.use.Publish)) %>% 
    filter(Gender!= "Other", use.engage.Publish != 6, !is.na(learnt.use.Publish), !is.na(use.engage.Publish)),
  #filtering out use.engage.Publish = 6 ("I don't know")
  
  learn.clm.c.g = clm(use.engage.Publish ~ learnt.use.Publish + Gender, data = ana.data.3.2c)  ,
  
  dredge(learn.clm.c.g),
  
  #model with no fixed terms most parsimonious withing delta2-criterion. Discuss
  
  #3.2d Share open data
  ana.data.3.2d = bndata %>% 
    select(NO, Gender, share.engage.Data, learnt.share.Data) %>% 
    mutate(share.engage.Data = as.factor(share.engage.Data), learnt.share.Data = as.logical(learnt.share.Data)) %>% 
    filter(Gender!= "Other",  share.engage.Data != 6, !is.na(learnt.share.Data), !is.na(share.engage.Data)),
  #filtering out share.engage.Data = 6 ("I don't know")
  
  learn.clm.d.g = clm(share.engage.Data ~ learnt.share.Data + Gender, data = ana.data.3.2d)  ,
  
  dredge(learn.clm.d.g),
  
  learn.clm.d.f = clm(share.engage.Data ~ Gender, data = ana.data.3.2d)  ,
  
  summary(learn.clm.d.f),
  
  #checking for violation of proportional odds assumption
  
  nominal_test(learn.clm.d.f),
  
  #check ok
  
  
  #3.2e Share open code
  ana.data.3.2e = bndata %>% 
    select(NO, Gender, share.engage.Code, learnt.share.Code) %>% 
    mutate(share.engage.Code = as.factor(share.engage.Code), learnt.share.Code = as.logical(learnt.share.Code)) %>% 
    filter(Gender!= "Other", share.engage.Code != 6,  !is.na(learnt.share.Code), !is.na(share.engage.Code)),
  #filtering out share.engage.Code = 6 ("I don't know")
  
  learn.clm.e.g = clm(share.engage.Code ~ learnt.share.Code + Gender, data = ana.data.3.2e)  ,
  
  dredge(learn.clm.e.g),
  
  learn.clm.e.f = clm(share.engage.Code ~ learnt.share.Code, data = ana.data.3.2e)  ,
  
  summary(learn.clm.e.f),
  
  #checking for violation of proportional odds assumption
  
  nominal_test(learn.clm.e.f),
  
  #check ok
  
  
  #3.2f Publish open access
  ana.data.3.2f = bndata %>% 
    select(NO, Gender, share.engage.Publish, learnt.share.Publish) %>% 
    mutate(share.engage.Publish = as.factor(share.engage.Publish), learnt.share.Publish = as.logical(learnt.share.Publish)) %>% 
    filter(Gender!= "Other", share.engage.Publish != 6, !is.na(learnt.share.Publish), !is.na(share.engage.Publish)),
  #filtering out share.engage.Publish = 6 ("I don't know")
  
  learn.clm.f.g = clm(share.engage.Publish ~ learnt.share.Publish + Gender, data = ana.data.3.2f)  ,
  
  dredge(learn.clm.f.g),
  
  #model with no fixed terms most parsimonious within delta2-criterion. Discuss
  
  #3.2g Use open education tools
  ana.data.3.2g = bndata %>% 
    select(NO, Gender, use.engage.EduTool, learnt.use.EduTool) %>% 
    mutate(use.engage.EduTool = as.factor(use.engage.EduTool), learnt.use.EduTool = as.logical(learnt.use.EduTool)) %>% 
    filter(Gender!= "Other", use.engage.EduTool != 6, !is.na(learnt.use.EduTool), !is.na(use.engage.EduTool)),
  #filtering out use.engage.EduTool = 6 ("I don't know")
  
  learn.clm.g.g = clm(use.engage.EduTool ~ learnt.use.EduTool + Gender, data = ana.data.3.2g)  ,
  
  dredge(learn.clm.g.g),
  
  learn.clm.g.f = clm(use.engage.EduTool ~ learnt.use.EduTool, data = ana.data.3.2g)  ,
  
  summary(learn.clm.g.f),
  
  #checking for violation of proportional odds assumption
  
  nominal_test(learn.clm.g.f),
  
  #no lokLik for nominal model, using scale_test instead
  
  scale_test(learn.clm.g.f),
  
  #check ok
  
  
  #3.2h Does open review
  ana.data.3.2h = bndata %>% 
    select(NO, Gender, does.engage.Review, learnt.do.Review) %>% 
    mutate(does.engage.Review = as.factor(does.engage.Review), learnt.do.Review = as.logical(learnt.do.Review)) %>% 
    filter(Gender!= "Other", does.engage.Review != 6, !is.na(learnt.do.Review), !is.na(does.engage.Review)),
  #filtering out does.engage.Review = 6 ("I don't know")
  
  learn.clm.h.g = clm(does.engage.Review ~ learnt.do.Review + Gender, data = ana.data.3.2h)  ,
  
  dredge(learn.clm.h.g),
  
  
  #model with no fixed terms most parsimonious within delta2-criterion. Discuss
  
  
  #3.2i Does outreach
  ana.data.3.2i = bndata %>% 
    select(NO, Gender, does.engage.Outreach, learnt.do.Outreach) %>% 
    mutate(does.engage.Outreach = as.factor(does.engage.Outreach), learnt.do.Outreach = as.logical(learnt.do.Outreach)) %>% 
    filter(Gender!= "Other", does.engage.Outreach != 6, !is.na(learnt.do.Outreach), !is.na(does.engage.Outreach)),
  #filtering out does.engage.Outreach = 6 ("I don't know")
  
  learn.clm.i.g = clm(does.engage.Outreach ~ learnt.do.Outreach + Gender, data = ana.data.3.2i)  ,
  
  dredge(learn.clm.i.g),
  
  #model with no fixed terms most parsimonious
  
  
  #3.3 Colloquium participants "think" OS practices are more important in their research compared to teaching and supervision.
  ana.data.3.3 = ana.data %>% 
    filter(Action == "imp", Values != 1, !is.na(Values)) %>% 
    mutate(NO = as.factor(NO), Values = as.factor(Values)),
  #filtering out Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
  
  imp.clmm.g = clmm(Values ~ Domain + Aspect + Gender + (1|NO), data = ana.data.3.3),
  
  dredge(imp.clmm.g),
  
  imp.clmm.f = clmm(Values ~ Domain + Aspect + (1|NO), data = ana.data.3.3),
  
  summary(imp.clmm.f),
  
  #checking for violation of proportional odds assumption
  
  imp.clmm2.f1 = clmm2(Values ~ Domain + Aspect, random = NO, data = ana.data.3.3, Hess = TRUE),
  imp.clmm2.f2 = clmm2(Values ~ Aspect, nominal = ~Domain, random = NO, data = ana.data.3.3, Hess = TRUE),
  imp.clmm2.f3 = clmm2(Values ~ Domain, nominal = ~Aspect, random = NO, data = ana.data.3.3, Hess = TRUE),
  
  anova(imp.clmm2.f1, imp.clmm2.f2),
  anova(imp.clmm2.f1, imp.clmm2.f3),
  
  imp.clm.f = clm(Values ~ Domain + Aspect, data = ana.data.3.3, Hess = TRUE),
  
  #performing nominal test using clm due to high Hessian for imp.clmm2.f2
  
  nominal_test(imp.clm.f),
  
  #violation of proportional odds for Domain
  
  #scaling Domain to relax proportional odds assumption, resulting in partial-proportional odds model
  imp.clmm2.f = clmm2(Values ~ Domain + Aspect, scale = ~Domain, random = NO, data = ana.data.3.3, Hess = TRUE),
  
  summary(imp.clmm2.f)

)
