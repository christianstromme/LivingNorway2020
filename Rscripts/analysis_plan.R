#data analysis plan

analysis_plan = drake_plan(
  
  # prep data for analysis
  ana.data = bndata %>%
    mutate(does.T.teaching = if_else(does.T.teaching == 2, 0, does.T.teaching)) %>%
    select(NO:does.engage.Outreach, share.engage.Data:usein.S.prior) %>%
    pivot_longer( cols = imp.R.Data:share.engage.Methods , names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>% 
    mutate(NO = as.factor(NO), University = as.factor(University), Values = as.factor(Values)),
  

  ## Question 1.1: Researchers with academic affiliation have engaged more in open science-related practices compared to other researchers, and more so for early-career researchers.
  ana.data.1.1 = bndata %>% 
    select(NO, Gender, Degree, University, does.R.primary, Year, 
           use.engage.Data:does.engage.Outreach, share.engage.Data:share.engage.Methods) %>% 
    pivot_longer( cols = c(use.engage.Data:does.engage.Outreach, share.engage.Data:share.engage.Methods), 
                  names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>%
    
    # filtering out those without phd
    # filtering out Values = 6 ("I don't know")
    filter(Degree == 3, 
           does.R.primary ==1, 
           Values != 6, 
           !is.na(Values)) %>% 
    # make Value, NO and University a factor
    # defined early-career researchers as within eight years of phd defence
    mutate(NO = as.factor(NO), 
           University = as.factor(University), 
           Values = as.factor(Values), 
           Year = case_when(Year >= 2012 ~ "Early", 
                            Year < 2012 ~ "Late")),

  # run global mixed effects model
  eng.clmm.g = clmm(Values ~ University * Year + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.1),
  
  # model selection
  dredge.eng.clmm.g = dredge(eng.clmm.g),
  
  # final model
  eng.clmm.f = clmm(Values ~ University + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.1),
  
  # summary
  sum.eng.clmm.f = summary(eng.clmm.f),
  
  #checking for violation of proportional odds assumption
  
  eng.clmm2.f1 = clmm2(Values ~ University + Gender, random = NO, data = ana.data.1.1, Hess = TRUE),
  eng.clmm2.f2 = clmm2(Values ~ University, nominal = ~Gender, random = NO, data = ana.data.1.1, Hess = TRUE),
  eng.clmm2.f3 = clmm2(Values ~ Gender, nominal = ~University, random = NO, data = ana.data.1.1, Hess = TRUE),
  
  
  eng.clmm2.f.test1 = anova(eng.clmm2.f1, eng.clmm2.f2),
  eng.clmm2.f.test2 = anova(eng.clmm2.f1, eng.clmm2.f3),
  
  #assumption ok

  
  ## Questions 1.2: OS practices are perceived to be more important in data-, code-, methods-sharing and publishing than educational tools, depending on activities engaged in.

  #data sharing

  ana.data.1.2a = bndata %>%
    select(NO, Gender, does.T.teaching, does.R.primary, imp.R.Data, imp.T.Data, share.engage.Data) %>%
    pivot_longer(cols = c(imp.R.Data, imp.T.Data), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>%
    mutate(T_R = paste(does.T.teaching, does.R.primary, sep = '_'), 
           NO = as.factor(NO), 
           share.engage.Data = as.factor(share.engage.Data)) %>%
    
    #filtering out Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
    #filtering out share.engage.data = 6 ("I don't know")
    filter(T_R == "1_1", share.engage.Data != 6, 
           Values != 1, 
           !is.na(Values)) %>%
    mutate(Values = as.factor(Values)),
  
  # run global mixed effects model
  imp.clmm.a.g = clmm(Values ~ Domain +  Gender + (1|NO), na.action = "na.fail", data = ana.data.1.2a),

  # model selection
  dredge.imp.clmm.a.g = dredge(imp.clmm.a.g),

  # final model
  imp.clmm.a.f = clmm(Values ~ Domain + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.2a),

  # summary
  sum.imp.clmm.a.f = summary(imp.clmm.a.f),
  
  #checking for violation of proportional odds assumption

  imp.clmm2.a.f1 = clmm2(Values ~ Domain + Gender, random = NO, data = ana.data.1.2a, Hess = TRUE),
  imp.clmm2.a.f2 = clmm2(Values ~ Domain, nominal = ~Gender, random = NO, data = ana.data.1.2a, Hess = TRUE),
  imp.clmm2.a.f3 = clmm2(Values ~ Gender, nominal = ~Domain, random = NO, data = ana.data.1.2a, Hess = TRUE),

  imp.clmm.a.f.test1 = anova(imp.clmm2.a.f1, imp.clmm2.a.f2),
  imp.clmm.a.f.test2 = anova(imp.clmm2.a.f1, imp.clmm2.a.f3),

  #assumption ok


  #code sharing

  ana.data.1.2b = bndata %>%
    select(NO, Gender, does.T.teaching, does.R.primary,imp.R.Code, imp.T.Code, share.engage.Code) %>%
    pivot_longer( cols = c(imp.R.Code, imp.T.Code), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>%
    mutate(T_R = paste (does.T.teaching, does.R.primary, sep = '_'), 
           NO = as.factor(NO), 
           share.engage.Code = as.factor(share.engage.Code), 
           Values = as.factor(Values)) %>%
    
    #filtering Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
    #filtering out share.engage.data = 6 ("I don't know")
    filter(T_R == "1_1", share.engage.Code != 6, 
           Values != 1, 
           !is.na(Values)),
  
  # run global mixed effects model
  imp.clmm.b.g = clmm(Values ~ Domain + share.engage.Code + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.2b),

  # model selection
  dredge.imp.clmm.b.g = dredge(imp.clmm.b.g),
  #no significant terms -> null intercept model is the best

  # final model
  imp.clmm.b.f = clmm(Values ~ 1 + (1|NO), na.action = "na.fail", data = ana.data.1.2b),
  
  # summary
  sum.imp.clmm.b.f = summary(imp.clmm.b.f),
  

  #methods sharing

  ana.data.1.2c = bndata %>%
    select(NO, Gender, does.T.teaching, does.R.primary, imp.R.Method, imp.T.Method, share.engage.Methods) %>%
    pivot_longer( cols = c(imp.R.Method, imp.T.Method), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>%
    mutate(T_R = paste(does.T.teaching, does.R.primary, sep = '_'), 
           NO = as.factor(NO), 
           share.engage.Methods = as.factor(share.engage.Methods), 
           Values = as.factor(Values)) %>%
    
    #filtering Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
    #filtering out share.engage.data = 6 ("I don't know")
    filter(T_R == "1_1", 
           share.engage.Methods != 6, 
           Values != 1, 
           !is.na(Values)),

  # run global mixed effects model
  imp.clmm.c.g = clmm(Values ~ Domain + share.engage.Methods + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.2c),

  # model selection
  dredge.imp.clmm.c.g = dredge(imp.clmm.c.g),
  #no significant terms -> null intercept model is the best
  
  # final model
  imp.clmm.c.f = clmm(Values ~ 1 + (1|NO), na.action = "na.fail", data = ana.data.1.2c),
  
  # summary
  sum.imp.clmm.c.f = summary(imp.clmm.c.f),


  #open publishing

  ana.data.1.2d = bndata %>%
    select(NO, Gender, does.T.teaching, does.R.primary, imp.R.Publish, imp.T.Publish, share.engage.Publish) %>%
    pivot_longer( cols = c(imp.R.Publish, imp.T.Publish), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>%
    mutate(T_R = paste(does.T.teaching, does.R.primary, sep = '_'), 
           NO = as.factor(NO), 
           share.engage.Publish = as.factor(share.engage.Publish), 
           Values = as.factor(Values)) %>%
    
    #filtering Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
    #filtering out share.engage.data = 6 ("I don't know")
    filter(T_R == "1_1", 
           share.engage.Publish != 6, 
           Values != 1, 
           !is.na(Values)),

  # run global mixed effects model
  imp.clmm.d.g = clmm(Values ~ Domain + share.engage.Publish + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.2d),

  # model selection
  dredge.imp.clmm.d.g = dredge(imp.clmm.d.g),
  #no significant terms -> null intercept model is the best
  
  # final model
  imp.clmm.d.f = clmm(Values ~ 1 + (1|NO), na.action = "na.fail", data = ana.data.1.2d),
  
  # summary
  sum.imp.clmm.d.f = summary(imp.clmm.d.f),


  
  ## Question 1.3: BUT Most people have used open data and code, but have not contributed to open data and code.
  
  ana.data.1.3 = ana.data %>%
    mutate(Values = as.numeric(Values)) %>%
    
    #filtering out values = 6 ("I don't know")
    filter(Action %in% c("use", "share"), 
           Aspect %in% c("Code", "Data"), 
           Values != 6, 
           Values != 0, 
           !is.na(Values)) %>%
    mutate(Values = as.factor(Values)),


  # run global mixed effects model
  use.clmm.g = clmm(Values ~ Action + Aspect + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.3),

  # model selection
  dredge.use.clmm.g = dredge(use.clmm.g),

  use.clmm.f = clmm(Values ~ Action + Gender + (1|NO), na.action = "na.fail", data = ana.data.1.3),

  # summary
  sum.use.clmm.f = summary(use.clmm.f),
  
  #checking for violation of proportional odds assumption

  use.clmm2.f1 = clmm2(Values ~ Action + Gender, random = NO, data = ana.data.1.3, Hess = TRUE),
  use.clmm2.f2 = clmm2(Values ~ Action + Gender, scale = ~Gender, random = NO, data = ana.data.1.3, Hess = TRUE),
  use.clmm2.f3 = clmm2(Values ~ Action + Gender, scale = ~Action, random = NO, data = ana.data.1.3, Hess = TRUE),

  use.clmm2.test.1 = anova(use.clmm2.f1, use.clmm2.f2),
  use.clmm2.test.2 = anova(use.clmm2.f1, use.clmm2.f3),

  #Proportional odds violated for Gender. Scaling to relax assumption.


  sum.use.clmm2.f2 = summary(use.clmm2.f2),


  ## Question 1.4: People more likely to use OS in supervision and teaching when they use it more in their own research.

  ana.data.1.4 = bndata %>%
    select(NO, Gender, does.R.primary, useinTprior, useinSprior, use.engage.Data:does.engage.Outreach) %>%
    pivot_longer(cols = c(use.engage.Data:does.engage.Outreach), names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>%
    mutate(Values = as.numeric(Values)) %>%
    #filtering out values = 6 ("I don't know")
    filter(does.R.primary == "1",
           Values != 6, 
           !is.na(useinTprior), 
           !is.na(useinSprior)) %>%
    
    #make average values for OS activity scores
    group_by(NO, Gender, useinTprior, useinSprior) %>%
    summarize(Values = mean(Values, na.rm = TRUE)) %>% 
    # replace 2 with 0
    mutate(useinTprior = if_else(useinTprior == 2, 0, useinTprior),
           useinSprior = if_else(useinSprior == 2, 0, useinSprior)),

  # Teaching
  # run global model
  teach.glm.g = glm(useinTprior ~  Values + Gender, family = "binomial", na.action = "na.fail", data = ana.data.1.4),

  # model selection
  dredge.teach.glm.g = dredge(teach.glm.g),

  #no significant terms$
  
  # final model
  teach.glm.f = glm(useinTprior ~  1, family = "binomial", na.action = "na.fail", data = ana.data.1.4),
  
  # sum
  sum.teach.glm.f = summary(teach.glm.f),


  # Supervision
  # run global model
  supervise.glm.g = glm(useinSprior ~  Values + Gender, family = "binomial", na.action = "na.fail", data = ana.data.1.4),

  # model selection
  dredge.supervise.glm.g = dredge(supervise.glm.g),

  # final model
  supervise.glm.f = glm(useinSprior ~ Gender, family = "binomial", na.action = "na.fail", data = ana.data.1.4),

  # summary
  sum.supervise.glm.f = summary(supervise.glm.f),

  
  
  ## Question 1.4: People more likely to use OS in supervision and teaching when they use it more in their own research.

  ana.data.1.4.2 = bndata %>%
    select(NO, Gender:does.R.primary, imp.S.Data:imp.T.Publish, share.engage.Data:useinSprior) %>%
    mutate(useinTprior = if_else(useinTprior == 2, 0, useinTprior),
           useinSprior = if_else(useinSprior == 2, 0, useinSprior)) %>%
    filter(does.R.primary == "1"),

  ana.data.1.4.teach = ana.data.1.4.2 %>%
    mutate(imp.T.Data = as.factor(imp.T.Data), 
           share.engage.Data = as.factor(share.engage.Data),
           imp.T.Code = as.factor(imp.T.Code), 
           share.engage.Code = as.factor(share.engage.Code), 
           imp.T.Method = as.factor(imp.T.Method), 
           share.engage.Methods = as.factor(share.engage.Methods), 
           imp.T.Publish = as.factor(imp.T.Publish), 
           share.engage.Publish = as.factor(share.engage.Publish)) %>%
    filter(useinTprior == 1),
  

  # Data
  # run global model
  teach.clm.data.g = clm(imp.T.Data ~ share.engage.Data + Gender, na.action = "na.fail", 
                          data = ana.data.1.4.teach %>% 
                           filter(!is.na(imp.T.Data))),
  #not converging

  
  # Code
  # run global model
  teach.clm.code.g = clm(imp.T.Code ~ share.engage.Code + Gender, na.action = "na.fail", 
                          data = ana.data.1.4.teach %>% 
                            filter(!is.na(imp.T.Code))),
  #not converging

  
  # Method
  # run global model
  teach.clm.method.g = clm(imp.T.Method ~ share.engage.Methods + Gender, na.action = "na.fail",
                            data = ana.data.1.4.teach %>% 
                              filter(!is.na(imp.T.Method))),
  #not converging


  # Publish
  ana.data.1.4.teach.publish = ana.data.1.4.teach %>% 
    filter(!is.na(imp.T.Publish)),
  # run global model
  teach.clm.publish.g = clm(imp.T.Publish ~ share.engage.Publish + Gender, na.action = "na.fail",
                             data = ana.data.1.4.teach.publish),
  # model selection
  dredge.teach.clm.publish.g = dredge(teach.clm.publish.g),
  #no significant terms

  teach.clm.publish.f = clm(imp.T.Publish ~ 1, na.action = "na.fail",
                            data = ana.data.1.4.teach.publish),
  
  
  #supervision

  ana.data.1.4.superv = ana.data.1.4.2 %>%
    mutate(imp.S.Data = as.factor(imp.S.Data), 
           share.engage.Data = as.factor(share.engage.Data),
           imp.S.Code = as.factor(imp.S.Code), 
           share.engage.Code = as.factor(share.engage.Code), 
           imp.S.Method = as.factor(imp.S.Method), 
           share.engage.Methods = as.factor(share.engage.Methods), 
           imp.S.Publish = as.factor(imp.S.Publish), 
           share.engage.Publish = as.factor(share.engage.Publish)) %>%
    filter(useinTprior == 1),

  
  # Data
  # run global model
  superv.clm.data.g = clm(imp.S.Data ~share.engage.Data + Gender, na.action = "na.fail", 
                          data = ana.data.1.4.superv %>%
                            filter(!is.na(imp.S.Data))),
  #not converging

  # Code
  # run global model
  superv.clm.code.g = clm(imp.S.Code ~share.engage.Code + Gender, na.action = "na.fail", 
                           data = ana.data.1.4.superv %>% 
                            filter(!is.na(imp.S.Code))),
  #not converging


  # Method
  # run global model
  superv.clm.method.g = clm(imp.S.Method ~ share.engage.Methods + Gender, na.action = "na.fail", 
                           data = ana.data.1.4.superv %>% 
                             filter(!is.na(imp.S.Method))),
  #not converging

  # Publish
  # run global model
  superv.clm.publish.g = clm(imp.S.Publish ~ share.engage.Publish + Gender, na.action = "na.fail", 
                            data = ana.data.1.4.superv %>% 
                              filter(!is.na(imp.S.Publish))),
  #not converging

  

  ## Question 3.1: Researchers who teach are more likely to include open science-related practices in their teaching if they also frequently engage in those practices as researchers.
  #This prediction is addressed in 1.1

  ## Question 3.2: Colloquium participants having more experience with being taught open science-related practices are more likely to engage in those practices as researchers, and teachers.

  #3.2a Use open data
  ana.data.3.2a = bndata %>%
    select(NO, Gender, use.engage.Data, learnt.use.Data) %>%
    mutate(use.engage.Data = as.factor(use.engage.Data), 
           learnt.use.Data = as.logical(learnt.use.Data)) %>%
    #filtering out use.engage.data = 6 ("I don't know")
    filter(use.engage.Data != 6, 
           !is.na(learnt.use.Data), 
           !is.na(use.engage.Data)),


  # run global model
  learn.clm.a.g = clm(use.engage.Data ~ learnt.use.Data + Gender, na.action = "na.fail", data = ana.data.3.2a),

  # model selection
  dredge.learn.clm.a.g = dredge(learn.clm.a.g),

  # final model
  learn.clm.a.f = clm(use.engage.Data ~ Gender, data = ana.data.3.2a),

  # summary
  sum.learn.clm.a.f = summary(learn.clm.a.f),

  
  #checking for violation of proportional odds assumption
  test.res.learn.clm.a.f = nominal_test(learn.clm.a.f),

  #check ok
  #frequency of learning

  
  #3.2b Use open code
  ana.data.3.2b = bndata %>%
    select(NO, Gender, use.engage.Code, learnt.use.Code) %>%
    mutate(use.engage.Code = as.factor(use.engage.Code), 
           learnt.use.Code = as.logical(learnt.use.Code)) %>%
    #filtering out use.engage.Code = 6 ("I don't know")
    filter(use.engage.Code != 6, 
           !is.na(learnt.use.Code), 
           !is.na(use.engage.Code)),

  # run global model
  learn.clm.b.g = clm(use.engage.Code ~ learnt.use.Code + Gender, na.action = "na.fail", data = ana.data.3.2b),

  # model selection
  dredge.learn.clm.b.g = dredge(learn.clm.b.g),

  # final model
  learn.clm.b.f = clm(use.engage.Code ~ learnt.use.Code, na.action = "na.fail", data = ana.data.3.2b),

  # summary
  sum.learn.clm.b.f = summary(learn.clm.b.f),

  
  #checking for violation of proportional odds assumption
  test.res.learn.clm.b.f = nominal_test(learn.clm.b.f),

  #check ok


  #3.2c Use open publications
  ana.data.3.2c = bndata %>%
    select(NO, Gender, use.engage.Publish, learnt.use.Publish) %>%
    mutate(use.engage.Publish = as.factor(use.engage.Publish), 
           learnt.use.Publish = as.logical(learnt.use.Publish)) %>%
    #filtering out use.engage.Publish = 6 ("I don't know")
    filter(use.engage.Publish != 6, 
           !is.na(learnt.use.Publish), 
           !is.na(use.engage.Publish)),

  # run global model
  learn.clm.c.g = clm(use.engage.Publish ~ learnt.use.Publish + Gender, na.action = "na.fail", data = ana.data.3.2c),

  # model selection
  dredge.learn.clm.c.g = dredge(learn.clm.c.g),
  #model with no fixed terms most parsimonious within delta2-criterion.
  
  # final model
  learn.clm.c.f = clm(use.engage.Publish ~ 1, na.action = "na.fail", data = ana.data.3.2c),
  
  

  #3.2d Share open data
  ana.data.3.2d = bndata %>%
    select(NO, Gender, share.engage.Data, learnt.share.Data) %>%
    mutate(share.engage.Data = as.factor(share.engage.Data), 
           learnt.share.Data = as.logical(learnt.share.Data)) %>%
    #filtering out share.engage.Data = 6 ("I don't know")
    filter(share.engage.Data != 6, 
           !is.na(learnt.share.Data), 
           !is.na(share.engage.Data)),

  # run global model
  learn.clm.d.g = clm(share.engage.Data ~ learnt.share.Data + Gender, na.action = "na.fail", data = ana.data.3.2d),

  # model selection
  dredge.learn.clm.d.g = dredge(learn.clm.d.g),

  learn.clm.d.f = clm(share.engage.Data ~ Gender, na.action = "na.fail", data = ana.data.3.2d),

  sum.learn.clm.d.f = summary(learn.clm.d.f),

  
  #checking for violation of proportional odds assumption
  test.learn.clm.d.f = nominal_test(learn.clm.d.f),

  #check ok


  #3.2e Share open code
  ana.data.3.2e = bndata %>%
    select(NO, Gender, share.engage.Code, learnt.share.Code) %>%
    mutate(share.engage.Code = as.factor(share.engage.Code), 
           learnt.share.Code = as.logical(learnt.share.Code)) %>%
    #filtering out share.engage.Code = 6 ("I don't know")
    filter(share.engage.Code != 6,  
           !is.na(learnt.share.Code), 
           !is.na(share.engage.Code)),

  # run global model
  learn.clm.e.g = clm(share.engage.Code ~ learnt.share.Code + Gender, na.action = "na.fail", data = ana.data.3.2e),

  # model selection
  dredge.learn.clm.e.g = dredge(learn.clm.e.g),

  # final model
  learn.clm.e.f = clm(share.engage.Code ~ learnt.share.Code, na.action = "na.fail", data = ana.data.3.2e),

  # summary
  sum.learn.clm.e.f = summary(learn.clm.e.f),

  
  #checking for violation of proportional odds assumption
  test.learn.clm.e.f = nominal_test(learn.clm.e.f),

  #check ok


  #3.2f Publish open access
  ana.data.3.2f = bndata %>%
    select(NO, Gender, share.engage.Publish, learnt.share.Publish) %>%
    mutate(share.engage.Publish = as.factor(share.engage.Publish), 
           learnt.share.Publish = as.logical(learnt.share.Publish)) %>%
    #filtering out share.engage.Publish = 6 ("I don't know")
    filter(share.engage.Publish != 6, 
           !is.na(learnt.share.Publish), 
           !is.na(share.engage.Publish)),


  # run global model
  learn.clm.f.g = clm(share.engage.Publish ~ learnt.share.Publish + Gender, na.action = "na.fail", data = ana.data.3.2f),

  dredge.learn.clm.f.g = dredge(learn.clm.f.g),
  #no significant terms
  
  # final model
  learn.clm.f.f = clm(share.engage.Publish ~ 1, na.action = "na.fail", data = ana.data.3.2f),


  #3.2g Use open education tools
  ana.data.3.2g = bndata %>%
    select(NO, Gender, use.engage.EduTool, learnt.use.EduTool) %>%
    mutate(use.engage.EduTool = as.factor(use.engage.EduTool), 
           learnt.use.EduTool = as.logical(learnt.use.EduTool)) %>%
    #filtering out use.engage.EduTool = 6 ("I don't know")
    filter(use.engage.EduTool != 6, 
           !is.na(learnt.use.EduTool), 
           !is.na(use.engage.EduTool)),
  
  # run global model
  learn.clm.g.g = clm(use.engage.EduTool ~ learnt.use.EduTool + Gender, na.action = "na.fail", data = ana.data.3.2g),

  # model selection
  dredge.learn.clm.g.g = dredge(learn.clm.g.g),

  # final model
  learn.clm.g.f = clm(use.engage.EduTool ~ learnt.use.EduTool, na.action = "na.fail", data = ana.data.3.2g),
  
  # summary
  sum.learn.clm.g.f = summary(learn.clm.g.f),

  
  #checking for violation of proportional odds assumption
  test.1.learn.clm.g.f = nominal_test(learn.clm.g.f),
  #no lokLik for nominal model
  
  # using scale_test instead
  test.2.learn.clm.g.f = scale_test(learn.clm.g.f),

  #check ok


  #3.2h Does open review
  ana.data.3.2h = bndata %>%
    select(NO, Gender, does.engage.Review, learnt.do.Review) %>%
    mutate(does.engage.Review = as.factor(does.engage.Review), 
           learnt.do.Review = as.logical(learnt.do.Review)) %>%
    #filtering out does.engage.Review = 6 ("I don't know")
    filter(does.engage.Review != 6, 
           !is.na(learnt.do.Review), 
           !is.na(does.engage.Review)),

  # run global model
  learn.clm.h.g = clm(does.engage.Review ~ learnt.do.Review + Gender, na.action = "na.fail", data = ana.data.3.2h),

  # model selection
  dredge.learn.clm.h.g = dredge(learn.clm.h.g),
  #no significant terms
  
  # final model
  learn.clm.h.f = clm(does.engage.Review ~ 1, na.action = "na.fail", data = ana.data.3.2h),

  

  #3.2i Does outreach
  ana.data.3.2i = bndata %>%
    select(NO, Gender, does.engage.Outreach, learnt.do.Outreach) %>%
    mutate(does.engage.Outreach = as.factor(does.engage.Outreach),
           learnt.do.Outreach = as.logical(learnt.do.Outreach)) %>%
    #filtering out does.engage.Outreach = 6 ("I don't know")
    filter(does.engage.Outreach != 6, 
           !is.na(learnt.do.Outreach), 
           !is.na(does.engage.Outreach)),

  # run global model
  learn.clm.i.g = clm(does.engage.Outreach ~ learnt.do.Outreach + Gender, na.action = "na.fail", data = ana.data.3.2i),

  dredge.learn.clm.i.g = dredge(learn.clm.i.g),
  #no significant terms

  # final model
  learn.clm.i.f = clm(does.engage.Outreach ~ 1, na.action = "na.fail", data = ana.data.3.2i),
  

  
  ## Quesiton 3.3: Colloquium participants "think" OS practices are more important in their research compared to teaching and supervision.
  ana.data.3.3 = ana.data %>%
    mutate(Values = as.numeric(Values)) %>%
    #filtering out Values = 1 ("Not applicable to my work" does not make sense to include in the scaled responses Y)
    filter(Action == "imp", 
           does.T.teaching == 1, 
           does.R.primary == 1,
           Values != 1, 
           !is.na(Values)) %>%
    mutate(NO = as.factor(NO), 
           Values = as.factor(Values)),

  # run global model
  imp.clmm.g = clmm(Values ~ Domain + Aspect + Gender + (1|NO), na.action = "na.fail", data = ana.data.3.3),

  # model selection
  dredge.imp.clmm.g = dredge(imp.clmm.g),

  # final model
  imp.clmm.f = clmm(Values ~ Domain + Aspect + (1|NO), na.action = "na.fail", data = ana.data.3.3),
  
  # summary
  sum.imp.clmm.f = summary(imp.clmm.f),

  
  #checking for violation of proportional odds assumption
  imp.clmm2.f1 = clmm2(Values ~ Domain + Aspect, random = NO, data = ana.data.3.3, Hess = TRUE),
  imp.clmm2.f2 = clmm2(Values ~ Aspect, nominal = ~Domain, random = NO, data = ana.data.3.3, Hess = TRUE),
  imp.clmm2.f3 = clmm2(Values ~ Domain, nominal = ~Aspect, random = NO, data = ana.data.3.3, Hess = TRUE),

  test.1.res.imp.clmm.f = anova(imp.clmm2.f1, imp.clmm2.f2),
  test.2.res.imp.clmm.f = anova(imp.clmm2.f1, imp.clmm2.f3)


  #check ok

)
