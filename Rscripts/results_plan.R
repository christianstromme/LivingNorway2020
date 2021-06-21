# results plan

results_plan = drake_plan(
  
  ## Question 1.1
  
  result_Q1.1 = bind_rows(
    global = tidy(eng.clmm.g),
    final = tidy(eng.clmm.f),
    .id = "model"
  ) %>% 
    filter(coef.type == "location") %>% 
    mutate(term = case_when(term == "University1" ~ "University",
                            term == "University1:YearLate" ~ "University:YearLate",
                            term == "GenderMale"~ "Male",
                            TRUE ~ term),
           term = case_when(p.value < 0.001 ~ paste(term, "***"),
                               p.value < 0.01 ~ paste(term, "**"),
                               p.value < 0.05 ~ paste(term, "*"),
                               TRUE ~ term)) %>% 
    select(model, `Fixed effect terms` = term, Estimate = estimate, SE = std.error, `z value` = statistic),

  
  ## Questions 1.2
  
  result_Q1.2 = bind_rows(global_data.sharing = tidy(imp.clmm.a.g),
            final_data.sharing = tidy(imp.clmm.a.f),
            
            global_code.sharing = tidy(imp.clmm.b.g),
            final_code.sharing = tidy(imp.clmm.b.f),
            
            global_method.method = tidy(imp.clmm.c.g),
            final_method.method = tidy(imp.clmm.c.f),
            
            global_open.publish = tidy(imp.clmm.d.g),
            final_open.publish = tidy(imp.clmm.d.f),
            .id = "model_aspect"
            ) %>% 
    filter(coef.type == "location") %>% 
    separate(col = model_aspect, into = c("model", "aspect"), sep = "_") %>% 
    mutate(term = case_when(term == "DomainT" ~ "Teaching",
                            term == "GenderMale"~ "Male",
                            TRUE ~ term),
           term = case_when(p.value < 0.001 ~ paste(term, "***"),
                            p.value < 0.01 ~ paste(term, "**"),
                            p.value < 0.05 ~ paste(term, "*"),
                            TRUE ~ term)) %>% 
    select(model, aspect, `Fixed effect terms` = term, Estimate = estimate, SE = std.error, `z value` = statistic),
  
  result_Q1.3 = bind_rows(
    global = tidy(use.clmm.g),
    final = tidy(use.clmm.f),
    .id = "model"
  ) %>% 
    filter(coef.type == "location") %>% 
    mutate(term = case_when(term == "Actionuse" ~ "Use",
                            term == "AspectData" ~ "Data",
                            term == "GenderMale"~ "Male",
                            TRUE ~ term),
           term = case_when(p.value < 0.001 ~ paste(term, "***"),
                            p.value < 0.01 ~ paste(term, "**"),
                            p.value < 0.05 ~ paste(term, "*"),
                            TRUE ~ term)) %>% 
    select(model, `Fixed effect terms` = term, Estimate = estimate, SE = std.error, `z value` = statistic),
  
  
  result_Q1.4 = bind_rows(
    global_teach = tidy(teach.glm.g),
    final_teach = tidy(teach.glm.f),
    global_supervise = tidy(supervise.glm.g),
    final_supervise = tidy(supervise.glm.f),
    .id = "model_domain"
  ) %>% 
    separate(col = model_domain, into = c("model", "domain"), sep = "_") %>% 
    mutate(term = case_when(term == "(Intercept)" ~ "Intercept",
                            term == "GenderMale"~ "Male",
                            TRUE ~ term),
           term = case_when(p.value < 0.001 ~ paste(term, "***"),
                            p.value < 0.01 ~ paste(term, "**"),
                            p.value < 0.05 ~ paste(term, "*"),
                            TRUE ~ term)) %>%
    select(Model = model, Domain = domain, `Fixed effect terms` = term, Estimate = estimate, SE = std.error, `z value` = statistic),
  
  
  result_Q1.4.2 = bind_rows(global_teach_data = tidy(teach.clm.data.g),
                            
                            global_teach_code = tidy(teach.clm.code.g),
                            
                            global_teach_method = tidy(teach.clm.method.g),
                            
                            global_teach_publish = tidy(teach.clm.publish.g),
                            final_teach_publish = tidy(teach.clm.publish.f),
                            
                            global_supervise_data = tidy(superv.clm.data.g),
                            
                            global_supervise_code = tidy(superv.clm.code.g),
                            
                            global_supervise_method = tidy(superv.clm.method.g),
                            
                            global_supervise_publish = tidy(superv.clm.publish.g),
                            
                            .id = "model_domain_aspect"
  ) %>% 
    filter(coef.type == "location"),
  
  
  result_Q3.2 = bind_rows(global_use_data = tidy(learn.clm.a.g),
                          final_use_data = tidy(learn.clm.a.f),
                          
                          global_use_code = tidy(learn.clm.b.g),
                          final_use_code = tidy(learn.clm.b.f),
                          
                          global_use_publish = tidy(learn.clm.c.g),
                          final_use_publish = tidy(learn.clm.c.f),
                          
                          global_share_data = tidy(learn.clm.d.g),
                          final_share_data = tidy(learn.clm.d.f),
                          
                          global_share_code = tidy(learn.clm.e.g),
                          final_share_code = tidy(learn.clm.e.f),
                          
                          global_share_publish = tidy(learn.clm.f.g),
                          final_share_publish = tidy(learn.clm.f.f),
                          
                          global_use_edutool = tidy(learn.clm.g.g),
                          final_use_edutool = tidy(learn.clm.g.f),
                          
                          global_do_review = tidy(learn.clm.h.g),
                          final_do_review = tidy(learn.clm.h.f),
                          
                          global_do_outreach = tidy(learn.clm.i.g),
                          final_do_outreach = tidy(learn.clm.i.f),
                          
                          .id = "model_action_aspect"
  ) %>% 
    filter(coef.type == "location"),
  
  
  result_Q3.3 = bind_rows(
    global_R_vs_T = tidy(imp.clmm.g),
    final_R_vs_T = tidy(imp.clmm.f),
    .id = "model_domain"
  ) %>% 
    filter(coef.type == "location")

  
)

