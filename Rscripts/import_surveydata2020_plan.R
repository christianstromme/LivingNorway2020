#data import drake plan

import_plan = drake_plan(
  #Load the three partial surveys
  data_raw1 = read_csv2( file = "data/1dataset_clean.csv", skip_empty_rows = TRUE ),
  data_raw2 = read_csv2( file = "data/2dataset_clean.csv", skip_empty_rows = TRUE ),
  data_raw3 = read_csv2( file = "data/3dataset_clean.csv", skip_empty_rows = TRUE ),

  #Join datasets
  data_raw12 = full_join(data_raw1, data_raw2, by="NO"),
  data_raw_full = full_join(data_raw12, data_raw3, by="NO"),

  ### I DON'T THINK WE NEED THIS!!!
  #Load complete questions
  #questions1 = read_csv2( file = "data/1complete_clean.csv", n_max = 1, col_names = FALSE ),
  #questions2 = read_csv2( file = "data/2complete_clean.csv", n_max = 1, col_names = FALSE ),
  #questions3 = read_csv2( file = "data/3complete_clean.csv", n_max = 1, col_names = FALSE ),



  # clean and 
  data = data_raw_full %>%

    # rename questions for part 1
    rename( Define_1 = s_2.x, 
                              Shared_data = s_3.x,
                              Shared_code = s_4.x,
                              Shared_methods = s_45,
                              Used_open_data = s_5.x,
                              Used_codes = s_6.x,
                              Published_open = s_7.x,
                              Edu_tools = s_8.x,
                              Read_papers = s_9.x,
                              Open_review = s_10.x,
                              Outreach = s_11.x,
                              Other_act = s_12.x,
                              Help = s_13.x,
                              Hinder = s_14.x,
                              Motiv = s_15.x,
                              Explain_R = s_48,
                              Explain_T = s_47,
                              Explain_S = s_35,
                              Research = s_46_1,
                              Teaching = s_46_2,
                              Supervision = s_46_3,
                              None = s_46_4,
                              
                              Data_sharing_R = s_16.x, 
                              Data_sharing_T = s_22,
                              Data_sharing_S = s_28,
                              Code_sharing_R = s_17,
                              Code_sharing_T = s_23,
                              Code_sharing_S = s_29,
                              Method_sharing_R = s_50,
                              Method_sharing_T = s_51,
                              Method_sharing_S = s_52,
                              Publish_open_R = s_18,
                              Publish_open_T = s_24,
                              Publish_open_S = s_30,
                              Comm_science_R = s_19,
                              Comm_science_T = s_25,
                              Comm_science_S = s_31,
                              Reproducib_R = s_20,
                              Reproducib_T = s_26,
                              Reproducib_S = s_32,
                              Transparency_R = s_21,
                              Transparency_T = s_27,
                              Transparency_S = s_33) %>% 
  
    # rename co-variables 
    rename( University = s_36_1,
                              Institute = s_36_2,
                              Gov_agency = s_36_3,
                              Private_comp = s_36_4,
                              Other = s_36_5,
                              Norway = s_37_1,
                              EU = s_37_2,
                              Non_EU = s_37_3,
                              Position = s_38,
                              Degree = s_39,
                              Year = s_40,
                              Undergrad_classes = s_41_1,
                              Grad_classes = s_41_2,
                              Superv_undergrad = s_41_3,
                              Superv_grad = s_41_4,
                              Superv_postdoc = s_41_5,
                              TPublic_outreach = s_41_6,
                              TOther = s_41_7,
                              Prim_research = s_49_1,
                              Synthesis = s_49_2,
                              Assessment = s_49_3,
                              Policy_interf = s_49_4,
                              APublic_outreach = s_49_5,
                              AOther = s_49_6,
                              Gender = s_42,
                              Day1 = s_43_1,
                              An_workshop = s_43_2,
                              Ed_workshop = s_43_3,
                              Attending = s_44) %>% 
  
  
    # rename questions for part 2
    rename( Teaches = s_2.y,
                               OS_in_teaching = s_3.y,
                               Inc_teach = s_4.y,
                               Inc_teach_future = s_5.y,
                               Other_edu = s_11.y,
                               Supervises = s_8.y,
                               OS_in_supervision = s_12.y,
                               
                               OS_own_edu = s_9.y,
                               Edu_tools2 = s_10_1,
                               OS_literature = s_10_2,
                               Used_open_data2 = s_10_3,
                               Used_codes2 = s_10_4,
                               Shared_data2 = s_10_5,
                               Shared_code2 = s_10_6,
                               Published_open2 = s_10_7,
                               Transparency2 = s_10_8,
                               Reproducib2 = s_10_9,
                               Open_review2 = s_10_10,
                               Outreach2 = s_10_11,
                               None2 = s_10_12) %>% 
  
 
  
  
  
    # rename questions for part 3
    rename( Define_2 = s_2, 
                               Change = s_3,
                               Change_how = s_4,
                               Future = s_5,
                               Comments = s_6, 
                               Lectures = s_7,
                               Discuss_1 = s_8,
                               Model_workshop = s_9,
                               Edu_workshop = s_10.y,
                               Discuss_2 = s_11,
                               Surveys = s_12,
                               
                               Use_research = s_13.y,
                               Use_teaching = s_14.y,
                               Use_supervision = s_15.y,
                               Use_edu = s_16.y),

  #prettify data
  plotdata = data %>%
    mutate_if(is.numeric,as.character, is.factor, as.character) %>% 
    pivot_longer(cols = c(Shared_data:Outreach, Research:Transparency_R, Data_sharing_T:Transparency_T, Data_sharing_S:Transparency_S, University:Attending, Teaches, OS_in_teaching, Supervises, OS_in_supervision, OS_own_edu:None2, Lectures:Use_edu ), names_to = "Question", values_drop_na = TRUE, values_to = "Value") %>%
  
    # make categories
    mutate(Category = case_when(Question %in% c("Shared_data", "Shared_code", "Shared_methods", "Used_open_data","Used_codes", "Published_open", "Edu_tools", "Read_papers", "Open_review", "Outreach", "Other_act") ~ "OS_activity",
                              
          
                              Question %in% c( "Research", "Teaching", "Supervision") ~ "Academic_activities",

                              Question %in% c("Data_sharing_R", "Code_sharing_R", "Method_sharing_R", "Publish_open_R", "Comm_science_R", "Reproducib_R", "Transparency_R") ~ "Importance_research",
                              
                              Question %in% c("Data_sharing_T", "Code_sharing_T", "Method_sharing_T", "Publish_open_T", "Comm_science_T", "Reproducib_T", "Transparency_T") ~ "Importance_teaching",
                              
                              Question %in% c("Data_sharing_S", "Code_sharing_S", "Method_sharing_S", "Publish_open_S", "Comm_science_S", "Reproducib_S", "Transparency_S") ~ "Importance_supervision",
                              
                              Question %in% c("University", "Institute", "Gov_agency", "Private_comp", "Other") ~"Background",
                              
                              Question %in% c("Position", "Degree", "Year") ~ "Carreer",
                              
                              Question %in% c("Norway", "EU", "Non_EU") ~ "Country",
                              
                              Question %in% c( "Day1", "An_workshop", "Ed_workshop", "Attending") ~ "Participation",
                              
                              
                              Question %in% c( "OS_in_teaching", "OS_in_supervision", "OS_own_edu") ~ "OS_in_education",
                              
                              Question %in% c( "Teaches", "Supervises") ~ "Duties",
                              
                              Question %in% c( "Edu_tools2", "OS_literature", "Used_open_data2", "Used_codes2", "Shared_data2", "Shared_code2", "Published_open2", "Transparency2", "Reproducib2", "Open_review2", "Outreach2", "None2") ~ "Practices_personal_education",
                              
                              Question %in% c( "Lectures", "Discuss_1", "Model_workshop", "Edu_workshop", "Discuss_2", "Surveys", "Use_research", "Use_teaching", "Use_supervision", "Use_edu") ~ "Workshop_utility",
         
                              Question %in% c("Undergrad_classes", "Grad_classes", "Superv_undergrad", "Superv_grad", "Superv_postdoc", "TPublic_outreach", "TOther") ~ "Teaching_types")),
  
# BN data -------------------
  
  # Data organisation ---      
  # rename and reorganise variables into categories
  
  bndata = data %>%
    select(
      # identification and covariates   (part1)
      NO,         # person ID
      University, # affiliated with university 0:1
      Position,   # position 1:7
      Degree,     # highest degree 1:4
      Year,       # year highest degree obtained xxxx
      Gender,     # male:female
      
      # teaching & supervision (we can simplify these) 0:1  (part1)
      does.T.ug = Undergrad_classes, 
      does.T.pg = Grad_classes,           
      does.T.outreach = TPublic_outreach,         
      does.T.other = TOther,  
      does.S.ug = Superv_undergrad, 
      does.S.pg = Superv_grad, 
      does.S.pd = Superv_postdoc, 
      # versions from (part 2) 1:2 - we may need these if people are not all same
      does.T.teaching = Teaches,           
      does.S.supervision = Supervises,  
      
      # engagement in research, synthesis or policy/public 0:1  (part1)
      does.R.primary = Prim_research,                         
      does.R.synthesis = Synthesis, 
      does.R.Assessment = Assessment,                         # Specifically policy type assessments
      does.R.policy = Policy_interf, 
      does.R.outreach = APublic_outreach, 
      does.R.other = AOther,                                  
      
      # learnt OS in own education   (part 2) most 0:1, except learnt.OS
      learnt.principle.OS = OS_own_edu,                  # 1:3  what are the three codes here?
      
      learnt.use.Data = Used_open_data2,
      learnt.use.Code = Used_codes2,
      learnt.use.Publish = OS_literature,
      
      learnt.share.Data = Shared_data2,
      learnt.share.Code = Shared_code2,
      learnt.share.Publish = Published_open2,
      
      learnt.use.EduTool = Edu_tools2,
      learnt.do.Review = Open_review2,
      learnt.do.Outreach = Outreach2,
      
      learnt.principle.Transparency = Transparency2,
      learnt.principle.Reproducibility = Reproducib2,
      
      # then we also have relevance?importance? to 0:1   (part1) 
      # suggest we remove this it seems, be reading the associated comments, that people did not really understand it, it was too vague 
      # rel.Teaching = Teaching,
      # rel.Supervision = Supervision,
      # rel.Research = Research,
      
      # percieved importance of activities in Research, Teaching, Supervision 1:5 (part1)
      imp.R.Data = Data_sharing_R,
      imp.R.Code = Code_sharing_R,
      imp.R.Method = Method_sharing_R,
      imp.R.Publish = Publish_open_R,
      
      imp.S.Data = Data_sharing_S,
      imp.S.Code = Code_sharing_S,
      imp.S.Method = Method_sharing_S,
      imp.S.Publish = Publish_open_S,
      
      imp.T.Data = Data_sharing_T,
      imp.T.Code = Code_sharing_T,
      imp.T.Method = Method_sharing_T,
      imp.T.Publish = Publish_open_T,
      
      # percieved importance of principles in Research, Teaching, Supervision 1:5 (part1)
      imp.R.Communication = Comm_science_R,
      imp.R.Reproducibility = Reproducib_R,
      imp.R.Transparency = Transparency_R,
      
      imp.S.Communication = Comm_science_S,
      imp.S.Reproducibility = Reproducib_S,
      imp.S.Transparency = Transparency_S,
      
      imp.T.Communication = Comm_science_T,
      imp.T.Reproducibility = Reproducib_T,
      imp.T.Transparency = Transparency_T,
      
      # use and sharing (in research?) of OS 0:1  (part1)
      use.engage.Data = Used_open_data,
      use.engage.Code = Used_codes,
      use.engage.Publish = Read_papers,
      
      use.engage.EduTool = Edu_tools,
      does.engage.Review = Open_review,
      does.engage.Outreach = Outreach,
      does.engage.Other = Other_act,
      
      share.engage.Data = Shared_data,
      share.engage.Code = Shared_code,
      share.engage.Publish = Published_open,
      share.engage.Methods = Shared_methods,
      
      # prior use in teaching (1:2)(part 2)
      usein.T.prior = OS_in_teaching,  
      usein.S.prior = OS_in_supervision,
      useinTprior = OS_in_teaching,
      useinSprior = OS_in_supervision
      # made non-splittable cols for analysis 1.4
      
      ## As stated future use is long answer, none of these workshop participation/value questions are predictors for the current BBN.
      ## We could include them as responses if we want to go there, or code the future intentions and then include these as predictors.
      ## But, as we don't really have a stated future use prior to the workshop, we need to be careful about the interpretation.
      # # participation in workshop  
      # workshop.day1 = Day1,
      # workshop.analysis = An_workshop,
      # workshop.education = Ed_workshop,
      # 
      # # value of workshop 1:5
      # value.Lectures = Lectures,
      # value.Discuss_1 = Discuss_1,
      # value.Model_workshop = Model_workshop,
      # value.Edu_workshop = Edu_workshop,
      # value.Discuss_2 = Discuss_2,
      # value.Surveys = Surveys,
      # 
      # # value of workshop 
      # valuein.R = Use_research,
      # valuein.T = Use_teaching,
      # valuein.S = Use_supervision,
      # valuein.Education = Use_edu            
    ) %>% 
  #Removed "Other" gender category due to singleton.
  filter(Gender!= "Other")
)

