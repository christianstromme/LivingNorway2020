#data analysis plan

analysis_plan <- drake_plan(
  # ...
  
  plotdata %>% 
    select(NO, Question, Category, Value) %>% 
    filter(Category %in% c("Importance_research", "Importance_teaching")) %>% 
    mutate(Question = str_sub(Question, 1, str_length(Question)-2)) %>% 
    pivot_wider(names_from = Category, values_from = Value)
  
)