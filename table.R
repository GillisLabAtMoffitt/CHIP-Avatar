library(gtsummary)


table1 <- age_germline_patient_data %>%
  select(Age_at_diagosis, Race) %>% 
  tbl_summary(by = Race , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Race) ~ 2)) %>% 
  add_p()
table2 <- age_germline_patient_data %>%
  select(Age_at_diagosis, Ethnicity) %>% 
  tbl_summary(by = Ethnicity , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Race) ~ 2)) %>% 
  add_p()
tbl_merge(list(table1, table2),
               tab_spanner = c("**Race**", "**Ethnicity**")) %>%
  bold_labels() %>%
  italicize_levels()

library(tableone)



  



