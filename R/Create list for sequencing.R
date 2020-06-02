# create list for sequencing

# For Ctrl, choose in before drugs, bmt, rad
sum(str_count(germ_BF_drug_bmt_rad$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"))
# 103
sum(str_count(germ_BF_drug_bmt_rad$Disease_Status_germline, "Mgus"))
# 54
sum(str_count(germ_BF_drug_bmt_rad$Disease_Status_germline, "Smoldering Multiple Myeloma"))
# 46

# Could be good to select by age to see increase CHIP by age


germline_patient_data$Age_range_at_germcollect <- 
  findInterval(germline_patient_data$Age_at_germcollect,c(0,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,200))
germline_patient_data <- germline_patient_data %>% 
  mutate(Age_range_at_germcollect = as.factor(Age_range_at_germcollect))
levels(germline_patient_data$Age_range_at_germcollect) <-  
  c("<26","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","65-70", "70-75", 
     "75-80", "80-85", "85-90", "90-95", "95-100", ">100")


germ_BF_drugs <- germline_patient_data[which(germline_patient_data$germlineBFdrugs =="OK"),]
germ_BF_bmt1 <- germline_patient_data[which(germline_patient_data$germlineBFbmt1 == "OK"),] # 375
germ_BF_drugsBMT <- germline_patient_data[which(germline_patient_data$germBFdrugsbmt == "OK"),]
germ_BF_rad <- germline_patient_data[which(germline_patient_data$germlineBFrad1 == "OK"),]
germ_BF_drug_bmt_rad <- germline_patient_data[which(germline_patient_data$germBFdbr == "OK"),]

germ_BF_drug_bmt_rad_pre <- germ_BF_drug_bmt_rad %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma") #103

# germ_BF_drug_bmt_rad_post <- germ_BF_drug_bmt_rad %>% 
#   filter(Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma")

# germ_BF_drug_bmt_rad_early <- germ_BF_drug_bmt_rad %>% 
#   filter(Disease_Status_germline == "Early Relapse Multiple Myeloma")

# germ_BF_drug_bmt_rad_late <- germ_BF_drug_bmt_rad %>% 
#   filter(Disease_Status_germline == "Late Relapse Multiple Myeloma")


germ_BF_drug_bmt_rad_mgus <- germ_BF_drug_bmt_rad %>% 
  filter(Disease_Status_germline == "Mgus") # 54

germ_BF_drug_bmt_rad_smoldering <- germ_BF_drug_bmt_rad %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma") #46

table1 <- germ_BF_drug_bmt_rad_pre %>%
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>% 
  select(Age_range_at_germcollect, Race) %>% 
  tbl_summary(by = Age_range_at_germcollect)
table2 <- germ_BF_drug_bmt_rad_pre %>%
  select(Age_range_at_germcollect, Ethnicity) %>% 
  tbl_summary(by = Age_range_at_germcollect)
table_pre <- tbl_stack(list(table1, table2)) %>% 
  modify_header(label = "**Age_range_at_germcollect**") %>%
  as_gt(include = -tab_footnote) %>%
  gt::tab_spanner(label = gt::md("**Pre-Treatment patients germline before drugs, bmt, radiation**"),
                  columns = gt::starts_with("stat_"))
gt::gtsave(table_pre, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/List patients for sequencing/Age range for Pre-Treatment.pdf"))

table1 <- germ_BF_drug_bmt_rad_mgus %>%
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>% 
  select(Age_range_at_germcollect, Race) %>% 
  tbl_summary(by = Age_range_at_germcollect)
table2 <- germ_BF_drug_bmt_rad_mgus %>%
  select(Age_range_at_germcollect, Ethnicity) %>% 
  tbl_summary(by = Age_range_at_germcollect)
table_mgus <- tbl_stack(list(table1, table2)) %>% 
  modify_header(label = "**Age_range_at_germcollect**") %>%
  as_gt(include = -tab_footnote) %>%
  gt::tab_spanner(label = gt::md("**MGUS patients germline before drugs, bmt, radiation**"),
                  columns = gt::starts_with("stat_"))
gt::gtsave(table_mgus, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/List patients for sequencing/Age range for Mgus.pdf"))

table1 <- germ_BF_drug_bmt_rad_smoldering %>%
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>% 
  select(Age_range_at_germcollect, Race) %>% 
  tbl_summary(by = Age_range_at_germcollect)
table2 <- germ_BF_drug_bmt_rad_smoldering %>%
  select(Age_range_at_germcollect, Ethnicity) %>% 
  tbl_summary(by = Age_range_at_germcollect)
table_smoldering <- tbl_stack(list(table1, table2)) %>% 
  modify_header(label = "**Age_range_at_germcollect**") %>% 
  as_gt(include = -tab_footnote) %>%
  gt::tab_spanner(label = gt::md("**Smoldering patients germline before drugs, bmt, radiation**"),
                  columns = gt::starts_with("stat_"))
gt::gtsave(table_smoldering, expand = 1, zoom = 2, 
           paste0(
             path, 
                  "/List patients for sequencing/Age range for Smoldering.pdf"))

