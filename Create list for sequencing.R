# create list for sequencing

# For Ctrl, choose in before drugs, bmt, rad
sum(str_count(germ_BF_drug_bmt_rad$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"))
# 103
sum(str_count(germ_BF_drug_bmt_rad$Disease_Status_germline, "Mgus"))
# 54
sum(str_count(germ_BF_drug_bmt_rad$Disease_Status_germline, "Smoldering Multiple Myeloma"))
# 46

# Could be good to select by age to see increase CHIP by age
Age_data <- germline_patient_data

enddate <- today()
Age_data$Age <- interval(start= germline_patient_data$Date_of_Birth, end= enddate)/                      
  duration(n=1, unit="years")
Age_data$Age <- round(Age_data$Age, 3)
# summary(Age_data$Age)

Age_data$Age_at_diagosis <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$date_of_diagnosis_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_diagosis <- round(Age_data$Age_at_diagosis, 3)
# summary(Age_data$Age_at_diagosis, na.rm = TRUE)

Age_data$Age_at_death <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$date_death)/                      
  duration(n=1, unit="years")
Age_data$Age_at_death <- round(Age_data$Age_at_death, 3)
# summary(Age_data$Age_at_death, na.rm = TRUE)

Age_data$Age_at_lastfollowup <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$date_last_follow_up)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastfollowup <- round(Age_data$Age_at_lastfollowup, 3)
# summary(Age_data$Age_at_lastfollowup, na.rm = TRUE)

Age_data$Age_at_lastdate <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$last_date_available)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastdate <- round(Age_data$Age_at_lastdate, 3)
# summary(Age_data$Age_at_lastdate, na.rm = TRUE)

Age_data$Age_at_firstdrug <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$drug_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstdrug <- round(Age_data$Age_at_firstdrug, 3)
# summary(Age_data$Age_at_firstdrug, na.rm = TRUE)

Age_data$Age_at_firstbmt <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$date_of_first_bmt)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstbmt <- round(Age_data$Age_at_firstbmt, 3)
# summary(Age_data$Age_at_firstbmt, na.rm = TRUE)

Age_data$Age_at_firstrad <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$rad_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstrad <- round(Age_data$Age_at_firstrad, 3)
# summary(Age_data$Age_at_firstrad, na.rm = TRUE)

Age_data$Age_at_germcollect <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$collectiondt_germline)/                      
  duration(n=1, unit="years")
Age_data$Age_at_germcollect <- round(Age_data$Age_at_germcollect, 3)
# summary(Age_data$Age_at_germcollect, na.rm = TRUE)

Age_data$Age_at_tumorcollect <- interval(start= germline_patient_data$Date_of_Birth, end= germline_patient_data$collectiondt_tumor_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_tumorcollect <- round(Age_data$Age_at_tumorcollect, 3)
# summary(Age_data$Age_at_tumorcollect, na.rm = TRUE)

Age_data$Age_range_at_germcollect <- 
  findInterval(Age_data$Age_at_germcollect,c(0,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,200))
Age_data <- Age_data %>% 
  mutate(Age_range_at_germcollect = as.factor(Age_range_at_germcollect))
levels(Age_data$Age_range_at_germcollect) <-  
  c("<26","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","65-70", "70-75", 
     "75-80", "80-85", "85-90", "90-95", "95-100", ">100")


germ_BF_drugs <- Age_data[which(Age_data$germlineBFdrugs =="OK"),]
germ_BF_bmt1 <- Age_data[which(Age_data$germlineBFbmt1 == "OK"),] # 375
germ_BF_drugsBMT <- Age_data[which(Age_data$germBFdrugsbmt == "OK"),]
germ_BF_rad <- Age_data[which(Age_data$germlineBFrad1 == "OK"),]
germ_BF_drug_bmt_rad <- Age_data[which(Age_data$germBFdbr == "OK"),]

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
tbl_stack(list(table1, table2)) %>% 
  modify_header(label = "**Age_range_at_germcollect**") %>%
  as_gt(include = -tab_footnote) %>%
  gt::tab_spanner(label = gt::md("**Pre-Treatment**"),
                  columns = gt::starts_with("stat_"))


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
tbl_stack(list(table1, table2)) %>% 
  modify_header(label = "**Age_range_at_germcollect**") %>%
  as_gt(include = -tab_footnote) %>%
  gt::tab_spanner(label = gt::md("**MGUS**"),
                  columns = gt::starts_with("stat_"))


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
  gt::tab_spanner(label = gt::md("**Smoldering**"),
                  columns = gt::starts_with("stat_"))
table_smoldering
gt::gtsave(table_smoldering, expand = 1, zoom = 2, 
           paste0(
             path, 
                  "/List patients for sequencing/hgghgfgh.pdf"))

