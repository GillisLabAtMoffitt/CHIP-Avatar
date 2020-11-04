################################################################################################## I ### For the 192 patients
path1 <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "Grant disparities Avatar")
Patient_sequenced <-
  read_csv(paste0(path1, "/data/Avatar_TargetedSeq.csv")) %>% 
  rename(Disease_Status_germline1 = "Disease_Status_germline")

Patient_sequenced <- left_join(Patient_sequenced, germline_patient_data %>% 
                                  select(c(avatar_id, Gender, Ethnicity, Race, Disease_Status_germline)),
                                by = "avatar_id") %>% 
  mutate(keep = case_when(
    Disease_Status_germline1 == Disease_Status_germline ~ "keep"
  )) %>% 
  filter(keep == "keep") %>% 
  mutate(Race = case_when(
    Race %in% c("African American")                      ~ "Black",
    TRUE                                                 ~ Race
  )) %>% 
  mutate(Ethnicity = case_when(
    Ethnicity %in% c("Spanish; Hispanic")                                   ~ "Hispanic",
    Ethnicity %in% c("Non- Hispanic", "Non-Spanish; non-Hispanic")          ~ "Non-Hispanic",
    Ethnicity %in% c("Unknown", "Prefer not to answer")                     ~ "Unknown",
    TRUE                                                                    ~ Ethnicity
  ))


tbl <- 
  Patient_sequenced %>% select( "Race", "Ethnicity", "Disease_Status_germline") %>% 
  tbl_summary(by= Disease_Status_germline, missing = "no", sort = list(everything() ~ "frequency")) %>% 
  italicize_labels() %>% add_overall() %>% 
  as_gt
gt::gtsave(tbl, expand = 1, zoom = 1.5,
           paste0(
             path1,
             "/Output/summary table disparities in 192 sequenced patients.pdf"))


################################################################################################## II ### For whole germline patients

germline_patient_data <- germline_patient_data %>% 
  mutate(Race = case_when(
    Race %in% c("African American")                           ~ "Black",
    Race %in% c("Other")                                      ~ "Others",
    Race %in% c("More Than One Race")                         ~ "More than one race",
    Race %in% c("Other Asian including Asian and Oriental")   ~ "Asian",
    Race %in% c("PT Not Present")                             ~ "Unknown",
    TRUE                                                      ~ Race
  )) %>% 
  mutate(Ethnicity = case_when(
    Ethnicity %in% c("Spanish; Hispanic")                                   ~ "Hispanic",
    Ethnicity %in% c("Non- Hispanic", "Non-Spanish; non-Hispanic")          ~ "Non-Hispanic",
    Ethnicity %in% c("Unknown", "Prefer not to answer", "PT Not Present")   ~ "Unknown",
    TRUE                                                                    ~ Ethnicity
  ))

tbl <- 
  germline_patient_data %>% select( "Race", "Ethnicity", "Disease_Status_germline") %>% 
  tbl_summary(by= Disease_Status_germline, missing = "no", sort = list(everything() ~ "frequency")) %>% 
  italicize_labels() %>% add_overall() %>% 
  as_gt
gt::gtsave(tbl, expand = 1, zoom = 1.5,
           paste0(
             path1,
             "/Output/summary table disparities in whole germline patients.pdf"))



