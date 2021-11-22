mrn1 <-
  read_csv(paste0(path, "/M2GEN/Garrick_raw data/10R20000134_2020-05-05_avatar_v2_clinical-with-events/MRN.csv")) %>% 
  rename(avatar_id = "AvatarKey")
mrn2 <-
  read_csv(paste0(path, "/M2GEN/Garrick_raw data/10R20000134_2020-05-05_avatar_v4_clinical-with-events/MRN.csv")) %>% 
  rename(avatar_id = "AvatarKey")
mrn <- bind_rows(mrn1, mrn2, Demo_linkage) %>% 
  distinct() %>% 
  mutate(MRN = as.character(MRN)) %>% 
  arrange(MRN) %>% 
  distinct(avatar_id, .keep_all = TRUE)


# Radiation
radiation <- radiation %>% 
  mutate(radiation_check = case_when(
    rad_start_date < rad_stop_date ~ "OK",
    rad_start_date > rad_stop_date ~ "not good"
  ))
table(radiation$radiation_check)
wrong_date <- (radiation[which(radiation$radiation_check == "not good"),]) %>% 
  left_join(., mrn, by = c("avatar_id"))
write.csv(wrong_date, paste0(path, "/sanity check output/radiation start date after stop date.csv"))
# Treatment
treatment <- treatment %>% 
  mutate(treatment_check = case_when(
    drug_start_date < drug_stop_date ~ "OK",
    drug_start_date > drug_stop_date ~ "not good"
  ))
table(treatment$treatment_check)
wrong_date <- (treatment[which(treatment$treatment_check == "not good"),]) %>% 
  left_join(., mrn, by = c("avatar_id"))
write.csv(wrong_date, paste0(path, "/sanity check output/drugs start date after stop date.csv"))

sanity_check <- germline_patient_data %>% 
  mutate(diag_check = case_when(
    date_of_diagnosis_1 < date_of_diagnosis_2 &
      date_of_diagnosis_2 < date_of_diagnosis_3 &
      date_of_diagnosis_3 < date_of_diagnosis_4 ~ "OK",
    date_of_diagnosis_1 < date_of_diagnosis_2 &
      date_of_diagnosis_2 < date_of_diagnosis_3 ~ "OK",
    date_of_diagnosis_1 < date_of_diagnosis_2 ~ "OK",
    date_of_diagnosis_1 > date_of_diagnosis_2 &
      date_of_diagnosis_2 > date_of_diagnosis_3 &
      date_of_diagnosis_3 > date_of_diagnosis_4 ~ "not good",
    date_of_diagnosis_1 > date_of_diagnosis_2 &
      date_of_diagnosis_2 > date_of_diagnosis_3 ~ "not good",
    date_of_diagnosis_1 > date_of_diagnosis_2 ~ "not good"
  )) %>% 
  mutate(rad_check = case_when(
    rad_start_date_1 < rad_start_date_2 &
      rad_start_date_2 < rad_start_date_3 &
      rad_start_date_3 < rad_start_date_4 ~ "OK",
    rad_start_date_1 < rad_start_date_2 &
      rad_start_date_2 < rad_start_date_3 ~ "OK",
    rad_start_date_1 < rad_start_date_2 ~ "OK",
    rad_start_date_1 > rad_start_date_2 &
      rad_start_date_2 > rad_start_date_3 &
      rad_start_date_3 >= rad_start_date_4 ~ "not good",
    rad_start_date_1 > rad_start_date_2 &
      rad_start_date_2 >= rad_start_date_3 ~ "not good",
    rad_start_date_1 >= rad_start_date_2 ~ "not good"
    )) %>% 
  mutate(sct_check = case_when(
    date_of_bmt_1 < date_of_bmt_2 &
      date_of_bmt_2 < date_of_bmt_3 ~ "OK",
    date_of_bmt_1 < date_of_bmt_2 ~ "OK",
    date_of_bmt_2 >= date_of_bmt_3 |
      date_of_bmt_1 >= date_of_bmt_2 ~ "not good"
  )) %>% 
  mutate(treat_check = case_when(
    line_start_date_1 > line_start_date_2 |
      line_start_date_2 > line_start_date_3 |
      line_start_date_3 > line_start_date_4 |
      line_start_date_4 > line_start_date_5 |
      line_start_date_5 > line_start_date_6 |
      line_start_date_6 > line_start_date_7 |
      line_start_date_7 > line_start_date_8 |
      line_start_date_8 > line_start_date_9 |
      line_start_date_9 > line_start_date_10 |
      line_start_date_10 > line_start_date_11 |
      line_start_date_11 > line_start_date_12 |
      line_start_date_12 > line_start_date_13 |
      line_start_date_13 > line_start_date_14 |
      line_start_date_14 > line_start_date_15 ~ "not good",
    line_start_date_14 <= line_start_date_15 ~ "OK",
    line_start_date_13 <= line_start_date_14 ~ "OK",
    line_start_date_12 <= line_start_date_13 ~ "OK",
    line_start_date_11 <= line_start_date_12 ~ "OK",
    line_start_date_10 <= line_start_date_11 ~ "OK",
    line_start_date_9 <= line_start_date_10 ~ "OK",
    line_start_date_8 <= line_start_date_9 ~ "OK",
    line_start_date_7 <= line_start_date_8 ~ "OK",
    line_start_date_6 <= line_start_date_7 ~ "OK",
    line_start_date_5 <= line_start_date_6 ~ "OK",
    line_start_date_4 <= line_start_date_5 ~ "OK",
    line_start_date_3 <= line_start_date_4 ~ "OK",
    line_start_date_2 <= line_start_date_3 ~ "OK",
    line_start_date_1 <= line_start_date_2 ~ "OK"
  )) %>% 
  mutate(birth_BF_lastdate = case_when(
    last_date_available > Date_of_Birth ~ "OK",
    last_date_available <= Date_of_Birth ~ "not good"
  )) %>% 
  mutate(birth_BF_diag = case_when(
    date_of_diagnosis_1 > Date_of_Birth ~ "OK",
    date_of_diagnosis_1 <= Date_of_Birth ~ "not good"
  )) %>% 
  mutate(diag_BF_lastdate = case_when(
    last_date_available > Dx_date_closest_germline ~ "OK",
    last_date_available <= Dx_date_closest_germline ~ "not good"
  )) %>% 
  mutate(birth_diag_lastdate = case_when(
    Date_of_Birth < Dx_date_closest_germline &
      Dx_date_closest_germline < last_date_available ~ "OK"
  )) %>% 
  mutate(rad_after_diag = case_when(
    rad_start_date_1 > Dx_date_closest_germline ~ "OK"
  )) %>% 
  mutate(bmt_after_diag = case_when(
    date_of_bmt_1 > Dx_date_closest_germline ~ "OK"
  )) %>% 
  mutate(drug_after_diag = case_when(
    line_start_date_1 > Dx_date_closest_germline ~ "OK"
  )) %>% 
  mutate(diag_BF_progression = case_when(
    progression_date > Dx_date_closest_germline ~ "OK",
    progression_date <= Dx_date_closest_germline ~ "not good"
  )) %>% 
  mutate(progression_BF_death = case_when(
    progression_date < date_death ~ "OK",
    progression_date >= date_death ~ "not good"
  )) %>% 
  mutate(drug_bf_bmt = case_when(
    line_start_date_1 <= date_of_bmt_1  ~ "OK",
    line_start_date_1 > date_of_bmt_1  ~ "not good",
    is.na(line_start_date_1) & !is.na(date_of_bmt_1)  ~ "not good"
  ))

table_sanity_check <- as.data.table(matrix(c("check", "radiation_check", "treatment_check", "diag_check", "rad_check", "sct_check", "treat_check", "birth_BF_lastdate",
                              "birth_BF_diag", "diag_BF_lastdate", "birth_diag_lastdate", 
                              "rad_after_diag", "bmt_after_diag", "drug_after_diag", 
                              "diag_BF_progression", "progression_BF_death",
                              
                              "comments", "if stop date after start date", "if stop date after start date",
                              "if diag1 before diag2 before diag3 etc", "if rad1 before rad2 before rad3 etc",
                              "if bmt1 before bmt2 before bmt3 etc", "if drug1 before drug2 before drug3 etc",
                              "if birth before last date available", "if birth before diag", "if diag before last date available",
                              "if birth before diag before last date available",
                              "if rad1 after diag","if bmt1 after diag", "if drug1 after diag", 
                              "if progression after diag", "if progression before death",
                              "OK", sum(str_count(radiation$radiation_check, "OK"), na.rm = TRUE), sum(str_count(treatment$treatment_check, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_check, "OK"), na.rm = TRUE), sum(str_count(sanity_check$rad_check, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$sct_check, "OK"), na.rm = TRUE), sum(str_count(sanity_check$treat_check, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$birth_BF_lastdate, "OK"), na.rm = TRUE), sum(str_count(sanity_check$birth_BF_diag, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_BF_lastdate, "OK"), na.rm = TRUE), sum(str_count(sanity_check$birth_diag_lastdate, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$rad_after_diag, "OK"), na.rm = TRUE), sum(str_count(sanity_check$bmt_after_diag, "OK"), na.rm = TRUE), 
                              sum(str_count(sanity_check$drug_after_diag, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_BF_progression, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$progression_BF_death, "OK"), na.rm = TRUE),
                              
                              "not good", sum(str_count(radiation$radiation_check, "not good"), na.rm = TRUE), sum(str_count(treatment$treatment_check, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_check, "not good"), na.rm = TRUE), sum(str_count(sanity_check$rad_check, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$sct_check, "not good"), na.rm = TRUE), sum(str_count(sanity_check$treat_check, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$birth_BF_lastdate, "not good"), na.rm = TRUE), sum(str_count(sanity_check$birth_BF_diag, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_BF_lastdate, "not good"), na.rm = TRUE), sum(str_count(sanity_check$birth_diag_lastdate, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$rad_after_diag, "not good"), na.rm = TRUE), sum(str_count(sanity_check$bmt_after_diag, "not good"), na.rm = TRUE), 
                              sum(str_count(sanity_check$drug_after_diag, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_BF_progression, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$progression_BF_death, "not good"), na.rm = TRUE)
                              ), ncol = 16, byrow=TRUE))

write.csv(table_sanity_check, paste0(path, "/sanity check output/sanity check.csv"))

wrong_date_bmt <- as.data.table(sanity_check[which(sanity_check$sct_check == "not good"), c("avatar_id", "mrn", "date_of_bmt_1", "date_of_bmt_2")])
write.csv(wrong_date_bmt, paste0(path, "/sanity check output/wrong_date_bmt.csv"))

wrong_date_drug <- sanity_check[which(sanity_check$treat_check == "not good"), ] %>% 
  select("avatar_id", "mrn", starts_with("drug_start"), starts_with("drug_name"))
wrong_date_drug <- as.data.table(wrong_date_drug)
write.csv(wrong_date_drug, paste0(path, "/sanity check output/wrong_date_drug.csv"))

wrong_date_rad <- sanity_check[which(sanity_check$rad_check == "not good"), c("avatar_id", "rad_start_date_1", "rad_start_date_2", "rad_start_date_3")] # no big deal, they are the same (1=2)
write.csv(wrong_date_rad, paste0(path, "/sanity check output/wrong_date_rad.csv"))

wrong_diag_or_lastdate <- as.data.table(sanity_check[which(sanity_check$diag_BF_lastdate == "not good"), c("avatar_id", "Dx_date_closest_germline", "last_date_available",
                                                                        "date_death", "date_last_follow_up")]) %>% 
  left_join(., mrn, by = c("avatar_id"))
write.csv(wrong_diag_or_lastdate, paste0(path, "/sanity check output/wrong_diag_or_lastdate.csv"))

missing_diag <- as.data.table(sanity_check[which(is.na(sanity_check$Dx_date_closest_germline)), c("avatar_id", "Dx_date_closest_germline")])
write.csv(missing_diag, paste0(path, "/sanity check output/missing_diag date.csv"))

wrong_progression <- as.data.table(sanity_check[which(sanity_check$diag_BF_progression == "not good"), 
                                                c("avatar_id", "progression_date", "Dx_date_closest_germline", "date_of_diagnosis_1",
                                                  "date_of_diagnosis_2", "Disease_Status_germline")])
write.csv(wrong_progression, paste0(path, "/sanity check output/wrong_progression date.csv"))

wrong_progression_BF_death <- as.data.table(sanity_check[which(sanity_check$progression_BF_death == "not good"), 
                                                c("avatar_id", "progression_date", "Dx_date_closest_germline", 
                                                  "date_death" , "Disease_Status_germline")])
write.csv(wrong_progression_BF_death, paste0(path, "/sanity check output/progression_BF_death date.csv"))

table(sanity_check$drug_bf_bmt)
drug_bf_bmt <- as.data.table(sanity_check[which(sanity_check$drug_bf_bmt == "not good"), 
                                                         c("avatar_id", "date_of_bmt_1", "drug_start_date_1",
                                                           "line_start_date_1", "drug_name__1", "imids_maintenance")]) %>% 
  left_join(., mrn, by = c("avatar_id"))
write.csv(drug_bf_bmt, paste0(path, "/sanity check output/drugs start before bmt.csv"))


rm(sanity_check, table_sanity_check, wrong_date, wrong_date_bmt, 
   wrong_date_drug, wrong_date_rad, wrong_diag_or_lastdate, missing_diag)


######## Check long drug name

trial_reg_name <- germline_patient_data %>% 
  select(avatar_id, starts_with("regimen_name_"), starts_with("drug_start_date_"), 
         starts_with("drug_stop_date_"),
         starts_with("line_start_date_"), starts_with("line_stop_date_")) %>% 
  pivot_longer(cols = starts_with("regimen_name_"),
               names_to = "regimen_number", values_to = "regimen_name_", values_drop_na = TRUE) %>% 
  filter(str_detect(regimen_name_, "clinical"))
trial_reg_name <- left_join(trial_reg_name, mrn, by = "avatar_id")
write_csv(trial_reg_name, paste0(path, "/sanity check output/clinical trial as reg_name.csv"))

# trial_reg_name <- germline_patient_data %>% 
#   select(avatar_id, starts_with("regimen_name_"), starts_with("drug_start_date_"), 
#          starts_with("drug_stop_date_"),
#          starts_with("line_start_date_"), starts_with("line_stop_date_")) %>% 
#   filter(.vars == vars(starts_with("regimen_name_") ) , 
#          .vars_predicate == any_vars(str_detect(. , "clinical")))
  

trial_reg_name <- treatment %>% 
  filter(str_detect(drug_name_, "clinical")) %>% left_join(., mrn, by = "avatar_id")
write_csv(trial_reg_name, paste0(path, "/sanity check output/clinical trial as reg_name.csv"))


# How many patients had HCT but no melphalan----
hct_data <- germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  filter(!is.na(date_of_bmt_1))

hct_data1 <- hct_data %>% 
  pivot_longer(cols = starts_with("drug_name_"), values_to = "drug_name_") %>% 
  filter(str_detect(drug_name_, "melphalan")) %>% 
  distinct(avatar_id, .keep_all = TRUE)
uid <- paste(unique(hct_data1$avatar_id), collapse = '|')

hct_data_no_mel <- hct_data[(!grepl(uid, hct_data$avatar_id)),] %>% 
  select(avatar_id, starts_with("date_of_bmt_"), starts_with("drug_name"))
write_csv(hct_data_no_mel, paste0(path, "/sanity check output/patients received HCT but no melphalan.csv"))























