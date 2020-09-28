# Radiation
radiation <- radiation %>% 
  mutate(radiation_check = case_when(
    rad_start_date < rad_stop_date ~ "OK",
    rad_start_date > rad_stop_date ~ "not good"
  ))
table(radiation$radiation_check)
wrong_date <- as.data.table(radiation[which(radiation$radiation_check == "not good"),])
write.csv(wrong_date, paste0(path, "/sanity check output/wrong date radiation.csv"))
# Treatment
treatment <- treatment %>% 
  mutate(treatment_check = case_when(
    drug_start_date < drug_stop_date ~ "OK",
    drug_start_date > drug_stop_date ~ "not good"
  ))
table(treatment$treatment_check)
wrong_date <- as.data.table(treatment[which(treatment$treatment_check == "not good"),])
write.csv(wrong_date, paste0(path, "/sanity check output/wrong date treatment.csv"))

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
    drug_start_date_1 > drug_start_date_2 |
      drug_start_date_2 > drug_start_date_3 |
      drug_start_date_3 > drug_start_date_4 |
      drug_start_date_4 > drug_start_date_5 |
      drug_start_date_5 > drug_start_date_6 |
      drug_start_date_6 > drug_start_date_7 |
      drug_start_date_7 > drug_start_date_8 |
      drug_start_date_8 > drug_start_date_9 |
      drug_start_date_9 > drug_start_date_10 |
      drug_start_date_10 > drug_start_date_11 |
      drug_start_date_11 > drug_start_date_12 |
      drug_start_date_12 > drug_start_date_13 |
      drug_start_date_13 > drug_start_date_14 |
      drug_start_date_14 > drug_start_date_15 |
      drug_start_date_15 > drug_start_date_16 |
      drug_start_date_16 > drug_start_date_17 ~ "not good",
    drug_start_date_16 <= drug_start_date_17 ~ "OK",
    drug_start_date_15 <= drug_start_date_16 ~ "OK",
    drug_start_date_14 <= drug_start_date_15 ~ "OK",
    drug_start_date_13 <= drug_start_date_14 ~ "OK",
    drug_start_date_12 <= drug_start_date_13 ~ "OK",
    drug_start_date_11 <= drug_start_date_12 ~ "OK",
    drug_start_date_10 <= drug_start_date_11 ~ "OK",
    drug_start_date_9 <= drug_start_date_10 ~ "OK",
    drug_start_date_8 <= drug_start_date_9 ~ "OK",
    drug_start_date_7 <= drug_start_date_8 ~ "OK",
    drug_start_date_6 <= drug_start_date_7 ~ "OK",
    drug_start_date_5 <= drug_start_date_6 ~ "OK",
    drug_start_date_4 <= drug_start_date_5 ~ "OK",
    drug_start_date_3 <= drug_start_date_4 ~ "OK",
    drug_start_date_2 <= drug_start_date_3 ~ "OK",
    drug_start_date_1 <= drug_start_date_2 ~ "OK"
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
    last_date_available > date_of_diagnosis ~ "OK",
    last_date_available <= date_of_diagnosis ~ "not good"
  )) %>% 
  mutate(birth_diag_lastdate = case_when(
    Date_of_Birth < date_of_diagnosis &
      date_of_diagnosis < last_date_available ~ "OK"
  )) %>% 
  mutate(rad_after_diag = case_when(
    rad_start_date_1 > date_of_diagnosis ~ "OK"
  )) %>% 
  mutate(bmt_after_diag = case_when(
    date_of_bmt_1 > date_of_diagnosis ~ "OK"
  )) %>% 
  mutate(drug_after_diag = case_when(
    drug_start_date_1 > date_of_diagnosis ~ "OK"
  )) %>% 
  mutate(diag_BF_progression = case_when(
    progression_date > date_of_diagnosis ~ "OK",
    progression_date <= date_of_diagnosis ~ "not good"
  )) %>% 
  mutate(progression_BF_death = case_when(
    progression_date < date_death ~ "OK",
    progression_date >= date_death ~ "not good"
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

wrong_date_bmt <- as.data.table(sanity_check[which(sanity_check$sct_check == "not good"), c("avatar_id", "date_of_bmt_1", "date_of_bmt_2")])
write.csv(wrong_date_bmt, paste0(path, "/sanity check output/wrong_date_bmt.csv"))

wrong_date_drug <- sanity_check[which(sanity_check$treat_check == "not good"), ] %>% 
  select("avatar_id", starts_with("drug_start"), starts_with("drug_name"))
wrong_date_drug <- as.data.table(wrong_date_drug)
write.csv(wrong_date_drug, paste0(path, "/sanity check output/wrong_date_drug.csv"))

wrong_date_rad <- sanity_check[which(sanity_check$rad_check == "not good"), c("avatar_id", "rad_start_date_1", "rad_start_date_2", "rad_start_date_3")] # no big deal, they are the same (1=2)
write.csv(wrong_date_rad, paste0(path, "/sanity check output/wrong_date_rad.csv"))

wrong_diag_or_lastdate <- as.data.table(sanity_check[which(sanity_check$diag_BF_lastdate == "not good"), c("avatar_id", "date_of_diagnosis", "last_date_available",
                                                                        "date_death", "date_last_follow_up")])
write.csv(wrong_diag_or_lastdate, paste0(path, "/sanity check output/wrong_diag_or_lastdate.csv"))

missing_diag <- as.data.table(sanity_check[which(is.na(sanity_check$date_of_diagnosis)), c("avatar_id", "date_of_diagnosis")])
write.csv(missing_diag, paste0(path, "/sanity check output/missing_diag date.csv"))

wrong_progression <- as.data.table(sanity_check[which(sanity_check$diag_BF_progression == "not good"), 
                                                c("avatar_id", "progression_date", "date_of_diagnosis", "date_of_diagnosis_1",
                                                  "date_of_diagnosis_2", "Disease_Status_germline")])
write.csv(wrong_progression, paste0(path, "/sanity check output/wrong_progression date.csv"))

wrong_progression_BF_death <- as.data.table(sanity_check[which(sanity_check$progression_BF_death == "not good"), 
                                                c("avatar_id", "progression_date", "date_of_diagnosis", 
                                                  "date_death" , "Disease_Status_germline")])
write.csv(wrong_progression_BF_death, paste0(path, "/sanity check output/progression_BF_death date.csv"))

rm(sanity_check, table_sanity_check, wrong_date, wrong_date_bmt, 
   wrong_date_drug, wrong_date_rad, wrong_diag_or_lastdate, missing_diag)
