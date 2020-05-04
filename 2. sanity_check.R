# Radiation
radiation <- radiation %>% 
  mutate(radiation_check = case_when(
    rad_start_date < rad_stop_date ~ "OK",
    rad_start_date > rad_stop_date ~ "not good"
  ))
table(radiation$radiation_check)
# Treatment
treatment <- treatment %>% 
  mutate(treatment_check = case_when(
    drug_start_date < drug_stop_date ~ "OK",
    drug_start_date > drug_stop_date ~ "not good"
  ))
table(treatment$treatment_check)
# wrong_date <- as.data.table(treatment[which(treatment$treatment_check == "not good"),])
# write.csv(wrong_date, paste0(path, "/wrong date treatment.csv"))

sanity_check <- Global_data %>% 
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
    date_of_first_bmt < date_of_second_bmt &
      date_of_second_bmt < date_of_third_bmt ~ "OK",
    date_of_first_bmt < date_of_second_bmt ~ "OK",
      date_of_second_bmt >= date_of_third_bmt |
    date_of_first_bmt >= date_of_second_bmt ~ "not good"
  )) %>% 
  mutate(treat_check = case_when(
    drug_start_date_1 <= drug_start_date_2 &
      drug_start_date_2 <= drug_start_date_3 &
      drug_start_date_3 <= drug_start_date_4 &
      drug_start_date_4 <= drug_start_date_5 &
      drug_start_date_5 <= drug_start_date_6 &
      drug_start_date_6 <= drug_start_date_7 &
      drug_start_date_7 <= drug_start_date_8 &
      drug_start_date_8 <= drug_start_date_9 &
      drug_start_date_9 <= drug_start_date_10 &
      drug_start_date_10 <= drug_start_date_11 &
      drug_start_date_11 <= drug_start_date_12 &
      drug_start_date_12 <= drug_start_date_13 &
      drug_start_date_13 <= drug_start_date_14 &
      drug_start_date_14 <= drug_start_date_15 &
      drug_start_date_15 <= drug_start_date_16 &
      drug_start_date_16 <= drug_start_date_17 ~ "OK",
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
      drug_start_date_16 > drug_start_date_17 ~ "not good"
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
    last_date_available > date_of_diagnosis_1 ~ "OK",
    last_date_available <= date_of_diagnosis_1 ~ "not good"
  )) %>% 
  mutate(birth_diag_lastdate = case_when(
    Date_of_Birth < date_of_diagnosis_1 &
      date_of_diagnosis_1 < last_date_available ~ "OK"
  )) %>% 
  mutate(rad_after_diag = case_when(
    rad_start_date_1 > date_of_diagnosis_1 ~ "OK"
  )) %>% 
  mutate(bmt_after_diag = case_when(
    date_of_first_bmt > date_of_diagnosis_1 ~ "OK"
  )) %>% 
  mutate(drug_after_diag = case_when(
    drug_start_date_1 > date_of_diagnosis_1 ~ "OK"
  ))

table_sanity_check <- as.data.table(matrix(c("check", "radiation_check", "treatment_check", "diag_check", "rad_check", "sct_check", "treat_check", "birth_BF_lastdate",
                              "birth_BF_diag", "diag_BF_lastdate", "birth_diag_lastdate", 
                              "rad_after_diag", "bmt_after_diag", "drug_after_diag",
                              "comments", "if stop date after start date", "if stop date after start date",
                              "if diag1 before diag2 before diag3 etc", "if rad1 before rad2 before rad3 etc",
                              "if bmt1 before bmt2 before bmt3 etc", "if drug1 before drug2 before drug3 etc",
                              "if birth before last date available", "if birth before diag", "if diag before last date available",
                              "if birth before diag before last date available",
                              "if rad1 after diag","if bmt1 after diag", "if drug1 after diag", 
                              "OK", sum(str_count(radiation$radiation_check, "OK"), na.rm = TRUE), sum(str_count(treatment$treatment_check, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_check, "OK"), na.rm = TRUE), sum(str_count(sanity_check$rad_check, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$sct_check, "OK"), na.rm = TRUE), sum(str_count(sanity_check$treat_check, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$birth_BF_lastdate, "OK"), na.rm = TRUE), sum(str_count(sanity_check$birth_BF_diag, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_BF_lastdate, "OK"), na.rm = TRUE), sum(str_count(sanity_check$birth_diag_lastdate, "OK"), na.rm = TRUE),
                              sum(str_count(sanity_check$rad_after_diag, "OK"), na.rm = TRUE), sum(str_count(sanity_check$bmt_after_diag, "OK"), na.rm = TRUE), 
                              sum(str_count(sanity_check$drug_after_diag, "OK"), na.rm = TRUE),
                              
                              "not good", sum(str_count(radiation$radiation_check, "not good"), na.rm = TRUE), sum(str_count(treatment$treatment_check, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_check, "not good"), na.rm = TRUE), sum(str_count(sanity_check$rad_check, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$sct_check, "not good"), na.rm = TRUE), sum(str_count(sanity_check$treat_check, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$birth_BF_lastdate, "not good"), na.rm = TRUE), sum(str_count(sanity_check$birth_BF_diag, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$diag_BF_lastdate, "not good"), na.rm = TRUE), sum(str_count(sanity_check$birth_diag_lastdate, "not good"), na.rm = TRUE),
                              sum(str_count(sanity_check$rad_after_diag, "not good"), na.rm = TRUE), sum(str_count(sanity_check$bmt_after_diag, "not good"), na.rm = TRUE), 
                              sum(str_count(sanity_check$drug_after_diag, "not good"), na.rm = TRUE)
                              ), ncol = 14, byrow=TRUE))

write.csv(table_sanity_check, paste0(path, "/sanity check.csv"))

wrong_date_bmt <- as.data.table(sanity_check[which(sanity_check$sct_check == "not good"), c("avatar_id", "date_of_first_bmt_1", "date_of_second_bmt_1")])
write.csv(wrong_date_bmt, paste0(path, "/wrong_date_bmt.csv"))

wrong_date_drug <- sanity_check[which(sanity_check$treat_check == "not good"), ] %>% 
  select("avatar_id", starts_with("drug_start"), starts_with("drug_name"))
wrong_date_drug <- as.data.table(wrong_date_drug)
# write.csv(wrong_date_drug, paste0(path, "/wrong_date_drug.csv"))

wrong_diag_or_lastdate <- as.data.table(sanity_check[which(sanity_check$diag_BF_lastdate == "not good"), c("avatar_id", "date_of_diagnosis_1", "last_date_available",
                                                                        "date_death", "date_last_follow_up")])
# write.csv(wrong_diag_or_lastdate, paste0(path, "/wrong_diag_or_lastdate.csv"))

missing_diag <- as.data.table(sanity_check[which(is.na(sanity_check$date_of_diagnosis_1)), c("avatar_id", "date_of_diagnosis_1")])
# write.csv(missing_diag, paste0(path, "/missing_diag date.csv"))

