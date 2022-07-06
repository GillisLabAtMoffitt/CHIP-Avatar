#######################################################################################  Somatic mutations
WES_jan2022 <- WES_jan2022 %>% 
  arrange(avatar_id, collectiondt_germline, collectiondt_tumor)

# Select the disease status closest to germline
WES_jan2022 <- WES_jan2022 %>% 
  mutate(int = interval(start = collectiondt_germline, 
                        end = collectiondt_tumor) /
           duration(n=1, units = "days")) %>% 
  group_by(avatar_id) %>% 
  mutate(min = min(int)) %>% 
  mutate(Disease_Status_germline = case_when(
    min == int               ~ Disease_Status,
    TRUE                     ~ NA_character_
  )) %>% 
  fill(Disease_Status_germline, .direction = "updown") %>% 
  ungroup()

# Pivot wider
Germline <- dcast(setDT(WES_jan2022), 
                  avatar_id+mrn+SLID_germline+moffittSampleId_germline+collectiondt_germline+Disease_Status_germline ~ 
                    rowid(avatar_id),
                  value.var = c(
                    "SLID_tumor",
                    "moffittSampleId_tumor",
                    "collectiondt_tumor",
                    "moffittSampleId",
                    "DNASequencingLibraryID",
                    "Disease_Status"
                  )
)

####################################################################
########## NOT USED ANYMORE SINCE WE ONLY HAVE 1 FILE NOW ########## 
####################################################################

# Germline <- Germline %>% 
#   distinct()
# 
# # One of the sequencing data is in 2 part so merge that first
# # Are moffitt_sample_id are equal in WES and Sequencing ?
# # Sequencing <- Sequencing[order(Sequencing$moffitt_sample_id),]
# # WES <- WES[order(WES$moffitt_sample_id),]
# # Sequencing$moffitt_sample_id == WES$moffitt_sample_id # =>>>>>>> YES
# Sequencing <-
#   full_join(
#     Sequencing,
#     WES_tumor,
#     by = "moffittSampleId_tumor")
# # Bind Sequencing
# Seq_WES <- bind_rows(Seq_WES_Raghu, Sequencing2, Seq_WES_Raghu2, Sequencing, .id = "vers") %>% 
#   arrange(collectiondt_germline) %>% 
#   distinct(SLID_tumor, moffittSampleId_tumor, SLID_germline, .keep_all = TRUE)
# 
# # duplicated(WES_seq$moffittSampleId_tumor) # No duplicate
# # duplicated(WES_seq$avatar_id) # has duplicate so
# # Reshape to have duplicate ID on same row (per date) but
# # Really important to order by dates otherwise cannot find the duplicated lines
# Seq_WES <- Seq_WES[order(Seq_WES$collectiondt_tumor), ]
# # pivot wider
# WES_seq <-
#   dcast(setDT(Seq_WES), avatar_id+SLID_germline+moffittSampleId_germline+collectiondt_germline ~ rowid(avatar_id),
#         value.var = c(
#           "SLID_tumor",
#           "moffittSampleId_tumor",
#           "collectiondt_tumor",
#           "BaitSet"
#         )
#   )
# 
# # # Merge with Germ (date) with WES_seq (sequencing)
# # Combined_data_MM <- merge.data.frame(Germ, WES_seq,
# #                                      by.x = "avatar_id", by.y = "avatar_id", 
# #                                      all.x = TRUE, all.y = TRUE)
# # 
# # # I checked the ID they are all the same no missing nor added
# # 
# # #######################################################################################  III  # For 2nd sequencing file
# # # Really important to order by dates otherwise cannot find the duplicated lines
# # Seq_WES_Raghu <- Seq_WES_Raghu[order(Seq_WES_Raghu$collectiondt_tumor), ]
# # Seq_WES_Raghu <-
# #   dcast(setDT(Seq_WES_Raghu), avatar_id+SLID_germline+moffittSampleId_germline ~ rowid(avatar_id),
# #         value.var = c(
# #           "SLID_tumor",
# #           "moffittSampleId_tumor",
# #           "collectiondt_tumor",
# #           "BaitSet"
# #         )
# #   ) 
# # 
# # Seq_WES_Raghu <- merge.data.frame(Germ2, Seq_WES_Raghu, 
# #                           by.x = "avatar_id", by.y = "avatar_id",
# #                           all.x = TRUE, all.y = TRUE) 
# # #######################################################################################  III  # For 3rd sequencing file
# # Sequencing2 <- merge.data.frame(Germ3, Sequencing2, 
# #                                by.x = "avatar_id", by.y = "avatar_id",
# #                                all.x = TRUE, all.y = TRUE) %>% 
# #   arrange(collectiondt_germline)
# # #######################################################################################  III  # For 4th sequencing file
# # # Really important to order by dates otherwise cannot find the duplicated lines
# # Seq_WES_Raghu2 <- Seq_WES_Raghu2[order(Seq_WES_Raghu2$collectiondt_tumor), ]
# # Seq_WES_Raghu2 <-
# #   dcast(setDT(Seq_WES_Raghu2), avatar_id+SLID_germline+moffittSampleId_germline+collectiondt_germline ~ rowid(avatar_id),
# #         value.var = c(
# #           "SLID_tumor",
# #           "moffittSampleId_tumor",
# #           "collectiondt_tumor",
# #           "BaitSet"
# #         )
# #   )
# # Seq_WES_Raghu2 <- full_join(Germ4, Seq_WES_Raghu2, by = "SLID_germline") %>% 
# #   select(avatar_id = "avatar_id.x", everything(), -avatar_id.y)
# # ########### Binds
# # 
# # # Germline <- bind_rows(Combined_data_MM, Seq_WES_Raghu,Sequencing2, .id = "vers")
# # # Germline <- Germline %>% distinct(avatar_id,
# # #                              SLID_germline , .keep_all = TRUE) 
# # Germline <- bind_rows(Combined_data_MM, Seq_WES_Raghu, Sequencing2, Seq_WES_Raghu2, .id = "vers")
# # Germline <- Germline %>% distinct(avatar_id,
# #                                   SLID_germline , .keep_all = TRUE) 
# # # write.csv(Germline, paste0(path, "/Combined germline_seq data.csv"))
# 
# 
# # Merge all
# Germline <- left_join(WES_seq, Germline, by = "avatar_id") %>% 
#   # To eliminate the duplicate with 2 slid and date for A108 patient
#   filter(SLID_germline.x == SLID_germline.y | is.na(SLID_germline.x == SLID_germline.y)) %>%
#   rename(SLID_germline = "SLID_germline.x", collectiondt_germline = "collectiondt_germline.x") %>% 
#   # distinct(avatar_id, SLID_germline, .keep_all = TRUE) %>% 
#   mutate(collectiondt_germline = coalesce(collectiondt_germline, collectiondt_germline.y)) %>% 
#   select(-SLID_germline.y, -collectiondt_germline.y)
# 
# 
# old_version_not_found_in_new_version <- Germline %>% 
#   filter(!str_detect(avatar_id, paste0(WES_jan2022$avatar_id, collapse = "|")))
# write_csv(old_version_not_found_in_new_version, "old_version_not_found_in_new_version.csv")
# 
# new_version_not_found_in_old_version <- WES_jan2022 %>% 
#   filter(!str_detect(avatar_id, paste0(Germline$avatar_id, collapse = "|")))
# write_csv(new_version_not_found_in_old_version, "new_version_not_found_in_old_version.csv")
# 




# Cleaning
rm(WES_jan2022)


#######################################################################################  II  ## Bind Version, Clean----
#######################################################################################  II  ## Create a wider format for each to facilitate date comparison
# Demographic ----
Demo_HRI <- full_join(Demo_linkage, Demo_HRI, by= "MRN") %>% 
  distinct(.) %>% 
  mutate(MRN = as.character(MRN))
Demo_HRI$Date_of_Birth <- as.POSIXct(strptime(Demo_HRI$Date_of_Birth, 
                                                  format = "%m/%d/%Y", tz = "UTC"))
uid <- paste(unique(Demo_RedCap_V4ish$avatar_id), collapse = '|')
Demo_HRI <- Demo_HRI[(!grepl(uid, Demo_HRI$avatar_id)),]
Demo_RedCap_V4ish <- bind_rows(Demo_RedCap_V4ish, Demo_HRI, .id = "versionDemo")
Demo_RedCap_V4ish <- Demo_RedCap_V4ish %>% 
  mutate(Race = case_when(
    Race %in% c("African American")                           ~ "Black",
    Race %in% c("Other")                                      ~ "Others",
    Race %in% c("More Than One Race")                         ~ "More than one race",
    Race %in% c("Other Asian including Asian and Oriental")   ~ "Asian",
    Race %in% c("PT Not Present")                             ~ "Unknown",
    Race %in% c("AM INDIAN")                                  ~ "Am Indian",
    TRUE                                                      ~ Race
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Am Indian", "Asian", "More than one race", "Others", "Unknown"))) %>% 
  mutate(Race1 = factor(Race, levels=c("White", "Black"))) %>% 
  mutate(Ethnicity = case_when(
    Ethnicity %in% c("Spanish; Hispanic")                                   ~ "Hispanic",
    Ethnicity %in% c("Non- Hispanic", "Non-Spanish; non-Hispanic")          ~ "Non-Hispanic",
    Ethnicity %in% c("Unknown", "Prefer not to answer", "PT Not Present")   ~ "Unknown",
    TRUE                                                                    ~ Ethnicity
  )) %>% 
  mutate(Ethnicity1 = factor(Ethnicity, levels= c("Non-Hispanic", "Hispanic"))) %>% 
  mutate(raceeth = case_when(
    Race1 == "Black"                  ~ "Black",
    (Race1 == "Black" &
      Ethnicity == "Hispanic")        ~ "Others",
    Ethnicity == "Hispanic"           ~ "Hispanic",
    Race1 == "White" &
      Ethnicity == "Non-Hispanic"     ~ "White Non-Hispanic",
    TRUE                              ~ "Others"
  )) %>% 
  mutate(raceeth = factor(raceeth, levels=c("White Non-Hispanic", "Hispanic", "Black", "Others")))

# Patient history ----
# Create function to code disease stage for latest version and fit to older version
history_disease <- function(data){
  data <- data %>% 
    filter(histology == 97651 | histology == 97323) %>% 
    mutate(disease_stage = case_when(
      hematological_malignancy_phase == 1         ~ "active",
      hematological_malignancy_phase == 2         ~ "smoldering",
      histology == 97651                          ~ "mgus"
    )) %>% 
    select(-c(hematological_malignancy_phase, histology))
}
MM_history_V12 <- history_disease(MM_history_V12)
MM_historyV4 <- history_disease(MM_historyV4)
MM_historyV4.1 <- history_disease(MM_historyV4.1)

# Change status for patients that Nancy checked
id <- paste("A022604","A027407","A029244","A007364","A000238",
            "A000530","A007146","A010533","A016764", sep = "|")
# Add last Raghu date of MM diagnosis
Dx_date <- Diagnosis_ISS %>% select("avatar_id", "last_mrn", date_of_diagnosis = "MM_date_dx") %>% 
  mutate(disease_stage = "active")

mm_history <- bind_rows(MM_history_V12, #MM_history, MM_historyV2,
                        MM_historyV4, MM_historyV4.1,
                        Dx_date) %>%
  drop_na("date_of_diagnosis") %>%
  mutate(mrn = coalesce(mrn, last_mrn)) %>% 
  group_by(avatar_id) %>% 
  fill(mrn, .direction = "downup") %>% 
  ungroup() %>% 
  distinct(avatar_id, date_of_diagnosis, disease_stage, .keep_all = TRUE) %>% 
  arrange(avatar_id, date_of_diagnosis) %>% 
  # Change status for patients that Nancy checked
  mutate(disease_stage = case_when(
    str_detect(avatar_id, id) &
      disease_stage == "active"   ~ "wrongly classified", 
    TRUE                          ~ disease_stage
    )) %>% 
  # mutate(date_of_diagnosis = case_when(
  #   str_detect(avatar_id, id) &
  #     disease_stage == "wrongly classified"   ~ NA_POSIXct_, 
  #   TRUE                                      ~ date_of_diagnosis)) %>% 

  # code smoldering diagnosis date
  group_by(avatar_id) %>% 
  mutate(sm_date_diagnosis = case_when(
    disease_stage == "smoldering"           ~ date_of_diagnosis,
    TRUE                                    ~ NA_POSIXct_
  )) %>% 
  arrange(avatar_id, sm_date_diagnosis) %>% 
  mutate(sm_date_diagnosis = first(sm_date_diagnosis)) %>% 
  ungroup() %>% 
  
  # Code the first active/relapse date of diagnosis as MM diagnosis
  arrange(avatar_id, date_of_diagnosis) %>% 
  group_by(avatar_id) %>% 
  mutate(date_of_MM_diagnosis = case_when(
    str_detect(disease_stage, "active|relapse")        ~ date_of_diagnosis,
    TRUE                                               ~ NA_POSIXct_
  )) %>% 
  arrange(avatar_id, date_of_MM_diagnosis) %>% 
  mutate(date_of_MM_diagnosis = first(date_of_MM_diagnosis)) %>% 
  ungroup() %>% 
  arrange(avatar_id, date_of_diagnosis) %>% 
  
  mutate(is_patient_MM = case_when(
    !is.na(date_of_MM_diagnosis)           ~ "Yes",
    is.na(date_of_MM_diagnosis)            ~ "No"
  )) %>% 
  
  # Create Dx_date_closest_blood date (general diagnosis, in not specific MM)
  left_join(., Germline %>% select("avatar_id", "collectiondt_germline"), # For only 1 date of Dx when multiple germline collection
            by = "avatar_id") %>% 
  # For 180
  mutate(closest_date_180_cleaning = date_of_diagnosis-collectiondt_germline) %>% 
  group_by(avatar_id, date_of_diagnosis, disease_stage) %>% 
  arrange(closest_date_180_cleaning) %>% 
  distinct(avatar_id, date_of_diagnosis, .keep_all = TRUE) %>% 
  ungroup() %>% 
  
  mutate(interval = (interval(start= .$collectiondt_germline, end= .$date_of_diagnosis)/duration(n=1, unit="days"))) %>% 
  mutate(interval = if_else(interval>100, NA_real_, interval)) %>% 
  mutate(interval1 = abs(interval)) %>% 
  arrange(interval1) %>% 
  group_by(avatar_id) %>% mutate(id = 1:n()) %>% ungroup() %>%
  mutate(Dx_date_closest_germline = if_else(id == 1, date_of_diagnosis, NA_POSIXct_)) %>% 
  distinct(avatar_id, date_of_diagnosis, .keep_all = TRUE) %>% 
  group_by(avatar_id) %>% fill(Dx_date_closest_germline, .direction = "updown") %>% 
  arrange(avatar_id, date_of_diagnosis) %>% 
  ungroup() %>% 
  
  # code actual status
  mutate(smoldering_status = case_when(
    !is.na(date_of_MM_diagnosis) &
      !is.na(sm_date_diagnosis)        ~ "Progressed from Smoldering",
    !is.na(date_of_MM_diagnosis) &
      is.na(sm_date_diagnosis)         ~ "Never Smoldering",
    is.na(date_of_MM_diagnosis) &
      !is.na(sm_date_diagnosis)        ~ "Is Smoldering", 
    TRUE                               ~ NA_character_
  ))


MM_history <- dcast(setDT(mm_history), 
                    avatar_id+collectiondt_germline+date_of_MM_diagnosis+is_patient_MM+
                      Dx_date_closest_germline+
                      sm_date_diagnosis+smoldering_status ~ 
                      rowid(avatar_id), 
                    value.var = c("date_of_diagnosis", "disease_stage")) %>% 
  
  
  # unite(Dx_date_closest_germline, starts_with("Dx_date_closest_germline"), na.rm = TRUE, remove = TRUE) %>% 
  # mutate(Dx_date_closest_germline = as.POSIXct(.$Dx_date_closest_germline, format = "%Y-%m-%d")) %>% 
  # select(-collectiondt_germline) %>% 
  # Create var = first date of Dx for MM diagnostic aka "active" (not for mgus or smoldering)
  # Then when not "active" take the first date of Dx available (mgus or smoldering or NA without regarding order- 
  # usually mgus before smoldering)
  # mutate(date_of_MMonly_diagnosis = case_when(
  #   str_detect(disease_stage_1, "active|relapse")          ~ date_of_diagnosis_1
  # )) %>% 
  # mutate(date_of_MMonly_diagnosis2 = case_when(
  #   str_detect(disease_stage_2, "active|relapse")          ~ date_of_diagnosis_2
  # )) %>% 
  # mutate(date_of_MMonly_diagnosis3 = case_when(
  #   str_detect(disease_stage_3, "active|relapse")          ~ date_of_diagnosis_3
  # )) %>% 
  # mutate(date_of_MMonly_diagnosis4 = case_when(
  #   str_detect(disease_stage_4, "active|relapse")          ~ date_of_diagnosis_4
  # )) %>% 
  # mutate(date_of_MMonly_diagnosis = coalesce(date_of_MMonly_diagnosis, date_of_MMonly_diagnosis2, date_of_MMonly_diagnosis3, date_of_MMonly_diagnosis4)) %>% 
  

  mutate(date_of_MMSMMGUSdiagnosis = case_when(
    disease_stage_1 == "smoldering"      ~ date_of_diagnosis_1,
    disease_stage_2 == "smoldering"      ~ date_of_diagnosis_2,
    disease_stage_3 == "smoldering"      ~ date_of_diagnosis_3,
    disease_stage_4 == "smoldering"      ~ date_of_diagnosis_4,
    disease_stage_1 == "mgus"            ~ date_of_diagnosis_1,
    disease_stage_2 == "mgus"            ~ date_of_diagnosis_2,
    disease_stage_3 == "mgus"            ~ date_of_diagnosis_3,
    disease_stage_4 == "mgus"            ~ date_of_diagnosis_4
  )) %>% 
  mutate(date_of_MMSMMGUSdiagnosis = coalesce(date_of_MM_diagnosis, date_of_MMSMMGUSdiagnosis)) %>% 
  mutate(interval_MM = interval(start= date_of_MM_diagnosis, end= collectiondt_germline)/duration(n=1, unit="days")) %>% 
  mutate(is_MMDx_close_to_blood = case_when(
    is_patient_MM == "Yes" &
      interval_MM < -60                  ~ "No",
    is_patient_MM == "Yes"               ~ "Yes",
    TRUE                                 ~ NA_character_
  )) %>% 
  select(c("avatar_id", "Dx_date_closest_germline", "date_of_MM_diagnosis", "is_patient_MM", 
           "sm_date_diagnosis", "smoldering_status", "date_of_MMSMMGUSdiagnosis", 
           interval_MM, is_MMDx_close_to_blood, everything(), -collectiondt_germline))

# write.csv(MM_history,paste0(path, "/simplified files/MM_history simplify.csv"))


# Staging ISS
ISS_temp <- bind_rows(Staging_V12, StagingV4, StagingV4.1, .id = "verso") %>% 
  filter(staging_type == "iss") %>% select(avatar_id, date_staging_results, iss = staging_value)

ISS_df <- 
  # bind_rows(Staging %>% mutate(iss = as.character(iss)), StagingV2, ISS_temp) %>% 
  ISS_temp %>% 
  left_join(., MM_history %>% select(avatar_id, date_of_MM_diagnosis),
            by = "avatar_id") %>% 
  drop_na(iss) %>% 
  mutate(interval = (interval(start= date_staging_results, end= date_of_MM_diagnosis)/duration(n=1, unit="days"))) %>% 
  mutate(interval = abs(interval)) %>% 
  arrange(interval) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(iss = case_when(
    str_detect(iss, "3|III")          ~ "III",
    str_detect(iss, "2|II")          ~ "II",
    str_detect(iss, "1|I")          ~ "I",
    TRUE                          ~ NA_character_
  ))

EHR_ISS <- EHR_ISS %>% 
  mutate(ISS_EHR = coalesce(ISS_EHR, ISS_calculated)) %>% 
  mutate(ISS_EHR = str_remove(ISS_EHR, "stage ")) %>% 
  # mutate(interval = abs(interval(start = date_of_MM_diagnosis, end = date_B2)/
  #                         duration(n= 1, units = "days"))) %>% 
  # filter() %>% 
  # mutate(interval = abs(interval(start = date_of_MM_diagnosis, end = date_albumin)/
  #                           duration(n= 1, units = "days"))) %>% 
  filter(!is.na(ISS_EHR) | !is.na(B2) | !is.na(albumin))
    
Diagnosis_ISS <- Diagnosis_ISS %>% 
  full_join(., ISS_df %>% select(avatar_id, iss), by = "avatar_id") %>% 
  full_join(., EHR_ISS %>% select(avatar_id, ISS_EHR), by = "avatar_id") %>% 
  mutate(ISS_at_MMdx = coalesce(ISS_at_MMdx, iss, ISS_EHR)) %>% 
  select(avatar_id, last_mrn, ISS_at_MMdx)


rm(ISS_temp, ISS_df, EHR_ISS, history_disease)

# Cytogenetics

Cytogenetics <- Cytogenetics %>% 
  # fish_cytogenetics == 4 means not performed for that biopsy
  filter(fish_cytogenetics != 4) %>% 
  mutate_at(c("amp_dup_1q21", "del_1p", "del_17p", "del_13q",
              "t11_14", "t14_16", "t4_14"), ~ case_when(
                . == 1                                    ~ "Yes",
                fish_cytogenetics == 1 & is.na(.)         ~ "No"
              )) %>% 
  mutate(fish_cytogenetics = case_when(
    fish_cytogenetics == 1                                ~ "abnormal",
    fish_cytogenetics == 2                                ~ "normal"
  )) %>% 
  left_join(., MM_history %>% select("avatar_id", "date_of_MM_diagnosis"),
            by = "avatar_id") %>% 
  mutate(interval = (interval(start= cytogenetics_date, end= date_of_MM_diagnosis)/duration(n=1, unit="days"))) %>% 
  mutate(interval = abs(interval)) %>%
  filter(interval < 60) %>% 
  arrange(interval) %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(avatar_id : t4_14)

# Vitals ----
# Bind and arrange to have dates in order within each Alive, Dead, and Lost
Vitals <- bind_rows(Vitals_V12, #Vitals, VitalsV2, 
                    VitalsV4, VitalsV4.1, .id = "versionVit") %>% 
  # mutate(vital_status_rec = case_when(
  #   vital_status == 2         ~ "Dead",
  #   vital_status == 1         ~ "Alive",
  #   vital_status == 3         ~ "Lost"
  # )) %>% 
  distinct() %>% 
  arrange(vital_status, date_death, date_last_follow_up)

# Create a separate df to bind after cleaning to Vitals for tracking lost of contact
Contact_lost <- Vitals %>% 
  filter(vital_status == 3) %>% 
  mutate(was_contact_lost = "Loss of contact") %>% 
  distinct(.) %>% 
  select(c("avatar_id", "was_contact_lost", date_contact_lost = "date_last_follow_up"))

# Pivot wider
Vitals <- dcast(setDT(Vitals), avatar_id ~ rowid(avatar_id), 
                value.var = c("vital_status", "date_death", 
                              "date_last_follow_up")) %>% 
  purrr::keep(~!all(is.na(.))) %>%
  # Need to take the last date_follow_up recorded 
  # ex: use coalesce to fill up with date_last_follow_up_1 when date_last_follow_up_2 is NA
  mutate(date_last_follow_up = coalesce(!!! select(., last_col():"date_last_follow_up_1"))) %>%
  # Have multiple record ("abstraction") per patient so date_death can be recorded multiple times in col 1 and 2
  # Sometimes 1st "abstraction" record date_last_follow-up then second "abstraction" will record death (present in col 2)
  # Use coalesce so when is NA in date_death_1 will take the value in date_death_2 if present
  mutate(date_death = coalesce(!!! select(., starts_with("date_death_")))) %>% 
  # Create my own vital_status var because found record with 
  # 1st "abstraction" give date_death so vital = dead
  # 2nd "abstraction" doesn't give date (probably because already recorded) so vital = alive
  # mutate(end_vital_status = case_when(
  #   !is.na(date_death)                  ~ "Dead",
  #   !is.na(date_last_follow_up)         ~ "Alive"
  # )) %>% 
  select(c("avatar_id", "date_death", "date_last_follow_up"))

Vitals <- full_join(Vitals, Contact_lost, by= "avatar_id") %>% 
  # Remove the last date of follow up when is contact lost, otherwise would have doubled `last_date_available` in the future
  mutate(date_last_follow_up = case_when( 
    !is.na(date_contact_lost)   ~ NA_POSIXct_,
    is.na(date_contact_lost)    ~ date_last_follow_up
  )) # %>% Cannot use that when doing PFS, need to keep date of last follow up even before death 
  # mutate(date_last_follow_up = case_when(
  #   date_last_follow_up <= date_death       ~ NA_POSIXct_,
  #   is.na(date_death)                       ~ date_last_follow_up
  # )) 

# write.csv(Vitals,paste0(path, "/simplified files/Vitals simplify.csv"))
# For BMI, we may need to keep both if want to see evolution but for now keep the earliest (closest to diagnisis)
# then fill-up with second column when the first is NA using coalesce
# mutate(bmi_at_dx_v2 = coalesce(bmi_at_dx_v2_1, bmi_at_dx_v2_2)) %>% 
# Have patients who had "abstraction" 3 times and for who the alcohol_use and smoking_status was recorded only on the third
# Take third record, fill it up by the second when NA then the first when NA
# That is to get the latest info. We may switch it if we want the closest to diag.
# mutate(alcohol_use = coalesce(alcohol_use_2, alcohol_use_1)) %>% 
# mutate(alcohol_use = case_when(
#   alcohol_use %in% c(0,3) ~ "never",
#   alcohol_use == 2 ~ "former",
#   alcohol_use == 1 ~ "current",
#   TRUE ~ NA_character_
# )) %>% 
# # Smoking V1 have 2 var current_smoker_1 (1-2), smoking_status_1 ()
# # Fill-up current_smoker_1 by smoking_status_1
# # That is to get the lastest info. We may switch it if we want the closest to diag.
# mutate(smoking_status = coalesce(smoking_status_2, current_smoker_1, smoking_status_1)) %>% 
# mutate(smoking_status = case_when(
#   smoking_status %in% c(0,3) ~ "never",
#   smoking_status == 2 ~ "former",
#   smoking_status == 1 ~ "current",
#   TRUE ~ NA_character_
# )) %>% 
# Note for smoking
# 1 patient said 3 in V2 and 11 in V1
# 1 patient said 3 in V2 and 12 in V1

# Clean date_last_follow_up to be NA when:
# the date_last_follow_up is the date_contact_lost
# happen before or equal death

# Bone marrow transplant ----
# SCT <- SCT %>% pivot_longer(cols = c(date_of_first_bmt, date_of_second_bmt, date_of_third_bmt),
#                              values_to = "date_of_bmt", values_drop_na = TRUE)
# SCTV2 <- SCTV2 %>% pivot_longer(cols = c(date_of_first_bmt, date_of_second_bmt, date_of_third_bmt),
#                              values_to = "date_of_bmt", values_drop_na = TRUE)

sct <- bind_rows(SCT_V12, #SCT, SCTV2, 
                 SCTV4, SCTV4.1, .id = "versionSCT") %>% 
  distinct(avatar_id, date_of_bmt) %>% 
  arrange(date_of_bmt)
SCT <- dcast(setDT(sct), avatar_id ~ rowid(avatar_id), 
             value.var = "date_of_bmt") %>% 
  `colnames<-`(c("avatar_id", "date_of_bmt_1", "date_of_bmt_2", "date_of_bmt_3"))
# write.csv(SCT,paste0(path, "/simplified files/SCT simplify.csv"))

# Treatment ----
IMIDS_maintenance <- IMIDS_maintenance %>% 
  unite(drug_name_, drug_name_:drug_name_other, sep = ": ", remove = FALSE, na.rm = TRUE) %>%
  mutate(imids_maintenance = case_when(
    str_detect(drug_name_, "lidomide")    ~ "IMIDs as maintenance",
    TRUE                                  ~ "no IMIDs as maintenance"
  )) %>% 
  full_join(migration_patients, ., by = "avatar_id") %>% 
  mutate(imids_maintenance = ifelse(is.na(imids_maintenance), "not qc'd", imids_maintenance)) %>% 
  arrange(imids_maintenance) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select("avatar_id", "imids_maintenance")

# remove NA row in QC'd data
# Qcd_Treatment <- Qcd_Treatment %>% drop_na("drug_start_date", "drug_name_")
# Qcd_TreatmentV2 <- Qcd_TreatmentV2 %>% drop_na("drug_start_date", "drug_name_") %>%
#   arrange(drug_start_date) %>% 
#   group_by(avatar_id) %>% 
#   mutate(treatment_line_1 = as.character(dense_rank(interaction(avatar_id, drug_start_date)))) %>% 
#   mutate(treatment_line_ = coalesce(treatment_line, treatment_line_1)) %>% select(-treatment_line, -treatment_line_1)
# # remove the Ids found in Qc'd from the Treatment 
# uid <- paste(unique(Qcd_Treatment$avatar_id), collapse = '|')
# Treatment <- Treatment[(!grepl(uid, Treatment$avatar_id)),]
# uid <- paste(unique(Qcd_TreatmentV2$avatar_id), collapse = '|')
# TreatmentV2 <- TreatmentV2[(!grepl(uid, TreatmentV2$avatar_id)),]
# Bind QC'd and Treatment for each version
# Need to pivot longer Treatment from V1 (and V2) because not same formatting
# Having one drug per row will help to remove duplicate in drugs after binding all version together
# Treatment <- bind_rows(Qcd_Treatment, Treatment) %>% 
#   filter(drug_name_ != "Non-MM drugs") %>% 
#   group_by(avatar_id) %>% 
#   arrange(drug_start_date) %>% 
#   mutate(treatment_line_ = as.character(row_number())) %>% 
#   # mutate_at(("drug_name_"), ~ str_replace_all(., "/", ": ")) %>% 
#   separate(col = drug_name_, paste("drug_name_", 1:10, sep=""), sep = "; |;", extra = "warn", 
#            fill = "right") %>% 
#   purrr::keep(~!all(is.na(.))) %>%
#   pivot_longer(cols = starts_with("drug_name_"),
#                names_to = "drug", values_to = "drug_name_", values_drop_na = TRUE)
# 
# 
# TreatmentV2 <- TreatmentV2 %>% 
#   group_by(avatar_id, drug_start_date) %>% 
#   arrange(drug_start_date, treatment_line_) %>% 
#   fill(treatment_line_, .direction = "updown") %>% 
#   # group_by(avatar_id) %>% 
#   # # mutate(treatment_line = dense_rank(interaction(avatar_id, drug_start_date))) %>% 
#   # fill(treatment_line, .direction = "downup") %>%  # is not the best way, could do a 30 days rule
#   ungroup() %>% 
#   bind_rows(Qcd_TreatmentV2, .) %>% 
#   separate(col = drug_name_, paste("drug_name_", 1:7, sep=""), sep = "; |;", extra = "warn", 
#            fill = "right") %>% 
#   purrr::keep(~!all(is.na(.))) %>%
#   pivot_longer(cols = starts_with("drug_name_"),
#                names_to = "drug", values_to = "drug_name_", values_drop_na = TRUE)

Treatment_V12 <- Treatment_V12 %>% 
  separate(col = drug_name_, paste("drug_name_", 1:7, sep=""), sep = "\\+", extra = "warn", # Just for 1 row
           fill = "right") %>% 
  purrr::keep(~!all(is.na(.))) %>%
  pivot_longer(cols = starts_with("drug_name_"),
               names_to = "drug", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  filter(!is.na(avatar_id))

# ready to bind
treatment <- bind_rows(Treatment_V12, #Treatment, TreatmentV2, 
                       TreatmentV4, TreatmentV4.1, .id = "versionTreat") %>%
  filter(!str_detect(treatment_site, "moldering") | is.na(treatment_site)) %>% 
  mutate(treatment_line_ = case_when(
    str_detect(treatment_line_, "Tenth|10") ~ 10,
    str_detect(treatment_line_, "Eleventh|11") ~ 11,
    str_detect(treatment_line_, "Twelth|12") ~ 12,
    str_detect(treatment_line_, "Thirteenth|13") ~ 13,
    str_detect(treatment_line_, "Fourteenth|14") ~ 14,
    str_detect(treatment_line_, "Fifteenth|15") ~ 15,
    str_detect(treatment_line_, "Sixteenth|16") ~ 16,
    str_detect(treatment_line_, "First|1") ~ 1,
    str_detect(treatment_line_, "Second|2") ~ 2,
    str_detect(treatment_line_, "Third|3") ~ 3,
    str_detect(treatment_line_, "Fourth|4") ~ 4,
    str_detect(treatment_line_, "Fifth|5") ~ 5,
    str_detect(treatment_line_, "Sixth|6") ~ 6,
    str_detect(treatment_line_, "Seventh|7") ~ 7,
    str_detect(treatment_line_, "Eighth|8") ~ 8,
    str_detect(treatment_line_, "Ninth|9") ~ 9,
    treatment_line_ == "Maintenance"             ~ 90,
    treatment_line_ == "Palliative"              ~ 91,
    str_detect(treatment_line_, "Unknown")       ~ 99
  )) %>% 
  select(mrn, avatar_id, treatment_line_, drug_start_date, drug_stop_date, drug_name_#, treatment_site
         ) %>% 
  mutate(drug_name_ = tolower(drug_name_)) %>%
  mutate(drug_name_ = 
           str_remove_all(drug_name_, 
          "given with |investigational agent: |investigational therapy: |investigational therapy : |clinical trial: |clinical trial; |clinical trial-|clinical trial | sulfate|clinical trial/|other: |  |clinical trial|oral proteasome inhibitor = ")) %>%
  mutate(drug_name_ = case_when(
    drug_name_ == "cafilzomib"                                             ~ "carfilzomib",
    drug_name_ == "-doxorubicin"                                           ~ "doxorubicin",
    drug_name_ == "liposomal doxorubicin"                                  ~ "doxil",
    drug_name_ == "daratumuab"                                             ~ "daratumumab",
    str_detect(drug_name_, "^dex")                                         ~ "dexamethasone",
    drug_name_ == "cyclophosphomide: dex"                                  ~ "cyclophosphomide; dexamethasone",
    
    str_detect(drug_name_, "^lena|revlimid")                               ~ "lenalidomide",
    str_detect(drug_name_, "^mel")                                         ~ "melphalan",
    str_detect(drug_name_, "velcade")                                      ~ "bortezomib",
    drug_name_ == "vinicristine"                                           ~ "vincristine",
    (str_detect(drug_name_, "kpt") &
       str_detect(drug_name_, "300")) |
      str_detect(drug_name_, "selinexor")                                  ~ "kpt300",
    str_detect(drug_name_, "kpt") &
      str_detect(drug_name_, "8602")                                       ~ "kpt8602",
    drug_name_ %in% c("anastrozole", "azacitidine", "carmustine", 
                      "cytarabine", "decitabine", "denosumab", 
                      "docetaxel", "hydrocortisone", "methotrexate",
                      "methylprednisolone", "pegfilgrastim", "prednisone", 
                      "rapamycin", "rituxan", "rituximab", 
                      "sorafenib tosylate", "tamoxifen citrate", 
                      "zoledronic acid", "prevnar", "ruxolitinib")         ~ "non-mm drugs",
    TRUE                                                                   ~ drug_name_
  )) %>%
  filter(!is.na(avatar_id) | is.na(drug_start_date)) %>% 
  filter(drug_name_ != "non-mm drugs") %>%
  distinct() %>%
  # group_by(avatar_id, drug_start_date) %>%
  # arrange(drug_name_) %>%
  # ungroup() %>%
  arrange(avatar_id, treatment_line_, drug_name_, drug_start_date, drug_stop_date)

# treatment1 <- dcast(setDT(treatment), mrn+avatar_id+treatment_line_+drug_name_ ~ rowid(avatar_id), ## Old code
#                    value.var = c("drug_start_date", "drug_stop_date"))

# Make sure that the same drug in 1 line is counted once with the earliest start date and latest stop date
treatment1 <- treatment %>% group_by(mrn, avatar_id, treatment_line_, drug_name_) %>%
  summarise_at(vars(drug_start_date, drug_stop_date), c(paste), collapse = ";") %>% 
  separate(drug_start_date, "drug_start_date", sep = ";", extra = "drop", fill = "right") %>% 
  separate(drug_stop_date, paste("drug_stop_date", 1:10, sep = ""), sep = ";", remove = FALSE, extra = "warn", fill = "left") %>% 
  select("mrn", "avatar_id", "treatment_line_", "drug_name_", "drug_start_date", drug_stop_date = "drug_stop_date10") %>% 
  group_by(avatar_id, treatment_line_) %>% 
  mutate(line_start_date = min(drug_start_date)) %>% 
  mutate(line_stop_date = max(drug_stop_date)) %>% 
  arrange(avatar_id, drug_start_date, drug_stop_date) %>% 
  ungroup()
  

# Summarize by regimen/line
Treatment1 <- treatment1 %>% 
  group_by(mrn, avatar_id, treatment_line_, line_start_date, line_stop_date) %>%
  summarise_at(vars(drug_name_, drug_start_date, drug_stop_date), paste, collapse = "; ") %>%
  arrange(avatar_id, line_start_date, line_stop_date, drug_start_date) %>% 
  mutate(drug_count = sapply(strsplit(drug_name_, ";"), length)) %>% 

# Treatment <- dcast(setDT(treatment1), mrn+avatar_id+treatment_line_ ~ rowid(avatar_id), ## Old code
#                   value.var = c("drug_name_","drug_start_date", "drug_stop_date")) %>% 
#   unite(drug_name_, starts_with("drug_name_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  
  # unite(drug_start_date, starts_with("drug_start_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  # separate(drug_start_date, paste("drug_start_date", 1:max(.$drug_count), sep = ""), sep = "; ", remove = FALSE,
  #          extra = "warn", fill = "right") %>% 
  # mutate(max = max(drug_start_date1:drug_start_date, na.rm = TRUE))
  # 
  # select(c(mrn, avatar_id, treatment_line_, "drug_name_", drug_start_date, ncol(.):drug_stop_date_1)) %>% 
  # unite(drug_stop_date, starts_with("drug_stop_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  # separate(drug_stop_date, "drug_stop_date", sep = "; ",
  #          extra = "warn", fill = "right") %>% 
  mutate(line_start_date = as.POSIXct(line_start_date, format = "%Y-%m-%d")) %>% 
  mutate(line_stop_date = as.POSIXct(line_stop_date, format = "%Y-%m-%d")) %>% 
  mutate(regimen_name = case_when(
    drug_count == 7 &
      str_detect(drug_name_, "bort") &
      str_detect(drug_name_, "thalidomide") &
      str_detect(drug_name_, "cyclophosphamide") &
      str_detect(drug_name_, "cisplatin") &
      str_detect(drug_name_, "etoposide") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "doxo")                ~ "VDT-PACE",
    drug_count == 4 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "cyclophos") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "doxil")               ~ "ABCD",
    drug_count == 4 &
      str_detect(drug_name_, "daratumumab") &
      str_detect(drug_name_, "lena") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "carfilzomib")         ~ "Dara-KRd",
    drug_count == 4 &
      str_detect(drug_name_, "vincristine") &
      str_detect(drug_name_, "cyclophos") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "doxo")                ~ "C-VAD",
    drug_count == 4 &
      str_detect(drug_name_, "thalidomide") &
      str_detect(drug_name_, "vincristine") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "doxil")               ~ "T-VAD doxil",
    drug_count == 4 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "daratumumab") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "lena")                ~ "D-RVd",
    drug_count == 4 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "cyclophosphamide") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "doxo")                ~ "mCBAD",
    drug_count == 4 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "cyclophosphamide") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "lena")                ~ "VDCR",
    drug_count == 3 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "busulfan") &
      str_detect(drug_name_, "melphalan")           ~ "BuMelVel",
    drug_count == 3 &
      str_detect(drug_name_, "cyclophos") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "lena")                ~ "CRd",
    drug_count == 3 &
      str_detect(drug_name_, "cyclophos") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "bort")                ~ "VCd",
    drug_count == 3 &
      str_detect(drug_name_, "doxil") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "lena")                ~ "Dd-R",
    (drug_count == 3 &
       str_detect(drug_name_, "daratu") &
       str_detect(drug_name_, "lena") &
       str_detect(drug_name_, "dex")) |
      str_detect(drug_name_, "ddr")                 ~ "DRd",
    drug_count == 3 &
      str_detect(drug_name_, "vincristine") &
      str_detect(drug_name_, "doxil") &
      str_detect(drug_name_, "dex")                 ~ "DVd",
    drug_count == 3 &
      str_detect(drug_name_, "ixazomib") &
      str_detect(drug_name_, "lena") &
      str_detect(drug_name_, "dex")                 ~ "IRd",
    drug_count == 3 &
      str_detect(drug_name_, "cyclophosphamide") &
      str_detect(drug_name_, "carfilzomib") &
      str_detect(drug_name_, "dex")                 ~ "KCd",
    drug_count == 3 &
      str_detect(drug_name_, "lena") &
      str_detect(drug_name_, "carfilzomib") &
      str_detect(drug_name_, "dex")                 ~ "KRd",
    drug_count == 3 &
      str_detect(drug_name_, "lena") &
      str_detect(drug_name_, "oprozomib") &
      str_detect(drug_name_, "dex")                 ~ "ORd",
    drug_count == 3 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "doxil") &
      str_detect(drug_name_, "dex")                 ~ "PDd",
    drug_count == 3 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "doxo") &
      str_detect(drug_name_, "dex")                 ~ "PAd",
    drug_count == 3 &
      str_detect(drug_name_, "lena") &
      str_detect(drug_name_, "doxo") &
      str_detect(drug_name_, "dex")                 ~ "RAd",
    (drug_count == 3 &
       str_detect(drug_name_, "bortezomib") &
       str_detect(drug_name_, "lena") &
       str_detect(drug_name_, "dex")) |
      str_detect(drug_name_, "rvd")                 ~ "VRd", # bmt ctn 0702 (mcc 16529) rvd) # A022592
    drug_count == 3 &
      str_detect(drug_name_, "vincristine") &
      str_detect(drug_name_, "doxo") &
      str_detect(drug_name_, "dex")                 ~ "VAd",
    drug_count == 3 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "melph")               ~ "VMd",
    drug_count == 3 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "dex") &
      str_detect(drug_name_, "thalidomide")         ~ "VTd",
    drug_count == 2 &
      str_detect(drug_name_, "carfilzomib") &
      str_detect(drug_name_, "dex")                  ~ "Kd",
    drug_count == 2 &
      str_detect(drug_name_, "thal") &
      str_detect(drug_name_, "dex")                  ~ "Td",
    drug_count == 2 &
      str_detect(drug_name_, "lena") &
      str_detect(drug_name_, "dex")                 ~ "Rd",
    drug_count == 2 &
      str_detect(drug_name_, "bortezomib") &
      str_detect(drug_name_, "dex")                 ~ "Bor-Dex",
    drug_count == 1 &
      str_detect(drug_name_, "lenalidomide")        ~ "Lenalidomide",
    drug_count == 1 &
      str_detect(drug_name_, "dex")                 ~ "Dexamethasone",
    drug_count == 1 &
      str_detect(drug_name_, "doxo")                ~ "Doxorubicin",
    drug_count == 1 &
      str_detect(drug_name_, "doxil")               ~ "Doxil",
    drug_count == 1 &
      str_detect(drug_name_, "bortezomib")          ~ "Bortezomib",
    drug_count == 1 &
      str_detect(drug_name_, "melph")               ~ "Melphalan",
    drug_count == 1 &
      str_detect(drug_name_, "carfilzomib")         ~ "Carfilzomib", # Carfilzomib is next gen PI
    drug_count == 1 &
      str_detect(drug_name_, "cyclo")               ~ "Cyclophosphamide",
    drug_count == 1 &
      str_detect(drug_name_, "thalidomide")         ~ "Thalidomide",
    drug_count == 1 &
      str_detect(drug_name_, "vincristine")         ~ "Vincristine",
    TRUE                                            ~ drug_name_
  )) %>% 
  mutate(regimen_name = str_replace_na(regimen_name, replacement = "No Drugs")) %>% 
  mutate(is_PI = case_when(
    str_detect(drug_name_, "bortezomib|carfilzomib|oprozomib|ixazomib") |
      str_detect(regimen_name, "VRd|KRd") | 
      str_detect(regimen_name, "mln9708|mln 9708") ~ "PI",
    TRUE                                           ~ NA_character_
  )) %>% 
  mutate(received_IMIDs = case_when(
    str_detect(drug_name_, "lidomide") |
      str_detect(drug_name_, "ddr")                ~ "IMIDs",
    TRUE                                           ~ NA_character_
  )) %>% 
  mutate(regimen_category = case_when(
    str_detect(regimen_name, "VCd")                ~ "VCd", 
    is_PI == "PI" & received_IMIDs == "IMIDs"      ~ "PI + IMIDs",
    is.na(is_PI) & 
      is.na(received_IMIDs)                        ~ "Others",
    TRUE                                           ~ coalesce(is_PI, received_IMIDs)
  )) %>% 
  mutate(regimen_categoryVCD = case_when(
    str_detect(regimen_name, "VCd")                ~ "PI", 
    is_PI == "PI" & received_IMIDs == "IMIDs"      ~ "PI + IMIDs",
    is.na(is_PI) & 
      is.na(received_IMIDs)                        ~ "Others",
    TRUE                                           ~ coalesce(is_PI, received_IMIDs)
  ))

# Now can dcast to have line of drug_name_ for each line/regimen ## Old code
# 1st for regimen with same start and end date
# Treatment <- treatment %>% 
#   reshape2::dcast(mrn+avatar_id+treatment_line_+drug_start_date+drug_stop_date ~ rowid(avatar_id),
#                   value.var = c("drug_name_")) %>% 
#   unite(drug_name_, -mrn:-drug_stop_date, sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
#   arrange(drug_start_date, drug_stop_date)
# # 2nd for regimen with same start, I separated it to have the different end date in case
# Treatment <- dcast(setDT(Treatment), mrn+avatar_id+drug_start_date ~ rowid(avatar_id), 
#                    value.var = c("treatment_line_", "drug_name_", "drug_stop_date")) %>%
#   unite(treatment_line_, starts_with("treatment_line_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>%
#   separate(treatment_line_, "treatment_line_", sep = "; ",
#            extra = "warn", fill = "right") %>% 
#   unite(drug_name_, starts_with("drug_name_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
#   unite(drug_stop_date, starts_with("drug_stop_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
#   separate(drug_stop_date, paste("drug_stop_date", 1:3, sep="_"), sep = "; ",
#            extra = "warn", fill = "right") %>% 
#   arrange(drug_start_date)
# Treatment$drug_stop_date_1 <- as.POSIXct(Treatment$drug_stop_date_1, format = "%Y-%m-%d")
# Treatment$drug_stop_date_2 <- as.POSIXct(Treatment$drug_stop_date_2, format = "%Y-%m-%d")
# Treatment$drug_stop_date_3 <- as.POSIXct(Treatment$drug_stop_date_3, format = "%Y-%m-%d")

# Summarize by avatar_id
Treatment <- dcast(setDT(Treatment1), mrn+avatar_id ~ rowid(avatar_id), 
                   value.var = c("treatment_line_", "line_start_date", "drug_name_",
                                 "regimen_name", "regimen_category", "regimen_categoryVCD",
                                 "line_stop_date", "drug_start_date", "drug_stop_date"))

regimen_changed_id <- c("A000180", "A000414", "A014308", "A014310", "A015461", "A022588", "A025760")
Treatment <- Treatment %>% 
  purrr::keep(~!all(is.na(.))) %>% 
  full_join(., IMIDS_maintenance, by = "avatar_id") %>% 
  rename(first_regimen_name = regimen_name_1) %>% 
  mutate(first_regimen_name = ifelse((str_detect(avatar_id, paste0(regimen_changed_id, collapse = "|"))), "VRd", first_regimen_name))


rm(migration_patients, IMIDS_maintenance)
# write.csv(Treatment,paste0(path, "/simplified files/Treatment simplify.csv"))

# Radiation ----
# Radiation V1 doesn't have a date format
# RadiationV1$rad_start_date <- as.POSIXct(strptime(RadiationV1$rad_start_date, 
#                                                   format = "%m/%d/%Y", tz = "UTC"))
# RadiationV1$rad_stop_date <- as.POSIXct(strptime(RadiationV1$rad_stop_date, 
#                                                  format = "%m/%d/%Y", tz = "UTC"))

radiation <- bind_rows(Radiation_V12, #RadiationV1, RadiationV2, 
                       RadiationV4, RadiationV4.1, .id = "versionRad") %>% 
  drop_na("rad_start_date") %>% 
  filter(!str_detect(rad_start_date, "3013")) %>% 
  filter(!str_detect(rad_stop_date, "2300")) %>% 
  # left_join(., Germline %>% select(c("avatar_id", "collectiondt_germline")), by = "avatar_id") %>% 
  # mutate(rad_bf_germline = if_else(rad_start_date < collectiondt_germline, "Radiation before Germline", "No")) %>% 
  distinct(avatar_id, rad_start_date, rad_stop_date, .keep_all = TRUE) %>% 
  # select(-collectiondt_germline) %>% 
  arrange(rad_start_date)
Radiation <- dcast(setDT(radiation), avatar_id ~ rowid(avatar_id), value.var = 
                     c("rad_start_date", "rad_stop_date"))
# write.csv(Radiation,paste0(path, "/simplified files/Radiation simplify.csv"))

# Progression----
Progr_V12 <- Progr_V12 %>% 
  filter(QC == "Yes") %>% 
  drop_na(progression_date) %>% 
  distinct() %>% 
  select(-QC)
uid_P12 <- paste(unique(Progr_V12$avatar_id), collapse = '|')
Progression_V12 <- Progression_V12[(!grepl(uid_P12, Progression_V12$avatar_id)),] %>%  # Remove ID QC'd in Prog_V12
  #Progression_V12 <- Progression_V12 %>% 
  drop_na(progression_date) %>% 
  distinct()

Progression <- 
  bind_rows(Progr_V12, Progression_V12, 
            # Progression, ProgressionV2, 
            Progression_V4, Progression_V4.1) %>%
  distinct() %>% drop_na(progression_date) %>% 
  # Taking the dates of progression after the first Dx_date_closest_germline => For OS
  # (either if are MM and progressed or if are MGUS/SM and progressed to MM)
  left_join(., MM_history %>% select(c("avatar_id", "Dx_date_closest_germline")), by = "avatar_id") %>% 
  mutate(prog_after_diag = case_when(
    progression_date <= Dx_date_closest_germline         ~ "removed", # 7 are removed as they become MM
    progression_date > Dx_date_closest_germline          ~ "good" # No NA in date of Dx
  )) %>% 
  filter(prog_after_diag == "good") %>% 
  select(1:2) %>% 
  # Taking only the dates of progression before date_death (it's a sanity check-like)
  left_join(., Vitals %>% select(c("avatar_id", "date_death")), by = "avatar_id") %>% 
  mutate(prog_before_death = case_when(
    progression_date > date_death                 ~ "removed", # 0 patient removed :)
    progression_date < date_death |
      is.na (date_death)                          ~ "good"
  )) %>%
  filter(prog_before_death == "good") %>% 
  select(1:2)

# Create different df for dates from Dx or drug, hct, rad (will not have the same clean up)
Progression_hct <- Progression_rad <- Progression_drugs <- Progression 
# Progression_rad <- Progression
# Progression_hct <- Progression
# Progression_treat <- Progression # For hct and drugs

Progression <- Progression %>% # Keep earliest progression_date => For OS
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE)
# write.csv(Progression, paste0(path, "/simplified files/Progression simplify.csv"))

# Progression_treat <- Progression_treat %>% 
#   left_join(., Treatment %>% select(c("avatar_id", "line_start_date_1")), by = "avatar_id") %>%
#   left_join(., SCT %>% select(c("avatar_id", "date_of_bmt_1")), by = "avatar_id") %>% 
#   mutate(prog_before_treat = case_when(
#     progression_date <= line_start_date_1 &
#       progression_date <= date_of_bmt_1                 ~ "removed",
#     progression_date <= line_start_date_1 &
#       is.na(date_of_bmt_1)                              ~ "removed",
#     progression_date <= date_of_bmt_1 &
#       is.na(line_start_date_1)                          ~ "removed",
#     progression_date > line_start_date_1 |
#       progression_date > date_of_bmt_1 |
#       is.na(line_start_date_1) &
#       is.na(date_of_bmt_1)                              ~ "good"
#   )) %>%
#   filter(prog_before_treat == "good") %>% 
#   select(1:2) %>% 
#   arrange(progression_date) %>% 
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   rename(progression_treatment_date = "progression_date")

Progression_drugs <- Progression_drugs %>% # Remove progression < drug and keep earliest progression_drug_date
  left_join(., Treatment %>% select(c("avatar_id", "line_start_date_1")), by = "avatar_id") %>% 
  mutate(prog_before_drug = case_when(
    progression_date <= line_start_date_1               ~ "removed", # 0 patient removed :)
    progression_date > line_start_date_1 |
      is.na(line_start_date_1)                          ~ "good" # progression have to be strictly > drug
  )) %>%
  filter(prog_before_drug == "good") %>% 
  select(1:2) %>% 
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_drug_date = "progression_date")
# write.csv(Progression_drugs, paste0(path, "/simplified files/Progression used for survivals from drugs date.csv"))

Progression_rad <- Progression_rad %>% # Remove progression < rad and keep earliest progression_rad_date
  left_join(., Radiation %>% select(c("avatar_id", "rad_start_date_1")), by = "avatar_id") %>% 
  mutate(prog_before_rad = case_when(
    progression_date <= rad_start_date_1                ~ "removed", # 93 dates removed
    progression_date > rad_start_date_1 |
      is.na(rad_start_date_1)                           ~ "good" # progression have to be strictly > rad
  )) %>%
  filter(prog_before_rad == "good") %>% 
  select(1:2) %>% 
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_rad_date = "progression_date")

Progression_hct <- Progression_hct %>% # Remove progression < hct and keep earliest progression_hct_date
  left_join(., SCT %>% select(c("avatar_id", "date_of_bmt_1")), by = "avatar_id") %>% 
  mutate(prog_before_hct = case_when(
    progression_date <= date_of_bmt_1                  ~ "removed", # 75 dates removed
    progression_date > date_of_bmt_1 |
      is.na(date_of_bmt_1)                             ~ "good" # progression have to be strictly > hct
  )) %>%
  filter(prog_before_hct == "good") %>% 
  select(1:2) %>% 
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_hct_date = "progression_date")


# Metastasis
metastasis <- bind_rows(Metastasis_V12, MetastasisV4, MetastasisV4.1) %>% 
  mutate(have_metastasis = ifelse(have_metastasis == 3, "No Metastasis", "Metastasis")) %>% 
  mutate(metastasis_date = case_when(
    have_metastasis == "No Metastasis"      ~ NA_POSIXct_,
    have_metastasis == "Metastasis"         ~ metastasis_date
  )) %>% 
  arrange(avatar_id, metastasis_date) %>% 
  distinct(avatar_id, .keep_all = TRUE)


# Cleaning
rm(Demo_HRI, Demo_linkage, MM_history_V12, MM_historyV4, MM_historyV4.1,
   Vitals_V12, VitalsV4, VitalsV4.1, SCT_V12, SCTV4, SCTV4.1,
   Treatment_V12, TreatmentV4, TreatmentV4.1)

# Lab dates and biopsy to fill up last date of contact when not furnished ----
# LabsV1 <- gather(LabsV1, key = "event", value = "labs_last_date", 2:ncol(LabsV1)) %>% 
#   drop_na(labs_last_date)
# LabsV2 <- gather(LabsV2, key = "event", value = "labs_last_date", 2:ncol(LabsV2)) %>% 
#   drop_na(labs_last_date)
Labs_V12 <- gather(Labs_V12, key = "event", value = "labs_last_date", 2) %>% 
  drop_na(labs_last_date)
LabsV4 <- gather(LabsV4, key = "event", value = "labs_last_date", 2) %>% 
  drop_na(labs_last_date)
LabsV4.1 <- gather(LabsV4.1, key = "event", value = "labs_last_date", 2) %>% 
  drop_na(labs_last_date)
labs_dates <- bind_rows(Labs_V12, LabsV4, LabsV4.1)
rm(Labs_V12, LabsV4, LabsV4.1)

biopsy <- bind_rows(Biopsy_V12, BiopsyV4, BiopsyV4.1) %>% 
  drop_na(biopsy_date) %>% 
  gather(., key = "event", value = "labs_last_date", 2)
imaging <- bind_rows(Imaging_V12, ImagingV4, ImagingV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
metastasis2 <- bind_rows(Metastasis_V12, MetastasisV4, MetastasisV4.1) %>%
  drop_na() %>%
  gather(., key = "event", value = "labs_last_date", 2)
performance <- bind_rows(Performance_V12, PerformanceV4, PerformanceV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
staging <- bind_rows(# Staging %>% select("avatar_id", "date_staging_results"), 
                     Staging_V12 %>% select("avatar_id", "date_staging_results"), 
                     # StagingV2 %>% select("avatar_id", "date_staging_results"), 
                     StagingV4 %>% select("avatar_id", "date_staging_results"), 
                     StagingV4.1 %>% select("avatar_id", "date_staging_results")) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
tumormarker <- bind_rows(TumorMarker_V12, TumorMarkerV4, TumorMarkerV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
rm(Biopsy_V12, BiopsyV4, BiopsyV4.1,
   Imaging_V12, ImagingV4, ImagingV4.1,
   Metastasis_V12, MetastasisV4, MetastasisV4.1,
   Performance_V12, PerformanceV4, PerformanceV4.1,
   Staging_V12, StagingV4, StagingV4.1,
   TumorMarker_V12, TumorMarkerV4, TumorMarkerV4.1)

Last_labs_dates <- bind_rows(labs_dates, biopsy, imaging, metastasis2, performance, staging, tumormarker) %>% 
  filter(!str_detect(labs_last_date, "9999|2816|2077")) %>% # Remove mistakes and missing dates
  # remove if its <= to date_of_diagnosis (before MM diagnosis)
  # remove if its => to date_contact_lost 
  left_join(., MM_history %>% select(c("avatar_id", "Dx_date_closest_germline")), by = "avatar_id") %>% 
  # left_join(., Contact_lost %>% select(c("avatar_id", "date_contact_lost")), by = "avatar_id") %>% 
  left_join(., Vitals %>% select(c("avatar_id", "date_contact_lost", "date_last_follow_up")), by = "avatar_id") %>% 
  mutate(labs_before_diag = case_when(
    labs_last_date <= Dx_date_closest_germline             ~ "removed",
    labs_last_date >= date_contact_lost               ~ "removed",
    labs_last_date >= date_last_follow_up             ~ "removed"
  )) %>% 
  filter(is.na(labs_before_diag)) %>% 
  arrange(desc(labs_last_date)) %>% 
  distinct(avatar_id, .keep_all = TRUE)
rm(labs_dates, biopsy, imaging, metastasis2, performance, staging, tumormarker)

MMA <- MMA %>% 
  select(avatar_id, LAB_RESULT, LAB_UNIT, ORDER_DTM#ends_with("_DTM")
         ) %>% 
  filter(!is.na(LAB_RESULT)) %>% 
  # Create var closest MMA results to diagnosis
  inner_join(., MM_history %>% 
              select(avatar_id, date_of_MM_diagnosis),
            by = "avatar_id") %>% 
  mutate(int = abs(interval(start = ORDER_DTM, 
                        end = date_of_MM_diagnosis) /
           duration(n=1, units = "days"))) %>% 
  group_by(avatar_id) %>% 
  mutate(min = min(int)) %>% 
  ungroup() %>% 
  mutate(MMA_MMdx_results = case_when(
    min == int               ~ LAB_RESULT,
    TRUE                     ~ NA_character_
  )) %>% 
  # Create var closest MMA results to germline
  inner_join(., Germline %>% 
              select(avatar_id, collectiondt_germline),
            by = "avatar_id") %>% 
  mutate(int = abs(interval(start = ORDER_DTM, 
                            end = collectiondt_germline) /
                     duration(n=1, units = "days"))) %>% 
  group_by(avatar_id,collectiondt_germline) %>% 
  mutate(min = min(int)) %>% 
  mutate(MMA_germline_results = case_when(
    min == int               ~ LAB_RESULT,
    TRUE                     ~ NA_character_
  )) %>% 
  mutate(MMA_date = case_when(
    min == int               ~ ORDER_DTM,
    TRUE                     ~ NA_POSIXct_
  )) %>% 
  # Pick 1 value per patient
  fill(MMA_MMdx_results, MMA_germline_results, MMA_date,
       .direction = "updown") %>% 
  ungroup() %>% 
  select(avatar_id, MMA_MMdx_results, MMA_germline_results, 
         MMA_date, MMA_unit = LAB_UNIT) %>%
  distinct(avatar_id, MMA_germline_results, .keep_all = TRUE)


# Cleaning
rm(ClinicalCap_V12, ClinicalCap_V4, 
   uid, uid_A, uid_MM, uid_R, uid_S, uid_T, uid_V, uid_P, uid_P12,
   Alc_Smo_V12, Alc_SmoV4, Alc_SmoV4.1, 
   Radiation_V12, RadiationV4, RadiationV4.1,
   Progr_V12, Progression_V12, Progression_V4, Progression_V4.1,
   #Contact_lost
   # Alc_SmoV12_L_2, BiopsyV12_L_2, ImagingV12_L_2,
   # LabsV12_L_2, MetastasisV12_L_2, MM_historyV12_L_2, PerformanceV12_L_2, 
   # ProgressionV12_L_2, SCTV12_L_2, StagingV12_L_2, TreatmentV12_L_2, 
   # VitalsV12_L_2, TumorMarkerV12_L_2, RadiationV12_L_2, ProgrV12_L_2,
   Dx_date
   )


#######################################################################################  II  ## Plot---
# jpeg(paste0(path, "/barplot2.jpg"), width = 350, height = 350)
par(mar=c(3.5, 7.1, 4.1, 2.1)) # bottom left top right
par(cex.sub = .7)
barplot(
  height = cbind(
    "Desease History" = NROW(MM_history),
    "Vitals" = NROW(Vitals),
    "BMT" = NROW(SCT),
    "Treatment" = NROW(Treatment),
    "Radiation" = NROW(Radiation)
  ), horiz=TRUE, 
  las = 1, 
  main = "Nbr of unique patient ID recorded \nin each file tab",
  cex.main = 1,
  #xlim = c(0, 700),
  col = "#69b3a2",
  cex.axis = .8,
  cex.names = .8
)
# dev.off()




##################################################################################################  IV  ## Merge----
patients_removed_nonMM <- c("A000428", "A000456")
Global_data <- 
  # Do full join to keep extra patients we don't have germline for mow
  full_join(Germline %>%
              select(c("avatar_id", "mrn",
                       "moffittSampleId_germline", "SLID_germline",
                       "collectiondt_germline", "Disease_Status_germline", 
                       starts_with("SLID_tumor"), starts_with("collectiondt_tumor_"), 
                       starts_with("moffittSampleId_tumor"))),
            MM_history, by = "avatar_id") %>% 
  full_join(., Vitals, by = "avatar_id") %>% 
  full_join(., SCT, by = "avatar_id") %>% 
  full_join(., Treatment, by = "avatar_id") %>% 
  full_join(., Radiation, by = "avatar_id") %>% 
  full_join(., Progression, by= "avatar_id") %>% 
  full_join(., Progression_drugs, by= "avatar_id") %>% 
  # full_join(., Progression_treat, by= "avatar_id") %>% 
  full_join(., Progression_rad, by= "avatar_id") %>% 
  full_join(., Progression_hct, by= "avatar_id") %>% 
  full_join(., Last_labs_dates %>% select(c("avatar_id", "labs_last_date")), by = "avatar_id") %>% 
  full_join(., OS_data, by = "avatar_id") %>% 
  full_join(., Staging_ISS, by = c("avatar_id", "collectiondt_germline")) %>% 
  full_join(., Diagnosis_ISS, by = c("avatar_id")) %>% 
  full_join(., metastasis, by = "avatar_id") %>% 
  full_join(Demo_RedCap_V4ish %>% select(-TCC_ID), ., by = "avatar_id") %>% 
  full_join(., Cytogenetics, by = "avatar_id") %>% 
  full_join(., MMA, by = "avatar_id")
# write.csv(Global_data, paste0(path, "/Global_data.csv"))
Global_data <- 
  left_join(Global_data, CHIP_status, by = c("SLID_germline" = "patient_germline_id")) %>% 
  left_join(., CHIP_tageted_seq, by = c("SLID_germline" = "patient_id")) %>% 
  filter(!str_detect(avatar_id, paste0(patients_removed_nonMM, collapse = "|")))
write_rds(Global_data, file = "Global_data_pre.rds")
write_rds(Treatment1, "Treatment1.rds")
#--
# avatar_no_germline <- Global_data %>% filter(is.na(Global_data$Disease_Status_germline)) %>% 
#   select("avatar_id")
# # write.csv(avatar_no_germline, paste0(path, "/patient id with no germline.csv"))
rm(regimen_changed_id, patients_removed_nonMM)

# End Cleaning
