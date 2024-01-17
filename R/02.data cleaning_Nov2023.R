#######################################################################################  Somatic mutations
WES_jan2022 <- WES_jan2022 %>% 
  arrange(avatar_id, collectiondt_germline, collectiondt_tumor)

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
# rm(WES_jan2022)


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
    arrange(avatar_id, date_of_diagnosis) %>% 
    # Fix letter cases
    mutate(histology_details = str_to_title(histology_details)) %>% 
    filter(histology == 97651 | histology == 97323 | 
             str_detect(histology_details, "Multiple Myeloma")) %>% 
    mutate(disease_stage = case_when(
      hematological_malignancy_phase == 1         ~ "active",
      hematological_malignancy_phase == 2         ~ "smoldering",
      histology == 97651                          ~ "mgus"
    )) %>% 
    select(-c(hematological_malignancy_phase, histology)) %>%
    # If patient is active, stays active later until there is a change
    group_by(avatar_id) %>% 
    fill(disease_stage, .direction = "down") %>% 
    ungroup()
}
MM_history <- history_disease(MM_history)

# Change status for patients that Nancy checked
id <- paste0(status_change$id, collapse = "|")
rm(status_change)
# Add last Raghu date of MM diagnosis # NOT NEEDED ANYMORE
# Dx_date <- Diagnosis_ISS %>% select("avatar_id", "last_mrn", date_of_diagnosis = "MM_date_dx") %>% 
#   mutate(disease_stage = "active")

mm_history <- MM_history %>%
  drop_na("date_of_diagnosis") %>%
  group_by(avatar_id) %>% 
  fill(mrn, .direction = "downup") %>% 
  ungroup() %>% 
  distinct(avatar_id, mrn, date_of_diagnosis, disease_stage, 
           .keep_all = TRUE) %>% 
  arrange(avatar_id, date_of_diagnosis) %>% 
  # Change status for patients that Nancy checked
  mutate(disease_stage = case_when(
    str_detect(avatar_id, id) &
      disease_stage == "active"   ~ "wrongly classified", 
    TRUE                          ~ disease_stage
    )) %>% 
  select(-c(first_contact_date : date_of_diagnosis_flag)) %>% 
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

  # code smoldering diagnosis date
  mutate(sm_date_diagnosis = case_when(
    disease_stage == "smoldering"           ~ date_of_diagnosis,
    TRUE                                    ~ NA_POSIXct_
  )) %>% 
  arrange(avatar_id, sm_date_diagnosis) %>% 
  group_by(avatar_id) %>% 
  mutate(sm_date_diagnosis = first(sm_date_diagnosis)) %>% 
  ungroup() %>% 
  # code smoldering status
  mutate(smoldering_status = case_when(
    !is.na(date_of_MM_diagnosis) &
      !is.na(sm_date_diagnosis)        ~ "Progressed from Smoldering",
    !is.na(date_of_MM_diagnosis) &
      is.na(sm_date_diagnosis)         ~ "Never Smoldering",
    is.na(date_of_MM_diagnosis) &
      !is.na(sm_date_diagnosis)        ~ "Is Smoldering", 
    TRUE                               ~ NA_character_
  )) %>% 
  # code mgus date of dx
  mutate(mgus_date_diagnosis = case_when(
    disease_stage == "mgus"                 ~ date_of_diagnosis,
    TRUE                                    ~ NA_POSIXct_
  )) %>% 
  arrange(avatar_id, mgus_date_diagnosis) %>% 
  group_by(avatar_id) %>% 
  mutate(mgus_date_diagnosis = first(mgus_date_diagnosis)) %>% 
  ungroup() %>% 
  arrange(avatar_id, date_of_diagnosis)

MM_history <- dcast(setDT(mm_history), 
                    avatar_id+mrn+date_of_MM_diagnosis+is_patient_MM+
                      sm_date_diagnosis+smoldering_status+mgus_date_diagnosis ~ 
                      rowid(avatar_id), 
                    value.var = c("date_of_diagnosis", "disease_stage")) %>% 
  mutate(date_of_MMSMMGUSdiagnosis = 
           coalesce(date_of_MM_diagnosis, 
                    sm_date_diagnosis, 
                    mgus_date_diagnosis))

# Staging ISS----
ISS_df1 <- Staging %>% 
  filter(staging_type == "iss") %>% 
  select(avatar_id, date_staging_results, iss = staging_value)

iss_string_replace <- paste0(str_to_upper(c("international staging system : ",
                             "iss stage: |iss =stage |iss stage:",
                             "iss stage |iss :|iss :|iss:|iss |iss-")), 
                             collapse = "|")
ISS_df2 <- Staging %>% 
  filter(staging_type != "iss") %>% 
  mutate(staging_comments = str_to_upper(staging_comments)) %>% 
  filter(str_detect(staging_comments, "ISS")) %>% 
  mutate(staging_comments = 
           str_replace(staging_comments, 
                       iss_string_replace, "ISS: ")) %>% 
  mutate(iss = str_match(staging_comments, 
                         "(ISS: ) *([:digit:]|[I|L]*)")[,3]) %>% 
  select(avatar_id, date_staging_results, iss)

ISS_df <- 
  bind_rows(ISS_df1, ISS_df2) %>%
  # ISS_temp %>% 
  left_join(., MM_history %>% select(avatar_id, date_of_MM_diagnosis),
            by = "avatar_id") %>% 
  drop_na(iss) %>% 
  mutate(interval = (interval(start= date_staging_results, end= date_of_MM_diagnosis)/duration(n=1, unit="days"))) %>% 
  mutate(interval = abs(interval)) %>% 
  arrange(avatar_id, interval, date_staging_results) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(iss = str_remove(iss, "A|B"), 
         iss = case_when(
    str_detect(iss, "L")           ~ str_replace_all(iss, "L", "I"),
    iss == 1                       ~ "I",
    iss == 2                       ~ "II",
    iss == 3                       ~ "III",
    iss == ""                      ~ NA_character_,
    iss == 999                      ~ NA_character_,
    TRUE                           ~ iss
  )) %>% 
  select(avatar_id, date_staging_results, iss)

EHR_ISS <- EHR_ISS %>% 
  mutate(ISS_EHR = coalesce(ISS_EHR, ISS_calculated)) %>% 
  mutate(ISS_EHR = str_remove(ISS_EHR, "stage ")) %>% 
  filter(!is.na(ISS_EHR) | !is.na(B2) | !is.na(albumin))
    
Diagnosis_ISS <- Diagnosis_ISS %>% 
  full_join(., ISS_df %>% select(avatar_id, iss), by = "avatar_id") %>% 
  full_join(., EHR_ISS %>% select(avatar_id, ISS_EHR), by = "avatar_id") %>% 
  mutate(ISS_at_MMdx = coalesce(ISS_at_MMdx, iss, ISS_EHR)) %>% 
  select(avatar_id, ISS_at_MMdx)


rm(iss_string_replace, ISS_df1, ISS_df2,
   ISS_df, EHR_ISS, history_disease)

# Cytogenetics----
Cytogenetics <- Cytogenetics %>% 
  # fish_cytogenetics == 4 means not performed for that biopsy
  filter(cytogenetics != 4) %>% 
  mutate(across(-c(avatar_id:cytogenetics_date), ~ case_when(
                . == 1                                    ~ "Yes",
                cytogenetics == 1 & is.na(.)         ~ "No"
              ))) %>% 
  mutate(cytogenetics = case_when(
    cytogenetics == 1                                ~ "abnormal",
    cytogenetics == 2                                ~ "normal"
  )) %>% 
  left_join(., MM_history %>% select("avatar_id", "date_of_MM_diagnosis"),
            by = "avatar_id") %>% 
  mutate(interval = (interval(start= cytogenetics_date, end= date_of_MM_diagnosis)/duration(n=1, unit="days"))) %>% 
  mutate(interval = abs(interval)) %>%
  filter(interval < 60) %>% 
  arrange(avatar_id, interval) %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(avatar_id : addl_cytogenetics_to_report)

# Vitals ----
Vitals <- Vitals %>% 
  # If patient dies, only care about the date of death
  # This fixes when a patient has an identical date of death and date of f-up
  group_by(avatar_id) %>% 
  fill(date_death, cause_death, .direction = "updown") %>% 
  ungroup() %>% 
  # Create a single date to use for OS
  mutate(date_of_last_contact = coalesce(date_death, date_of_last_contact)) %>% 
  # # Check if multiple date of death for 1 patient
  # # If there is a 2 something is wrong
  # group_by(avatar_id) %>% 
  # mutate(too_many_date_of_death = row_number(date_death)) %>% 
  # ungroup() %>% 
  # Re-code vital status depending of dates in case of wrong coding
  mutate(vital_status = case_when(
    vital_status_old == 2 |
    !is.na(date_death)        ~ "Dead",
    vital_status_old == 3     ~ "Lost",
    is.na(date_death)         ~ "Alive"
  )) %>%
  select(-c(vital_status_old, vitals_review)) %>% 
  distinct() %>% 
  # Lost of contact are Alive but need to have a NA date
  # That we can fill later with last date available (from vitals, labs, etc)
  arrange(avatar_id, date_of_last_contact) %>% 
  group_by(avatar_id,vital_status) %>% 
  mutate(last_date_alive_before_lost = case_when(
    vital_status == "Alive"     ~ last(date_of_last_contact)
  )) %>% 
  group_by(avatar_id) %>% 
  fill(last_date_alive_before_lost, .direction = "down") %>% 
  ungroup() %>% 
  # Use for OS but will later fill NA with last date available
  mutate(date_of_last_contact = case_when( 
    vital_status == "Lost"      ~ NA_POSIXct_,
    TRUE                        ~ date_of_last_contact
  )) %>% 
  
  mutate(os_event = case_when(
    vital_status == "Dead"             ~ 1,
    vital_status == "Alive" |
      vital_status == "Lost"           ~ 0
  )) %>% 
  # Need to keep the latest record of follow up
  group_by(avatar_id) %>%
  mutate(record = row_number()) %>% 
  arrange(avatar_id, desc(record)) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(-record)

######### So if lost remove the date because we don't know what is the status 
######### BUT keep as alive with a last date available (from vitals or labs.....)
# Create a separate df to bind after cleaning to Vitals for tracking lost of contact
# Contact_lost <- Vitals %>%
#   filter(vital_status == 3) %>%
#   mutate(was_contact_lost = "Loss of contact") %>%
#   distinct() %>%
#   select(c(mrn, avatar_id, was_contact_lost, date_contact_lost = "date_last_follow_up"))

write.csv(Vitals,paste0(path, "/cleaned output files/Dec2023/Vitals_clean_Dec2023.csv"))

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

# HCT ----
sct <- SCT %>% 
  distinct() %>% 
  arrange(date_of_bmt)
SCT <- dcast(setDT(sct), avatar_id ~ rowid(avatar_id), 
             value.var = c("date_of_bmt", "type_of_bmt",
                           "bmt_cell_source"))
write.csv(SCT,paste0(path, "/cleaned output files/Dec2023/SCT_clean_Dec2023.csv"))

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
#   mutate(treatment_line1 = as.character(dense_rank(interaction(avatar_id, drug_start_date)))) %>% 
#   mutate(treatment_line = coalesce(treatment_line, treatment_line1)) %>% select(-treatment_line, -treatment_line1)
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
#   mutate(treatment_line = as.character(row_number())) %>% 
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
#   arrange(drug_start_date, treatment_line) %>% 
#   fill(treatment_line, .direction = "updown") %>% 
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

treatment <- Treatment %>% 
  select(c(avatar_id, mrn, regimen_num,
           drug_start_date, drug_stop_date,
           drug_name = "drug_name_", drug_name_other = "drug_name_other_",
           drug_comments, relapse_date, 
           treatment_site)) %>%
  # Fix drug name
  mutate_at(c("drug_name", "drug_name_other"), ~ str_to_lower(.)) %>%
  mutate(drug_name = str_replace(drug_name, "investigational", NA_character_)) %>% 
  mutate(drug_name = str_replace(drug_name, "//+", ", ")) %>% 
  unite(drug_name, c(drug_name,drug_name_other), sep = ", ", na.rm = TRUE, remove = TRUE) %>% 
  mutate(drug_name = case_when(
    drug_name == "cafilzomib"                                             ~ "carfilzomib",
    drug_name == "liposomal doxorubicin"                                  ~ "doxil",
    drug_name == "daratumuab"                                             ~ "daratumumab",
    str_detect(drug_name, "^dex")                                         ~ "dexamethasone",
    str_detect(drug_name, "^cyclo")                                       ~ "cyclophosphamide",
    str_detect(drug_name, "^lena|revlimid")                               ~ "lenalidomide",
    str_detect(drug_name, "^mel")                                         ~ "melphalan",
    str_detect(drug_name, "velcade")                                      ~ "bortezomib",
    drug_name == "vinicristine"                                           ~ "vincristine",
    (str_detect(drug_name, "kpt") &
       str_detect(drug_name, "300")) |
      str_detect(drug_name, "selinexor")                                  ~ "kpt300",
    str_detect(drug_name, "kpt") &
      str_detect(drug_name, "330")                                        ~ "kpt330",
    str_detect(drug_name, "kpt") &
      str_detect(drug_name, "8602")                                       ~ "kpt8602",
    str_detect(drug_name, "th") &
      str_detect(drug_name, "302")                                        ~ "th302",
    str_detect(drug_name, "bms") &
      str_detect(drug_name, "936564")                                     ~ "bms936564",
    str_detect(drug_name, "acy") &
      str_detect(drug_name, "241")                                        ~ "acy241",
    drug_name %in% c("anastrozole", "azacitidine", "carmustine", 
                      "cytarabine", "decitabine", "denosumab", 
                      "docetaxel", "hydrocortisone", "methotrexate",
                      "methylprednisolone", "pegfilgrastim", "prednisone", 
                      "rapamycin", "rituxan", "rituximab", 
                      "sorafenib tosylate", "tamoxifen citrate", 
                      "zoledronic acid", "prevnar", "ruxolitinib")         ~ "non-mm drugs",
    TRUE                                                                   ~ drug_name
  )) %>%
  filter(drug_name != "non-mm drugs") %>%
  arrange(avatar_id, regimen_num, drug_name, drug_start_date, drug_stop_date) %>%
  distinct()
write.csv(treatment,paste0(path, "/cleaned output files/Dec2023/treatment_long_clean_Dec2023.csv"))

flag_data <- treatment %>% 
  mutate(treatment_dates_check = case_when(
    drug_start_date > drug_stop_date        ~ "start after stop"
  )) %>% 
  filter(treatment_dates_check == "start after stop")
write_csv(flag_data, "treatment with dates of start after stop.csv")
# treatment1 <- dcast(setDT(treatment), mrn+avatar_id+treatment_line+drug_name ~ rowid(avatar_id), ## Old code
#                    value.var = c("drug_start_date", "drug_stop_date"))

# Make sure that the same drug in 1 line is counted once with the earliest start date and latest stop date
treatment1 <- treatment %>% 
  group_by(mrn, avatar_id, regimen_num, drug_name) %>%
  summarise_at(vars(drug_start_date, drug_stop_date), c(paste), collapse = ";") %>% 
  separate(drug_start_date, "drug_start_date", sep = ";", extra = "drop", fill = "right") %>% 
  separate(drug_stop_date, paste("drug_stop_date", 1:10, sep = ""), 
           sep = ";", remove = FALSE, extra = "warn", fill = "right") %>% ############### Fix when more info---------
  select("mrn", "avatar_id", "drug_name", "drug_start_date", drug_stop_date = "drug_stop_date1") %>% 
  group_by(avatar_id, ) %>% 
  mutate(line_start_date = min(drug_start_date)) %>% 
  mutate(line_stop_date = max(drug_stop_date)) %>% 
  arrange(avatar_id, drug_start_date, drug_stop_date) %>% 
  ungroup()
  

# Summarize by regimen/line
Treatment1 <- treatment1 %>% 
  group_by(mrn, avatar_id, regimen_num, line_start_date, line_stop_date) %>%
  summarise_at(vars(drug_name, drug_start_date, drug_stop_date), paste, collapse = "; ") %>%
  arrange(avatar_id, line_start_date, line_stop_date, drug_start_date) %>% 
  mutate(drug_count = sapply(strsplit(drug_name, ";"), length)) %>% 

# Treatment <- dcast(setDT(treatment1), mrn+avatar_id+regimen_num ~ rowid(avatar_id), ## Old code
#                   value.var = c("drug_name","drug_start_date", "drug_stop_date")) %>% 
#   unite(drug_name, starts_with("drug_name"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  
  # unite(drug_start_date, starts_with("drug_start_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  # separate(drug_start_date, paste("drug_start_date", 1:max(.$drug_count), sep = ""), sep = "; ", remove = FALSE,
  #          extra = "warn", fill = "right") %>% 
  # mutate(max = max(drug_start_date1:drug_start_date, na.rm = TRUE))
  # 
  # select(c(mrn, avatar_id, treatment_line, "drug_name", drug_start_date, ncol(.):drug_stop_date_1)) %>% 
  # unite(drug_stop_date, starts_with("drug_stop_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  # separate(drug_stop_date, "drug_stop_date", sep = "; ",
  #          extra = "warn", fill = "right") %>% 
  mutate(line_start_date = as.POSIXct(line_start_date, format = "%Y-%m-%d")) %>% 
  mutate(line_stop_date = as.POSIXct(line_stop_date, format = "%Y-%m-%d")) %>% 
  mutate(regimen_name = case_when(
    drug_count == 7 &
      str_detect(drug_name, "bort") &
      str_detect(drug_name, "thalidomide") &
      str_detect(drug_name, "cyclophosphamide") &
      str_detect(drug_name, "cisplatin") &
      str_detect(drug_name, "etoposide") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "doxo")                ~ "VDT-PACE",
    drug_count == 4 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "cyclophos") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "doxil")               ~ "ABCD",
    drug_count == 4 &
      str_detect(drug_name, "daratumumab") &
      str_detect(drug_name, "lena") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "carfilzomib")         ~ "Dara-KRd",
    drug_count == 4 &
      str_detect(drug_name, "vincristine") &
      str_detect(drug_name, "cyclophos") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "doxo")                ~ "C-VAD",
    drug_count == 4 &
      str_detect(drug_name, "thalidomide") &
      str_detect(drug_name, "vincristine") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "doxil")               ~ "T-VAD doxil",
    drug_count == 4 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "daratumumab") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "lena")                ~ "D-RVd",
    drug_count == 4 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "cyclophosphamide") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "doxo")                ~ "mCBAD",
    drug_count == 4 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "cyclophosphamide") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "lena")                ~ "VDCR",
    drug_count == 3 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "busulfan") &
      str_detect(drug_name, "melphalan")           ~ "BuMelVel",
    drug_count == 3 &
      str_detect(drug_name, "cyclophos") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "lena")                ~ "CRd",
    drug_count == 3 &
      str_detect(drug_name, "cyclophos") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "bort")                ~ "VCd",
    drug_count == 3 &
      str_detect(drug_name, "doxil") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "lena")                ~ "Dd-R",
    (drug_count == 3 &
       str_detect(drug_name, "daratu") &
       str_detect(drug_name, "lena") &
       str_detect(drug_name, "dex")) |
      str_detect(drug_name, "ddr")                 ~ "DRd",
    drug_count == 3 &
      str_detect(drug_name, "vincristine") &
      str_detect(drug_name, "doxil") &
      str_detect(drug_name, "dex")                 ~ "DVd",
    drug_count == 3 &
      str_detect(drug_name, "ixazomib") &
      str_detect(drug_name, "lena") &
      str_detect(drug_name, "dex")                 ~ "IRd",
    drug_count == 3 &
      str_detect(drug_name, "cyclophosphamide") &
      str_detect(drug_name, "carfilzomib") &
      str_detect(drug_name, "dex")                 ~ "KCd",
    drug_count == 3 &
      str_detect(drug_name, "lena") &
      str_detect(drug_name, "carfilzomib") &
      str_detect(drug_name, "dex")                 ~ "KRd",
    drug_count == 3 &
      str_detect(drug_name, "lena") &
      str_detect(drug_name, "oprozomib") &
      str_detect(drug_name, "dex")                 ~ "ORd",
    drug_count == 3 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "doxil") &
      str_detect(drug_name, "dex")                 ~ "PDd",
    drug_count == 3 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "doxo") &
      str_detect(drug_name, "dex")                 ~ "PAd",
    drug_count == 3 &
      str_detect(drug_name, "lena") &
      str_detect(drug_name, "doxo") &
      str_detect(drug_name, "dex")                 ~ "RAd",
    (drug_count == 3 &
       str_detect(drug_name, "bortezomib") &
       str_detect(drug_name, "lena") &
       str_detect(drug_name, "dex")) |
      str_detect(drug_name, "rvd")                 ~ "VRd", # bmt ctn 0702 (mcc 16529) rvd) # A022592
    drug_count == 3 &
      str_detect(drug_name, "vincristine") &
      str_detect(drug_name, "doxo") &
      str_detect(drug_name, "dex")                 ~ "VAd",
    drug_count == 3 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "melph")               ~ "VMd",
    drug_count == 3 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "dex") &
      str_detect(drug_name, "thalidomide")         ~ "VTd",
    drug_count == 2 &
      str_detect(drug_name, "carfilzomib") &
      str_detect(drug_name, "dex")                  ~ "Kd",
    drug_count == 2 &
      str_detect(drug_name, "thal") &
      str_detect(drug_name, "dex")                  ~ "Td",
    drug_count == 2 &
      str_detect(drug_name, "lena") &
      str_detect(drug_name, "dex")                 ~ "Rd",
    drug_count == 2 &
      str_detect(drug_name, "bortezomib") &
      str_detect(drug_name, "dex")                 ~ "Bor-Dex",
    drug_count == 1 &
      str_detect(drug_name, "lenalidomide")        ~ "Lenalidomide",
    drug_count == 1 &
      str_detect(drug_name, "dex")                 ~ "Dexamethasone",
    drug_count == 1 &
      str_detect(drug_name, "doxo")                ~ "Doxorubicin",
    drug_count == 1 &
      str_detect(drug_name, "doxil")               ~ "Doxil",
    drug_count == 1 &
      str_detect(drug_name, "bortezomib")          ~ "Bortezomib",
    drug_count == 1 &
      str_detect(drug_name, "melph")               ~ "Melphalan",
    drug_count == 1 &
      str_detect(drug_name, "carfilzomib")         ~ "Carfilzomib", # Carfilzomib is next gen PI
    drug_count == 1 &
      str_detect(drug_name, "cyclo")               ~ "Cyclophosphamide",
    drug_count == 1 &
      str_detect(drug_name, "thalidomide")         ~ "Thalidomide",
    drug_count == 1 &
      str_detect(drug_name, "vincristine")         ~ "Vincristine",
    TRUE                                            ~ drug_name
  )) %>% 
  mutate(regimen_name = str_replace_na(regimen_name, replacement = "No Drugs")) %>% 
  mutate(is_PI = case_when(
    str_detect(drug_name, "bortezomib|carfilzomib|oprozomib|ixazomib") |
      str_detect(regimen_name, "VRd|KRd") | 
      str_detect(regimen_name, "mln9708|mln 9708") ~ "PI",
    TRUE                                           ~ NA_character_
  )) %>% 
  mutate(received_IMIDs = case_when(
    str_detect(drug_name, "lidomide") |
      str_detect(drug_name, "ddr")                ~ "IMIDs",
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

# pivot wider
Treatment <- dcast(setDT(Treatment1), mrn+avatar_id ~ rowid(avatar_id), 
                   value.var = c("regimen_num", "line_start_date", "drug_name",
                                 "regimen_name", "regimen_category", "regimen_categoryVCD",
                                 "line_stop_date", "drug_start_date", "drug_stop_date"))

Treatment <- Treatment %>% 
  purrr::keep(~!all(is.na(.))) %>% 
  full_join(., IMIDS_maintenance, by = "avatar_id") %>%
  rename(first_regimen_name = regimen_name_1) %>% 
  mutate(first_regimen_name = ifelse((str_detect(avatar_id, paste0(regimen_changed_id$id, collapse = "|"))), "VRd", first_regimen_name))

write.csv(Treatment,paste0(path, "/cleaned output files/Dec2023/treatment_wide_clean_Dec2023.csv"))

rm(migration_patients, IMIDS_maintenance)

# Radiation ----
radiation <- Radiation %>% 
  distinct(avatar_id, rad_start_date, rad_stop_date, .keep_all = TRUE) %>% 
  arrange(avatar_id, rad_start_date)
# pivot wider
Radiation <- dcast(setDT(radiation), mrn+avatar_id ~ rowid(avatar_id), value.var = 
                     c("rad_start_date", "rad_stop_date", "radiation_line", "rad_dose"))
write.csv(Radiation,paste0(path, "/cleaned output files/Dec2023/Radiation_wide_clean_Dec2023.csv"))

# Progression----
Prog <- Prog %>% 
  filter(QC == "Yes") %>% 
  drop_na(progression_date) %>% 
  distinct() %>% 
  select(-QC)
# uid_P12 <- paste(unique(Progr_V12$avatar_id), collapse = '|')      ##################### See if we keep depending on Raghu's answer------
# Progression_V12 <- Progression_V12[(!grepl(uid_P12, Progression_V12$avatar_id)),] %>%  # Remove ID QC'd in Prog_V12
#   #Progression_V12 <- Progression_V12 %>% 
#   drop_na(progression_date) %>% 
#   distinct()

Progression <- 
  bind_rows(Prog, Progression) %>%
  distinct() %>% drop_na(progression_date) %>% 
  # Taking the dates of progression after the first date_of_MMSMMGUSdiagnosis => For OS
  # (either if are MM and progressed or if are MGUS/SM and progressed to MM)
  left_join(., MM_history %>% select(c("avatar_id", "date_of_MMSMMGUSdiagnosis")), by = "avatar_id") %>% 
  mutate(prog_after_diag = case_when(
    progression_date <= date_of_MMSMMGUSdiagnosis         ~ "removed", # often removed as they become MM
    progression_date > date_of_MMSMMGUSdiagnosis          ~ "good" 
  )) %>% 
  filter(prog_after_diag == "good") %>% 
  select(-c(date_of_MMSMMGUSdiagnosis, prog_after_diag)) %>% 
  # Taking only the dates of progression before date_death (it's a sanity check-like)
  left_join(., Vitals %>% select(c("avatar_id", "date_death")), by = "avatar_id") %>% 
  mutate(prog_before_death = case_when(
    progression_date > date_death                 ~ "removed", # 2 patient removed
    progression_date < date_death |
      is.na (date_death)                          ~ "good"
  )) 
write_csv(Progression, "progression date after death.csv")
Progression <- Progression %>%
  filter(prog_before_death == "good") %>% 
  select(-c(date_death, prog_before_death))

# Create different df for dates from Dx or drug, hct, rad (will not have the same clean up)
Progression_hct <- Progression_rad <- Progression_drugs <- Progression 


Progression <- Progression %>% 
  # Keep earliest progression_date => For OS
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_date_from_dx = "progression_date")
write.csv(Progression, paste0(path, "/cleaned output files/Dec2023/Progression_from_dx_clean_Dec2023.csv"))

Progression_drugs <- Progression_drugs %>% 
  # Remove progression date before drug and keep earliest progression_drug_date
  left_join(., Treatment %>% 
              select(c("avatar_id", "line_start_date_1")),
            by = "avatar_id") %>% 
  mutate(prog_before_drug = case_when(
    progression_date <= line_start_date_1               ~ "removed",
    progression_date > line_start_date_1 |
      is.na(line_start_date_1)                          ~ "good" 
  )) %>%
  mutate(did_patient_progressed_before_drugs = case_when(
    prog_before_drug == "removed"                       ~ "Yes"
  )) %>% 
  group_by(avatar_id) %>% 
  fill(did_patient_progressed_before_drugs, .direction = "updown") %>% 
  ungroup() %>% 
  filter(prog_before_drug == "good") %>% 
  select(-c(line_start_date_1, prog_before_drug)) %>% 
  arrange(avatar_id, progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_date_from_drug = "progression_date")
write.csv(Progression_drugs, paste0(path, "/cleaned output files/Dec2023/Progression clean used for survivals from drugs date_Dec2023.csv"))

Progression_rad <- Progression_rad %>% 
  # Remove progression < rad and keep earliest progression_rad_date
  left_join(., Radiation %>% select(c("avatar_id", "rad_start_date_1")), by = "avatar_id") %>% 
  mutate(prog_before_rad = case_when(
    progression_date <= rad_start_date_1                ~ "removed",
    progression_date > rad_start_date_1 |
      is.na(rad_start_date_1)                           ~ "good"
  )) %>%
  mutate(did_patient_progressed_before_rad = case_when(
    prog_before_rad == "removed"                        ~ "Yes"
  )) %>% 
  group_by(avatar_id) %>% 
  fill(did_patient_progressed_before_rad, .direction = "updown") %>% 
  ungroup() %>% 
  filter(prog_before_rad == "good") %>% 
  select(-c(rad_start_date_1, prog_before_rad)) %>% 
  arrange(avatar_id, progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_date_from_rad = "progression_date")

Progression_hct <- Progression_hct %>% 
  # Remove progression < hct and keep earliest progression_hct_date
  left_join(., SCT %>% 
              select(c("avatar_id", "date_of_bmt_1")), 
            by = "avatar_id") %>% 
  mutate(prog_before_hct = case_when(
    progression_date <= date_of_bmt_1                  ~ "removed",
    progression_date > date_of_bmt_1 |
      is.na(date_of_bmt_1)                             ~ "good"
  )) %>%
  mutate(did_patient_progressed_before_hct = case_when(
    prog_before_hct == "removed"                        ~ "Yes"
  )) %>% 
  group_by(avatar_id) %>% 
  fill(did_patient_progressed_before_hct, .direction = "updown") %>% 
  ungroup() %>% 
  filter(prog_before_hct == "good") %>% 
  select(-c(date_of_bmt_1, prog_before_hct)) %>% 
  arrange(avatar_id, progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_date_from_hct = "progression_date")


# Metastasis ----
metastasis <- Metastasis %>% 
  mutate(have_metastasis = ifelse(have_metastasis == 3, "No Metastasis", "Yes")) %>% 
  filter(have_metastasis == "Yes") %>% 
  arrange(avatar_id, metastasis_date)
# pivot wide
Metastasis <- dcast(setDT(metastasis), mrn+avatar_id+have_metastasis ~ rowid(avatar_id), 
                    value.var = c("metastasis_date", "initial_1_mets_origin", 
                                  "initial_1_mets_site"))

# Cleaning
# rm(Demo_HRI, Demo_linkage, MM_history_V12, MM_historyV4, MM_historyV4.1,
#    Vitals_V12, VitalsV4, VitalsV4.1, SCT_V12, SCTV4, SCTV4.1,
#    Treatment, TreatmentV4, TreatmentV4.1)

# Use lab dates, biopsy and more to fill up 
# last date of contact when not furnished ----

labs_dates <- Labs %>% 
  drop_na(lab_date) %>% 
  rename(last_date = lab_date)
biopsy <- Biopsy %>% 
  drop_na(biopsy_date) %>% 
  rename(last_date = biopsy_date)
imaging <- Imaging %>% 
  drop_na(imaging_date) %>% 
  rename(last_date = imaging_date)
metastasis2 <- metastasis %>%
  drop_na(metastasis_date) %>% 
  rename(last_date = metastasis_date)
performance <- Performance %>% 
  drop_na(date_perf_status_dx) %>% 
  rename(last_date = date_perf_status_dx)
staging <- Staging %>% 
  select(mrn, avatar_id, last_date = date_staging_results) %>% 
  drop_na(last_date)
tumormarker <- Tumor_marker %>% 
  drop_na(tumor_marker_date) %>% 
  rename(last_date = tumor_marker_date)

Last_labs_dates <- bind_rows(
  labs_dates, biopsy, imaging, metastasis2, 
  performance, staging, tumormarker,
  Vitals %>% select(avatar_id, mrn, last_date = last_date_alive_before_lost)) %>% 
  # filter(!str_detect(labs_last_date, "9999|2816|2077")) %>% # Remove mistakes and missing dates
  # remove if its <= to date_of_diagnosis (before MM diagnosis)
  # remove if its => to date_contact_lost 
  left_join(., MM_history %>% select(c("avatar_id", "date_of_MMSMMGUSdiagnosis")), by = "avatar_id") %>% 
  # left_join(., Contact_lost %>% select(c("avatar_id", "date_contact_lost")), by = "avatar_id") %>% 
  left_join(., Vitals %>% 
              select(c("avatar_id", "date_of_last_contact"#, 
                       # "date_last_follow_up"
                       )), 
            by = "avatar_id") %>% 
  mutate(labs_before_diag = case_when(
    last_date <= date_of_MMSMMGUSdiagnosis       ~ "removed",
    last_date >= date_of_last_contact            ~ "removed"#,
    # last_date >= date_last_follow_up             ~ "removed"
  )) %>% 
  filter(is.na(labs_before_diag)) %>% 
  arrange(desc(last_date)) %>% 
  distinct(avatar_id, .keep_all = TRUE)
rm(labs_dates, biopsy, imaging, metastasis2, performance, staging, tumormarker)

# MMA----
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
  inner_join(., WES_jan2022 %>% 
               select(avatar_id, collectiondt_germline) %>% 
               distinct(),
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
   Alc_Smo_V12, Comorbidities, Comorbidities.1, 
   Radiation_V12, RadiationV4, RadiationV4.1,
   Progr_V12, Progression_V12, Progression_V4, Progression_V4.1,
   #Contact_lost
   # Alc_SmoV12_L_2, BiopsyV12_L_2, ImagingV12_L_2,
   # LabsV12_L_2, MetastasisV12_L_2, MM_historyV12_L_2, PerformanceV12_L_2, 
   # ProgressionV12_L_2, SCTV12_L_2, StagingV12_L_2, TreatmentV12_L_2, 
   # VitalsV12_L_2, TumorMarkerV12_L_2, RadiationV12_L_2, ProgrV12_L_2,
   Dx_date
   )


# Pivot wider sequencing
Germline <- dcast(setDT(WES_jan2022), 
                  avatar_id+mrn ~ 
                    rowid(avatar_id),
                  value.var = c(
                    "SLID_germline",
                    "moffittSampleId_germline",
                    "collectiondt_germline",
                    "SLID_tumor",
                    "moffittSampleId_tumor",
                    "collectiondt_tumor",
                    "moffittSampleId",
                    "Disease_Status"
                  )
)

#######################################################################################  II  ## Plot---
# jpeg(paste0(path, "/barplot2.jpg"), width = 350, height = 350)
par(mar=c(3.5, 7.1, 4.1, 2.1)) # bottom left top right
par(cex.sub = .7)
barplot(
  height = cbind(
    "Disease History" = NROW(MM_history),
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


# Reload old germline data
Germline <- readRDS("/Users/colinccm/Documents/GitHub/CHIP-Avatar/germline_patient_data.rds")

Germline <- Germline %>% 
  select(avatar_id, SLID_germline, collectiondt_germline,
         moffittSampleId_germline, Disease_Status_germline)


##################################################################################################  IV  ## Merge----
patients_removed_nonMM <- paste0(patients_removed_nonMM$id, collapse = "|")
Global_data <- 
  # Do full join to keep extra patients we don't have germline for mow
  full_join(Germline,
            MM_history, by = "avatar_id") %>% 
  full_join(., Vitals, by = "avatar_id") %>% 
  full_join(., SCT, by = "avatar_id") %>% 
  full_join(., Treatment, by = "avatar_id") %>% 
  full_join(., Radiation, by = "avatar_id") %>% 
  full_join(., Progression, by= "avatar_id") %>% 
  full_join(., Progression_drugs, by= "avatar_id") %>% 
  full_join(., Progression_rad, by= "avatar_id") %>% 
  full_join(., Progression_hct, by= "avatar_id") %>% 
  full_join(., Last_labs_dates %>% select(c("avatar_id", "last_date")), by = "avatar_id") %>% 
  # full_join(., OS_data, by = "avatar_id") %>% 
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
  filter(!str_detect(avatar_id, patients_removed_nonMM))
write_rds(Global_data, file = "Global_data_pre_Dec2023.rds")
write_rds(Treatment1, "Treatment1_Dec2023.rds")
#--
# avatar_no_germline <- Global_data %>% filter(is.na(Global_data$Disease_Status_germline)) %>% 
#   select("avatar_id")
# # write.csv(avatar_no_germline, paste0(path, "/patient id with no germline.csv"))
rm(regimen_changed_id, patients_removed_nonMM)

# End Cleaning
