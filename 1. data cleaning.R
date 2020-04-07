# import library
library(tidyverse)
library(data.table)


##################################################################################################  I  ### Load data
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Cassin", "CHIP in Avatar")
#-----------------------------------------------------------------------------------------------------------------
Demo_RedCap_V4ish <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/extracted Avatar V124 data and dict/Avatar_Demographics_All_MM_OUT_03022020.xlsx")) %>%
  select(c("avatar_id","TCC_ID","Date_of_Birth", "Gender", "Ethnicity", "Race"))
#-----------------------------------------------------------------------------------------------------------------
Germ <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_Germl_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id","moffitt_sample_id","collectiondt", "WES_HUDSON_ALPHA",
           "Disease_Status")) %>% 
  `colnames<-`(c("avatar_id","moffitt_sample_id_germline","collectiondt_germline", "WES_HUDSON_ALPHA_germline",
                 "Disease_Status_germline"))

# We have 510 avatar-id which are unique
print(paste("We have", length(Germ$avatar_id) ,"subject-id with", 
            length(unique(Germ$avatar_id)) ,"unique id"))
#-----------------------------------------------------------------------------------------------------------------
WES <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", 
           "Disease_Status", "collectiondt", "WES_HUDSON_ALPHA")) %>% 
  `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", 
                 "Disease_Status_tumor", "collectiondt_tumor", "WES_HUDSON_ALPHA_tumor"))

# We have 510 avatar-id which are unique
print(paste("We have", length(WES$avatar_id) ,"subject-id in WES with", 
            length(unique(WES$avatar_id)) ,"unique id"))
#-----------------------------------------------------------------------------------------------------------------
Sequencing <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c(
    "SLID_germline", 
    "SLID_tumor" , "moffitt_sample_id_tumor", 
    "moffitt_sample_id_germline",
    "BaitSet", "ClinicalSpecimenLinkage_WES.Batch"))
print(paste("We have", length(Sequencing$subject) ,"subject-id in Sequencing with", 
            length(unique(Sequencing$subject)) ,"unique id"))
# Sequencing$moffitt_sample_id_tumor == Sequencing$moffitt_sample_id # yes so remove one var
# Sequencing$subject == Sequencing$avatar_id # yes so remove one var
#-----------------------------------------------------------------------------------------------------------------
Seq_WES_Raghu <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V044.xlsx")) %>% 
  select(c("subject", 
           "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline", 
           "SLID_tumor" , "moffitt_sample_id_tumor", "collectiondt_tumor", 
           "BaitSet")) %>% 
  rename(avatar_id = subject)
# Keep
# Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
#-----------------------------------------------------------------------------------------------------------------
ClinicalCap_V1 <-
  fs::path(
    "",
    "Volumes",
    "Gillis_Research",
    "Christelle Colin-Cassin",
    "CHIP in Avatar",
    "Raghu MM",
    "extracted Avatar V124 data and dict",
    "V1"
  )
#-----------------------------------------------------------------------------------------------------------------
Vitals <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_OUT_02072020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", "date_last_follow_up", "smoking_status","current_smoker","alcohol_use"))
#-----------------------------------------------------------------------------------------------------------------
MM_history <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_OUT_02072020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis", "disease_stage"))
# will need to modify date as 2000 then as.Date
#-----------------------------------------------------------------------------------------------------------------
Comorbidities <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_OUT_02072020.xlsx")),
                    sheet = "Comorbidities") #%>% 
#select(c("avatar_id","smoking_status", "alcohol_use"))
#-----------------------------------------------------------------------------------------------------------------
# Biopsy <-
#   readxl::read_xlsx(ClinicalCap_V1,
#                     sheet = "Biopsy") %>%
#   arrange(Biopsy$date_bonemarrow_biopsy_results) %>%
#   select(c("tcc_id" ,"number_of_bonemarrow_biopsies"))
# Biopsy <- Biopsy[order(Biopsy$date_bonemarrow_biopsy_results),]
# Biopsy <- Biopsy[,c("tcc_id" ,"number_of_bonemarrow_biopsies")]
#-----------------------------------------------------------------------------------------------------------------
Treatment <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_OUT_02072020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id","regimen_start_date", "regimen_end_date",
           "drug1_regimen", "drug2_regimen", "drug3_regimen", 
           "drug4_regimen", "drug5_regimen", "drug6_regimen", "drug7_regimen")) #%>% 
#   pivot_longer(c(4:10), names_to = "drug_regimen", values_to = "drug_name_",
#                values_drop_na = TRUE)
# 
# colnames(Treatment)[which(names(Treatment) == "regimen_start_date")] <- "drug_start_date" 
# colnames(Treatment)[which(names(Treatment) == "regimen_end_date")] <- "drug_stop_date"
# colnames(Treatment)[which(names(Treatment) == "drug_regimen")] <- "treatment_line_"
#-----------------------------------------------------------------------------------------------------------------
SCT <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_OUT_02072020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "prior_treatment", "number_of_bonemarrow_transplant","date_of_first_bmt", 
           "date_of_second_bmt", "date_of_third_bmt"))
#-----------------------------------------------------------------------------------------------------------------
RadiationV1 <- readxl::read_xlsx(paste0(ClinicalCap_V1, "/Radiation_Version1_Patients.xlsx")) %>%
  select(c("Avatar_ID", "Radiation Start Date", "Radiation End Date")) %>% 
  `colnames<-`(c("avatar_id", "rad_start_date", "rad_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
  ClinicalCap_V2 <-
    fs::path(
      "",
      "Volumes",
      "Gillis_Research",
      "Christelle Colin-Cassin",
      "CHIP in Avatar",
      "Raghu MM",
      "extracted Avatar V124 data and dict",
      "V2"
    )
  #-----------------------------------------------------------------------------------------------------------------
VitalsV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_OUT_02102020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death","smoking_status","alcohol_use", "bmi_at_dx_v2"))

#-----------------------------------------------------------------------------------------------------------------
MM_historyV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_OUT_02102020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id",  "date_of_diagnosis"))
#-----------------------------------------------------------------------------------------------------------------
TreatmentV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_OUT_02102020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "treatment_line_", "drug_start_date" , "drug_name_", "drug_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
SCTV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_OUT_02102020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
#-----------------------------------------------------------------------------------------------------------------
RadiationV2 <- readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_OUT_02102020.xlsx")),
                               sheet = "Radiation") %>%
    select(c("avatar_id", "rad_start_date_v2", "rad_stop_date_v2")) %>% 
    `colnames<-`(c("avatar_id", "rad_start_date", "rad_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
  ClinicalCap_V4 <-
    fs::path(
      "",
      "Volumes",
      "Gillis_Research",
      "Christelle Colin-Cassin",
      "CHIP in Avatar",
      "Raghu MM",
      "extracted Avatar V124 data and dict",
      "V4"
    )
  #-----------------------------------------------------------------------------------------------------------------
VitalsV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_02212020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", "date_of_last_contact")) %>% 
  `colnames<-`(c("avatar_id","vital_status","date_death", "date_last_follow_up"))
#-----------------------------------------------------------------------------------------------------------------
MM_historyV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_02212020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis"))
# will need to modify date as 2000 then as.Date
#-----------------------------------------------------------------------------------------------------------------
  # ComorbiditiesV4 <-
  #   readxl::read_xlsx((paste0(ClinicalCap_V4,"/Avatar_MM_Clinical_Data_V4_OUT_02212020.xlsx")),
  #                     sheet = "Comorbidities") %>%
  #   select(c("avatar_id", ))
  Alc_SmoV4 <-
    readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_02212020.xlsx")),
                      sheet = "Comorbidities") %>%
    select(c("avatar_id","smoking_status", "alcohol_use"))
#-----------------------------------------------------------------------------------------------------------------
# BiopsyV4 <-
#   readxl::read_xlsx(ClinicalCap_V4,
#                     sheet = "Biopsy") %>%
#   arrange(Biopsy$date_bonemarrow_biopsy_results) %>%
#   select(c("tcc_id" ,"number_of_bonemarrow_biopsies"))
# Biopsy <- Biopsy[order(Biopsy$date_bonemarrow_biopsy_results),]
# Biopsy <- Biopsy[,c("tcc_id" ,"number_of_bonemarrow_biopsies")]
#-----------------------------------------------------------------------------------------------------------------
TreatmentV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_02212020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "treatment_line_", "drug_start_date", "drug_name_", "drug_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
SCTV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_02212020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_bmt")) %>% 
  `colnames<-`(c("avatar_id", "date_of_first_bmt"))
#-----------------------------------------------------------------------------------------------------------------
RadiationV4 <- 
    readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_02212020.xlsx")),
                                 sheet = "Radiation") %>%
    select(c("avatar_id", "rad_start_date", "rad_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
par(mar=c(4.1, 6.1, 2.1, 2.1)) # bottom left top right
  barplot(
    height = cbind(
      "Clinical Data" = c(NROW(MM_history), NROW(MM_historyV2), NROW(MM_historyV4)),
      "Vitals" = c(NROW(Vitals), NROW(VitalsV2), NROW(VitalsV4)),
      "BMT" = c(NROW(SCT), NROW(SCTV2), NROW(SCTV4)),
      "Radiation" = c(NROW(RadiationV1), NROW(RadiationV2), NROW(RadiationV4)),
      "Treatment" = c(NROW(Treatment), NROW(TreatmentV2), NROW(TreatmentV4)),
      "Qc'd Treatment" = c(0, 0, 0)
    ),horiz=TRUE, 
    las = 1,
    main = "Total records per version",
    sub = "A single patient can present multiple record ",
    xlab = "Number records",
    beside = FALSE,
    # width = 1,
    # ylim = c(0, 3000),
    col = c("purple", "orange", "yellow"),
    #legend.text = c("version1", "version2", "version4"),
    #args.legend = list(x = "bottomright"),
    cex.axis = .8,
    cex.names = .8
  )
  legend("bottomright", legend = c("version1", "version2", "version4"),
         col = c("purple", "orange", "yellow"),
         bty = "n", pch=20 , pt.cex = 2, cex = 0.8, inset = c(0.05, 0.05))
##################################################################################################  II  ## Bind ### Align duplicated ID
mm_history <- bind_rows(MM_history, MM_historyV2, MM_historyV4, .id = "versionMM") %>%
  arrange(date_of_diagnosis)
MM_history <- dcast(setDT(mm_history), avatar_id ~ rowid(avatar_id), value.var = c("date_of_diagnosis", "disease_stage", "versionMM")) %>% 
  select(c("avatar_id", "date_of_diagnosis_1", "disease_stage_1", "date_of_diagnosis_2", "disease_stage_2", "date_of_diagnosis_3", "disease_stage_3",
           "date_of_diagnosis_4", "disease_stage_4", "versionMM_1", "versionMM_2", "versionMM_3", "versionMM_4"))
# write.csv(MM_history,paste0(path, "/MM_history simplify.csv"))
#-------------------------------------
vitals <- bind_rows(Vitals, VitalsV2, VitalsV4, Alc_SmoV4, .id = "versionVit")
Vitals <- dcast(setDT(vitals), avatar_id ~ rowid(avatar_id), 
                value.var = c("vital_status", "date_death", 
                              "date_last_follow_up", "smoking_status", 
                              "current_smoker", "alcohol_use", 
                              "bmi_at_dx_v2")) %>%
  # Have multiple record ("abstraction") per patient so date_death can be recorded multiple times in col 1 and 2
  # Sometimes 1st "abstraction" record date_last_follow-up then second "abstraction" will record death (present in col 2)
  # Use coalesce so when is NA in date_death_1 will take the value in date_death_2 if present
  mutate(date_death = coalesce(date_death_1, date_death_2)) %>% 
  # Need to take the last date_follow_up recorded so in 2 then 
  # use coalesce to fill up with date_last_follow_up_1 when date_last_follow_up_2 is NA
  mutate(date_last_follow_up = coalesce(date_last_follow_up_2, date_last_follow_up_1)) %>%
  # Create a last_date_available var for ourself (help to arrange by last_date_available)
  mutate(last_date_available = coalesce(date_death_1, date_last_follow_up_1)) %>% 
  # Create my own vital_satus var because found record with 
  # 1st "abstraction" give date_death so vital = dead
  # 2nd "abstraction" doesn't give date (probably because already recorded) so vital = alive
  mutate(vital_status = case_when(
    !is.na(date_death_1) ~ "Dead",
    !is.na(date_last_follow_up_1) ~ "Alive"
  )) %>% 
  # For BMI, we may need to keep both if want to see evolution but for now keep the earliest (closest to diagnisis)
  # then fill-up with second column when the first is NA using coalesce
  mutate(bmi_at_dx_v2 = coalesce(bmi_at_dx_v2_1, bmi_at_dx_v2_2)) %>% 
  # Have patients who had "abstraction" 3 times and for who the alcohol_use and smoking_status was recorded only on the third
  # Take third record, fill it up by the second when NA then the first when NA
  # That is to get the lastest info. We may switch it if we want the closest to diag.
  mutate(alcohol_use = coalesce(alcohol_use_3, alcohol_use_2, alcohol_use_1)) %>% 
  mutate(alcohol_use = case_when(
    alcohol_use %in% c(0,3) ~ "never",
    alcohol_use == 2 ~ "former",
    alcohol_use == 1 ~ "current",
    TRUE ~ NA_character_
  )) %>% 
  # Smoking V1 have 2 var current_smoker_1 (1-2), smoking_status_1 ()
  # Fill-up current_smoker_1 by smoking_status_1
  # That is to get the lastest info. We may switch it if we want the closest to diag.
  mutate(smoking_status = coalesce(smoking_status_3, smoking_status_2, current_smoker_1, smoking_status_1)) %>% 
  mutate(smoking_status = case_when(
    smoking_status %in% c(0,3) ~ "never",
    smoking_status == 2 ~ "former",
    smoking_status == 1 ~ "current",
    TRUE ~ NA_character_
  )) %>% 
  select(c("avatar_id","vital_status", "date_death", "date_last_follow_up", "last_date_available",
           "bmi_at_dx_v2", "alcohol_use", "smoking_status"))

# Note for smoking
# 1 patient said 3 in V2 and 11 in V1
# 1 patient said 3 in V2 and 12 in V1

# write.csv(Vitals,paste0(path, "/Vitals simplify.csv"))
#-------------------------------------
sct <- bind_rows(SCT, SCTV2, SCTV4, .id = "versionSCT") %>% 
  arrange(date_of_third_bmt) %>% 
  arrange(date_of_second_bmt) %>% 
  arrange(date_of_first_bmt)
SCT <- dcast(setDT(sct), avatar_id ~ rowid(avatar_id), value.var = c("prior_treatment", "number_of_bonemarrow_transplant",
                                                                     "date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
# write.csv(SCT,paste0(path, "/SCT simplify.csv"))
# A000302	1	NA	2	NA	2009-06-15	NA	2009-02-26
#------------------------------------
treatment <- bind_rows(Treatment, TreatmentV2, TreatmentV4, .id = "versionTreat") %>% 
  arrange(drug_start_date)
Treatment <- dcast(setDT(treatment), avatar_id ~ rowid(avatar_id), 
                   value.var = c("drug_start_date", "drug_stop_date", "drug_name_"))
# write.csv(Treatment,paste0(path, "/Treatment simplify.csv"))
#------------------------------------
radiation <- bind_rows(RadiationV2, RadiationV2, RadiationV4, .id = "versionRad") %>% 
  arrange(rad_start_date)
Radiation <- dcast(setDT(radiation), avatar_id ~ rowid(avatar_id), value.var = 
                     c("rad_start_date", "rad_stop_date"))
#------------------------------------
# Cleaning
rm(ClinicalCap_V1, ClinicalCap_V2, ClinicalCap_V4, MM_historyV2, MM_historyV4, VitalsV2, VitalsV4, SCTV2, SCTV4, TreatmentV2, TreatmentV4,
   Comorbidities, Alc_SmoV4, RadiationV2, RadiationV4)
# Plot
par(mar=c(4.1, 6.1, 4.1, 2.1)) # bottom left top right
barplot(
  height = cbind(
    "Desease History" = NROW(MM_history),
    "Vitals" = NROW(Vitals),
    "BMT" = NROW(SCT),
    "Treatment" = NROW(Treatment),
    "Radiation" = NROW(Radiation)
  ), horiz=TRUE, 
  las = 1, 
  main = "Nbr of record in each file tab",
  xlim = c(0, 700),
  col = "#69b3a2",
  cex.axis = .8,
  cex.names = .8
  )

##################################################################################################  III  # Merge WES and Sequencing
# Are moffitt_sample_id are equal in WES and Sequencing ?
# Sequencing <- Sequencing[order(Sequencing$moffitt_sample_id),]
# WES <- WES[order(WES$moffitt_sample_id),]
# Sequencing$moffitt_sample_id == WES$moffitt_sample_id # =>>>>>>> YES
WES_seq <-
  merge.data.frame(
    WES,
    Sequencing,
    by.x = "moffitt_sample_id_tumor",
    by.y = "moffitt_sample_id_tumor",
    all.x = TRUE,
    all.y = TRUE
  )
# rm(Sequencing)
#Align duplicate on same row (per date)-------------------------------------------
# Check for duplicate
colnames(WES_seq)

# duplicated(WES_seq$moffitt_sample_id_tumor) # No duplicate
# duplicated(WES_seq$avatar_id) # has duplicate

# Really important to order by dates otherwise cannot find the duplicated lines
WES_seq <- WES_seq[order(WES_seq$collectiondt_tumor), ]
WES_seq <-
  dcast(setDT(WES_seq), avatar_id ~ rowid(avatar_id),
    value.var = c(
      "SLID_tumor",
      "moffitt_sample_id_tumor",
      "Disease_Status_tumor",
      "collectiondt_tumor",
      "SLID_germline",
      "moffitt_sample_id_germline",
      "BaitSet",
      "ClinicalSpecimenLinkage_WES.Batch"
    )
  )


# WES_seq <-WES_seq[, c("avatar_id", "Disease_Status_tumor_1", "collectiondt_tumor_1", "SLID_germline_1", "SLID_tumor_1", "moffitt_sample_id_tumor_1",
#               "moffitt_sample_id_germline_1", "BaitSet_1", "ClinicalSpecimenLinkage_WES.Batch_1",
#               "Disease_Status_tumor_2", "collectiondt_tumor_2", "SLID_germline_2", "SLID_tumor_2", "moffitt_sample_id_tumor_2", 
#               "moffitt_sample_id_germline_2", "BaitSet_2", "ClinicalSpecimenLinkage_WES.Batch_2", 
#               "Disease_Status_tumor_3", "collectiondt_tumor_3", "SLID_germline_3", "SLID_tumor_3", "moffitt_sample_id_tumor_3",
#               "moffitt_sample_id_germline_3", "BaitSet_3", "ClinicalSpecimenLinkage_WES.Batch_3",
#               "Disease_Status_tumor_4", "collectiondt_tumor_4", "SLID_germline_4", "SLID_tumor_4", "moffitt_sample_id_tumor_4",
#               "moffitt_sample_id_germline_4", "BaitSet_4", "ClinicalSpecimenLinkage_WES.Batch_4",
#               "Disease_Status_tumor_5", "collectiondt_tumor_5", "SLID_germline_5", "SLID_tumor_5", "moffitt_sample_id_tumor_5",
#               "moffitt_sample_id_germline_5","BaitSet_5", "ClinicalSpecimenLinkage_WES.Batch_5",
#               "Disease_Status_tumor_6", "collectiondt_tumor_6", "SLID_germline_6", "SLID_tumor_6", "moffitt_sample_id_tumor_6",
#               "moffitt_sample_id_germline_6", "BaitSet_6", "ClinicalSpecimenLinkage_WES.Batch_6")]
WES_seq  <- WES_seq[order(WES_seq$collectiondt_tumor_1), ] %>%
  arrange(collectiondt_tumor_2) %>%
  arrange(collectiondt_tumor_3) %>%
  arrange(collectiondt_tumor_4) %>%
  arrange(collectiondt_tumor_5) %>%
  arrange(collectiondt_tumor_6)
# write.csv(WES_seq,paste0(path, "/WES_seq germline tumor.csv"))

colnames(Germ)

# Merge with Organized_WES
Combined_data_MM <- merge.data.frame(WES_seq, Germ, 
                                     by.x = "avatar_id", by.y = "avatar_id", 
                                     all.x = TRUE, all.y = TRUE)
colnames(Combined_data_MM)
# write.csv(Combined_data_MM, paste0(path, "/Combined data and dates MM.csv"))
# I checked the ID they are all the same no missing nor added
# Really important to order by dates otherwise cannot find the duplicated lines
Seq_WES_Raghu <- Seq_WES_Raghu[order(Seq_WES_Raghu$collectiondt_tumor), ]
Seq_WES_Raghu <-
  dcast(setDT(Seq_WES_Raghu), avatar_id ~ rowid(avatar_id),
        value.var = c(
          "SLID_tumor",
          "moffitt_sample_id_tumor",
          "collectiondt_tumor",
          "SLID_germline",
          "moffitt_sample_id_germline",
          "BaitSet"
        )
  )


########### Binds
Sequencing <- bind_rows(Combined_data_MM, Seq_WES_Raghu)
Sequencing <- Sequencing %>% distinct(avatar_id, moffitt_sample_id_tumor_1, collectiondt_tumor_1, 
                             SLID_germline_1 , .keep_all = TRUE)
colnames(Sequencing)

##################################################################################################  IV  ## Merge
b <- merge.data.frame(Sequencing[, c("avatar_id", "collectiondt_germline", "Disease_Status_germline", 
                                           "collectiondt_tumor_1", "Disease_Status_tumor_1")],
                      MM_history, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = FALSE, suffixes = c(".x",".y"))

c <- merge.data.frame(b, Vitals, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = FALSE, suffixes = c(".x",".y"))

d <- merge.data.frame(c, SCT, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = FALSE, suffixes = c(".x",".y"))

e <- merge.data.frame(d, Treatment, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = FALSE, suffixes = c(".x",".y"))

f <- merge.data.frame(e, Radiation,by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = FALSE, suffixes = c(".x",".y"))

Global_data <- merge.data.frame(Demo_RedCap_V4ish, f, by.x = "avatar_id", by.y = "avatar_id", all.x = FALSE, all.y = TRUE)
# write.csv(Global_data, paste0(path, "/Global_data.csv"))
rm(b,c,d,e,f)

# tempory dataframe for the time to plot simply
f <- Global_data[,c("avatar_id", "TCC_ID", "Date_of_Birth", "date_of_diagnosis_1","disease_stage_1",
                    "number_of_bonemarrow_transplant_1", "number_of_bonemarrow_transplant_2","date_of_first_bmt_1", "date_of_second_bmt_1", "date_of_third_bmt_1", 
                    
                    "collectiondt_germline", "Disease_Status_germline", "collectiondt_tumor_1", "Disease_Status_tumor_1",
                    
                    "vital_status", "date_death", "date_last_follow_up", "last_date_available", 
                    
                    "prior_treatment_1", "prior_treatment_2",
                    "drug_start_date_1",
                    
                    "rad_start_date_1", "rad_start_date_2", "rad_stop_date_1", "rad_stop_date_2",                  
                    
                    "smoking_status", "alcohol_use",
                    
                    "bmi_at_dx_v2", "Gender", "Ethnicity", "Race", "versionMM_1")]

