# import library
library(tidyverse)
library(data.table)
library(VennDiagram)
library(viridis)
library(lubridate)
library(gtsummary)

#######################################################################################  I  ### Load data
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")
#-----------------------------------------------------------------------------------------------------------------
Demo_RedCap_V4ish <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/extracted Avatar V124 data and dict/Avatar_Demographics_All_MM_modif_04292020.xlsx")) %>%
  select(c("avatar_id","TCC_ID","Date_of_Birth", "Gender", "Ethnicity", "Race"))
#-----------------------------------------------------------------------------------------------------------------
Germ <- 
  readxl::read_xlsx(paste0(path, 
                           "/Raghu MM/Moffitt_Germl_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "collectiondt", "WES_HUDSON_ALPHA",
           "Disease_Status")) %>% 
  `colnames<-`(c("avatar_id", "collectiondt_germline", "WES_HUDSON_ALPHA_germline",
                 "Disease_Status_germline"))
Germ2 <-
  readxl::read_xlsx(paste0(path,
                           "/Raghu MM/Moffitt_Germl_Disease_Classification_2patient_from_2nd_sequencingfile.xlsx")) %>%
  select(-moffitt_sample_id) %>% 
  `colnames<-`(c("avatar_id", "collectiondt_germline", "WES_HUDSON_ALPHA_germline", "Disease_Status_germline"))
# We have 510 + 2 avatar-id which are unique
print(paste("We have", length(Germ$avatar_id) ,"subject-id with", 
            length(unique(Germ$avatar_id)) ,"unique id in Germ"))
print(paste("We have", length(Germ2$avatar_id) ,"subject-id with", 
            length(unique(Germ2$avatar_id)) ,"unique id in Germ2"))
Germ3 <-
  readxl::read_xlsx(paste0(path,
                           "/Raghu MM/Germline_MM_Disease_Status_05052020_OUT .xlsx")) %>% 
  select(c("Avatar_id", "collectiondt", "WES_HUDSON_ALPHA", "Disease_Status")) %>% 
  `colnames<-`(c(c("avatar_id", "collectiondt_germline", "WES_HUDSON_ALPHA_germline", "Disease_Status_germline")))
#-----------------------------------------------------------------------------------------------------------------
WES <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", 
           "Disease_Status", "collectiondt", "WES_HUDSON_ALPHA")) %>% 
  `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", 
                 "Disease_Status_tumor", "collectiondt_tumor", "WES_HUDSON_ALPHA_tumor"))
# We have 510 avatar-id which are unique
print(paste("We have", length(WES$avatar_id) ,"subject-id in WES with", 
            length(unique(WES$avatar_id)) ,"unique id in WES"))
#-----------------------------------------------------------------------------------------------------------------
Sequencing <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c(
    "SLID_germline", 
    "SLID_tumor" , "moffitt_sample_id_tumor", 
    "moffitt_sample_id_germline",
    "BaitSet", "ClinicalSpecimenLinkage_WES.Batch")) # , "ClinicalSpecimenLinkage_HistologyBehavior"
print(paste("We have", length(Sequencing$SLID_tumor) ,"samples in Sequencing with", 
            length(unique(Sequencing$SLID_germline)) ,"unique id"))
# Sequencing$moffitt_sample_id_tumor == Sequencing$moffitt_sample_id # yes so remove one var
# Sequencing$subject == Sequencing$avatar_id # yes so remove one var
#-----------------------------------------------------------------------------------------------------------------
Seq_WES_Raghu <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V044.xlsx")) %>% 
  select(c(avatar_id = "subject", 
           "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline", 
           "SLID_tumor" , "moffitt_sample_id_tumor", "collectiondt_tumor", 
           "BaitSet"))
print(paste("We have", length(Seq_WES_Raghu$SLID_tumor) ,"samples in Seq_WES_Raghu with", 
            length(unique(Seq_WES_Raghu$SLID_germline)) ,"unique id"))
# Keep
# Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
#-----------------------------------------------------------------------------------------------------------------
Sequencing2 <- # warning message due to a TRUE added in a num var by Raghu (he copy paste an extra patient)
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V0441.xlsx")) %>% 
  select(c(avatar_id = "subject", 
           "SLID_germline", moffitt_sample_id_germline = "moffittSampleId_germline", 
           "collectiondt_germline", 
           "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor", 
           BaitSet = "baitSet", "clinicalSpecimenLinkageDiseaseTy"))
Sequencing2 <- dcast(setDT(Sequencing2), avatar_id+SLID_germline+moffitt_sample_id_germline ~ rowid(avatar_id),
                     value.var = c(
                       "SLID_tumor",
                       "moffitt_sample_id_tumor",
                       "BaitSet", "clinicalSpecimenLinkageDiseaseTy")) 
#-----------------------------------------------------------------------------------------------------------------
ClinicalCap_V1 <-
  fs::path(
    "",
    "Volumes",
    "Gillis_Research",
    "Christelle Colin-Leitzinger",
    "CHIP in Avatar",
    "Raghu MM",
    "extracted Avatar V124 data and dict",
    "V1"
  )
#-----------------------------------------------------------------------------------------------------------------
Vitals <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", "date_last_follow_up", "smoking_status","current_smoker","alcohol_use"))
#-----------------------------------------------------------------------------------------------------------------
MM_history <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis", "disease_stage"))
#-----------------------------------------------------------------------------------------------------------------
# Comorbidities <-
#   readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
#                     sheet = "Comorbidities") #%>% 
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
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", drug_start_date = "regimen_start_date", drug_stop_date =  "regimen_end_date",
           "drug1_regimen", "drug2_regimen", "drug3_regimen", 
           "drug4_regimen", "drug5_regimen", "drug6_regimen", "drug7_regimen")) %>% 
  unite(drug_name_, drug1_regimen:drug7_regimen, sep = "; ", na.rm = TRUE, remove = TRUE)

Qcd_Treatment <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "QC'd Treatment") %>%
  select(c("avatar_id","regimen_start_date", "regimen_end_date",
           "treatment")) %>%
  `colnames<-`(c("avatar_id","drug_start_date", "drug_stop_date", "drug_name_"))
#-----------------------------------------------------------------------------------------------------------------
SCT <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id","date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
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
      "Christelle Colin-Leitzinger",
      "CHIP in Avatar",
      "Raghu MM",
      "extracted Avatar V124 data and dict",
      "V2"
    )
  #-----------------------------------------------------------------------------------------------------------------
VitalsV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death","smoking_status","alcohol_use", "bmi_at_dx_v2"))
#-----------------------------------------------------------------------------------------------------------------
MM_historyV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id",  "date_of_diagnosis"))
#-----------------------------------------------------------------------------------------------------------------
TreatmentV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "drug_start_date" , "drug_name_", "drug_stop_date",
           "drug_name_other")) %>%  # didn't take "treatment_line_"
  unite(drug_name_, c(drug_name_,drug_name_other), sep = "; ", na.rm = TRUE, remove = FALSE)
Qcd_TreatmentV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "QC'd Treatment") %>%
  select(c("avatar_id", "drug_start_date" , "drug_name_", "drug_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
SCTV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
#-----------------------------------------------------------------------------------------------------------------
RadiationV2 <- readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                               sheet = "Radiation") %>%
    select(c("avatar_id", "rad_start_date_v2", "rad_stop_date_v2")) %>% 
    `colnames<-`(c("avatar_id", "rad_start_date", "rad_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
  ClinicalCap_V4 <-
    fs::path(
      "",
      "Volumes",
      "Gillis_Research",
      "Christelle Colin-Leitzinger",
      "CHIP in Avatar",
      "Raghu MM",
      "extracted Avatar V124 data and dict",
      "V4"
    )
#-----------------------------------------------------------------------------------------------------------------
VitalsV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", date_last_follow_up = "date_of_last_contact"))
#-----------------------------------------------------------------------------------------------------------------
MM_historyV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis"))
#-----------------------------------------------------------------------------------------------------------------
  # ComorbiditiesV4 <-
  #   readxl::read_xlsx((paste0(ClinicalCap_V4,"/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
  #                     sheet = "Comorbidities") %>%
  #   select(c("avatar_id", ))
Alc_SmoV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
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
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other")) %>%  # didn't take "treatment_line_"
  unite(drug_name_, c(drug_name_,drug_name_other), sep = "; ", na.rm = TRUE, remove = FALSE)
#-----------------------------------------------------------------------------------------------------------------
SCTV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_bmt")) %>%  # can be different than first if duplicated from v1 or v2
  drop_na("date_of_bmt")
SCTV4 <- dcast(setDT(SCTV4), avatar_id ~ rowid(avatar_id), value.var = c("date_of_bmt")) %>% 
  rename("date_of_first_bmt" = "1", "date_of_second_bmt" = "2")
#-----------------------------------------------------------------------------------------------------------------
RadiationV4 <- 
    readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                                 sheet = "Radiation") %>%
    select(c("avatar_id", "rad_start_date", "rad_stop_date"))
#-----------------------------------------------------------------------------------------------------------------
jpeg(paste0(path, "/barplot1.jpg"), width = 350, height = 350)
par(mar=c(5, 6.1, 2.1, 3.1)) # bottom left top right
par(cex.sub = .7)
barplot(
  height = cbind(
      "Clinical Data" = c(NROW(MM_history), NROW(MM_historyV2), NROW(MM_historyV4)),
      "Vitals" = c(NROW(Vitals), NROW(VitalsV2), NROW(VitalsV4)),
      "BMT" = c(NROW(SCT), NROW(SCTV2), NROW(SCTV4)),
      "Radiation" = c(NROW(RadiationV1), NROW(RadiationV2), NROW(RadiationV4)),
      "Treatment" = c(NROW(Treatment), NROW(TreatmentV2), NROW(TreatmentV4)),
      "Qc'd Treatment" = c(NROW(Qcd_Treatment), NROW(Qcd_TreatmentV2), 0)
      ),horiz=TRUE, 
  las = 1,
  main = "Total records per version",
  sub = "A single patient can present multiple record ", col.sub = "red",
  xlab = "Number records",
  beside = FALSE,
  # width = 1,
  xlim = c(0, 3000),
  col = c("purple", "orange", "yellow"),
  #legend.text = c("version1", "version2", "version4"),
  #args.legend = list(x = "bottomright"),
  cex.axis = .8,
  cex.names = .8,
  xpd = TRUE
  )
legend("bottomright", legend = c("version1", "version2", "version4"),
       col = c("purple", "orange", "yellow"),
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, inset = c(0.05, 0.05)) # horiz, vert
dev.off()

#######################################################################################  II  ## Bind Version
#######################################################################################  II  ## Align duplicated ID
mm_history <- bind_rows(MM_history, MM_historyV2, MM_historyV4, .id = "versionMM") %>%
  arrange(date_of_diagnosis)
MM_history <- dcast(setDT(mm_history), avatar_id ~ rowid(avatar_id), value.var = c("date_of_diagnosis", "disease_stage", "versionMM")) %>% 
  select(c("avatar_id", "date_of_diagnosis_1", "disease_stage_1", "date_of_diagnosis_2", "disease_stage_2", "date_of_diagnosis_3", "disease_stage_3",
           "date_of_diagnosis_4", "disease_stage_4", "versionMM_1", "versionMM_2", "versionMM_3", "versionMM_4"))
write.csv(MM_history,paste0(path, "/simplified files/MM_history simplify.csv"))
#-------------------------------------
Vitals <- bind_rows(Vitals, VitalsV2, VitalsV4, Alc_SmoV4, .id = "versionVit")
Vitals <- dcast(setDT(Vitals), avatar_id ~ rowid(avatar_id), 
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
write.csv(Vitals,paste0(path, "/simplified files/Vitals simplify.csv"))


#------------------------------------- SCT
sct <- bind_rows(SCT, SCTV2, SCTV4, .id = "versionSCT") %>% 
  arrange(date_of_third_bmt) %>% 
  arrange(date_of_second_bmt) %>% 
  arrange(date_of_first_bmt) %>% 
  drop_na("date_of_first_bmt") %>% 
  distinct(avatar_id, date_of_first_bmt, .keep_all = TRUE)
SCT <- sct

duplicated(sct$avatar_id) # No duplicated ID so good, if there is need to pivot longer, remove dupl,
# orrange by dates, pivot wider and rename 1st 2nd 3rd bmt

# SCT <- dcast(setDT(sct), avatar_id ~ rowid(avatar_id), 
#              value.var = c("date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
write.csv(SCT,paste0(path, "/simplified files/SCT simplify.csv"))


#------------------------------------ Treatment
# remove NA row in QC'd data
Qcd_Treatment <- Qcd_Treatment %>% drop_na("drug_start_date", "drug_name_")
Qcd_TreatmentV2 <- Qcd_TreatmentV2 %>% drop_na("drug_start_date", "drug_name_")
# remove the Ids found in Qc'd from the Treatment 
uid <- paste(unique(Qcd_Treatment$avatar_id), collapse = '|')
Treatment <- Treatment[(!grepl(uid, Treatment$avatar_id)),]
uid <- paste(unique(Qcd_TreatmentV2$avatar_id), collapse = '|')
TreatmentV2 <- TreatmentV2[(!grepl(uid, TreatmentV2$avatar_id)),]
# Bind QC'd and Treatment for each version then remove duplicated raws
# For even more tidy data
Treatment <- bind_rows(Qcd_Treatment, Treatment, .id = "Treatment") %>% 
  distinct(avatar_id, drug_start_date, drug_stop_date, drug_name_, .keep_all = TRUE) %>% # remove duplicated rows
  mutate_at(("drug_name_"), ~ str_replace_all(., ",", ";"))
TreatmentV2 <- bind_rows(Qcd_TreatmentV2, TreatmentV2, .id = "Treatment") %>% 
  distinct(avatar_id, drug_start_date, drug_stop_date, drug_name_, .keep_all = TRUE) # remove duplicated rows
# Need to pivot longer Treatment from V1 because not same formating
# Having one drug per row will help to remove duplicate in drugs after binding the 3 version together
Treatment <- separate(Treatment, drug_name_, paste("drug_name_", 1:7, sep="_"), sep = "; ", extra = "warn") %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE)

# ready to bind
treatment <- bind_rows(Treatment, TreatmentV2, TreatmentV4, .id = "versionTreat") %>% 
  distinct(avatar_id, drug_start_date, drug_stop_date, drug_name_, .keep_all = TRUE) %>% 
  select(avatar_id, drug_start_date, drug_stop_date, drug_name_) %>% 
  arrange(drug_start_date, drug_stop_date)
# Treatment <- treatment %>% 
#   dcast(avatar_id+drug_start_date+drug_stop_date ~ rowid(avatar_id),
#         value.var = c("drug_name_")) %>% 
#   unite(drug_name_, -avatar_id:-drug_stop_date, sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
#   arrange(drug_start_date)
# Treatment <- dcast(setDT(Treatment), avatar_id ~ rowid(avatar_id), 
#                    value.var = c("drug_start_date", "drug_name_", "drug_stop_date"))
Treatment <- dcast(setDT(treatment), avatar_id ~ rowid(avatar_id), 
                   value.var = c("drug_start_date", "drug_name_", "drug_stop_date"))
write.csv(Treatment,paste0(path, "/simplified files/Treatment simplify.csv"))


#------------------------------------
# Radiation V1 does't have a date format
RadiationV1$rad_start_date <- as.POSIXct(strptime(RadiationV1$rad_start_date, 
                                               format = "%m/%d/%Y", tz = "UTC"))
RadiationV1$rad_stop_date <- as.POSIXct(strptime(RadiationV1$rad_stop_date, 
                                              format = "%m/%d/%Y", tz = "UTC"))

radiation <- bind_rows(RadiationV1, RadiationV2, RadiationV4, .id = "versionRad")%>% 
  drop_na("rad_start_date") %>% 
  arrange(rad_start_date)
Radiation <- dcast(setDT(radiation), avatar_id ~ rowid(avatar_id), value.var = 
                     c("rad_start_date", "rad_stop_date"))
write.csv(Radiation,paste0(path, "/simplified files/Radiation simplify.csv"))


#------------------------------------
# Cleaning
rm(ClinicalCap_V1, ClinicalCap_V2, ClinicalCap_V4, MM_historyV2, MM_historyV4, 
   VitalsV2, VitalsV4, SCTV2, SCTV4, TreatmentV2, TreatmentV4, Qcd_Treatment, Qcd_TreatmentV2, uid,
   Alc_SmoV4, RadiationV1, RadiationV2, RadiationV4)


#######################################################################################  II  ## Plot
jpeg(paste0(path, "/barplot2.jpg"), width = 350, height = 350)
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
dev.off()

#######################################################################################  III  # Merge WES and Sequencing
#######################################################################################  III  # For 1st sequencing file
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

# Reshape to have duplicate ID on same row (per date)-------------------------------------------
# duplicated(WES_seq$moffitt_sample_id_tumor) # No duplicate
# duplicated(WES_seq$avatar_id) # has duplicate

# Really important to order by dates otherwise cannot find the duplicated lines
WES_seq <- WES_seq[order(WES_seq$collectiondt_tumor), ]
WES_seq <-
  dcast(setDT(WES_seq), avatar_id+SLID_germline+moffitt_sample_id_germline ~ rowid(avatar_id),
    value.var = c(
      "BaitSet",
      "ClinicalSpecimenLinkage_WES.Batch",
      "SLID_tumor",
      "moffitt_sample_id_tumor",
      "Disease_Status_tumor",
      "collectiondt_tumor"
    )
  )

WES_seq  <- WES_seq[order(WES_seq$collectiondt_tumor_1), ] %>%
  arrange(collectiondt_tumor_2) %>%
  arrange(collectiondt_tumor_3) %>%
  arrange(collectiondt_tumor_4) %>%
  arrange(collectiondt_tumor_5) %>%
  arrange(collectiondt_tumor_6)


# Merge with Germ (date) with WES_seq (sequencing)
Combined_data_MM <- merge.data.frame(Germ, WES_seq,
                                     by.x = "avatar_id", by.y = "avatar_id", 
                                     all.x = TRUE, all.y = TRUE)

# I checked the ID they are all the same no missing nor added

#######################################################################################  III  # For 2nd sequencing file
# Really important to order by dates otherwise cannot find the duplicated lines
Seq_WES_Raghu <- Seq_WES_Raghu[order(Seq_WES_Raghu$collectiondt_tumor), ]
Seq_WES_Raghu <-
  dcast(setDT(Seq_WES_Raghu), avatar_id+SLID_germline+moffitt_sample_id_germline ~ rowid(avatar_id),
        value.var = c(
          "SLID_tumor",
          "moffitt_sample_id_tumor",
          "collectiondt_tumor",
          "BaitSet"
        )
  ) 

Seq_WES_Raghu <- merge.data.frame(Germ2, Seq_WES_Raghu, 
                          by.x = "avatar_id", by.y = "avatar_id",
                          all.x = TRUE, all.y = TRUE) 
#######################################################################################  III  # For 3rd sequencing file
Sequencing2 <- merge.data.frame(Germ3, Sequencing2, 
                               by.x = "avatar_id", by.y = "avatar_id",
                               all.x = TRUE, all.y = TRUE) %>% 
  arrange(collectiondt_germline)

########### Binds

Germline <- bind_rows(Combined_data_MM, Seq_WES_Raghu,Sequencing2, .id = "vers")
Germline <- Germline %>% distinct(avatar_id,
                             SLID_germline , .keep_all = TRUE) 
write.csv(Germline, paste0(path, "/Combined germline_seq data.csv"))

#------------------------------------
# Cleaning
rm(Sequencing, Sequencing2, WES, WES_seq, Seq_WES_Raghu, Germ, Germ2, Germ3, Combined_data_MM)
##################################################################################################  IV  ## Merge
b <- merge.data.frame(Germline[, c("avatar_id", "moffitt_sample_id_germline",
                                   "collectiondt_germline", "Disease_Status_germline", 
                                           "collectiondt_tumor_1", "Disease_Status_tumor_1")],
                      MM_history, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = TRUE, suffixes = c(".x",".y"))

c <- merge.data.frame(b, Vitals, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = TRUE, suffixes = c(".x",".y"))

d <- merge.data.frame(c, SCT, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = TRUE, suffixes = c(".x",".y"))

e <- merge.data.frame(d, Treatment, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = TRUE, suffixes = c(".x",".y"))

f <- merge.data.frame(e, Radiation,by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = TRUE, suffixes = c(".x",".y"))

Global_data <- merge.data.frame(Demo_RedCap_V4ish, f, by.x = "avatar_id", by.y = "avatar_id", all.x = FALSE, all.y = TRUE)
write.csv(Global_data, paste0(path, "/Global_data.csv"))


#------------------------------------
# Cleaning
rm(b,c,d,e,f)


#------------------------------------
# avatar_no_germline <- Global_data %>% filter(is.na(Global_data$Disease_Status_germline)) %>% 
#   select("avatar_id")
# write.csv(avatar_no_germline, paste0(path, "/patient id with no germline.csv"))


