# import library
library(tidyverse)
library(data.table)
library(VennDiagram)
library(viridis)
library(lubridate)
library(gtsummary)
library(survival)
library(survminer)

#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")
# 1.1.Load Demographics data -------------------------------------------------------------------------------------
Demo_RedCap_V4ish <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/extracted Avatar V124 data and dict/Avatar_Demographics_All_MM_modif_06292020.xlsx")) %>%
  select(c("avatar_id","TCC_ID","Date_of_Birth", "Gender", "Ethnicity", "Race"))
Demo_HRI <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/extracted Avatar V124 data and dict/Demographics_HRI_Export.xlsx")) %>%
  select(c("MRN",Date_of_Birth = "Date of Birth", Gender = "Gender Cerner", Ethnicity = "Ethnicity Cerner", Race = "Race Cerner"))
Demo_linkage <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/extracted Avatar V124 data and dict/Demographics_HRI_Export.xlsx"),
                    sheet = "Sheet1")
# 1.2.Load Germline with disease status --------------------------------------------------------------------------
Germline <- readxl::read_xlsx(paste0(path, 
                                     "/Raghu MM/Germline data/Moffitt_Germl_v0.4.5_Disease_Classification_12082020.xlsx")) %>% 
  select(c("avatar_id", "SLID_germline", "collectiondt_germline", 
           "moffittSampleId_germline", Disease_Status_germline = "Disease_Status"))
# uid <- paste(unique(Germline$avatar_id), collapse = '|')
# Germ <- 
#   readxl::read_xlsx(paste0(path, 
#                            "/Raghu MM/Germline data/Moffitt_Germl_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
#   select(c("avatar_id", "collectiondt", "WES_HUDSON_ALPHA", "Disease_Status"))
# Germ <- Germ[(!grepl(uid, Germ$avatar_id)),]
# Germ2 <-
#   readxl::read_xlsx(paste0(path,
#                            "/Raghu MM/Germline data/Moffitt_Germl_Disease_Classification_2patient_from_2nd_sequencingfile.xlsx")) %>%
#   select(-moffitt_sample_id)
# Germ2 <- Germ2[(!grepl(uid, Germ2$avatar_id)),]
# Germ3 <-
#   readxl::read_xlsx(paste0(path,
#                            "/Raghu MM/Germline data/Germline_MM_Disease_Status_05052020_OUT .xlsx")) %>% 
#   select(c(avatar_id = "Avatar_id", "collectiondt", "WES_HUDSON_ALPHA", "Disease_Status"))
# Germ3 <- Germ3[(!grepl(uid, Germ3$avatar_id)),]
# Germ4 <-
#   readxl::read_xlsx(paste0(path,
#                            "/Raghu MM/Germline data/Moffitt_Germl_v0.4.5_Disease_Classification_OUT_07272020.xlsx")) %>% 
#   select(c("avatar_id", "SLID_germline", "Disease_Status"))
# Germ4 <- Germ4[(!grepl(uid, Germ4$avatar_id)),]
# 1.3.Load Sequencing data ---------------------------------------------------------------------------------------
WES_tumor <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", "collectiondt")) %>% 
  `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", "collectiondt_tumor"))

#---
Sequencing <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c(
    "SLID_germline", 
    "SLID_tumor" , "moffitt_sample_id_tumor", 
    "moffitt_sample_id_germline",
    "BaitSet"))
# Sequencing$moffitt_sample_id_tumor == Sequencing$moffitt_sample_id # yes so remove one var
# Sequencing$subject == Sequencing$avatar_id # yes so remove one var
#---
Seq_WES_Raghu <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V044.xlsx")) %>% 
  select(c(avatar_id = "subject", 
           "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline", 
           "SLID_tumor" , "moffitt_sample_id_tumor", "collectiondt_tumor", 
           "BaitSet"))
# Keep
# Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
#---
Sequencing2 <- # warning message due to a TRUE added in a num var by Raghu (he copy paste an extra patient)
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V0441.xlsx")) %>% 
  select(c(avatar_id = "subject",
           "SLID_germline", moffitt_sample_id_germline = "moffittSampleId_germline",
           "collectiondt_germline",
           "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor",
           BaitSet = "baitSet"))
#---
Seq_WES_Raghu2 <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V045.xlsx")) %>% 
  select(c(avatar_id = "subject", 
           "SLID_germline", moffitt_sample_id_germline = "moffittSampleId_germline", "collectiondt_germline", 
           "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor", 
           "BaitSet"))

# 1.4.Load Clinical data------------------------------------------------------------------------------------------
# V1 and V2 verified in V4 format
ClinicalCap_V12 <-
  fs::path(
    "",
    "Volumes",
    "Gillis_Research",
    "Christelle Colin-Leitzinger",
    "CHIP in Avatar",
    "Raghu MM",
    "extracted Avatar V124 data and dict",
    "V1V2 verified by CIOX in V4 format"
  )
#---
Vitals_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", date_last_follow_up = "date_of_last_contact"))
# Remove IDs from new CIOX V1V2 in the initial V! and V2 version
uid_V <- paste(unique(Vitals_V12$avatar_id), collapse = '|')

Alc_Smo_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Comorbidities") %>%
  select(c("avatar_id","smoking_status", "alcohol_use"))
uid_A <- paste(unique(Alc_Smo_V12$avatar_id), collapse = '|')
#---
MM_history_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis"))
uid_MM <- paste(unique(MM_history_V12$avatar_id), collapse = '|')
#---
Treatment_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("mrn", "avatar_id", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other_")) %>%  # didn't take "treatment_line_"
  unite(drug_name_, c(drug_name_,drug_name_other_), sep = ": ", na.rm = TRUE, remove = FALSE) %>% 
  rename(drug_other = "drug_name_other_")
uid_T <- paste(unique(Treatment_V12$avatar_id), collapse = '|')
#---
Progression_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Treatment_Outcomes") %>%
  select(c("avatar_id", progression_date = "initial_1_pd_date_1")) # For the non qc'd
Progr_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", progression_date = "relapse_date", "QC")) # For the qc'd
uid_P <- paste(c(unique(Progression_V12$avatar_id), unique(Progr_V12$avatar_id)), collapse = '|')
#---
SCT_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id","date_of_bmt")) %>%
  drop_na("date_of_bmt")
uid_S <- paste(unique(SCT_V12$avatar_id), collapse = '|')
#---
Radiation_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Radiation") %>%
  select(c("avatar_id", "rad_start_date", "rad_stop_date"))
uid_R <- paste(unique(Radiation_V12$avatar_id), collapse = '|')
#---
Labs_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Labs") %>%
  select(c("avatar_id", lab_date = "lab_initial_date_1"))
#---
Metastasis_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Metastatic_Disease") %>%
  select(c("avatar_id", metastasis_date = "mets_verify_date"))
#---
Staging_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Staging") %>%
  select(c("avatar_id", "date_staging_results"))
#---
Imaging_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Imaging") %>%
  select(c("avatar_id", "imaging_date"))
#---
Performance_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Performance") %>%
  select(c("avatar_id", "date_perf_status_dx"))
#---
TumorMarker_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Tumor_marker_flow") %>%
  select(c("avatar_id", "tumor_marker_date"))
#---
Biopsy_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", "biopsy_date"))

# V1 ----
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
#---
Vitals <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", "date_last_follow_up"))
Vitals <- Vitals[(!grepl(uid_V, Vitals$avatar_id)),]

Alc_Smo <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id", "smoking_status","current_smoker","alcohol_use"))
Alc_Smo <- Alc_Smo[(!grepl(uid_A, Alc_Smo$avatar_id)),]
#---
MM_history <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis", "disease_stage"))
MM_history <- MM_history[(!grepl(uid_MM, MM_history$avatar_id)),]
#---
# Comorbidities <-
#   readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
#                     sheet = "Comorbidities") #%>% 
#select(c("avatar_id","smoking_status", "alcohol_use"))
#---
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
  select(c("avatar_id", drug_start_date = "regimen_start_date", drug_stop_date = "regimen_end_date",
           drug_name_ = "treatment"))
Treatment <- Treatment[(!grepl(uid_T, Treatment$avatar_id)),]
Qcd_Treatment <- Qcd_Treatment[(!grepl(uid_T, Qcd_Treatment$avatar_id)),]
#---
Progression <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", progression_date = "relapse_date"))
Progression <- Progression[(!grepl(uid_P, Progression$avatar_id)),]
#---
SCT <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id","date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
SCT <- SCT[(!grepl(uid_S, SCT$avatar_id)),]
#---
RadiationV1 <- readxl::read_xlsx(paste0(ClinicalCap_V1, "/Radiation_Version1_Patients.xlsx")) %>%
  select(c("Avatar_ID", "Radiation Start Date", "Radiation End Date")) %>% 
  `colnames<-`(c("avatar_id", "rad_start_date", "rad_stop_date"))
RadiationV1 <- RadiationV1[(!grepl(uid_R, RadiationV1$avatar_id)),]
#---
# Cytogenetic <-
#   readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
#                     sheet = "QC'd_Cytogenetics") %>%
#   select(c("avatar_id","date_bonemarrow_biopsy_results"))
#---
Biopsy <-
    readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx")),
                    sheet = "Biopsy") %>%
    select(c("avatar_id", biopsy_date = "date_bonemarrow_biopsy_results"))
#---
Staging <- 
  readxl::read_xlsx(paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", "date_staging_results"))
#---
Imaging <- 
  readxl::read_xlsx(paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", imaging_date = "date_radiologicexam"))
#---
LabsV1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_modif_04292020.xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", "date_upep", "date_spep", "date_paraprotein_results",
           "date_flowcytometry_dna_"))
# V2 ----
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
#---
VitalsV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death"))
VitalsV2 <- VitalsV2[(!grepl(uid_V, VitalsV2$avatar_id)),]

Alc_Smo_V2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id", "smoking_status","alcohol_use")) # remove BMI
Alc_Smo_V2 <- Alc_Smo_V2[(!grepl(uid_A, Alc_Smo_V2$avatar_id)),]
#---
MM_historyV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id",  "date_of_diagnosis"))
MM_historyV2 <- MM_historyV2[(!grepl(uid_MM, MM_historyV2$avatar_id)),]
#---
TreatmentV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "drug_start_date" , "drug_name_", "drug_stop_date",
           "drug_name_other")) %>%  # didn't take "treatment_line_"
  unite(drug_name_, c(drug_name_,drug_name_other), sep = ": ", na.rm = TRUE, remove = TRUE) #%>% 
  # rename(drug_other = "drug_name_other")
Qcd_TreatmentV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "QC'd Treatment") %>%
  select(c("avatar_id", "drug_start_date" , "drug_name_", "drug_stop_date"))
TreatmentV2 <- TreatmentV2[(!grepl(uid_T, TreatmentV2$avatar_id)),]
Qcd_TreatmentV2 <- Qcd_TreatmentV2[(!grepl(uid_T, Qcd_TreatmentV2$avatar_id)),]
#---
ProgressionV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", progression_date = "relapse_date"))
ProgressionV2 <- ProgressionV2[(!grepl(uid_P, ProgressionV2$avatar_id)),]
#---
SCTV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
SCTV2 <- SCTV2[(!grepl(uid_S, SCTV2$avatar_id)),]
#---
RadiationV2 <- readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                               sheet = "Radiation") %>%
    select(c("avatar_id", "rad_start_date_v2", "rad_stop_date_v2")) %>% 
    `colnames<-`(c("avatar_id", "rad_start_date", "rad_stop_date"))
RadiationV2 <- RadiationV2[(!grepl(uid_R, RadiationV2$avatar_id)),]
#---
BiopsyV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", biopsy_date = "date_bonemarrow_biopsy_results"))
#---
PerformanceV2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx"),
                    sheet = "Performance") %>%
  select(c("avatar_id", date_perf_status_dx = "date_perf_status"))
#---
ImagingV2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx"),
                    sheet = "Imaging") %>%
  select(c("avatar_id", imaging_date = "date_radiologicexam"))
#---
StagingV2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx"),
                    sheet = "Staging") %>%
  select(c("avatar_id", "date_staging_results"))
#---
LabsV2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", "date_upep", "date_spep", "date_paraprotein_results"))
# V4 ----
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
#---
VitalsV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", date_last_follow_up = "date_of_last_contact"))
#---
MM_historyV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis"))
#---
# ComorbiditiesV4 <-
  #   readxl::read_xlsx((paste0(ClinicalCap_V4,"/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
  #                     sheet = "Comorbidities") %>%
  #   select(c("avatar_id", ))
Alc_SmoV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                      sheet = "Comorbidities") %>%
  select(c("avatar_id","smoking_status", "alcohol_use"))
#---
TreatmentV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other")) %>%  # didn't take "treatment_line_"
  unite(drug_name_, c(drug_name_,drug_name_other), sep = ": ", na.rm = TRUE, remove = TRUE)# %>% 
  # rename(drug_other = "drug_name_other")
#---
Progression_V4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Treatment_Outcomes") %>%
  select(c("avatar_id", progression_date = "initial_1_pd_date_1"))
#---
SCTV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_bmt")) %>%  # can be different than first if duplicated from v1 or v2
  drop_na("date_of_bmt")
# SCTV4 <- dcast(setDT(SCTV4), avatar_id ~ rowid(avatar_id), value.var = c("date_of_bmt")) %>% 
#   rename("date_of_first_bmt" = "1", "date_of_second_bmt" = "2")
#---
RadiationV4 <- 
    readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                                 sheet = "Radiation") %>%
    select(c("avatar_id", "rad_start_date", "rad_stop_date"))
#---
LabsV4 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx"),
                    sheet = "Labs") %>%
  select(c("avatar_id", lab_date = "lab_initial_date_1"))
#---
MetastasisV4 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx"),
                    sheet = "Metastatic_Disease") %>%
  select(c("avatar_id", metastasis_date = "mets_verify_date"))
#---
StagingV4 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx"),
                    sheet = "Staging") %>%
  select(c("avatar_id", "date_staging_results"))
#---
ImagingV4 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx"),
                    sheet = "Imaging") %>%
  select(c("avatar_id", "imaging_date"))
#---
PerformanceV4 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx"),
                    sheet = "Performance") %>%
  select(c("avatar_id", "date_perf_status_dx"))
#---
TumorMarkerV4 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx"),
                    sheet = "Tumor_marker_flow") %>%
  select(c("avatar_id", "tumor_marker_date"))
#---
BiopsyV4 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", biopsy_date = "date_bonemarrow_biopsy_results"))

# V4.august2020 ----
#
VitalsV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", date_last_follow_up = "date_of_last_contact"))
#
MM_historyV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis"))
#
Alc_SmoV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Comorbidities") %>%
  select(c("avatar_id","smoking_status", "alcohol_use"))
#
TreatmentV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other_")) %>%  # didn't take "treatment_line_"
  unite(drug_name_, c(drug_name_,drug_name_other_), sep = ": ", na.rm = TRUE, remove = TRUE)# %>% 
  # rename(drug_other = "drug_name_other_")
#
Progression_V4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Treatment_Outcomes") %>%
  select(c("avatar_id", progression_date = "initial_1_pd_date_1"))
#
SCTV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_bmt")) %>%  # can be different than first if duplicated from v1 or v2
  drop_na("date_of_bmt")
# SCTV4.1 <- dcast(setDT(SCTV4), avatar_id ~ rowid(avatar_id), value.var = c("date_of_bmt")) %>% 
#   rename("date_of_first_bmt" = "1", "date_of_second_bmt" = "2")
#
RadiationV4.1 <- 
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Radiation") %>%
  select(c("avatar_id", "rad_start_date", "rad_stop_date"))
#
LabsV4.1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx"),
                    sheet = "Labs") %>%
  select(c("avatar_id", lab_date = "lab_initial_date_1"))
#
MetastasisV4.1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx"),
                    sheet = "Metastatic_Disease") %>%
  select(c("avatar_id", metastasis_date = "mets_verify_date"))
#
StagingV4.1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx"),
                    sheet = "Staging") %>%
  select(c("avatar_id", "date_staging_results"))
#
ImagingV4.1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx"),
                    sheet = "Imaging") %>%
  select(c("avatar_id", "imaging_date"))
#
PerformanceV4.1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx"),
                    sheet = "Performance") %>%
  select(c("avatar_id", "date_perf_status_dx"))
#
TumorMarkerV4.1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx"),
                    sheet = "Tumor_marker_flow") %>%
  select(c("avatar_id", "tumor_marker_date"))
#
BiopsyV4.1 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", biopsy_date = "date_bonemarrow_biopsy_results"))
# Others
OS_data <- readxl::read_xlsx(paste0(path, "/Raghu MM/Overall Survival/HRI_Last_Followupdata.xlsx")) %>% 
  select(avatar_id = "germline_patient_data_avatar_id", "final_vitals", "Vital_Status_Date") %>% 
  distinct()
#
Staging_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Staging_09142020.xlsx")) %>% 
  select("avatar_id", "collectiondt_germline", "Labs_Result_Date", "Final_Albumin", "Final_Beta2", "Final_LDH", "ISS") %>% 
  mutate(ISS = str_replace(ISS, pattern = "NA", replacement = NA_character_)) %>% 
  arrange(collectiondt_germline) %>% 
  distinct(avatar_id, .keep_all = TRUE) # remove the duplicate of patient 180
#
CHIP_status <- read_csv(paste0(path, "/Nancy's working files/CHcalls_12.10.20.csv")) %>% 
  # mutate(CH_status = ifelse(CH_status == "CH", "CHIP", "No CHIP")) %>% 
  mutate(patient_germline_id = str_remove(patient_germline_id, "_normal"))
#
IMIDS_maintenance <- readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Maintainance_Regimen.xlsx"))
#
migration_patients <- readxl::read_xlsx(paste0(path, "/Raghu MM/Avatar List For Migration.xlsx"))


# Plot data recorded ---
# jpeg(paste0(path, "/barplot1.jpg"), width = 350, height = 350)
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
# dev.off()

#######################################################################################  III  # Merge WES and Sequencing----
#######################################################################################  III  # For 1st sequencing file
### Bind Germline
Germline <- Germline %>% 
  distinct()# %>% 
  # filter(!str_detect(avatar_id, "A000428|A000456"))
#   `colnames<-`(c("avatar_id", "collectiondt_germline", 
#                  "WES_HUDSON_ALPHA_germline", "Disease_Status_germline", "SLID_germline")) %>% 
#   arrange(SLID_germline) %>% 
#   distinct(avatar_id, Disease_Status_germline, .keep_all = TRUE)

# One of the sequencing data is in 2 part so merge that first
# Are moffitt_sample_id are equal in WES and Sequencing ?
# Sequencing <- Sequencing[order(Sequencing$moffitt_sample_id),]
# WES <- WES[order(WES$moffitt_sample_id),]
# Sequencing$moffitt_sample_id == WES$moffitt_sample_id # =>>>>>>> YES
Sequencing <-
  full_join(
    Sequencing,
    WES_tumor,
    by = "moffitt_sample_id_tumor")
# Bind Sequencing
Seq_WES <- bind_rows(Seq_WES_Raghu, Sequencing2, Seq_WES_Raghu2, Sequencing, .id = "vers") %>% 
  arrange(collectiondt_germline) %>% 
  distinct(SLID_tumor, moffitt_sample_id_tumor, SLID_germline, .keep_all = TRUE)

# duplicated(WES_seq$moffitt_sample_id_tumor) # No duplicate
# duplicated(WES_seq$avatar_id) # has duplicate so
# Reshape to have duplicate ID on same row (per date) but
# Really important to order by dates otherwise cannot find the duplicated lines
Seq_WES <- Seq_WES[order(Seq_WES$collectiondt_tumor), ]
# pivot wider
WES_seq <-
  dcast(setDT(Seq_WES), avatar_id+SLID_germline+moffitt_sample_id_germline+collectiondt_germline ~ rowid(avatar_id),
        value.var = c(
          "SLID_tumor",
          "moffitt_sample_id_tumor",
          "collectiondt_tumor",
          "BaitSet"
        )
  )

# WES_seq  <- WES_seq[order(WES_seq$collectiondt_tumor_1), ] %>%
#   arrange(collectiondt_tumor_2) %>%
#   arrange(collectiondt_tumor_3) %>%
#   arrange(collectiondt_tumor_4) %>%
#   arrange(collectiondt_tumor_5) %>%
#   arrange(collectiondt_tumor_6)


# # Merge with Germ (date) with WES_seq (sequencing)
# Combined_data_MM <- merge.data.frame(Germ, WES_seq,
#                                      by.x = "avatar_id", by.y = "avatar_id", 
#                                      all.x = TRUE, all.y = TRUE)
# 
# # I checked the ID they are all the same no missing nor added
# 
# #######################################################################################  III  # For 2nd sequencing file
# # Really important to order by dates otherwise cannot find the duplicated lines
# Seq_WES_Raghu <- Seq_WES_Raghu[order(Seq_WES_Raghu$collectiondt_tumor), ]
# Seq_WES_Raghu <-
#   dcast(setDT(Seq_WES_Raghu), avatar_id+SLID_germline+moffitt_sample_id_germline ~ rowid(avatar_id),
#         value.var = c(
#           "SLID_tumor",
#           "moffitt_sample_id_tumor",
#           "collectiondt_tumor",
#           "BaitSet"
#         )
#   ) 
# 
# Seq_WES_Raghu <- merge.data.frame(Germ2, Seq_WES_Raghu, 
#                           by.x = "avatar_id", by.y = "avatar_id",
#                           all.x = TRUE, all.y = TRUE) 
# #######################################################################################  III  # For 3rd sequencing file
# Sequencing2 <- merge.data.frame(Germ3, Sequencing2, 
#                                by.x = "avatar_id", by.y = "avatar_id",
#                                all.x = TRUE, all.y = TRUE) %>% 
#   arrange(collectiondt_germline)
# #######################################################################################  III  # For 4th sequencing file
# # Really important to order by dates otherwise cannot find the duplicated lines
# Seq_WES_Raghu2 <- Seq_WES_Raghu2[order(Seq_WES_Raghu2$collectiondt_tumor), ]
# Seq_WES_Raghu2 <-
#   dcast(setDT(Seq_WES_Raghu2), avatar_id+SLID_germline+moffitt_sample_id_germline+collectiondt_germline ~ rowid(avatar_id),
#         value.var = c(
#           "SLID_tumor",
#           "moffitt_sample_id_tumor",
#           "collectiondt_tumor",
#           "BaitSet"
#         )
#   )
# Seq_WES_Raghu2 <- full_join(Germ4, Seq_WES_Raghu2, by = "SLID_germline") %>% 
#   select(avatar_id = "avatar_id.x", everything(), -avatar_id.y)
# ########### Binds
# 
# # Germline <- bind_rows(Combined_data_MM, Seq_WES_Raghu,Sequencing2, .id = "vers")
# # Germline <- Germline %>% distinct(avatar_id,
# #                              SLID_germline , .keep_all = TRUE) 
# Germline <- bind_rows(Combined_data_MM, Seq_WES_Raghu, Sequencing2, Seq_WES_Raghu2, .id = "vers")
# Germline <- Germline %>% distinct(avatar_id,
#                                   SLID_germline , .keep_all = TRUE) 
# write.csv(Germline, paste0(path, "/Combined germline_seq data.csv"))


# Merge all

Germline <- left_join(WES_seq, Germline, by = "avatar_id") %>% 
  filter(SLID_germline.x == SLID_germline.y | is.na(SLID_germline.x == SLID_germline.y)) %>% 
  rename(SLID_germline = "SLID_germline.x", collectiondt_germline = "collectiondt_germline.x") %>% 
  # distinct(avatar_id, SLID_germline, .keep_all = TRUE) %>% 
  mutate(collectiondt_germline = coalesce(collectiondt_germline, collectiondt_germline.y)) %>% 
  select(-SLID_germline.y, -collectiondt_germline.y)



# Cleaning
rm(Sequencing, Sequencing2, WES_tumor, WES_seq, Seq_WES_Raghu, Seq_WES, Seq_WES_Raghu2)


#######################################################################################  II  ## Bind Version----
#######################################################################################  II  ## Align duplicated ID
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
  mutate(Ethnicity = case_when(
    Ethnicity %in% c("Spanish; Hispanic")                                   ~ "Hispanic",
    Ethnicity %in% c("Non- Hispanic", "Non-Spanish; non-Hispanic")          ~ "Non-Hispanic",
    Ethnicity %in% c("Unknown", "Prefer not to answer", "PT Not Present")   ~ "Unknown",
    TRUE                                                                    ~ Ethnicity
  ))

# Patient history ----
mm_history <- bind_rows(MM_history_V12, MM_history, MM_historyV2, MM_historyV4, MM_historyV4.1, .id = "versionMM") %>%
  drop_na("date_of_diagnosis") %>%
  distinct(avatar_id, date_of_diagnosis, .keep_all = TRUE) %>% 
  # Create date of diagnosis closest to germline date
  left_join(., Germline %>% distinct(avatar_id, .keep_all = TRUE) %>% select("avatar_id", "collectiondt_germline"), # For only 1 date of Dx when multiple germline collection
            by = "avatar_id") %>% 
  mutate(interval = (interval(start= .$collectiondt_germline, end= .$date_of_diagnosis)/duration(n=1, unit="days"))) %>% 
  mutate(interval1 = if_else(interval>100, NA_real_, interval)) %>% 
  mutate(interval1 = abs(interval1)) %>% 
  arrange(interval1) %>% 
  group_by(avatar_id) %>% mutate(id = 1:n()) %>% ungroup() %>%
  mutate(Dx_date_closest_germline = if_else(id == 1, date_of_diagnosis, NA_POSIXct_)) %>% 
  distinct(avatar_id, date_of_diagnosis, .keep_all = TRUE) %>% 
  arrange(date_of_diagnosis)

# diagn_data <- Global_data %>%
#   # distinct(avatar_id, .keep_all = TRUE) %>% 
#   # select("avatar_id", starts_with("disease_stage_"), "Disease_Status_germline", collectiondt_germline, starts_with("date_of_diagnosis_")) %>% 
#   # pivot_longer(cols = date_of_diagnosis_1:ncol(.), names_to = "event", values_to = "date") %>%
#   # drop_na(date) %>% 
#   
#   
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   select("avatar_id", Dx_date_closest_germline = "date")
# write.csv(diagn_data, paste0(path, "/diagnosis data with germline and interval.csv"))

MM_history <- dcast(setDT(mm_history), avatar_id+collectiondt_germline ~ rowid(avatar_id), 
                    value.var = c("Dx_date_closest_germline", "date_of_diagnosis", "disease_stage")) %>% 
  unite(Dx_date_closest_germline, starts_with("Dx_date_closest_germline"), na.rm = TRUE, remove = TRUE) %>% 
  mutate(Dx_date_closest_germline = as.POSIXct(.$Dx_date_closest_germline, format = "%Y-%m-%d")) %>% 
  select(-collectiondt_germline) %>% 
  # Create var = first date of Dx for MM diagnostic aka "active" (not for mgus or smoldering)
  # Then when not "active" take the first date of Dx available (mgus or smoldering or NA without regarding order- 
  # usually mgus before smoldering)
  mutate(date_of_MMonly_diagnosis = case_when(
    disease_stage_1 == "active"          ~ date_of_diagnosis_1,
    disease_stage_2 == "active"          ~ date_of_diagnosis_2,
    disease_stage_3 == "active"          ~ date_of_diagnosis_3,
    disease_stage_4 == "active"          ~ date_of_diagnosis_4,
  )) %>% 
  mutate(date_of_MMSMMGUSdiagnosis = coalesce(date_of_MMonly_diagnosis, date_of_diagnosis_1)) %>% 
  select(c("avatar_id", "date_of_MMonly_diagnosis", "date_of_MMSMMGUSdiagnosis", everything()))

write.csv(MM_history,paste0(path, "/simplified files/MM_history simplify.csv"))

# Vitals ----
# Bind and arrange to have dates in order within each Alive, Dead, and Lost
Vitals <- bind_rows(Vitals_V12, Vitals, VitalsV2, VitalsV4, VitalsV4.1, .id = "versionVit") %>% 
  # mutate(vital_status_rec = case_when(
  #   vital_status == 2         ~ "Dead",
  #   vital_status == 1         ~ "Alive",
  #   vital_status == 3         ~ "Lost"
  # )) %>% 
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
  mutate(date_last_follow_up = case_when( # just to remove the last date of follow up when is contact lost
    !is.na(date_contact_lost)   ~ NA_POSIXct_,
    is.na(date_contact_lost)    ~ date_last_follow_up
  )) # %>% Cannot use that when doing PFS, need to keep date of last follow up even before death 
  # mutate(date_last_follow_up = case_when(
  #   date_last_follow_up <= date_death       ~ NA_POSIXct_,
  #   is.na(date_death)                       ~ date_last_follow_up
  # )) 

write.csv(Vitals,paste0(path, "/simplified files/Vitals simplify.csv"))
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
SCT <- SCT %>% pivot_longer(cols = c(date_of_first_bmt, date_of_second_bmt, date_of_third_bmt),
                             values_to = "date_of_bmt", values_drop_na = TRUE)
SCTV2 <- SCTV2 %>% pivot_longer(cols = c(date_of_first_bmt, date_of_second_bmt, date_of_third_bmt),
                             values_to = "date_of_bmt", values_drop_na = TRUE)

sct <- bind_rows(SCT_V12, SCT, SCTV2, SCTV4, SCTV4.1, .id = "versionSCT") %>% 
  distinct(avatar_id, date_of_bmt) %>% 
  arrange(date_of_bmt)
SCT <- dcast(setDT(sct), avatar_id ~ rowid(avatar_id), 
             value.var = "date_of_bmt") %>% 
  `colnames<-`(c("avatar_id", "date_of_bmt_1", "date_of_bmt_2", "date_of_bmt_3"))
write.csv(SCT,paste0(path, "/simplified files/SCT simplify.csv"))

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
Qcd_Treatment <- Qcd_Treatment %>% drop_na("drug_start_date", "drug_name_")
Qcd_TreatmentV2 <- Qcd_TreatmentV2 %>% drop_na("drug_start_date", "drug_name_")
# remove the Ids found in Qc'd from the Treatment 
uid <- paste(unique(Qcd_Treatment$avatar_id), collapse = '|')
Treatment <- Treatment[(!grepl(uid, Treatment$avatar_id)),]
uid <- paste(unique(Qcd_TreatmentV2$avatar_id), collapse = '|')
TreatmentV2 <- TreatmentV2[(!grepl(uid, TreatmentV2$avatar_id)),]
# Bind QC'd and Treatment for each version
# Need to pivot longer Treatment from V1 (and V2) because not same formatting
# Having one drug per row will help to remove duplicate in drugs after binding all version together
Treatment <- bind_rows(Qcd_Treatment, Treatment, .id = "Treatment") %>% 
  # mutate_at(("drug_name_"), ~ str_replace_all(., ",", ";")) %>% 
  separate(col = drug_name_, paste("drug_name_", 1:7, sep=""), sep = "; |;", extra = "warn", 
           fill = "right") %>% 
  purrr::keep(~!all(is.na(.))) %>%
  pivot_longer(cols = starts_with("drug_name_"),
               names_to = "drug", values_to = "drug_name_", values_drop_na = TRUE)
TreatmentV2 <- bind_rows(Qcd_TreatmentV2, TreatmentV2, .id = "Treatment") %>% 
  separate(col = drug_name_, paste("drug_name_", 1:7, sep=""), sep = "; |;", extra = "warn", 
           fill = "right") %>% 
  purrr::keep(~!all(is.na(.))) %>%
  pivot_longer(cols = starts_with("drug_name_"),
               names_to = "drug", values_to = "drug_name_", values_drop_na = TRUE)

Treatment_V12 <- Treatment_V12 %>% 
  separate(col = drug_name_, paste("drug_name_", 1:7, sep=""), sep = "\\+", extra = "warn", 
           fill = "right") %>% 
  purrr::keep(~!all(is.na(.))) %>%
  pivot_longer(cols = starts_with("drug_name_"),
               names_to = "drug", values_to = "drug_name_", values_drop_na = TRUE)

# ready to bind
treatment <- bind_rows(Treatment_V12, Treatment, TreatmentV2, TreatmentV4, TreatmentV4.1, .id = "versionTreat") %>% 
  select(avatar_id, drug_start_date, drug_stop_date, drug_name_) %>% 
  mutate(drug_name_ = tolower(drug_name_)) %>% 
  mutate(drug_name_ = case_when(
    drug_name_ == "cafilzomib"                                             ~ "carfilzomib",
    drug_name_ == "daratumuab"                                             ~ "daratumumab",
    str_detect(drug_name_, "^dex")                                         ~ "dexamethasone",
    str_detect(drug_name_, "^lena")                                        ~ "lenalidomide",
    str_detect(drug_name_, "^mel")                                         ~ "melphalan",
    drug_name_ == "vinicristine"                                           ~ "vincristine",
    drug_name_ %in% c("anastrozole", "azacitidine", "carmustine", 
                      "cytarabine", "decitabine", "denosumab", 
                      "docetaxel", "hydrocortisone", "methotrexate",
                      "methylprednisolone", "pegfilgrastim", "prednisone", 
                      "rapamycin", "rituxan", "rituximab", 
                      "sorafenib tosylate", "tamoxifen citrate", 
                      "zoledronic acid", "prevnar", "ruxolitinib")         ~ "Non-MM drugs",
    TRUE                                                                   ~ drug_name_
  )) %>% 
  filter(!is.na(avatar_id) & drug_name_ != "Non-MM drugs") %>% 
  distinct() %>%
  group_by(avatar_id, drug_start_date) %>%
  arrange(drug_name_) %>%
  ungroup() %>%
  arrange(drug_start_date, drug_stop_date)


# Now can dcast to have line of drug_name_ for each line/regimen
# 1st for regimen with same start and end date
Treatment <- treatment %>% 
  reshape2::dcast(avatar_id+drug_start_date+drug_stop_date ~ rowid(avatar_id),
                  value.var = c("drug_name_")) %>% 
  unite(drug_name_, -avatar_id:-drug_stop_date, sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  arrange(drug_start_date, drug_stop_date)
# 2nd for regimen with same start, I separated it to have the different end date in case
Treatment <- dcast(setDT(Treatment), avatar_id+drug_start_date ~ rowid(avatar_id), 
                   value.var = c("drug_name_", "drug_stop_date")) %>% 
  unite(drug_name_, starts_with("drug_name_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite(drug_stop_date, starts_with("drug_stop_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  separate(drug_stop_date, paste("drug_stop_date", 1:3, sep="_"), sep = "; ",
           extra = "warn", fill = "right") %>% 
  arrange(drug_start_date)
Treatment$drug_stop_date_1 <- as.POSIXct(Treatment$drug_stop_date_1, format = "%Y-%m-%d")
Treatment$drug_stop_date_2 <- as.POSIXct(Treatment$drug_stop_date_2, format = "%Y-%m-%d")
Treatment$drug_stop_date_3 <- as.POSIXct(Treatment$drug_stop_date_3, format = "%Y-%m-%d")
# 3rd dcast per avatar_id
Treatment <- dcast(setDT(Treatment), avatar_id ~ rowid(avatar_id), 
                   value.var = c("drug_start_date", "drug_name_", "drug_stop_date_1", 
                                 "drug_stop_date_2", "drug_stop_date_3"))
Treatment <- Treatment %>% 
  purrr::keep(~!all(is.na(.))) %>% 
  mutate(received_IMIDs = case_when(
    str_detect(drug_name__1, "lidomide") |
      str_detect(drug_name__2, "lidomide") |
      str_detect(drug_name__3, "lidomide") |
      str_detect(drug_name__4, "lidomide") |
      str_detect(drug_name__5, "lidomide") |
      str_detect(drug_name__6, "lidomide") |
      str_detect(drug_name__7, "lidomide") |
      str_detect(drug_name__8, "lidomide") |
      str_detect(drug_name__9, "lidomide") |
      str_detect(drug_name__10, "lidomide") |
      str_detect(drug_name__11, "lidomide") |
      str_detect(drug_name__12, "lidomide") |
      str_detect(drug_name__13, "lidomide") |
      str_detect(drug_name__14, "lidomide") |
      str_detect(drug_name__15, "lidomide") |
      str_detect(drug_name__16, "lidomide") |
      str_detect(drug_name__17, "lidomide") |
      str_detect(drug_name__18, "lidomide")    ~ "IMIDs",
    TRUE ~ "No IMIDs"
  )) %>% 
  full_join(., IMIDS_maintenance, by = "avatar_id")

rm(migration_patients, IMIDS_maintenance)
write.csv(Treatment,paste0(path, "/simplified files/Treatment simplify.csv"))

# Radiation ----
# Radiation V1 doesn't have a date format
RadiationV1$rad_start_date <- as.POSIXct(strptime(RadiationV1$rad_start_date, 
                                                  format = "%m/%d/%Y", tz = "UTC"))
RadiationV1$rad_stop_date <- as.POSIXct(strptime(RadiationV1$rad_stop_date, 
                                                 format = "%m/%d/%Y", tz = "UTC"))

radiation <- bind_rows(Radiation_V12, RadiationV1, RadiationV2, RadiationV4, RadiationV4.1, .id = "versionRad") %>% 
  drop_na("rad_start_date") %>% 
  filter(!str_detect(rad_start_date, "3013")) %>% 
  filter(!str_detect(rad_stop_date, "2300")) %>% 
  left_join(., Germline %>% select(c("avatar_id", "collectiondt_germline")), by = "avatar_id") %>% 
  mutate(rad_bf_germline = if_else(rad_start_date < collectiondt_germline, "Radiation before Germline", "No")) %>% 
  distinct(avatar_id, rad_start_date, rad_stop_date, collectiondt_germline, .keep_all = TRUE) %>% 
  select(-collectiondt_germline) %>% 
  arrange(rad_start_date)
Radiation <- dcast(setDT(radiation), avatar_id ~ rowid(avatar_id), value.var = 
                     c("rad_start_date", "rad_stop_date", "rad_bf_germline"))
write.csv(Radiation,paste0(path, "/simplified files/Radiation simplify.csv"))

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
            Progression, ProgressionV2, Progression_V4, Progression_V4.1) %>%
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
Progression_drugs <- Progression # Create 2 df for dates from Dx or drug (will not have the same clean up)
Progression_rad <- Progression
Progression_hct <- Progression
Progression_treat <- Progression # For hct and drugs

Progression <- Progression %>% # Keep earliest progression_date => For OS
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE)
write.csv(Progression, paste0(path, "/simplified files/Progression simplify.csv"))

Progression_treat <- Progression_treat %>% 
  left_join(., Treatment %>% select(c("avatar_id", "drug_start_date_1")), by = "avatar_id") %>%
  left_join(., SCT %>% select(c("avatar_id", "date_of_bmt_1")), by = "avatar_id") %>% 
  mutate(prog_before_treat = case_when(
    progression_date <= drug_start_date_1 &
      progression_date <= date_of_bmt_1                 ~ "removed",
    progression_date <= drug_start_date_1 &
      is.na(date_of_bmt_1)                              ~ "removed",
    progression_date <= date_of_bmt_1 &
      is.na(drug_start_date_1)                          ~ "removed",
    progression_date > drug_start_date_1 |
      progression_date > date_of_bmt_1 |
      is.na(drug_start_date_1) &
      is.na(date_of_bmt_1)                              ~ "good"
  )) %>%
  filter(prog_before_treat == "good") %>% 
  select(1:2) %>% 
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_treatment_date = "progression_date")

Progression_drugs <- Progression_drugs %>% # Remove progression < drug and keep earliest progression_drug_date
  left_join(., Treatment %>% select(c("avatar_id", "drug_start_date_1")), by = "avatar_id") %>% 
  mutate(prog_before_drug = case_when(
    progression_date <= drug_start_date_1               ~ "removed", # 0 patient removed :)
    progression_date > drug_start_date_1 |
      is.na(drug_start_date_1)                          ~ "good" # progression have to be strictly > drug
  )) %>%
  filter(prog_before_drug == "good") %>% 
  select(1:2) %>% 
  arrange(progression_date) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  rename(progression_drug_date = "progression_date")
write.csv(Progression_drugs, paste0(path, "/simplified files/Progression used for survivals from drugs date.csv"))

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


# Cleaning
rm(Demo_HRI, Demo_linkage, MM_history_V12, MM_historyV2, MM_historyV4, MM_historyV4.1,
   Vitals_V12, VitalsV2, VitalsV4, VitalsV4.1, SCT_V12, SCTV2, SCTV4, SCTV4.1,
   Treatment_V12, TreatmentV2, TreatmentV4, Qcd_Treatment, Qcd_TreatmentV2, TreatmentV4.1)

# Lab dates and biopsy to fill up last date of contact when not furnished ----
LabsV1 <- gather(LabsV1, key = "event", value = "labs_last_date", 2:ncol(LabsV1)) %>% 
  drop_na(labs_last_date)
LabsV2 <- gather(LabsV2, key = "event", value = "labs_last_date", 2:ncol(LabsV2)) %>% 
  drop_na(labs_last_date)
Labs_V12 <- gather(Labs_V12, key = "event", value = "labs_last_date", 2) %>% 
  drop_na(labs_last_date)
LabsV4 <- gather(LabsV4, key = "event", value = "labs_last_date", 2) %>% 
  drop_na(labs_last_date)
LabsV4.1 <- gather(LabsV4.1, key = "event", value = "labs_last_date", 2) %>% 
  drop_na(labs_last_date)
labs_dates <- bind_rows(LabsV1, LabsV2, Labs_V12, LabsV4, LabsV4.1)
rm(LabsV1, LabsV2, Labs_V12, LabsV4, LabsV4.1)

biopsy <- bind_rows(Biopsy_V12, Biopsy, BiopsyV2, BiopsyV4, BiopsyV4.1) %>% 
  drop_na(biopsy_date) %>% 
  gather(., key = "event", value = "labs_last_date", 2)
imaging <- bind_rows(Imaging, Imaging_V12, ImagingV2, ImagingV4, ImagingV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
metastasis <- bind_rows(Metastasis_V12, MetastasisV4, MetastasisV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
performance <- bind_rows(Performance_V12, PerformanceV2, PerformanceV4, PerformanceV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
staging <- bind_rows(Staging, Staging_V12, StagingV2, StagingV4, StagingV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
tumormarker <- bind_rows(TumorMarker_V12, TumorMarkerV4, TumorMarkerV4.1) %>% 
  drop_na() %>% 
  gather(., key = "event", value = "labs_last_date", 2)
rm(Biopsy_V12, Biopsy, BiopsyV2, BiopsyV4, BiopsyV4.1,
   Imaging, Imaging_V12, ImagingV2, ImagingV4, ImagingV4.1,
   Metastasis_V12, MetastasisV4, MetastasisV4.1,
   Performance_V12, PerformanceV2, PerformanceV4, PerformanceV4.1,
   Staging, Staging_V12, StagingV2, StagingV4, StagingV4.1,
   TumorMarker_V12, TumorMarkerV4, TumorMarkerV4.1)

Last_labs_dates <- bind_rows(labs_dates, biopsy, imaging, metastasis, performance, staging, tumormarker) %>% 
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
rm(labs_dates, biopsy, imaging, metastasis, performance, staging, tumormarker)


# Cleaning
rm(ClinicalCap_V12, ClinicalCap_V1, ClinicalCap_V2, ClinicalCap_V4, 
   uid, uid_A, uid_MM, uid_R, uid_S, uid_T, uid_V, uid_P, uid_P12,
   Alc_Smo, Alc_Smo_V12, Alc_Smo_V2, Alc_SmoV4, Alc_SmoV4.1, 
   Radiation_V12, RadiationV1, RadiationV2, RadiationV4, RadiationV4.1,
   Progr_V12, Progression_V12, ProgressionV2, Progression_V4, Progression_V4.1#,
   #Contact_lost
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
Global_data <- full_join(Germline %>%  select(c("avatar_id", "moffitt_sample_id_germline", "SLID_germline",
                             "collectiondt_germline", "Disease_Status_germline", 
                             starts_with("SLID_tumor"), starts_with("collectiondt_tumor_", 
                             starts_with("moffitt_sample_id_tumor")))),
               MM_history, by = "avatar_id") %>% 
  full_join(., Vitals, by = "avatar_id") %>% 
  full_join(., SCT, by = "avatar_id") %>% 
  full_join(., Treatment, by = "avatar_id") %>% 
  full_join(., Radiation, by = "avatar_id") %>% 
  full_join(., Progression, by= "avatar_id") %>% 
  full_join(., Progression_drugs, by= "avatar_id") %>% 
  full_join(., Progression_treat, by= "avatar_id") %>% 
  full_join(., Progression_rad, by= "avatar_id") %>% 
  full_join(., Progression_hct, by= "avatar_id") %>% 
  full_join(., Last_labs_dates %>% select(c("avatar_id", "labs_last_date")), by = "avatar_id") %>% 
  full_join(., OS_data, by = "avatar_id") %>% 
  full_join(., Staging_ISS, by = c("avatar_id", "collectiondt_germline")) %>% 
  full_join(Demo_RedCap_V4ish, ., by = "avatar_id")
# write.csv(Global_data, paste0(path, "/Global_data.csv"))
Global_data <- left_join(Global_data, CHIP_status, by = c("SLID_germline" = "patient_germline_id")) %>% 
  filter(!str_detect(avatar_id, "A000428|A000456"))
write_rds(Global_data, path = "Global_data_pre.rds")
#--
# avatar_no_germline <- Global_data %>% filter(is.na(Global_data$Disease_Status_germline)) %>% 
#   select("avatar_id")
# write.csv(avatar_no_germline, paste0(path, "/patient id with no germline.csv"))


