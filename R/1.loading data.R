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
# V1 and V2 in V4 format----
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

Alc_Smo_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Comorbidities") %>%
  select(c("avatar_id","smoking_status", "alcohol_use"))
#---
MM_history_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "mrn", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))
#---
Treatment_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("mrn", "avatar_id", "treatment_line_", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other_", "treatment_site")) %>%
  unite(drug_name_, c(drug_name_,drug_name_other_), sep = ": ", na.rm = TRUE, remove = TRUE) # %>% 
# rename(drug_other = "drug_name_other_")
#---
Progression_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Treatment_Outcomes") %>%
  select(c("avatar_id", progression_date = "initial_1_pd_date_1")) # For the non qc'd
Progr_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", progression_date = "relapse_date", "QC")) # For the qc'd
#---
SCT_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id","date_of_bmt")) %>%
  drop_na("date_of_bmt")
#---
Radiation_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Radiation") %>%
  select(c("avatar_id", "rad_start_date", "rad_stop_date"))
#---
Labs_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Labs") %>%
  select(c("avatar_id", lab_date = "lab_initial_date_1"))
#---
Metastasis_V12 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx"),
                    sheet = "Metastatic_Disease") %>%
  select(c("avatar_id", metastasis_date = "mets_verify_date", have_metastasis = "initial_1_mets_1"))
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
#
# V12 legacy.april2021 ----
#
VitalsV12_L_2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "Vitals") %>%
  select(c("avatar_id","vital_status","date_death", date_last_follow_up = "date_of_last_contact"))
#
MM_historyV12_L_2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))
#
Alc_SmoV12_L_2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "Comorbidities") %>%
  select(c("avatar_id","smoking_status", "alcohol_use"))
#
TreatmentV12_L_2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "treatment_line_", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other_", "treatment_site")) %>%
  unite(drug_name_, c(drug_name_,drug_name_other_), sep = ": ", na.rm = TRUE, remove = TRUE)
#
ProgressionV12_L_2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "Treatment_Outcomes") %>%
  select(c("avatar_id", progression_date = "initial_1_pd_date_1")) # For the non qc'd
ProgrV12_L_2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", progression_date = "relapse_date", "QC")) # For the qc'd
#
SCTV12_L_2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "date_of_bmt")) %>% 
  drop_na("date_of_bmt")
#
RadiationV12_L_2 <- 
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx")),
                    sheet = "Radiation") %>%
  select(c("avatar_id", "rad_start_date", "rad_stop_date"))
#
LabsV12_L_2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx"),
                    sheet = "Labs") %>%
  select(c("avatar_id", lab_date = "lab_initial_date_1"))
#
MetastasisV12_L_2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx"),
                    sheet = "Metastatic_Disease") %>%
  select(c("avatar_id", metastasis_date = "mets_verify_date", have_metastasis = "initial_1_mets_1"))
#
StagingV12_L_2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx"),
                    sheet = "Staging") %>%
  select(c("avatar_id", "date_staging_results"))
#
ImagingV12_L_2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx"),
                    sheet = "Imaging") %>%
  select(c("avatar_id", "imaging_date"))
#
PerformanceV12_L_2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx"),
                    sheet = "Performance") %>%
  select(c("avatar_id", "date_perf_status_dx"))
#
TumorMarkerV12_L_2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx"),
                    sheet = "Tumor_marker_flow") %>%
  select(c("avatar_id", "tumor_marker_date"))
#
BiopsyV12_L_2 <- 
  readxl::read_xlsx(paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_04212020.xlsx"),
                    sheet = "Biopsy") %>%
  select(c("avatar_id", "biopsy_date"))

# Combine the 2 legacy version and remove id from V1 and V2
Vitals_V12 <-
  bind_rows(Vitals_V12, VitalsV12_L_2)
Alc_Smo_V12 <-
  bind_rows(Alc_Smo_V12, Alc_SmoV12_L_2)
MM_history_V12 <-
  bind_rows(MM_history_V12, MM_historyV12_L_2)
Treatment_V12 <-
  bind_rows(Treatment_V12, TreatmentV12_L_2)
Progression_V12 <-
  bind_rows(Progression_V12, ProgressionV12_L_2)
Progr_V12 <-
  bind_rows(Progr_V12, ProgrV12_L_2)
SCT_V12 <-
  bind_rows(SCT_V12, SCTV12_L_2)
Radiation_V12 <-
  bind_rows(Radiation_V12, RadiationV12_L_2)

uid_V <- paste(unique(Vitals_V12$avatar_id), collapse = '|')
uid_A <- paste(unique(Alc_Smo_V12$avatar_id), collapse = '|')
uid_MM <- paste(unique(MM_history_V12$avatar_id), collapse = '|')
uid_T <- paste(unique(Treatment_V12$avatar_id), collapse = '|')
uid_P <- paste(c(unique(Progression_V12$avatar_id), unique(Progr_V12$avatar_id)), collapse = '|')
uid_S <- paste(unique(SCT_V12$avatar_id), collapse = '|')
uid_R <- paste(unique(Radiation_V12$avatar_id), collapse = '|')

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
  select(c("avatar_id",  "date_of_diagnosis", disease_stage = "disease_state"))
MM_historyV2 <- MM_historyV2[(!grepl(uid_MM, MM_historyV2$avatar_id)),]
#---
TreatmentV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "treatment_line_", "drug_start_date" , "drug_name_", "drug_stop_date",
           "drug_name_other", "treatment_site")) %>%
  unite(drug_name_, c(drug_name_,drug_name_other), sep = ": ", na.rm = TRUE, remove = TRUE)
Qcd_TreatmentV2 <-
  readxl::read_xlsx((paste0(ClinicalCap_V2, "/Avatar_MM_Clinical_Data_V2_modif_05042020.xlsx")),
                    sheet = "QC'd Treatment") %>%
  select(-relapse_date, -"...7")
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
  select(c("avatar_id", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))
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
  select(c("avatar_id", "treatment_line_", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other", "treatment_site")) %>%
  unite(drug_name_, c(drug_name_,drug_name_other), sep = ": ", na.rm = TRUE, remove = TRUE)
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
  select(c("avatar_id", metastasis_date = "mets_verify_date", have_metastasis = "initial_1_mets_1"))
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
  select(c("avatar_id", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))
#
Alc_SmoV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Comorbidities") %>%
  select(c("avatar_id","smoking_status", "alcohol_use"))
#
TreatmentV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", "treatment_line_", "drug_start_date", "drug_name_", "drug_stop_date",
           "drug_name_other_", "treatment_site")) %>%
  unite(drug_name_, c(drug_name_,drug_name_other_), sep = ": ", na.rm = TRUE, remove = TRUE)
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
  select(c("avatar_id", metastasis_date = "mets_verify_date", have_metastasis = "initial_1_mets_1"))
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
