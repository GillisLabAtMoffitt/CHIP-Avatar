# import library
library(tidyverse)
library(data.table)

#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar", "MM avatar")
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
# Germline <- readxl::read_xlsx(paste0(path, 
#                                      "/Raghu MM/Germline data/Avatar_MM_03162021_OUT.xlsx")) %>% 
#   filter(DiseaseType == "Not applicable (germline)") %>% 
#   select(c("avatar_id", SLID_germline = "DNASequencingLibraryID", collectiondt_germline = "collectiondt", 
#            moffittSampleId_germline = "ORIENSpecimenID", Disease_Status_germline = "disease_status"))
# 
# # 1.3.Load Sequencing data ---------------------------------------------------------------------------------------
# WES_tumor <-
#   readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
#   select(c("avatar_id", "moffitt_sample_id", "collectiondt")) %>% 
#   `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", "collectiondt_tumor"))

# #---
# Sequencing <-
#   read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
#   select(c(
#     "SLID_germline", 
#     "SLID_tumor" , "moffitt_sample_id_tumor", 
#     "moffitt_sample_id_germline",
#     "BaitSet"))
# #---
# Seq_WES_Raghu <- 
#   readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V044.xlsx")) %>% 
#   select(c(avatar_id = "subject", 
#            "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline", 
#            "SLID_tumor" , "moffitt_sample_id_tumor", "collectiondt_tumor", 
#            "BaitSet"))
# # Keep
# # Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# # Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# # Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# # Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# # Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
# #---
# Sequencing2 <- # warning message due to a TRUE added in a num var by Raghu (he copy paste an extra patient)
#   readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V0441.xlsx")) %>% 
#   select(c(avatar_id = "subject",
#            "SLID_germline", "moffittSampleId_germline",
#            "collectiondt_germline",
#            "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor",
#            BaitSet = "baitSet"))
# #---
# Seq_WES_Raghu2 <- 
#   readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V045.xlsx")) %>% 
#   select(c(avatar_id = "subject", 
#            "SLID_germline", "moffittSampleId_germline", "collectiondt_germline", 
#            "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor", 
#            "BaitSet"))

WES_jan2022 <- readxl::read_xlsx(paste0(path, 
                                        "/data/WES/Moffitt_WES_V046_All.xlsx")) %>% 
  select(c(avatar_id, SLID_germline, collectiondt_germline, SLID_tumor, collectiondt_tumor,
           moffittSampleId_germline, moffittSampleId_tumor, moffittSampleId,
           DNASequencingLibraryID,
           mrn, Disease_Status))

# 1.4.Load Clinical data------------------------------------------------------------------------------------------
#---
Vitals <-
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "Vitals") %>%
  select(c(avatar_id, mrn, vital_status_old = "vital_status", "date_death", 
           date_of_last_contact, cause_death, vitals_review))
#---
MM_history <-
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c(avatar_id, mrn, first_contact_date : another_cancer))
#---
Comorbidities <-
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "Comorbidities") %>%
  select(-c(Patient_History_Comments : avatar_ctm_patient_h_v_1))
#---
Biopsy <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Biopsy") %>%
  select(c(avatar_id, mrn, biopsy_date))
#---
Tumor_marker <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Tumor_marker_flow") %>%
  select(c(avatar_id, mrn, tumor_marker_date))
#---
Performance <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Performance")
#---
Imaging <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Imaging") %>%
  select(c(avatar_id, mrn, imaging_date))
#---
Labs <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Labs") %>%
  select(c(avatar_id, mrn, lab_date = "lab_initial_date_1"))
#---
Metastasis <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Metastatic_Disease") %>%
  select(avatar_id, mrn, metastasis_date = "initial_1_mets_date",
         have_metastasis = "initial_1_mets_1",
         initial_1_mets_origin, initial_1_mets_site,
         mets_verify_date
         ) #%>% 
  # filter(have_metastasis == 1 | have_metastasis == 2)
#---
Staging <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Staging") %>%
  select(c(avatar_id, mrn, date_staging_results, 
           staging_type, staging_value,
           staging_comments))
Staging_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Staging_Germline_07272021.xlsx"),
                                 na = "NA") %>% 
  select("avatar_id", "collectiondt_germline", "Labs_Result_Date", "Final_Albumin", "Final_Beta2", "Final_LDH", "ISS") %>% 
  arrange(collectiondt_germline) %>% 
  distinct(avatar_id, .keep_all = TRUE) # remove the duplicate of patient 180
Diagnosis_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Staging_MM_Diagnosis_07262021_OUT.xlsx")) %>% 
  mutate(iss = str_replace(iss, pattern = "Unknown/Not Reported", replacement = NA_character_)) %>% 
  select(avatar_id, last_mrn = mrn, MM_date_dx = date_of_diagnosis, ISS_at_MMdx = iss)
EHR_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Myeloma_ISS_Sweta07302021.xlsx")) %>% 
  select("avatar_id", "date_of_MM_diagnosis", "ISS_EHR", "B2", "date_B2", "albumin", "date_albumin", "ISS_calculated")
#---
Cytogenetics <- 
  readxl::read_xlsx(paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx"),
                    sheet = "Biopsy_Cytogenetics")
#---
Treatment <-
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "Treatment") #%>%
  # select(c(avatar_id, mrn, treatment_line = "treatment_line_", 
  #          drug_start_date, drug_stop_date,
  #          drug_name = "drug_name_", drug_name_other = "drug_name_other_",
  #          drug_comments, relapse_date, regimen_num)) %>%
  # unite(drug_name, c(drug_name,drug_name_other), sep = ", ", na.rm = TRUE, remove = TRUE)
#---
SCT <-
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "SCT") %>%
  select(c(avatar_id, mrn, date_of_bmt,
           type_of_bmt, bmt_cell_source)) %>%  # can be different than first if duplicated from v1 or v2
  drop_na("date_of_bmt")
#---
Radiation <- 
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "Radiation") %>%
  filter(!is.na(radiation_primary_site)) %>% 
  select(c(avatar_id, mrn, radiation_line : rad_stop_date))
#---
Progression <-
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "Treatment_Outcomes") %>%
  select(c(avatar_id, mrn, progression_date = "initial_1_pd_date_1"), # For the non qc'd
         response_aml : response_solid_tumor)
 
Prog <-
  readxl::read_xlsx((paste0(here::here(), "/data/Avatar_MM_Clinical_Data.xlsx")),
                    sheet = "Treatment") %>%
  select(c("avatar_id", progression_date = "relapse_date", "QC")) # For the qc'd

# Others----
# OS_data <- readxl::read_xlsx(paste0(path, "/Raghu MM/Overall Survival/HRI_Last_Followupdata.xlsx")) %>% 
#   select(avatar_id = "germline_patient_data_avatar_id", "final_vitals", "Vital_Status_Date") %>% 
#   distinct()
#
# Staging_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Staging_Germline_07272021.xlsx"),
#                                  na = "NA") %>% 
#   select("avatar_id", "collectiondt_germline", "Labs_Result_Date", "Final_Albumin", "Final_Beta2", "Final_LDH", "ISS") %>% 
#   arrange(collectiondt_germline) %>% 
#   distinct(avatar_id, .keep_all = TRUE) # remove the duplicate of patient 180
# Diagnosis_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Staging_MM_Diagnosis_07262021_OUT.xlsx")) %>% 
#   mutate(iss = str_replace(iss, pattern = "Unknown/Not Reported", replacement = NA_character_)) %>% 
#   select(avatar_id, last_mrn = mrn, MM_date_dx = date_of_diagnosis, ISS_at_MMdx = iss)
# EHR_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Myeloma_ISS_Sweta07302021.xlsx")) %>% 
#   select("avatar_id", "date_of_MM_diagnosis", "ISS_EHR", "B2", "date_B2", "albumin", "date_albumin", "ISS_calculated")
# CH----
CHIP_status <- read_csv(paste0(path, "/Nancy's working files/CHcalls_12.10.20.csv")) %>% 
  mutate(CH_status = str_replace(CH_status, "No_CH", "No CH")) %>% 
  mutate(patient_germline_id = str_remove(patient_germline_id, "_normal"))
#
CHIP_tageted_seq <- readxl::read_xlsx(paste0(path, "/Nancy's working files/M4M_MM Avatar targeted_CH from 03.03.21 paired_for Christelle.xlsx")) %>% 
  mutate(CH_status_TS = ifelse(CH == 1, "CHIP", "No CHIP")) %>% 
  select(patient_id, CH_status_TS) %>% 
  arrange(desc(CH_status_TS)) %>% 
  distinct(patient_id, .keep_all = TRUE)
#
IMIDS_maintenance <- readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Maintainance_Regimen.xlsx"))
#
migration_patients <- readxl::read_xlsx(paste0(path, "/Raghu MM/Avatar List For Migration.xlsx"))
# Somatic mutations
# load(file = paste0(path, "/TumorMuts/Nancy650AF.DP.AF.rda"))
MMA <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/MMA in Avatar patients.xlsx"))
VitB12 <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Vit b12 in Avatar patients.xlsx"))

status_change <- read_csv((paste0(here::here(), "/data/patients need to change status_2020.csv")))
regimen_changed_id <- read_csv((paste0(here::here(), "/data/patients with a change of regimen name to VRd_2020.csv")))

patients_removed_nonMM <- read_csv((paste0(here::here(), "/data/ids to remove as not MM patients.csv")))

## End Loading