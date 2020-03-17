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
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_Germl_v0.4.3_Disease_Classification_OUT01312020.xlsx"))
#-----------------------------------------------------------------------------------------------------------------
WES <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", "Disease_Status", "collectiondt"))
#-----------------------------------------------------------------------------------------------------------------
Sequencing <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c(
    "SLID_germline", "SLID_tumor" , "moffitt_sample_id_tumor", "moffitt_sample_id_germline",
    "BaitSet", "ClinicalSpecimenLinkage_WES.Batch", "moffitt_sample_id"))
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
# Will need to change smoker and alcohol number
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
  select(c("avatar_id", "number_drugs_regimen","regimen_start_date", "drug1_regimen", "drug2_regimen",
           "drug3_regimen", "drug4_regimen", "drug5_regimen", "drug6_regimen", "drug7_regimen")) #%>% 
  #mutate(drug_start_date = regimen_start_date) %>% 
  colnames(Treatment)[which(names(Treatment) == "regimen_start_date")] <- "drug_start_date"
#-----------------------------------------------------------------------------------------------------------------
SCT <-
  readxl::read_xlsx((paste0(ClinicalCap_V1, "/Avatar_MM_Clinical_Data_V1_OUT_02072020.xlsx")),
                    sheet = "SCT") %>%
  select(c("avatar_id", "prior_treatment", "number_of_bonemarrow_transplant","date_of_first_bmt", 
           "date_of_second_bmt", "date_of_third_bmt"))
#-----------------------------------------------------------------------------------------------------------------

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
  select(c("avatar_id", "treatment_line_", "drug_start_date" , "drug_name_"))
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
ComorbiditiesV4 <-
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
  select(c("avatar_id", "treatment_line_", "drug_start_date", "drug_name_"))
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
  barplot(
    height = cbind(
      "Clinical Data" = c(NROW(MM_history), NROW(MM_historyV2), NROW(MM_historyV4)),
      "Vitals" = c(NROW(Vitals), NROW(VitalsV2), NROW(VitalsV4)),
      "BMT" = c(NROW(SCT), NROW(SCTV2), NROW(SCTV4)),
      "Treatment" = c(NROW(Treatment), NROW(TreatmentV2), NROW(TreatmentV4)),
      "Radiation" = c(0, NROW(RadiationV2), NROW(RadiationV4))
    ),
    beside = FALSE,
    width = 1,
    ylim = c(0, 3000),
    col = c("purple", "orange", "yellow"),
    legend.text = c("version1", "version2", "version4"),
    args.legend = list(x = "top")
  )
##################################################################################################  II  ## Bind ### Align duplicated ID
MM_history <- bind_rows(MM_history, MM_historyV2, MM_historyV4, .id = "versionMM") %>%
  arrange(date_of_diagnosis)
MM_history <- dcast(setDT(MM_history), avatar_id ~ rowid(avatar_id), value.var = c("date_of_diagnosis", "disease_stage", "versionMM")) %>% 
  select(c("avatar_id", "date_of_diagnosis_1", "disease_stage_1", "date_of_diagnosis_2", "disease_stage_2", "date_of_diagnosis_3", "disease_stage_3",
           "date_of_diagnosis_4", "disease_stage_4", "versionMM_1", "versionMM_2", "versionMM_3", "versionMM_4"))
# write.csv(MM_history,paste0(path, "/MM_history simplify.csv"))
#-------------------------------------
Vitals <- bind_rows(Vitals, VitalsV2, VitalsV4, .id = "versionVit")
Vitals <- dcast(setDT(Vitals), avatar_id ~ rowid(avatar_id), value.var = c("vital_status", "date_death", "date_last_follow_up", "smoking_status",
                                                                           "current_smoker", "alcohol_use", "bmi_at_dx_v2"))%>%
  mutate(date_death_1 = coalesce(date_death_1, date_death_2)) %>% 
  mutate(date_last_follow_up_1 = coalesce(date_last_follow_up_2, date_last_follow_up_1)) %>%
  mutate(last_date_available = coalesce(date_death_1, date_last_follow_up_1)) %>% 
  arrange(last_date_available)

# write.csv(Vitals,paste0(path, "/Vitals simplify.csv"))
#-------------------------------------
SCT <- bind_rows(SCT, SCTV2, SCTV4, .id = "versionSCT") %>% 
  arrange(date_of_first_bmt) %>% 
  arrange(date_of_second_bmt) %>% 
  arrange(date_of_third_bmt)
SCT <- dcast(setDT(SCT), avatar_id ~ rowid(avatar_id), value.var = c("prior_treatment", "number_of_bonemarrow_transplant",
                                                                     "date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"))
# write.csv(SCT,paste0(path, "/SCT simplify.csv"))
#------------------------------------
Treatment <- bind_rows(Treatment, TreatmentV2, TreatmentV4, .id = "versionTreat") %>% 
  arrange(drug_start_date)
Treatment <- dcast(setDT(Treatment), avatar_id ~ rowid(avatar_id), value.var = c("number_drugs_regimen", "drug_start_date","drug_stop_date", "drug1_regimen",
                                                                                 "drug2_regimen", "drug3_regimen", "drug4_regimen", "drug5_regimen",
                                                                                 "drug6_regimen", "drug7_regimen", "treatment_line_", "drug_name_"))
# write.csv(Treatment,paste0(path, "/Treatment simplify.csv"))
#------------------------------------
Radiation <- bind_rows(RadiationV2, RadiationV4, .id = "versionRad") %>% 
  arrange(rad_start_date)
Radiation <- dcast(setDT(Radiation), avatar_id ~ rowid(avatar_id), value.var = 
                     c("rad_start_date", "rad_stop_date"))
#------------------------------------
# Cleaning
rm(ClinicalCap_V1, ClinicalCap_V2, ClinicalCap_V4, MM_historyV2, MM_historyV4, VitalsV2, VitalsV4, SCTV2, SCTV4, TreatmentV2, TreatmentV4,
   Comorbidities, ComorbiditiesV4, RadiationV2, RadiationV4)
# Plot
barplot(
  height = cbind(
    "Desease History" = NROW(MM_history),
    "Vitals" = NROW(Vitals),
    "BMT" = NROW(SCT),
    "Treatment" = NROW(Treatment),
    "Radiation" = NROW(Radiation)
  ),
  main = "Nbr of record in each file tab",
  sub = "tab",
  ylim = c(0, 700),
  col = "darkblue"
)

##################################################################################################  III  # Merge WES and Sequencing
# Are moffitt_sample_id are equal in WES and Sequencing ?
# Sequencing <- Sequencing[order(Sequencing$moffitt_sample_id),]
# WES <- WES[order(WES$moffitt_sample_id),]
# Sequencing$moffitt_sample_id == WES$moffitt_sample_id # =>>>>>>> YES

WES <-
  merge.data.frame(
    WES,
    Sequencing,
    by.x = "moffitt_sample_id",
    by.y = "moffitt_sample_id",
    all.x = TRUE,
    all.y = TRUE
  )
# rm(Sequencing)
#Align duplicate on same row (per date)-------------------------------------------
# Check for duplicate
colnames(WES)

duplicated(WES$moffitt_sample_id) # No duplicate
duplicated(WES$avatar_id) # has duplicate

WES <- WES[order(WES$collectiondt), ]
WES <- dcast(setDT(WES), avatar_id ~ rowid(avatar_id), value.var = c("moffitt_sample_id","Disease_Status",
                                                                     "collectiondt","SLID_germline", "SLID_tumor", "moffitt_sample_id_tumor",
                                                                     "moffitt_sample_id_germline", "BaitSet", "ClinicalSpecimenLinkage_WES.Batch"))
WES <-WES[, c("avatar_id","moffitt_sample_id_1", "Disease_Status_1", "collectiondt_1", "SLID_germline_1", "SLID_tumor_1", "moffitt_sample_id_tumor_1",
              "moffitt_sample_id_germline_1", "BaitSet_1", "ClinicalSpecimenLinkage_WES.Batch_1",
              "moffitt_sample_id_2", "Disease_Status_2", "collectiondt_2", "SLID_germline_2", "SLID_tumor_2", "moffitt_sample_id_tumor_2", 
              "moffitt_sample_id_germline_2", "BaitSet_2", "ClinicalSpecimenLinkage_WES.Batch_2", 
              "moffitt_sample_id_3", "Disease_Status_3", "collectiondt_3", "SLID_germline_3", "SLID_tumor_3", "moffitt_sample_id_tumor_3",
              "moffitt_sample_id_germline_3", "BaitSet_3", "ClinicalSpecimenLinkage_WES.Batch_3",
              "moffitt_sample_id_4", "Disease_Status_4", "collectiondt_4", "SLID_germline_4", "SLID_tumor_4", "moffitt_sample_id_tumor_4",
              "moffitt_sample_id_germline_4", "BaitSet_4", "ClinicalSpecimenLinkage_WES.Batch_4",
              "moffitt_sample_id_5", "Disease_Status_5", "collectiondt_5", "SLID_germline_5", "SLID_tumor_5", "moffitt_sample_id_tumor_5",
              "moffitt_sample_id_germline_5","BaitSet_5", "ClinicalSpecimenLinkage_WES.Batch_5",
              "moffitt_sample_id_6", "Disease_Status_6", "collectiondt_6", "SLID_germline_6", "SLID_tumor_6", "moffitt_sample_id_tumor_6",
              "moffitt_sample_id_germline_6", "BaitSet_6", "ClinicalSpecimenLinkage_WES.Batch_6")]
WES  <- WES[order(WES$collectiondt_1), ] %>%
  arrange(collectiondt_2) %>%
  arrange(collectiondt_3) %>%
  arrange(collectiondt_4) %>%
  arrange(collectiondt_5) %>%
  arrange(collectiondt_6)
# write.csv(WES,paste0(path, "/WES germline tumor.csv"))
is.na(Germ$collectiondt.germline)
colnames(Germ)
Germ <- Germ[, c("avatar_id","moffitt_sample_id","collectiondt","SpecimenType","WES_HUDSON_ALPHA",
                 "DNASpecimenStatus1", "Disease_Status")] %>% 
  `colnames<-`(c("avatar_id","moffitt_sample_id.germline","collectiondt.germline","SpecimenType.germline","WES_HUDSON_ALPHA.germline",
                 "DNASpecimenStatus1.germline", "Disease_Status.germline"))

# Merge with Organized_WES
Combined_data_MM <- merge.data.frame(WES, Germ, 
                                     by.x = "avatar_id", by.y = "avatar_id", 
                                     all.x = TRUE, all.y = TRUE)
colnames(Combined_data_MM)
# write.csv(Combined_data_MM, paste0(path, "/Combined data and dates MM.csv"))
# I checked the ID they are all the same no missing nor added



##################################################################################################  IV  ## Merge
b <- merge.data.frame(Combined_data_MM[, c("avatar_id", "collectiondt.germline", "Disease_Status.germline", 
                                           "collectiondt_1", "Disease_Status_1")],
                      MM_history, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = TRUE, all.y = FALSE, suffixes = c(".x",".y"))

c <- merge.data.frame(b, Vitals, by.x = "avatar_id", by.y = "avatar_id", 
                      all.x = FALSE, all.y = FALSE, suffixes = c(".x",".y"))

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
                    
                    "collectiondt.germline", "Disease_Status.germline", "collectiondt_1", "Disease_Status_1",
                    
                    "date_death_1", "date_death_2",
                    "date_last_follow_up_1", "date_last_follow_up_2", "vital_status_1", "vital_status_2", 
                    
                    "prior_treatment_1", "prior_treatment_2",
                    "drug_start_date_1",
                    
                    "rad_start_date_1", "rad_start_date_2", "rad_stop_date_1", "rad_stop_date_2",                  
                    
                    "smoking_status_1", "smoking_status_2", "current_smoker_1", "current_smoker_2", "alcohol_use_1", "alcohol_use_2",
                    
                    "bmi_at_dx_v2_1", "Gender", "Ethnicity", "Race", "versionMM_1")]

