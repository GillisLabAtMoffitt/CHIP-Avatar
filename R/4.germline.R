# Here we centered the data on the ones having a germline collection date

################################################################################# TABLE disease status year ####
head(germline_patient_data)
germline_patient_data$Disease_Status_germline
Disease_status_table <- table(germline_patient_data$Disease_Status_germline)
write.csv(Disease_status_table, paste0(path, "/Table germline disease status.csv"))

################################################################################# TABLE Year of germline sample collection ####
Amyloidosis_Diagnostic <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Amyloidosis") #2
Myelofib <- germline_patient_data %>% 
  filter(Disease_Status_germline == "MYELOFIBROSIS")    #1  
Normal_marrow <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Normal marrow") #2
Refractory_anemia <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Refractory anemia with ring sideroblasts")  #1                  
Solitary_Plasmacytoma <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Solitary Plasmacytoma")    #5              
Walderstrom <- germline_patient_data %>% 
  filter(Disease_Status_germline == "WALDENSTROM MACROGLOBULINEMIA") #1

disease_status_by_year <- matrix(
  c("group", "nbr of patients", "earliest date", "latest date","2011", "2012", "2013","2014","2015","2016","2017","2018","2019",
    
    "General", nrow(germline_patient_data), as.character(min(germline_patient_data$collectiondt_germline)), as.character(max(germline_patient_data$collectiondt_germline)), 
    sum(str_count(germline_patient_data$collectiondt_germline, "2011")),sum(str_count(germline_patient_data$collectiondt_germline, "2012")),sum(str_count(germline_patient_data$collectiondt_germline, "2013")),
    sum(str_count(germline_patient_data$collectiondt_germline, "2014")),sum(str_count(germline_patient_data$collectiondt_germline, "2015")),sum(str_count(germline_patient_data$collectiondt_germline, "2016")),
    sum(str_count(germline_patient_data$collectiondt_germline, "2015")),sum(str_count(germline_patient_data$collectiondt_germline, "2018")),sum(str_count(germline_patient_data$collectiondt_germline, "2019")),
    "Pre Treatment Newly Diagnosed", nrow(Pre_Treat), as.character(min(Pre_Treat$collectiondt_germline)),as.character(max(Pre_Treat$collectiondt_germline)),
    sum(str_count(Pre_Treat$collectiondt_germline, "2011")),sum(str_count(Pre_Treat$collectiondt_germline, "2012")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2013")),sum(str_count(Pre_Treat$collectiondt_germline, "2014")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2015")),sum(str_count(Pre_Treat$collectiondt_germline, "2016")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2017")),sum(str_count(Pre_Treat$collectiondt_germline, "2018")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2019")),
    "Post Treatment Newly Diagnosed", nrow(Post_Treat), as.character(min(Post_Treat$collectiondt_germline)),as.character(max(Post_Treat$collectiondt_germline)),
    sum(str_count(Post_Treat$collectiondt_germline, "2011")),sum(str_count(Post_Treat$collectiondt_germline, "2012")),
    sum(str_count(Post_Treat$collectiondt_germline, "2013")),sum(str_count(Post_Treat$collectiondt_germline, "2014")),
    sum(str_count(Post_Treat$collectiondt_germline, "2015")),sum(str_count(Post_Treat$collectiondt_germline, "2016")),
    sum(str_count(Post_Treat$collectiondt_germline, "2017")),sum(str_count(Post_Treat$collectiondt_germline, "2018")),
    sum(str_count(Post_Treat$collectiondt_germline, "2019")),
    "Amyloidosis-Diagnostic marrow", nrow(Amyloidosis_Diagnostic), as.character(min(Amyloidosis_Diagnostic$collectiondt_germline)),as.character(max(Amyloidosis_Diagnostic$collectiondt_germline)),
    sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2011")),sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2012")),
    sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2013")),sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2014")),
    sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2015")),sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2016")),
    sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2017")),sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2018")),
    sum(str_count(Amyloidosis_Diagnostic$collectiondt_germline, "2019")),
    "Early Relapse Multiple Myeloma", nrow(Early_Relapse), as.character(min(Early_Relapse$collectiondt_germline)),as.character(max(Early_Relapse$collectiondt_germline)),
    sum(str_count(Early_Relapse$collectiondt_germline, "2011")),sum(str_count(Early_Relapse$collectiondt_germline, "2012")),
    sum(str_count(Early_Relapse$collectiondt_germline, "2013")),sum(str_count(Early_Relapse$collectiondt_germline, "2014")),
    sum(str_count(Early_Relapse$collectiondt_germline, "2015")),sum(str_count(Early_Relapse$collectiondt_germline, "2016")),
    sum(str_count(Early_Relapse$collectiondt_germline, "2017")),sum(str_count(Early_Relapse$collectiondt_germline, "2018")),
    sum(str_count(Early_Relapse$collectiondt_germline, "2019")),
    "Late Relapse Multiple Myeloma", nrow(Late_Relapse), as.character(min(Late_Relapse$collectiondt_germline)),as.character(max(Late_Relapse$collectiondt_germline)),
    sum(str_count(Late_Relapse$collectiondt_germline, "2011")),sum(str_count(Late_Relapse$collectiondt_germline, "2012")),
    sum(str_count(Late_Relapse$collectiondt_germline, "2013")),sum(str_count(Late_Relapse$collectiondt_germline, "2014")),
    sum(str_count(Late_Relapse$collectiondt_germline, "2015")),sum(str_count(Late_Relapse$collectiondt_germline, "2016")),
    sum(str_count(Late_Relapse$collectiondt_germline, "2017")),sum(str_count(Late_Relapse$collectiondt_germline, "2018")),
    sum(str_count(Late_Relapse$collectiondt_germline, "2019")),
    "Mgus", nrow(Mgus), as.character(min(Mgus$collectiondt_germline)),as.character(max(Mgus$collectiondt_germline)),
    sum(str_count(Mgus$collectiondt_germline, "2011")),sum(str_count(Mgus$collectiondt_germline, "2012")),
    sum(str_count(Mgus$collectiondt_germline, "2013")),sum(str_count(Mgus$collectiondt_germline, "2014")),
    sum(str_count(Mgus$collectiondt_germline, "2015")),sum(str_count(Mgus$collectiondt_germline, "2016")),
    sum(str_count(Mgus$collectiondt_germline, "2017")),sum(str_count(Mgus$collectiondt_germline, "2018")),
    sum(str_count(Mgus$collectiondt_germline, "2019")),
    "MYELOFIBROSIS", nrow(Myelofib), as.character(min(Myelofib$collectiondt_germline)),as.character(max(Myelofib$collectiondt_germline)),
    sum(str_count(Myelofib$collectiondt_germline, "2011")),sum(str_count(Myelofib$collectiondt_germline, "2012")),
    sum(str_count(Myelofib$collectiondt_germline, "2013")),sum(str_count(Myelofib$collectiondt_germline, "2014")),
    sum(str_count(Myelofib$collectiondt_germline, "2015")),sum(str_count(Myelofib$collectiondt_germline, "2016")),
    sum(str_count(Myelofib$collectiondt_germline, "2017")),sum(str_count(Myelofib$collectiondt_germline, "2018")),
    sum(str_count(Myelofib$collectiondt_germline, "2019")),
    "Normal marrow", nrow(Normal_marrow), as.character(min(Normal_marrow$collectiondt_germline)),as.character(max(Normal_marrow$collectiondt_germline)),
    sum(str_count(Normal_marrow$collectiondt_germline, "2011")),sum(str_count(Normal_marrow$collectiondt_germline, "2012")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2013")),sum(str_count(Normal_marrow$collectiondt_germline, "2014")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2015")),sum(str_count(Normal_marrow$collectiondt_germline, "2016")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2017")),sum(str_count(Normal_marrow$collectiondt_germline, "2018")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2019")),
    "Refractory anemia with ring sideroblasts", nrow(Refractory_anemia), as.character(min(Refractory_anemia$collectiondt_germline)),as.character(max(Refractory_anemia$collectiondt_germline)),
    sum(str_count(Refractory_anemia$collectiondt_germline, "2011")),sum(str_count(Refractory_anemia$collectiondt_germline, "2012")),
    sum(str_count(Refractory_anemia$collectiondt_germline, "2013")),sum(str_count(Refractory_anemia$collectiondt_germline, "2014")),
    sum(str_count(Refractory_anemia$collectiondt_germline, "2015")),sum(str_count(Refractory_anemia$collectiondt_germline, "2016")),
    sum(str_count(Refractory_anemia$collectiondt_germline, "2017")),sum(str_count(Refractory_anemia$collectiondt_germline, "2018")),
    sum(str_count(Refractory_anemia$collectiondt_germline, "2019")),
    "Smoldering Multiple Myeloma", nrow(Smoldering), as.character(min(Smoldering$collectiondt_germline)),as.character(max(Smoldering$collectiondt_germline)),
    sum(str_count(Smoldering$collectiondt_germline, "2011")),sum(str_count(Smoldering$collectiondt_germline, "2012")),
    sum(str_count(Smoldering$collectiondt_germline, "2013")),sum(str_count(Smoldering$collectiondt_germline, "2014")),
    sum(str_count(Smoldering$collectiondt_germline, "2015")),sum(str_count(Smoldering$collectiondt_germline, "2016")),
    sum(str_count(Smoldering$collectiondt_germline, "2017")),sum(str_count(Smoldering$collectiondt_germline, "2018")),
    sum(str_count(Smoldering$collectiondt_germline, "2019")),
    "Solitary Plasmacytoma", nrow(Solitary_Plasmacytoma), as.character(min(Solitary_Plasmacytoma$collectiondt_germline)),as.character(max(Solitary_Plasmacytoma$collectiondt_germline)),
    sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2011")),sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2012")),
    sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2013")),sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2014")),
    sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2015")),sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2016")),
    sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2017")),sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2018")),
    sum(str_count(Solitary_Plasmacytoma$collectiondt_germline, "2019")),
    "WALDENSTROM MACROGLOBULINEMIA", nrow(Walderstrom), as.character(min(Walderstrom$collectiondt_germline)),as.character(max(Walderstrom$collectiondt_germline)),
    sum(str_count(Walderstrom$collectiondt_germline, "2011")),sum(str_count(Walderstrom$collectiondt_germline, "2012")),
    sum(str_count(Walderstrom$collectiondt_germline, "2013")),sum(str_count(Walderstrom$collectiondt_germline, "2014")),
    sum(str_count(Walderstrom$collectiondt_germline, "2015")),sum(str_count(Walderstrom$collectiondt_germline, "2016")),
    sum(str_count(Walderstrom$collectiondt_germline, "2017")),sum(str_count(Walderstrom$collectiondt_germline, "2018")),
    sum(str_count(Walderstrom$collectiondt_germline, "2019"))), ncol = 13, byrow=TRUE)

write.csv(disease_status_by_year,paste0(path, "/Germline Disease status classified by year of collection.csv"))

rm(
  Amyloidosis_Diagnostic,
  Myelofib,
  Normal_marrow,
  Refractory_anemia,
  Solitary_Plasmacytoma,
  Walderstrom,
  disease_status_by_year
)


##################################################################################################  I  ### Germline date VS other

#------------------------------------------------------------- Table
germline_patient_data[which(germline_patient_data$diag_BF_lastdate == "not good"), c("avatar_id", "date_of_diagnosis_1", "last_date_available")]

germline_compared_dates <-matrix(
  c("Category", "nbr in germline population", "comments",
    "birth date available", sum(!is.na(germline_patient_data$Date_of_Birth)), "",
    "diagnosis date available", sum(!is.na(germline_patient_data$date_of_diagnosis_1)),  "",
    "last date available", sum(!is.na(germline_patient_data$last_date_available)),  "", 
    "death date available", sum(!is.na(germline_patient_data$date_death)), "",
    "nbr of patients born before last date", sum(str_count(germline_patient_data$birth_BF_lastdate, "OK"), na.rm = TRUE), "",
    "nbr of patients diag before last date", sum(str_count(germline_patient_data$diag_BF_lastdate, "OK"), na.rm = TRUE), "2 patients present same date diagnosis/last day",
    "germline date available", sum(!is.na(germline_patient_data$collectiondt_germline)),  "",
    "drug date available", sum(!is.na(germline_patient_data$drug_start_date_1)),  "",
    "bmt1 date available", sum(!is.na(germline_patient_data$date_of_first_bmt)),  "",
    "rad date available", sum(!is.na(germline_patient_data$rad_start_date_1)),  "",
    "nbr of patients germline before drugs", sum(str_count(germline_patient_data$germlineBFdrugs, "OK"), na.rm = TRUE), "",
    "nbr of patients germline before bmt1", sum(str_count(germline_patient_data$germlineBFbmt1, "OK"), na.rm = TRUE), "",
    "nbr of patients germline before bmt2", sum(str_count(germline_patient_data$germlineBFbmt2, "OK"), na.rm = TRUE), "",
    "nbr of patients germline before bmt3", sum(str_count(germline_patient_data$germlineBFbmt3, "OK"), na.rm = TRUE),  "",
    "nbr of patients germline before radiation", sum(str_count(germline_patient_data$germlineBFrad1, "OK"), na.rm = TRUE), "",
    "nbr of patients germline before drugs and bmt1", sum(str_count(germline_patient_data$germBFdrugsbmt, "OK"), na.rm = TRUE),  "",
    "nbr of patients germline before drugs, bmt1 and radiation", sum(str_count(germline_patient_data$germBFdbr, "OK"), na.rm = TRUE), ""
    ),
  ncol = 3, byrow=TRUE)
germline_compared_dates <- as.table(germline_compared_dates)
germline_compared_dates
write.csv(germline_compared_dates, paste0(path, "/table compared germline dates and Demographics.csv"))

rm(germline_compared_dates)


#------------------------------------------------------------- Venn
germ_BF_drugs <- germline_patient_data[which(germline_patient_data$germlineBFdrugs =="OK"),]
germ_BF_bmt1 <- germline_patient_data[which(germline_patient_data$germlineBFbmt1 == "OK"),] # 375
germ_BF_drugsBMT <- germline_patient_data[which(germline_patient_data$germBFdrugsbmt == "OK"),]
germ_BF_rad <- germline_patient_data[which(germline_patient_data$germlineBFrad1 == "OK"),]
germ_BF_drug_bmt_rad <- germline_patient_data[which(germline_patient_data$germBFdbr == "OK"),]

venn.diagram(
  x = list(germ_BF_drugs$avatar_id, germ_BF_bmt1$avatar_id),
  category.names = c("Germline \nbefore drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced before or no drugs-and-BMT.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#2A406CFF", "#E95562FF"), # # , #2A406CFF = before bmt
  margin = 0.09,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(0, 180),
  cat.dist = c(-0.07, 0.025),
  cat.cex = .8,
  euler.d = FALSE,
  scaled = FALSE
)
venn.diagram( # Limiting to patients who strickly had bmt or drugs
  x = list(drug_patients$avatar_id, bmt_patients$avatar_id),
  category.names = c("Germline \nbefore drugs" , "Germline \nbefore BMT"),
  filename = 'Patient who had Germline sequenced strictly before drugs-and-BMT.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#2A406CFF", "#E95562FF"), # # , #2A406CFF = before bmt
  margin = 0.09,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  # cat.pos = c(0, 180),
  # cat.dist = c(-0.07, 0.025),
  cat.cex = .8
)


venn.diagram(
  x = list(germline_patient_data$avatar_id, germ_BF_drugs$avatar_id, germ_BF_bmt1$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT"),
  filename = 'Patient who had Germline sequenced before or no drugs-and-BMT in Total Germline population eulerF.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.16,
  
  # Numbers
  cex = .5,
  fontface = "bold",
  fontfamily = "sans",
  # cat.pos = c(0, 180),
  # cat.dist = c(-0.07, 0.025),
   
  # Set names
  cat.cex = .7,
  # cat.fontface = "bold",
  # cat.default.pos = "outer",
  cat.pos = c(-35, 35, 180), # germ, drugs, bmt
  cat.dist = c(0.1, 0.1, 0.05), # germ, drugs, bmt
  # cat.fontfamily = "sans"
  euler.d = FALSE,
  scaled = FALSE
)
venn.diagram( # Limiting to patients who strickly had bmt or drugs
  x = list(germline_patient_data$avatar_id, drug_patients$avatar_id, bmt_patients$avatar_id),
  category.names = c("Germline available", "Germline \nbefore drugs" , "Germline \nbefore BMT1"),
  filename = 'Patient who had Germline sequenced strickly before drugs-and-BMT in Total Germline population.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.09,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  # cat.pos = c(0, 180),
  # cat.dist = c(-0.07, 0.025),
  
  # Set names
  cat.cex = 0.8,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(220, 0, 120), # bmt, germ , drugs
  cat.dist = c(0.06, 0.05, 0.11)#, # germ, drugs, bmt
  # cat.fontfamily = "sans"
)



Mul_Myeloma <- germline_patient_data %>% 
      filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
               Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma" |
               Disease_Status_germline == "Early Relapse Multiple Myeloma" |
               Disease_Status_germline == "Late Relapse Multiple Myeloma")
# Smoldering <- germline_patient_data %>% 
#   filter(Disease_Status_germline == "Smoldering Multiple Myeloma")
# Mgus <- germline_patient_data %>% 
#   filter(Disease_Status_germline == "Mgus")
####### Var for germline before or no drugs-and-bmt
germ_BF_drugs_MM <- germ_BF_drugs %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Early Relapse Multiple Myeloma" |
           Disease_Status_germline == "Late Relapse Multiple Myeloma")
germ_BF_bmt1_MM <- germ_BF_bmt1 %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Early Relapse Multiple Myeloma" |
           Disease_Status_germline == "Late Relapse Multiple Myeloma")

germ_BF_drugs_mgus <- germ_BF_drugs %>% 
  filter(Disease_Status_germline == "Mgus")
germ_BF_bmt1_mgus <- germ_BF_bmt1 %>% 
  filter(Disease_Status_germline == "Mgus")

germ_BF_drugs_smoldering <- germ_BF_drugs %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma")
germ_BF_bmt1_smoldering <- germ_BF_bmt1 %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma")
####### Var for germline strickly before drugs-and-bmt
germ_stricBF_drugs_MM <- drug_patients %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Early Relapse Multiple Myeloma" |
           Disease_Status_germline == "Late Relapse Multiple Myeloma")
germ_stricBF_bmt1_MM <- bmt_patients %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Early Relapse Multiple Myeloma" |
           Disease_Status_germline == "Late Relapse Multiple Myeloma")

germ_stricBF_drugs_mgus <- drug_patients %>% 
  filter(Disease_Status_germline == "Mgus")
germ_stricBF_bmt1_mgus <- bmt_patients %>% 
  filter(Disease_Status_germline == "Mgus")

germ_stricBF_drugs_smoldering <- drug_patients %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma")
germ_stricBF_bmt1_smoldering <- bmt_patients %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma")


rm(drug_patients, bmt_patients, rad_patients)

########################################################################### Venn treatment before germline
# MM
venn.diagram(
  x = list(Mul_Myeloma$avatar_id, germ_BF_drugs_MM$avatar_id, germ_BF_bmt1_MM$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced before or no drugs-and-BMT in MM Germline population eulerF.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c(viridis::cividis(n=3)), # c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.15,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(-29, 43, 0), # germ , bmt, drugs
  # cat.dist = c(0.019, -0.13, 0.011), # germ, drugs, bmt
  # cat.fontfamily = "sans",
  rotation = 1,
  euler.d = FALSE,
  scaled = FALSE
)
venn.diagram(
  x = list(Mul_Myeloma$avatar_id, germ_stricBF_drugs_MM$avatar_id, germ_stricBF_bmt1_MM$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT"),
  filename = 'Patient who had Germline sequenced strickly before in MM Germline population eulerF.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.15,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(220, 0, 120), # bmt, germ , drugs
  # cat.dist = c(0.06, 0.055, 0.1), # germ, drugs, bmt
  # cat.fontfamily = "sans",
  euler.d = FALSE,
  scaled = FALSE
)

# MGUS
venn.diagram(
  x = list(Mgus$avatar_id, germ_BF_drugs_mgus$avatar_id, germ_BF_bmt1_mgus$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced before or no drugs-and-BMT in MGUS Germline population eulerF.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c(viridis::magma(n=3)), # c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.15,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  #cat.pos = c(-27, 153, 0), # germ , bmt, drugs
  #cat.dist = c(0.015, -0.025, 0.015), # germ, drugs, bmt
  # cat.fontfamily = "sans",
  euler.d = FALSE,
  scaled = FALSE
)
venn.diagram(
  x = list(Mgus$avatar_id, germ_stricBF_drugs_mgus$avatar_id, germ_stricBF_bmt1_mgus$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced strickly before drugs-and-BMT in MGUS Germline population.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c(viridis::magma(n=3)), # c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.15,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer"
  #cat.pos = c(-27, 153, 0), # germ , bmt, drugs
  #cat.dist = c(0.015, -0.025, 0.015), # germ, drugs, bmt
  # cat.fontfamily = "sans",
)

# Smoldering
venn.diagram(
  x = list(Smoldering$avatar_id, germ_BF_drugs_smoldering$avatar_id, germ_BF_bmt1_smoldering$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced before or no drugs-and-BMT in Smoldering Germline population eulerF.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c(viridis::cividis(n=3)), # c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.18,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  #cat.pos = c(-27, 153, 0), # germ , bmt, drugs
  #cat.dist = c(0.015, -0.025, 0.015), # germ, drugs, bmt
  cat.fontfamily = "sans",
  rotation = 1,
  euler.d = FALSE,
  scaled = FALSE
)
venn.diagram(
  x = list(Smoldering$avatar_id, germ_stricBF_drugs_smoldering$avatar_id, germ_stricBF_bmt1_smoldering$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced strickly before drugs-and-BMT in Smoldering Germline population eulerF.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 700 , 
  width = 700 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c(viridis::cividis(n=3)), # c("#FFEA46FF", "#2A406CFF", "#E95562FF"), # germ, before bmt, 
  margin = 0.15,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(0, 180, 0), # germ, drugs, bmt
  # cat.dist = c(0.018, 0, 0.003), # germ, drugs, bmt
  # cat.fontfamily = "sans",
  euler.d = FALSE,
  scaled = FALSE
)
#################################################################################################  II  ### Disease status at Germline collection

disease_stat_germVStreatment <- matrix(
  c("Disease_Status_germline", 
    "Pre Treatment Newly Diagnosed Multiple Myeloma",
    "Post Treatment Newly Diagnosed Multiple Myeloma",
    "Amyloidosis", 
    "Early Relapse Multiple Myeloma", 
    "Late Relapse Multiple Myeloma", 
    "Mgus", 
    "MYELOFIBROSIS", 
    "Normal marrow", 
    "Refractory anemia with ring sideroblasts", 
    "Smoldering Multiple Myeloma", 
    "Solitary Plasmacytoma", 
    "WALDENSTROM MACROGLOBULINEMIA", 
    "nbr of patients germline before drugs",
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Mgus"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status_germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE),
    "nbr of patients germline before bmt1",
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Mgus"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status_germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE),
    "nbr of patients germline before drugs and bmt1",
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Mgus"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status_germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE)),
  
  ncol = 4, byrow = FALSE)
disease_stat_germVStreatment <- as.table(disease_stat_germVStreatment)
write.csv(disease_stat_germVStreatment, paste0(path, "/Disease status in germline dates.csv"))


###########################################################################################################################################

temp <- germ_BF_drugs[(germ_BF_drugs$Disease_Status_germline == "Early Relapse Multiple Myeloma"), c("avatar_id", "Date_of_Birth", "date_of_diagnosis_1",
                                                                                             "date_of_first_bmt", "GermBFtumorWES",
                                                                                             "collectiondt_germline","Disease_Status_germline", 
                                                                                             "collectiondt_tumor_1", "Disease_Status_tumor_1",
                                                                                             "date_death", "date_last_follow_up",
                                                                                             "drug_start_date_1",
                                                                                             "rad_start_date_1", "versionMM_1")]
# write.csv(temp, paste0(path, "/temp file.csv"))
rm(temp)

# Here is a list of patients classified as “Pre-treatment” but who HAD received drugs before germline.
Pre_Treat_ <- germline_patient_data %>%
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma") %>%
  select(c("avatar_id", "collectiondt_germline",
           "drug_start_date_1", "germlineBFdrugs",
           "date_of_first_bmt", "germlineBFbmt1",
           "rad_start_date_1", "germlineBFrad1")) %>%
  arrange(germlineBFdrugs)

write.csv(Pre_Treat_, paste0(path, "/Pretreatment patient already treated.csv"))




######################## LIST OF PATIENTs
germ_BF_drugs <- germ_BF_drugs %>% # 228
  select(c("avatar_id", "Disease_Status_germline", "moffitt_sample_id_germline", "collectiondt_germline")) %>% 
  arrange(Disease_Status_germline)
write.csv(germ_BF_drugs, paste0(path, "/List patients for sequencing/list patients germline before drugs.csv"))

germ_BF_bmt1 <- germ_BF_bmt1 %>% 
  select(c("avatar_id", "Disease_Status_germline", "moffitt_sample_id_germline", "collectiondt_germline")) %>% 
  arrange(Disease_Status_germline)
write.csv(germ_BF_bmt1, paste0(path, "/List patients for sequencing/list patients germline before bmt.csv"))

germ_BF_rad <- germ_BF_rad %>% 
  select(c("avatar_id", "Disease_Status_germline", "moffitt_sample_id_germline", "collectiondt_germline")) %>% 
  arrange(Disease_Status_germline)
write.csv(germ_BF_rad, paste0(path, "/List patients for sequencing/list patients germline before radiation.csv"))

germ_BF_drugsBMT <- germ_BF_drugsBMT %>% 
  select(c("avatar_id", "Disease_Status_germline", "moffitt_sample_id_germline", "collectiondt_germline")) %>% 
  arrange(Disease_Status_germline)
write.csv(germ_BF_drugsBMT, paste0(path, "/List patients for sequencing/list patients germline before drugs and bmt.csv"))

germ_BF_drug_bmt_rad <- germ_BF_drug_bmt_rad %>% 
  select(c("avatar_id", "Disease_Status_germline", "moffitt_sample_id_germline", "collectiondt_germline")) %>% 
  arrange(Disease_Status_germline)
write.csv(germ_BF_drug_bmt_rad, paste0(path, "/List patients for sequencing/list patients germline before drugs, bmt and radiation.csv"))

# List when treatment was delivered
germ_stricBF_drugs_MM <- germ_stricBF_drugs_MM %>% # 115 patients
  select(c("avatar_id", "Disease_Status_germline", "moffitt_sample_id_germline", "collectiondt_germline")) %>% 
  arrange(Disease_Status_germline)
write.csv(germ_stricBF_drugs_MM, paste0(path, "/List patients for sequencing/list patients germline before drugs when drugs delivered.csv"))

germ_stricBF_bmt1_MM <- germ_stricBF_bmt1_MM %>% # 90 patients
  select(c("avatar_id", "Disease_Status_germline", "moffitt_sample_id_germline", "collectiondt_germline")) %>% 
  arrange(Disease_Status_germline)
write.csv(germ_stricBF_bmt1_MM, paste0(path, "/List patients for sequencing/list patients germline before bmt when bmt delivered.csv"))


rm(germ_stricBF_bmt1_mgus, germ_stricBF_bmt1_MM, germ_stricBF_bmt1_smoldering, germ_BF_drug_bmt_rad, 
  germ_stricBF_drugs_mgus, germ_stricBF_drugs_MM, germ_stricBF_drugs_smoldering, germ_BF_drugs, 
  germ_BF_bmt1_smoldering, germ_BF_bmt1_MM, germ_BF_bmt1_mgus,
  germ_BF_drugs_mgus, germ_BF_drugs_MM, germ_BF_drugs_smoldering)

#----------------- Smoldering patient list

# a <- germline_patient_data %>% filter(Disease_Status_germline == "Smoldering Multiple Myeloma") %>% 
#   select(avatar_id, collectiondt_germline, drug_start_date_1, germlineBFdrugs) %>% 
#   arrange(drug_start_date_1)
# write.csv(a, paste0(path, "/list smoldering patients who had drugs.csv"))
