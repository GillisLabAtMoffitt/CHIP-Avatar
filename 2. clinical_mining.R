# We have 512 unique patient IDs in Sequencing, does they match the treatment
# Treatment$avatar_id == Germline$avatar_id # No

# color_vir <- list(p2 <- c(viridis::plasma(n = 2)),
#                   p3 <- c(viridis::plasma(n = 3)),
#                   p4 <- c(viridis::plasma(n = 4)),
#                   m2 <- c(viridis::magma(n = 2)),
#                   m3 <- c(viridis::magma(n = 3)),
#                   m4 <- c(viridis::magma(n = 4)),
#                   i3 <- c(viridis::inferno(n = 3)),
#                   i4 <- c(viridis::inferno(n = 4)),
#                   c3 <- c(viridis::cividis(n = 3)),
#                   c4 <- c(viridis::cividis(n = 4)),
#                   c5 <- c(viridis::cividis(n = 5)))

venn.diagram(
  x = list(MM_history$avatar_id, Germline$avatar_id, Demo_RedCap_V4ish$avatar_id),
  category.names = c("Clinical data" , "Germline data", "Demographics data"),
  filename = 'Germline patients in Demographic and Clinical data.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 1000 , 
  width = 1000 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("darkgrey", "#FEA873FF", "#00204DFF"),# clin, germ, demo
  margin = 0.2,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(-30, 30, 180),
  cat.dist = c(0.09, 0.09, 0.05),
  ext.percent = 2
  #ext.percent = 5
)

# venn.diagram(
#   x = list(MM_history$avatar_id, Germline$avatar_id, WES$avatar_id, Demo_RedCap_V4ish$avatar_id),
#   category.names = c("Clinical data" , "Germline data" , "WES data", "Demographics data"),
#   filename = 'Germline, WES and Demo.png',
#   output=TRUE,
#   
#   # Output features
#   imagetype="png" ,
#   height = 1000 , 
#   width = 1000 , 
#   resolution = 300,
#   compression = "lzw",
#   
#   # Circles
#   lwd = 2,
#   lty = 'blank',
#   fill = color4,
#   margin = 0.2,
#   
#   # Numbers
#   cex = .6,
#   fontface = "bold",
#   fontfamily = "sans",
#   ext.percent = 5,
#   cat.pos = c(-38, 30, -30, 30),
#   cat.dist = c(0.28, 0.25, 0.15, 0.15)
# )

# Patient who had Drugs and BMT
venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id),
  category.names = c("Clinical data" , "Drugs" , "BMT"),
  filename = 'Patients treated with Drugs and or BMT in Clinical data.png',
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
  fill = c("darkgrey", "#FFEA46FF", "#C83E73FF"), # clin, drug, bmt "darkgrey", "#ED7953FF", "#A3307EFF"
  margin = 0.09,
  
  # Numbers 
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold", # names
  cat.default.pos = "outer",
  cat.pos = c(0, 0, 0),
  cat.dist = c(0.02, -0.015, -0.045),
  # cat.fontfamily = "sans",
  ext.text = TRUE,
  ext.percent = 100,
  ext.pos = 3
)

# Patient who had Drugs and BMT
venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, Radiation$avatar_id),
  category.names = c("Clinical data" , "Drugs" , "BMT", "Radiation"),
  filename = 'Patients treated with Drugs BMT Radiation in Clinical data.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 1000 , 
  width = 1000 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("darkgrey", "#FFEA46FF", "#C83E73FF", "#ED7953FF"), # clin, drugs, bmt, rad
  # older purple #0D0887FF darkbluegrey "#00204DFF" = clinical , salmon #ED7953FF = Drugs ,
  #  , yellow #F0F921FF = radiation
  margin = 0.2,
  #  lightgrey "#7C7B78FF" = yellow, yellow "#FFEA46FF" = germ
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  ext.percent = 5,
  cat.pos = c(-38, 30, -30, 30),
  cat.dist = c(0.28, 0.25, 0.13, 0.13) # clin, drug, bmt, rad
)

venn.diagram(
  x = list(MM_history$avatar_id, Germline$avatar_id),
  category.names = c("Clinical data" , "Germline data"),
  filename = 'Patient who had Germline sequenced.png',
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
  fill = c("darkgrey", "#FEA873FF"), # clin, germ
  # darkbluegrey "#00204DFF" = clinical , yellow #FFEA46FF = germ
  margin = 0.05,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(-20, 160),
  cat.dist = c(0.055, 0.055),
  #ext.percent = 2,
  rotation.degree = -90

)

venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, Germline$avatar_id),
  category.names = c("Clinical data" , "Drugs" , "BMT", "Germline data"),
  filename = 'Patient who had Drugs, BMT and Germline sequenced.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 1000 , 
  width = 1000 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("darkgrey", "#FFEA46FF", "#C83E73FF", "#FEA873FF"), # clin, drug, bmt, germ
  # darkbluegrey "#00204DFF" = clinical , salmon #ED7953FF = treat , 
  #  , yellow #FFEA46FF = germ
  margin = 0.2,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  ext.percent = 5,
  cat.pos = c(-38, 30, -30, 30),
  cat.dist = c(0.28, 0.25, 0.13, 0.13)
  
)

# venn.diagram(
#   x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, Germline$avatar_id, Radiation$avatar_id),
#   category.names = c("Clinical data" , "Drugs" , "BMT", "Germline data", "Radiation"),
#   filename = 'Patient who had Drugs, BMT, Radiation and Germline sequenced.png',
#   output=TRUE,
#   
#   # Output features
#   imagetype="png" ,
#   height = 1000 , 
#   width = 1000 , 
#   resolution = 300,
#   compression = "lzw",
#   
#   # Circles
#   lwd = 2,
#   lty = 'blank',
#   fill = c5,
#   # pink #B63679FF = treat
#   # darkbluegrey "#00204DFF" = clinical
#   margin = 0.2,
#   
#   # Numbers
#   cex = .6,
#   fontface = "bold",
#   fontfamily = "sans",
#   ext.percent = 5,
#   #cat.pos = c(-38, 30, -30, 30),
#   #cat.dist = c(0.28, 0.25, 0.15, 0.15)
# )

venn.diagram(
  x = list(Treatment$avatar_id, SCT$avatar_id, Germline$avatar_id),
  category.names = c("Drugs" , "BMT", "Germline data"),
  filename = 'Patient who had Drugs, BMT and Germline sequenced 2.png',
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
  fill = c("#FFEA46FF", "#C83E73FF", "#FEA873FF"), # drug, bmt, germ
  # lightgrey #000004FF  # salmon #ED7953FF = treat, Really light grey #7C7B78FF = BMT , #FFEA46FF = germ
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(195, 167, 0), # germ treat
  cat.dist = c(0.020, -0.035, 0.05), # x BMT germ
  cat.fontfamily = "sans"
)

library(RColorBrewer)
###################################################################################################  I  ## Venn 1
# Restart from the Global_data
# Who had BMT or/and drugs in the Germline available patient samples

# nbr of germline collection
NROW(germline_patient_data) #533
# nbr tcc id
NROW(which(!is.na(germline_patient_data$TCC_ID))) # 533
# nbr birth
NROW(which(!is.na(germline_patient_data$Date_of_Birth))) # 533
# nbr death
NROW(which(!is.na(germline_patient_data$date_death))) # 89
# nbr diag
NROW(which(!is.na(germline_patient_data$date_of_diagnosis_1))) # 532

# nbr had bmt1 
NROW(which(!is.na(germline_patient_data$date_of_first_bmt))) # 251
bmtINgerm <- germline_patient_data[!is.na(germline_patient_data$date_of_first_bmt),]
# nbr had drug1
NROW(which(!is.na(germline_patient_data$drug_start_date_1))) # 435
drugINgerm <- germline_patient_data[!is.na(germline_patient_data$drug_start_date_1),]
# nbr commun in bmt1 and drug
had_GERM_BMT_DRUGS <- germline_patient_data[!is.na(bmtINgerm$drug_start_date_1),] # 251
NROW(which(!is.na(drugINgerm$date_of_first_bmt))) # same
NROW(which(!is.na(bmtINgerm$drug_start_date_1)))


myCol1 <- brewer.pal(3, "Pastel1")
myCol2 <- brewer.pal(3, "Pastel2")

draw.triple.venn(nrow(germline_patient_data), 
                 nrow(bmtINgerm),
                 nrow(drugINgerm),
                 n12 = nrow(bmtINgerm), n23 = nrow(had_GERM_BMT_DRUGS),
                 n13 = nrow(drugINgerm), n123 = nrow(had_GERM_BMT_DRUGS),
                 category = c("all germline", "had BMT1", "had drugs"), 
                 # col = "transparent" make cercle line transparent
                 fill = myCol1, # circle filling color
                 # alpha = c(.2, .3, .3), # circle filling transparency 1 = solide
                 cex = 1, fontface = "bold", fontfamily = "sans",
                 cat.col = c("darkgreen", "red", "blue"), # label color
                 cat.pos = c(-25,5,25), cat.dist = c(0,0,0),
                 cat.cex = 1, cat.fontface = "bold")
rm(myCol1, myCol2)


###################################################################################################  I  ## Treatment

################### Radiation / BMT

bmt_patients <- germline_patient_data %>% 
  filter(!is.na(germline_patient_data$date_of_first_bmt))
rad_patients <- germline_patient_data %>% 
  filter(!is.na(germline_patient_data$rad_start_date_1))
drug_patients <- germline_patient_data %>% 
  filter(!is.na(germline_patient_data$drug_start_date_1))
a <- as.table(table(bmt_patients$Disease_Status_germline))
b <- as.table(table(rad_patients$Disease_Status_germline))
c <- as.table(table(drug_patients$Disease_Status_germline))
write.csv(a, paste0(path, "/BMT patient number per disease status.csv"))
write.csv(b, paste0(path, "/Radiation patient number per disease status.csv"))
write.csv(c, paste0(path, "/Drug patient number per disease status.csv"))

bmt_patients <- bmt_patients %>% 
  mutate(germlineBFbmt1 = case_when(
    collectiondt_germline > date_of_first_bmt  ~ "No",
    collectiondt_germline <= date_of_first_bmt  ~ "OK"
  )) %>% 
  filter(germlineBFbmt1 == "OK")

rad_patients <- rad_patients %>% 
  mutate(germlineBFrad1 = case_when(
    collectiondt_germline <= rad_start_date_1 ~ "OK",
    collectiondt_germline > rad_start_date_1 ~ "No"
  )) %>% 
  filter(germlineBFrad1 == "OK")

drug_patients <- drug_patients %>% 
  mutate(germlineBFdrugs = case_when(
    collectiondt_germline > drug_start_date_1 ~ "No",
    collectiondt_germline <= drug_start_date_1 ~ "OK"
  )) %>% 
  filter(germlineBFdrugs == "OK")

germ_before_treatment <- matrix(
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
    sum(str_count(drug_patients$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Mgus"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(drug_patients$Disease_Status_germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE),
    "nbr of patients germline before bmt",
    sum(str_count(bmt_patients$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Mgus"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(bmt_patients$Disease_Status_germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE),
    "nbr of patients germline before radiation",
    sum(str_count(rad_patients$Disease_Status_germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Mgus"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(rad_patients$Disease_Status_germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE)),
  
  ncol = 4, byrow = FALSE)
germ_before_treatment <- as.table(germ_before_treatment)
# write.csv(germ_before_treatment, paste0(path, "/germ_before_treatment when treatment happened.csv"))



# How many time paitents had KRd, VRd, Rd, DRd, Len, Len+dex by disease status?
germline_patient_data <- germline_patient_data %>% 
  mutate(drugs_first_regimen = case_when(
    (str_detect(drug_name__1, "Cafilzomib") |
       str_detect(drug_name__1, "Carfilzomib")) &
      (str_detect(drug_name__1, "lena") |
         str_detect(drug_name__1, "LENA") |
         str_detect(drug_name__1, "Lena")) &
      (str_detect(drug_name__1, "Dex") |
         str_detect(drug_name__1, "dex"))                   ~ "KRd",
    
    str_detect(drug_name__1, "Daratu") &
      (str_detect(drug_name__1, "lena") |
         str_detect(drug_name__1, "LENA") |
         str_detect(drug_name__1, "Lena")) &
      (str_detect(drug_name__1, "Dex") |
         str_detect(drug_name__1, "dex"))                ~ "DRd",
    
    str_detect(drug_name__1, "Bortezomib") &
      (str_detect(drug_name__1, "lena") |
         str_detect(drug_name__1, "LENA") |
         str_detect(drug_name__1, "Lena")) &
      (str_detect(drug_name__1, "Dex") |
         str_detect(drug_name__1, "dex"))                ~ "VRd",
    
    (str_detect(drug_name__1, "lena") |
       str_detect(drug_name__1, "LENA") |
       str_detect(drug_name__1, "Lena")) &
      (str_detect(drug_name__1, "Dex") |
         str_detect(drug_name__1, "dex"))                ~ "Rd",
    
    (str_detect(drug_name__1, "lena") |
       str_detect(drug_name__1, "LENA") |
       str_detect(drug_name__1, "Lena"))                 ~ "Len"
  ))
table(germline_patient_data$drugs_first_regimen)


Pre_Treat <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma")
Post_Treat <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma")
Early_Relapse <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Early Relapse Multiple Myeloma")
Late_Relapse <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Late Relapse Multiple Myeloma")
Smoldering <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma")
Mgus <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Mgus")

a <- matrix(c("drugs", "Pre MM", "Post MM", "ER MM", "LR MM", "MGUS", "Smoldering",
              "KRd", sum(Pre_Treat$drugs_first_regimen == "KRd", na.rm = TRUE), sum(Post_Treat$drugs_first_regimen == "KRd", na.rm = TRUE),
              sum(Early_Relapse$drugs_first_regimen == "KRd", na.rm = TRUE), sum(Late_Relapse$drugs_first_regimen == "KRd", na.rm = TRUE),
              sum(Mgus$drugs_first_regimen == "KRd", na.rm = TRUE), sum(Smoldering$drugs_first_regimen == "KRd", na.rm = TRUE),
              "DRd", sum(Pre_Treat$drugs_first_regimen == "DRd", na.rm = TRUE), sum(Post_Treat$drugs_first_regimen == "DRd", na.rm = TRUE),
              sum(Early_Relapse$drugs_first_regimen == "DRd", na.rm = TRUE), sum(Late_Relapse$drugs_first_regimen == "DRd", na.rm = TRUE),
              sum(Mgus$drugs_first_regimen == "DRd", na.rm = TRUE), sum(Smoldering$drugs_first_regimen == "DRd", na.rm = TRUE),
              
              "VRd", sum(Pre_Treat$drugs_first_regimen == "VRd", na.rm = TRUE)+2, sum(Post_Treat$drugs_first_regimen == "VRd", na.rm = TRUE),
              sum(Early_Relapse$drugs_first_regimen == "VRd", na.rm = TRUE), sum(Late_Relapse$drugs_first_regimen == "VRd", na.rm = TRUE),
              sum(Mgus$drugs_first_regimen == "VRd", na.rm = TRUE), sum(Smoldering$drugs_first_regimen == "VRd", na.rm = TRUE),
              
              "Rd", sum(Pre_Treat$drugs_first_regimen == "Rd", na.rm = TRUE), sum(Post_Treat$drugs_first_regimen == "Rd", na.rm = TRUE),
              sum(Early_Relapse$drugs_first_regimen == "Rd", na.rm = TRUE), sum(Late_Relapse$drugs_first_regimen == "Rd", na.rm = TRUE),
              sum(Mgus$drugs_first_regimen == "Rd", na.rm = TRUE), sum(Smoldering$drugs_first_regimen == "Rd", na.rm = TRUE),
              
              "Len", sum(Pre_Treat$drugs_first_regimen == "Len", na.rm = TRUE), sum(Post_Treat$drugs_first_regimen == "Len", na.rm = TRUE),
              sum(Early_Relapse$drugs_first_regimen == "Len", na.rm = TRUE), sum(Late_Relapse$drugs_first_regimen == "Len", na.rm = TRUE),
              sum(Mgus$drugs_first_regimen == "Len", na.rm = TRUE), sum(Smoldering$drugs_first_regimen == "DRd", na.rm = TRUE)
), ncol = 7, byrow = TRUE)
write.csv(a, paste0(path, "/table regimen type as first therapy.csv"))


treatment_number <- matrix(c(
  "Treatment", "PreMM", "PostMM", "ER MM", "LR MM", "MGUS", "Smoldering",
  "Drug", sum(!is.na(Pre_Treat$drug_start_date_1)), sum(!is.na(Post_Treat$drug_start_date_1)),
  sum(!is.na(Early_Relapse$drug_start_date_1)), sum(!is.na(Late_Relapse$drug_start_date_1)),
  sum(!is.na(Mgus$drug_start_date_1)), sum(!is.na(Smoldering$drug_start_date_1)),
  "Radiation", sum(!is.na(Pre_Treat$rad_start_date_1)), sum(!is.na(Post_Treat$rad_start_date_1)),
  sum(!is.na(Early_Relapse$rad_start_date_1)), sum(!is.na(Late_Relapse$rad_start_date_1)),
  sum(!is.na(Mgus$rad_start_date_1)), sum(!is.na(Smoldering$rad_start_date_1)),
  "SCT", sum(!is.na(Pre_Treat$date_of_first_bmt)), sum(!is.na(Post_Treat$date_of_first_bmt)),
  sum(!is.na(Early_Relapse$date_of_first_bmt)), sum(!is.na(Late_Relapse$date_of_first_bmt)),
  sum(!is.na(Mgus$date_of_first_bmt)), sum(!is.na(Smoldering$date_of_first_bmt)),
  "No treatment", sum(is.na(Pre_Treat$drug_start_date_1) & is.na(Pre_Treat$rad_start_date_1) & is.na(Pre_Treat$date_of_first_bmt)),
  sum(is.na(Post_Treat$drug_start_date_1) & is.na(Post_Treat$rad_start_date_1) & is.na(Post_Treat$date_of_first_bmt)),
  sum(is.na(Early_Relapse$drug_start_date_1) & is.na(Early_Relapse$rad_start_date_1) & is.na(Early_Relapse$date_of_first_bmt)),
  sum(is.na(Late_Relapse$drug_start_date_1) & is.na(Late_Relapse$rad_start_date_1) & is.na(Late_Relapse$date_of_first_bmt)),
  sum(is.na(Mgus$drug_start_date_1) & is.na(Mgus$rad_start_date_1) & is.na(Mgus$date_of_first_bmt)),
  sum(is.na(Smoldering$drug_start_date_1) & is.na(Smoldering$rad_start_date_1) & is.na(Smoldering$date_of_first_bmt))
), ncol = 7, byrow = TRUE)

# write.csv(treatment_number, paste0(path, "/Treatment of MM patients per disease status.csv"))


############ More about drugs

# All the drugs received at all time counted multiple time per patients
# drug_table <- as.data.table(table(treatment$drug_name_))
# write.csv(drug_table, paste0(path, "/drug in regimen.csv"))

# drugs <- treatment %>% pivot_wider(id_cols = NULL,
#                                    names_from = avatar_id,
#                                    names_prefix = "",
#                                    names_sep = "_",
#                                    names_repair = "check_unique",
#                                    values_from = drug_name_,
#                                    values_fill = TRUE,
#                                    values_fn = NULL
#                                    )

TREATM <- separate(treatment, drug_name_, paste("drug_name_", 1:7, sep="_"), sep = "; ", extra = "warn")
TREATME <- TREATM %>% 
  pivot_longer(cols = drug_name__1:ncol(TREATM),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE)
drug_table_2 <- as.data.table(table(TREATME$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_2, paste0(path, "/table alldrugs used at all time.csv"))

TREATMEN <- TREATME %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_3 <- as.data.table(table(TREATMEN$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_3, paste0(path, "/table alldrugs single used per patient.csv"))


regimen1 <- Treatment[, c("avatar_id", "drug_name__1")] %>% 
  separate(drug_name__1, paste("drug_name_", 1:7, sep="_"), sep = "; ", extra = "warn")

# Separate drugs by Pre-Post-Early-Late...
Pre_Treat <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma") %>% 
  select("avatar_id")

Pre_Treat <- merge.data.frame(Pre_Treat, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Pre_Treat$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as pre-treat in first regimen.csv"))

Post_Treat <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma") %>% 
  select("avatar_id")
Post_Treat <- merge.data.frame(Post_Treat, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Post_Treat$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as post-treat in first regimen.csv"))

Early_Relapse <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Early Relapse Multiple Myeloma") %>% 
  select("avatar_id")
Early_Relapse <- merge.data.frame(Early_Relapse, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Early_Relapse$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as early relapse in first regimen.csv"))

Late_Relapse <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Late Relapse Multiple Myeloma") %>% 
  select("avatar_id")
Late_Relapse <- merge.data.frame(Late_Relapse, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Late_Relapse$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as late relapse in first regimen.csv"))

Smoldering <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma") %>% 
  select("avatar_id")
Smoldering <- merge.data.frame(Smoldering, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Smoldering$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as smoldering in first regimen.csv"))

Mgus <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Mgus") %>% 
  select("avatar_id")
Mgus <- merge.data.frame(Mgus, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Mgus$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as mgus in first regimen.csv"))


regimen1 <- regimen1 %>% 
  pivot_longer(cols = drug_name__1:ncol(regimen1),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_4 <- as.data.table(table(regimen1$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_4, paste0(path, "/table drugs single used per patient in first regimen.csv"))




# Cleaning
rm(drug_table, Pre_Treat, Post_Treat, Early_Relapse, Late_Relapse, Smoldering, Mgus)
rm(TREATM, TREATME, TREATMEN)
