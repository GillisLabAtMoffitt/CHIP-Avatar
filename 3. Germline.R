


##################################################################################################  I  ### Germline date VS other

f <- Global_data %>% 
  mutate(check_birthBFlastdate = case_when(
    last_date_available > Date_of_Birth ~ "OK",
    last_date_available == Date_of_Birth |
      last_date_available < Date_of_Birth ~ "not good"
  )) %>% 
  mutate(check_birthBFdiag = case_when(
    date_of_diagnosis_1 > Date_of_Birth ~ "OK",
    date_of_diagnosis_1 == Date_of_Birth |
      date_of_diagnosis_1 > Date_of_Birth ~ "not good"
  )) %>% 
  mutate(check_diagBFlastdate = case_when(
    last_date_available > date_of_diagnosis_1 ~ "OK",
    last_date_available == date_of_diagnosis_1 |
      last_date_available < date_of_diagnosis_1~ "not good"
  ))%>% 
  mutate(check_diag_birthANDdeath = case_when(
    date_of_diagnosis_1 > Date_of_Birth &
      date_of_diagnosis_1 < last_date_available  ~ "OK"
  )) %>% 
  mutate(germlineBFdrugs = case_when(
    collectiondt_germline > drug_start_date_1 ~ ":(",
    collectiondt_germline <= drug_start_date_1 ~ "OK"
  )) %>% 
  mutate(germlineBFbmt1 = case_when(
    collectiondt_germline > date_of_first_bmt_1  ~ ":(",
    collectiondt_germline < date_of_first_bmt_1  ~ "OK",
    collectiondt_germline == date_of_first_bmt_1 ~ "same date"
  )) %>% 
  mutate(germlineBFbmt2 = case_when(
    collectiondt_germline > date_of_second_bmt_1  ~ ":(",
    collectiondt_germline < date_of_second_bmt_1  ~ "OK",
    collectiondt_germline == date_of_second_bmt_1 ~ "same date"
  )) %>% 
  mutate(germlineBFbmt3 = case_when(
    collectiondt_germline > date_of_third_bmt_1 ~ ":(",
    collectiondt_germline < date_of_third_bmt_1 ~ "OK",
    collectiondt_germline == date_of_third_bmt_1 ~ "same date"
  )) %>% 
  mutate(germlineBFrad1 = case_when(
    collectiondt_germline < rad_start_date_1 ~ "OK",
    collectiondt_germline > rad_start_date_1 ~ "No",
    collectiondt_germline == rad_start_date_1 ~ "same date"
  )) %>% 
  mutate(germlineBFrad2 = case_when(
    collectiondt_germline < rad_start_date_2 ~ "OK",
    collectiondt_germline > rad_start_date_2 ~ "No",
    collectiondt_germline == rad_start_date_2 ~ "same date"
  )) %>% 
  mutate(germBFdrugsbmt1 = case_when(
    collectiondt_germline <= drug_start_date_1 &
      collectiondt_germline < date_of_first_bmt_1  ~ "OK"
  )) %>% 
  mutate(germBFdbr = case_when(
    collectiondt_germline < drug_start_date_1 &
      collectiondt_germline < date_of_first_bmt_1 &
      collectiondt_germline < rad_start_date_1 ~ "OK"
  )) %>% 
  mutate(bmt1_BF_drug = case_when(
    date_of_first_bmt_1 < drug_start_date_1 ~ "OK",
    date_of_first_bmt_1 > drug_start_date_1 ~ "No"
  )) %>% 
  mutate(rad_BF_drugbmt1 = case_when(
    rad_start_date_1 < date_of_first_bmt_1 &
    rad_start_date_1 < drug_start_date_1 ~ "OK"
  )) %>% 
  mutate(GandBmt1BEFOREdrug = case_when(
    date_of_first_bmt_1 < drug_start_date_1 &
      collectiondt_germline < drug_start_date_1 ~ "OK"
  )) %>% 
  mutate(GermBFtumorWES = case_when(
    collectiondt_germline < collectiondt_tumor_1 ~ "Germ first",
    collectiondt_germline > collectiondt_tumor_1 ~ "tumorWES first",
    collectiondt_germline == collectiondt_tumor_1 ~ "same date"
  ))

write.csv(f, paste0(path, "/compared germline dates and Demographics.csv"))
a <- table(f$GermBFtumorWES)
barplot(a, main = "Frequency of collection date first observed", ylim = c(0,500))


#------------------------------------------------------------- Table


germline_compared_dates <-matrix(
  c("Category", "nbr", "comments",
    "birth date available", sum(!is.na(f$Date_of_Birth)), "",
    "diagnosis date available", sum(!is.na(f$date_of_diagnosis_1)),  "",
    "last date available", sum(!is.na(f$last_date_available)),  "", "death date available", sum(!is.na(f$date_death_1)), "",
    "nbr of patients born before last date", sum(str_count(f$check_birthBFlastdate, "OK"), na.rm = TRUE), "",
    "nbr of patients diag before last date", sum(str_count(f$check_diagBFlastdate, "OK"), na.rm = TRUE), "3 patients present same date diagnosis/last day",
    "germline date available", sum(!is.na(f$collectiondt_germline)),  "",
    "drug date available", sum(!is.na(f$drug_start_date_1)),  "",
    "bmt1 date available", sum(!is.na(f$date_of_first_bmt_1)),  "",
    "rad date available", sum(!is.na(f$rad_start_date_1)),  "",
    "nbr of patients germline before drugs", sum(str_count(f$germlineBFdrugs, "OK"), na.rm = TRUE), "6 patients same date. They would be pretreatment newly diagn",
    "nbr of patients germline before bmt1", sum(str_count(f$germlineBFbmt1, "OK"), na.rm = TRUE), "",
    "nbr of patients germline before bmt2", sum(str_count(f$germlineBFbmt2, "OK"), na.rm = TRUE), "",
    "nbr of patients germline before bmt3", sum(str_count(f$germlineBFbmt3, "OK"), na.rm = TRUE),  "",
    "nbr of patients germline before radiation", sum(str_count(f$germlineBFrad1, "OK"), na.rm = TRUE), "",
    "nbr of patients germline before drugs and bmt1", sum(str_count(f$germBFdrugsbmt1, "OK"), na.rm = TRUE),  "up to 42 if include =drug date",
    "nbr of patients germline before drugs, bmt1 and radiation", sum(str_count(f$germBFdbr, "OK"), na.rm = TRUE), ""
    ),
  ncol = 3, byrow=TRUE)
germline_compared_dates <- as.table(germline_compared_dates)
germline_compared_dates
# write.csv(germline_compared_dates, paste0(path, "/table compared germline dates and Demographics.csv"))

rm(a, germline_compared_dates)


#------------------------------------------------------------- Venn
germ_BF_drugs <- f[which(f$germlineBFdrugs =="OK"),]
germ_BF_bmt1 <- f[which(f$germlineBFbmt1 == "OK"),]
germ_BF_drugsBMT <- f[which(f$germBEFOREdrugsBMT == "OK"),]

venn.diagram(
  x = list(germ_BF_drugs$avatar_id, germ_BF_bmt1$avatar_id),
  category.names = c("Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced before drugs and BMT.png',
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
  fill = m2,
  margin = 0.09,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(-20, 165),
  cat.dist = c(0.045, 0.045),
  cat.cex = .8
)


# germ_BF_drugs <- f[which(f$germlineBFdrugs =="OK"),]
# germ_BF_bmt1 <- f[which(f$germlineBFbmt1 == "OK"),]
# germ_BF_drugsBMT <- f[which(f$germBEFOREdrugsBMT == "OK"),]
germ_available <-  f[which(!is.na(f$collectiondt_germline)),]

venn.diagram(
  x = list(germ_available$avatar_id, germ_BF_drugs$avatar_id, germ_BF_bmt1$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had Germline sequenced before drugs and BMT in Total Germline population.png',
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
  fill = c("lightblue", "#B63679FF", "red"),
  margin = 0.09,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-30, 27, 150),
  cat.dist = c(0.075, 0.045, 0.045),
  cat.fontfamily = "sans",
  rotation = 1
)
#################################################################################################  II  ### Disease status when Germline collection

# in f
colnames(f)


disease_stat_germVStreatment <- matrix(
  c("Disease_Status.germline", 
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
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Mgus"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugs$Disease_Status.germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE),
    "nbr of patients germline before bmt1",
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Mgus"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(germ_BF_bmt1$Disease_Status.germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE),
    "nbr of patients germline before drugs and bmt1",
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Pre Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Post Treatment Newly Diagnosed Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Amyloidosis"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Early Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Late Relapse Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Mgus"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "MYELOFIBROSIS"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Normal marrow"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Refractory anemia with ring sideroblasts"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Smoldering Multiple Myeloma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "Solitary Plasmacytoma"), na.rm = TRUE),
    sum(str_count(germ_BF_drugsBMT$Disease_Status.germline, "WALDENSTROM MACROGLOBULINEMIA"), na.rm = TRUE)),
  
  ncol = 4, byrow = FALSE)
disease_stat_germVStreatment <- as.table(disease_stat_germVStreatment)
disease_stat_germVStreatment

write.csv(disease_stat_germVStreatment, paste0(path, "/Disease status in germline dates.csv"))

###########################################################################################################################################

temp <- germ_BF_drugs[(germ_BF_drugs$Disease_Status.germline == "Early Relapse Multiple Myeloma"), c("avatar_id", "Date_of_Birth", "date_of_diagnosis_1",
                                                                                             "date_of_first_bmt_1", "GermBFtumorWES",
                                                                                             "collectiondt_germline","Disease_Status.germline", 
                                                                                             "collectiondt_1", "Disease_Status_1",
                                                                                             "date_death_1", "date_last_follow_up_1",
                                                                                             "date_last_follow_up_2", "drug_start_date_1",
                                                                                             "rad_start_date_1", "versionMM_1")]
write.csv(temp, paste0(path, "/temp file.csv"))

