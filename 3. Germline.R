library(VennDiagram)


##################################################################################################  I  ### Germline date VS other

f <- f %>% 
  mutate(check_birthBFdeath = case_when(
    date_death_1 > Date_of_Birth |
      date_last_follow_up_1 > Date_of_Birth ~ "OK"
  )) %>% 
  mutate(check_diagBF_birthANDdeath = case_when(
    date_of_diagnosis_1 > Date_of_Birth &
      (date_of_diagnosis_1 < date_death_1 |
         date_of_diagnosis_1 < date_last_follow_up_1) ~ "OK!!"
  )) %>% 
  mutate(germlineBFdrugs = case_when(
    collectiondt.germline > drug_start_date_1 ~ ":(",
    collectiondt.germline < drug_start_date_1 ~ "OK"
  )) %>% 
  mutate(germlineBFbmt1 = case_when(
    collectiondt.germline > date_of_first_bmt_1  ~ ":(",
    collectiondt.germline < date_of_first_bmt_1  ~ "OK",
  )) %>% 
  mutate(germlineBFbmt2 = case_when(
    collectiondt.germline > date_of_second_bmt_1  ~ ":(",
    collectiondt.germline < date_of_second_bmt_1  ~ "OK",
  )) %>% 
  mutate(germlineBFbmt3 = case_when(
    collectiondt.germline > date_of_third_bmt_1 ~ ":(",
    collectiondt.germline < date_of_third_bmt_1 ~ "OK",
  )) %>% 
  mutate(germBEFOREdrugsBMT1 = case_when(
    collectiondt.germline < drug_start_date_1 &
      collectiondt.germline < date_of_first_bmt_1  ~ "OK"
  )) %>% 
  mutate(bmt1_BF_treat = case_when(
    date_of_first_bmt_1 < drug_start_date_1 ~ "OK",
    date_of_first_bmt_1 > drug_start_date_1 ~ "No"
  )) %>% 
  mutate(GandBmt1BEFOREdrug = case_when(
    date_of_first_bmt_1 < drug_start_date_1 &
      collectiondt.germline < drug_start_date_1 ~ "OK"
  )) %>% 
  mutate(GermBFtumorWES = case_when(
    collectiondt.germline < collectiondt_1 ~ "Germ first",
    collectiondt.germline > collectiondt_1 ~ "tumorWES first"
  ))
# write.csv(f, paste0(path, "/compared germline dates and Demographics.csv"))
table(f$GermBFtumorWES)
#------------------------------------------------------------- Table

f$last_date_deathorfollowup  <-  coalesce(f$date_death_1, f$date_last_follow_up_1)
b <- f$last_date_deathorfollowup[!is.na(f$last_date_deathorfollowup)]
c <- f$Date_of_Birth[!is.na(f$Date_of_Birth)]
d <- f$drug_start_date_1[!is.na(f$drug_start_date_1)]

germline_compared_dates <-matrix(
  c("nbr of patients born before death", "nbr of patients diag before death", "nbr of patients germline before drugs",
    "nbr of patients germline before bmt1","nbr of patients germline before bmt2", "nbr of patients germline before bmt3",
    "nbr of patients germline before drugs and bmt1", "last date available", "birth date available","drug date available",
    sum(str_count(f$check_birthBFdeath, "OK"), na.rm = TRUE),sum(str_count(f$check_diagBF_birthANDdeath, "OK"), na.rm = TRUE),
    sum(str_count(f$germlineBFdrugs, "OK"), na.rm = TRUE),
    sum(str_count(f$germlineBFbmt1, "OK"), na.rm = TRUE),sum(str_count(f$germlineBFbmt2, "OK"), na.rm = TRUE),
    sum(str_count(f$germlineBFbmt3, "OK"), na.rm = TRUE),sum(str_count(f$germBEFOREdrugsBMT, "OK"), na.rm = TRUE), NROW(b),
    NROW(c),NROW(d)),
  ncol = 10, byrow=TRUE)
germline_compared_dates <- as.table(table)
germline_compared_dates
# write.csv(germline_compared_dates, paste0(path, "table compared germline dates and Demographics.csv"))

rm(b,c,d,e, germline_compared_dates)


#------------------------------------------------------------- Venn
colors2 <- c(viridis::magma(n = 2))
germ_BF_drugs <- f[which(f$germlineBFdrugs =="OK"),]
germ_BF_bmt1 <- f[which(f$germlineBFbmt1 == "OK"),]
germ_BF_drugsBMT <- f[which(f$germBEFOREdrugsBMT == "OK"),]

venn.diagram(
  x = list(germ_BF_drugs$avatar_id, germ_BF_bmt1$avatar_id),
  category.names = c("Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had germlime sequenced before drugs and BMT.png',
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
  fill = colors2,
  margin = 0.09,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  cat.pos = c(-20, 165),
  cat.dist = c(0.045, 0.045),
  cat.cex = .8
)

color3 <- c(viridis::magma(n = 3))
germ_BF_drugs <- f[which(f$germlineBFdrugs =="OK"),]
germ_BF_bmt1 <- f[which(f$germlineBFbmt1 == "OK"),]
germ_BF_drugsBMT <- f[which(f$germBEFOREdrugsBMT == "OK"),]
germ_available <-  f[which(!is.na(f$collectiondt.germline)),]

venn.diagram(
  x = list(germ_available$avatar_id, germ_BF_drugs$avatar_id, germ_BF_bmt1$avatar_id),
  category.names = c("Germline available", "Germline before drugs" , "Germline before BMT1"),
  filename = 'Patient who had germlime sequenced before drugs and BMT in Total Germline population.png',
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

# write.csv(disease_stat_germVStreatment, paste0(path, "/Disease status in germline dates.csv"))

###########################################################################################################################################

temp <- germ_BF_drugs[(germ_BF_drugs$Disease_Status.germline == "Early Relapse Multiple Myeloma"), c("avatar_id", "Date_of_Birth", "date_of_diagnosis_1",
                                                                                             "date_of_first_bmt_1", 
                                                                                             "collectiondt.germline","Disease_Status.germline", 
                                                                                             "collectiondt_1", "Disease_Status_1",
                                                                                             "date_death_1", "date_last_follow_up_1",
                                                                                             "date_last_follow_up_2", "drug_start_date_1")]
write.csv(temp, paste0(path, "/temp file.csv"))


