################################################################################################## I ## Global data mining ----
# tbl <- 
  Global_data  %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "MM Avatar patients") %>% 
  select(Age_at_diagnosis_closest_germline, Gender, Race, Ethnicity, Whole, ISS) %>%
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Race) ~ 2)) %>% 
  bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics in MM Avatar patients.pdf"))

# tbl <- 
  Global_data  %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "MM Avatar patients") %>% 
  select(Age_at_diagnosis_closest_germline, Gender, Race, Ethnicity, Disease_Status_facet, ISS) %>% 
  mutate(Disease_Status_facet = forcats::fct_explicit_na(Disease_Status_facet)) %>% 
  tbl_summary(by = Disease_Status_facet, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Race) ~ 2)) %>% add_p() %>% 
  bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics in MM Avatar patients by DS with missing.pdf"))

Global_data  %>% 
  distinct(avatar_id, .keep_all = TRUE) %>%
  select(avatar_id, starts_with("SLID_tumor")) %>% 
  pivot_longer(cols = -avatar_id, names_to = "SLID_tumor", names_prefix = "SLID_tumor_", values_to = "value") %>% 
  filter(!is.na(value)) %>%
  arrange(desc(SLID_tumor)) %>% distinct(avatar_id, .keep_all = TRUE) %>% 
  group_by(SLID_tumor) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x=SLID_tumor, y = count))+
  geom_bar(stat="identity")+
  labs(x = "Tumor sequenced", title = "Nbr of tumor sequenced patients")+
  geom_text(aes(label = paste0("n=", count)), size = 3, hjust = "inward", position = position_stack(vjust = 1.08))+
  theme_minimal()+
  coord_flip()












################################################################################################## II ## Germline data mining ----


# tbl <- 
  germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(ISS, Whole) %>% 
  tbl_summary(by = Whole) %>% bold_labels() %>% as_gt() %>%
  gt::tab_style(
    style = gt::cell_borders(
      sides = c("top", "bottom"),
      color = "#BBBBBB",
      weight = 1.5,
      style = "solid"
    ),
    locations = gt::cells_body(
      columns = everything(),
      rows = everything()
    )
  )
gt::gtsave(tbl, zoom = .4, paste0(path, "/Figures/ISS/ISS staging in germline patients.pdf"))

# tbl <- 
  germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, Gender, Race, Ethnicity, Whole, ISS) %>%
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Race) ~ 2)) %>% 
  bold_labels() %>% as_gt() %>% gt::tab_options(column_labels.border.bottom.color = "#A92E5EFF",
                                                table_body.border.bottom.color = "#A92E5EFF")
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics in germline patients.pdf"))

# tbl <- 
  germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(Age_at_diagnosis_closest_germline, Gender, Race, Ethnicity, Disease_Status_facet, ISS
    ) %>% 
  tbl_summary(by = Disease_Status_facet, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Race) ~ 2)) %>% add_p() %>% 
  bold_labels() %>% as_gt() %>% gt::tab_options(table.font.color = "#A92E5EFF")
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics in germline patients by DS no missing.pdf"))

# tbl <- 
  germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(Age_at_diagnosis_closest_germline, Gender, Race, Ethnicity, Disease_Status_facet, ISS) %>% 
  mutate(Disease_Status_facet = forcats::fct_explicit_na(Disease_Status_facet)) %>% 
  mutate(Race = str_replace(Race, "Asian|More than one race|Am Indian", "Others")) %>% 
  tbl_summary(by = Disease_Status_facet, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Race) ~ 2)) %>% add_p() %>% 
  bold_labels() %>% as_gt() %>% gt::tab_options(table.font.size = 14, data_row.padding = gt::px(1))
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics race simplified in germline patients by DS with missing.pdf"))

germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(CH_status) %>% 
  tbl_summary(sort = list(everything() ~ "frequency")) %>% 
  bold_labels() %>% as_gt()  %>%
  gt::tab_style(
    style = gt::cell_text(
      color = "#0099CC"
    ),
    locations = gt::cells_column_labels(everything())
  ) 

# tbl <- 
  germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(Age_at_diagnosis_closest_germline, Gender, Race, Ethnicity, CH_status, ISS) %>% 
  mutate(Race = str_replace(Race, "Unknown", NA_character_)) %>%
  mutate(Ethnicity = str_replace(Ethnicity, "Unknown", NA_character_)) %>%
  mutate(Race = str_replace(Race, "Asian|More than one race|Am Indian", "Others")) %>% 
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Race) ~ 2)) %>% add_p() %>% add_overall() %>% add_stat_label() %>% 
  bold_labels() %>% as_gt()  %>%
    gt::tab_style(
      style = gt::cell_text(
        # size = px(11),
        color = "#0099CC"#,
        # font = "arial",
        # transform = "uppercase"
      ),
      locations = gt::cells_column_labels(everything())
    ) 
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics by CH simplified race in germline patients.pdf"))

germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Disease_Status_germline, Whole) %>% 
  tbl_summary(by = Whole) %>% bold_labels() %>% as_gt()  %>%
  gt::tab_style(
    style = gt::cell_text(
      color = "#0099CC"
    ),
    locations = gt::cells_column_labels(everything())
  ) 

germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(CH_status, Disease_Status_germline) %>% 
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency")) %>% add_p() %>% 
  bold_labels() %>% as_gt()




# We have 512 unique patient IDs in Sequencing, does they match the treatment
# Treatment$avatar_id == germline_patient_data$avatar_id # No


venn.diagram(
  x = list(MM_history$avatar_id, germline_patient_data$avatar_id, Demo_RedCap_V4ish$avatar_id),
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
#   x = list(MM_history$avatar_id, germline_patient_data$avatar_id, WES$avatar_id, Demo_RedCap_V4ish$avatar_id),
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
  x = list(MM_history$avatar_id, germline_patient_data$avatar_id),
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
ISS1 <- germline_patient_data %>% filter(ISS == "I") %>% select(avatar_id)
ISS2 <- germline_patient_data %>% filter(ISS == "II") %>% select(avatar_id)
ISS3 <- germline_patient_data %>% filter(ISS == "III") %>% select(avatar_id)
venn.diagram(
  x = list(germline_patient_data$avatar_id, 
           ISS1$avatar_id, ISS2$avatar_id, ISS3$avatar_id),
  category.names = c("Germline data", "ISS-I", "ISS-II", "ISS-III"),
  filename = 'ISS Patient who had Germline sequenced.png',
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
  fill = c("#FEA873FF", "lightblue", "pink", "lightgreen"), # clin, germ
  # darkbluegrey "#00204DFF" = clinical , yellow #FFEA46FF = germ
  margin = 0.05,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans"#,
  # cat.pos = c(-20, 160),
  # cat.dist = c(0.055, 0.055),
  #ext.percent = 2,
  # rotation.degree = -90
  
)

venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, germline_patient_data$avatar_id),
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
#   x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, germline_patient_data$avatar_id, Radiation$avatar_id),
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
  x = list(Treatment$avatar_id, SCT$avatar_id, germline_patient_data$avatar_id),
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
NROW(which(!is.na(germline_patient_data$date_of_diagnosis))) # 532

# nbr had bmt1 
NROW(which(!is.na(germline_patient_data$date_of_bmt_1))) # 251
bmtINgerm <- germline_patient_data[!is.na(germline_patient_data$date_of_bmt_1),]
# nbr had drug1
NROW(which(!is.na(germline_patient_data$drug_start_date_1))) # 435
drugINgerm <- germline_patient_data[!is.na(germline_patient_data$drug_start_date_1),]
# nbr commun in bmt1 and drug
had_GERM_BMT_DRUGS <- bmtINgerm[!is.na(bmtINgerm$drug_start_date_1),] # 251
NROW(which(!is.na(drugINgerm$date_of_bmt_1))) # same
NROW(which(!is.na(bmtINgerm$drug_start_date_1)))


draw.triple.venn(nrow(germline_patient_data), 
                 nrow(bmtINgerm),
                 nrow(drugINgerm),
                 n12 = nrow(bmtINgerm), n23 = nrow(had_GERM_BMT_DRUGS),
                 n13 = nrow(drugINgerm), n123 = nrow(had_GERM_BMT_DRUGS),
                 category = c("all germline", "had BMT1", "had drugs"), 
                 # col = "transparent" make cercle line transparent
                 # fill = myCol1, # circle filling color
                 # alpha = c(.2, .3, .3), # circle filling transparency 1 = solide
                 cex = 1, fontface = "bold", fontfamily = "sans",
                 cat.col = c("darkgreen", "red", "blue"), # label color
                 cat.pos = c(-25,5,25), cat.dist = c(0,0,0),
                 cat.cex = 1, cat.fontface = "bold")

rm(bmtINgerm, drugINgerm, had_GERM_BMT_DRUGS)

###################################################################################################  I  ## Treatment

################### Radiation / BMT

bmt_patients <- germline_patient_data %>% 
  filter(!is.na(germline_patient_data$date_of_bmt_1))
rad_patients <- germline_patient_data %>% 
  filter(!is.na(germline_patient_data$rad_start_date_1))
drug_patients <- germline_patient_data %>% 
  filter(!is.na(germline_patient_data$drug_start_date_1))
a <- as.table(table(bmt_patients$Disease_Status_germline))
write.csv(a, paste0(path, "/BMT patient number per disease status.csv"))
a <- as.table(table(rad_patients$Disease_Status_germline))
write.csv(a, paste0(path, "/Radiation patient number per disease status.csv"))
a <- as.table(table(drug_patients$Disease_Status_germline))
write.csv(a, paste0(path, "/Drug patient number per disease status.csv"))


bmt_patients <- bmt_patients %>%
  filter(germlineBFbmt1 == "OK")

rad_patients <- rad_patients %>%
  filter(germlineBFrad1 == "OK")

drug_patients <- drug_patients %>%
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

################### Drugs
write_csv(germline_patient_data %>% 
            select(avatar_id, drug_name__1, drug_name_1_for_MM, drug_count, first_regimen_name), 
          paste0(path, "/Figures/Treatment/regimen name in germline.csv"))

# tbl <- germline_patient_data %>% count(first_regimen_name) %>% arrange(desc(n))
write_csv(tbl, paste0(path, "/Figures/Treatment/list regimen.csv"))

germline_patient_data %>% #select(first_regimen_name) %>%
  group_by(first_regimen_name) %>% mutate(n = n()) %>% filter(n >= 5) %>% select(first_regimen_name) %>% 
  tbl_summary(sort = everything() ~ "frequency", missing_text = "No Drugs")

a <- germline_patient_data %>% #select(first_regimen_name) %>%
  group_by(first_regimen_name) %>% mutate(n = n()) %>% filter(n >= 5) %>% select(first_regimen_name) %>% distinct()

paste0(a$first_regimen_name, 
       collapse = "|")


# What are the duration between start data of regimen?
# regimen_data <- 
  treatment %>% 
  reshape2::dcast(avatar_id+drug_start_date+drug_stop_date ~ rowid(avatar_id),
                  value.var = c("drug_name_")) %>%
  unite(drug_name_, -avatar_id:-drug_stop_date, sep = "; ", na.rm = TRUE, remove = TRUE) %>%
  mutate(drug_start_date = as_date(drug_start_date)) %>%
  # mutate(drug_start_date = as.POSIXct(drug_start_date, format = "%Y-%m-%d")) %>% 
  arrange(avatar_id, drug_start_date, drug_stop_date) %>% 
  group_by(avatar_id) %>% 
  mutate(drug_start_interval = (drug_start_date - lag(drug_start_date)), 
         drug_start_interval = ifelse(is.na(drug_start_interval), 0, drug_start_interval)
         ) %>% 
  mutate(regimen_duration = interval(start = drug_start_date, end = drug_stop_date)/
           duration(n =1 , units = "days"))# %>% 
  # arrange(avatar_id, drug_start_date, drug_stop_date)# %>% 
  # filter(drug_start_interval<30)
  
write_csv(regimen_data, path = paste0(path, "/Figures/Treatment/Duration and Gap of regimen.csv"))

# mrn_regimen <- right_join(mrn, regimen_data, by = "avatar_id") %>% 
#   filter(drug_start_interval %in% c(1:30)) %>% 
#   distinct(avatar_id, .keep_all = TRUE)
# write_csv(mrn_regimen, path = paste0(path, "/Figures/Treatment/Duration and Gap of regimen with mrn.csv"))

# What are the duration between start and stop of each regimen?
# Duration <- 
  dcast(setDT(treatment), mrn+avatar_id+treatment_line_ ~ rowid(avatar_id),
                  value.var = c("drug_name_","drug_start_date", "drug_stop_date")) %>% 
  unite(drug_name_, starts_with("drug_name_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  
  unite(drug_start_date, starts_with("drug_start_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  separate(drug_start_date, "drug_start_date", sep = "; ",
           extra = "warn", fill = "right") %>% 
  
  select(c(mrn, avatar_id, treatment_line_, "drug_name_", drug_start_date, ncol(.):drug_stop_date_1)) %>% 
  unite(drug_stop_date, starts_with("drug_stop_date"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  separate(drug_stop_date, "drug_stop_date", sep = "; ",
           extra = "warn", fill = "right") %>% 
  mutate(drug_stop_date = as.POSIXct(drug_stop_date, format = "%Y-%m-%d")) %>% 
  filter(treatment_line_ == "1") %>% 
  # dcast(setDT(treatment), avatar_id+drug_start_date ~ rowid(avatar_id), 
  #     value.var = c("drug_name_", "drug_stop_date")) %>% 
  # select(ncol(.):drug_stop_date_1, everything()) %>% 
  # unite(drug_stop_date, 1:"drug_stop_date_1", sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  # separate(drug_stop_date, paste("drug_stop_date", 1, sep="_"), sep = "; ",
  #          extra = "warn", fill = "right") %>% 
  # unite(drug_name_, starts_with("drug_name_"), sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  # arrange(avatar_id, drug_start_date) %>% 
  mutate(drug_start_date = as.POSIXct(drug_start_date, format = "%Y-%m-%d")) %>%
  mutate(drug_stop_date = as.POSIXct(drug_stop_date, format = "%Y-%m-%d")) %>%
  
  mutate(regimen_duration = interval(start = drug_start_date, end = drug_stop_date)/
           duration(n =1 , units = "days")) %>% 
  
  mutate(drug_name_1_for_MM = 
           str_remove_all(drug_name_, "given with investigational therapy: |clinical trial: |investigational agent: clinical trial|investigational agent: | -|-|; $| sulfate|clinical trial/")) %>% 
  mutate(drug_count = sapply(strsplit(drug_name_1_for_MM, ";"), length)) %>% 
  mutate(drug_name_1_for_MM = str_replace_all(drug_name_1_for_MM, "liposomal doxorubicin", "doxil")) %>% 
  mutate(regimen_name = case_when(
    drug_count == 7 &
      str_detect(drug_name_1_for_MM, "bort") &
      str_detect(drug_name_1_for_MM, "thalidomide") &
      str_detect(drug_name_1_for_MM, "cyclophosphamide") &
      str_detect(drug_name_1_for_MM, "cisplatin") &
      str_detect(drug_name_1_for_MM, "etoposide") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "doxo")                ~ "VDT-PACE",
    drug_count == 4 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "cyclophos") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "doxil")               ~ "ABCD",
    drug_count == 4 &
      str_detect(drug_name_1_for_MM, "daratumumab") &
      str_detect(drug_name_1_for_MM, "lena") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "carfilzomib")         ~ "Dara-KRd",
    drug_count == 4 &
      str_detect(drug_name_1_for_MM, "vincristine") &
      str_detect(drug_name_1_for_MM, "cyclophos") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "doxo")                ~ "C-VAD",
    drug_count == 4 &
      str_detect(drug_name_1_for_MM, "thalidomide") &
      str_detect(drug_name_1_for_MM, "vincristine") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "doxil")               ~ "T-VAD doxil",
    drug_count == 4 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "daratumumab") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "lena")                ~ "D-RVd or dara-RVd",
    drug_count == 4 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "cyclophosphamide") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "doxo")                ~ "D-RVd or dara-RVd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "busulfan") &
      str_detect(drug_name_1_for_MM, "melphalan")           ~ "BuMelVel",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "cyclophos") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "lena")                ~ "CRd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "cyclophos") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "bort")                ~ "CyBorD or VCd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "doxil") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "lena")                ~ "Dd-R",
    (drug_count == 3 &
       str_detect(drug_name_1_for_MM, "daratu") &
       str_detect(drug_name_1_for_MM, "lena") &
       str_detect(drug_name_1_for_MM, "dex")) |
      str_detect(drug_name_1_for_MM, "ddr")                 ~ "DRd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "vincristine") &
      str_detect(drug_name_1_for_MM, "doxil") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "DVd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "ixazomib") &
      str_detect(drug_name_1_for_MM, "lena") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "IRd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "cyclophosphamide") &
      str_detect(drug_name_1_for_MM, "carfilzomib") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "KCd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "lena") &
      str_detect(drug_name_1_for_MM, "carfilzomib") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "KRd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "lena") &
      str_detect(drug_name_1_for_MM, "oprozomib") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "ORd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "doxil") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "PDd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "doxo") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "PAd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "lena") &
      str_detect(drug_name_1_for_MM, "doxo") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "RAd",
    (drug_count == 3 &
       str_detect(drug_name_1_for_MM, "bortezomib") &
       str_detect(drug_name_1_for_MM, "lena") &
       str_detect(drug_name_1_for_MM, "dex")) |
      str_detect(drug_name_1_for_MM, "rvd")                 ~ "VRd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "vincristine") &
      str_detect(drug_name_1_for_MM, "doxo") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "VAd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "melph")               ~ "VMd",
    drug_count == 3 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "dex") &
      str_detect(drug_name_1_for_MM, "thalidomide")         ~ "VTd",
    drug_count == 2 &
      str_detect(drug_name_1_for_MM, "carfilzomib") &
      str_detect(drug_name_1_for_MM, "dex")                  ~ "Kd",
    drug_count == 2 &
      str_detect(drug_name_1_for_MM, "thal") &
      str_detect(drug_name_1_for_MM, "dex")                  ~ "Td",
    drug_count == 2 &
      str_detect(drug_name_1_for_MM, "lena") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "Rd",
    drug_count == 2 &
      str_detect(drug_name_1_for_MM, "bortezomib") &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "Bor-Dex",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "lenalidomide")        ~ "Lenalidomide",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "dex")                 ~ "Dexamethasone",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "doxo")                ~ "Doxorubicin",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "doxil")               ~ "Doxil",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "bortezomib")          ~ "Bortezomib",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "melph")               ~ "Melphalan",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "carfilzomib")         ~ "Carfilzomib",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "cyclo")               ~ "Cyclophosphamide",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "thalidomide")         ~ "Thalidomide",
    drug_count == 1 &
      str_detect(drug_name_1_for_MM, "vincristine")         ~ "Vincristine",
    TRUE                                                    ~ drug_name_1_for_MM
  )) %>% 
  mutate(regimen_name = str_replace_na(regimen_name, replacement = "No Drugs")) %>% 
  select(avatar_id, regimen_name, drug_start_date, drug_stop_date, regimen_duration) 

stat_data <- germline_patient_data %>% 
                         select(c("mrn", "avatar_id", "Date_of_Birth", "Gender", "Ethnicity", "Race", "ISS",
                                  "Disease_Status_germline", 
                                  date_of_diagnosis = "Dx_date_closest_germline",
                                  "date_death",
                                  "date_last_follow_up", "date_contact_lost", 
                                  first_regimen_name = "regimen_name_1", 
                                  first_line_start_date = "line_start_date_1", 
                                  first_line_stop_date = "line_stop_date_1",
                                  "progression_event", "pfs_drug_progression_date", "month_at_progression_drug",
                                  "age_at_progression",
                                  "os_date_surv", "os_event", "month_at_os", "age_at_os")) %>% 
  mutate(regimen_duration = interval(start = first_line_start_date, end = first_line_stop_date)/
         duration(n =1 , units = "days")) %>%
  mutate(date_last_follow_up = coalesce(date_last_follow_up, date_contact_lost)) %>% 
  mutate(vital_status = ifelse(is.na(date_death), "Alive", "Dead")) %>% 
  select(c("avatar_id", "Date_of_Birth", "vital_status", everything()), -date_contact_lost)
                       
write_csv(stat_data, paste0(path, "/data for stats.csv"))

# tbl <- 
  stat_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Gender, Race, Ethnicity, Whole, ISS, Disease_Status_germline) %>%
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric")) %>% 
  bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics3 in germline patients.pdf"))

# # tbl <- stat_data %>% 
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   mutate(Whole = "Germline patients") %>% 
#   select(Disease_Status_germline, Whole) %>%
#   tbl_summary(by = Whole, 
#               sort = list(everything() ~ "frequency")) %>% 
#   bold_labels() %>% as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Disease_Status_germline in germline patients.pdf"))

# tbl <- 
  stat_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(month_at_progression_drug, month_at_os, first_regimen_name, regimen_duration, Whole) %>%
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency"),
              digits = list(c(month_at_progression_drug, month_at_os) ~ 2)) %>% 
  bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Treatment and outcomes in germline patients.pdf"))

# # tbl <- stat_data %>% 
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   mutate(Whole = "Germline patients") %>% 
#   select(regimen_duration, Whole) %>%
#   tbl_summary(by = Whole, 
#               sort = list(everything() ~ "frequency")) %>% 
#   bold_labels() %>% as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/regimen_duration in germline patients.pdf"))

# # tbl <- stat_data %>% 
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   mutate(Whole = "Germline patients") %>% 
#   select(month_at_progression_drug, month_at_os, Whole) %>%
#   tbl_summary(by = Whole, 
#               sort = list(everything() ~ "frequency"),
#               digits = list(c(month_at_progression_drug, month_at_os) ~ 2)) %>% 
#   bold_labels() %>% as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/month_at_progression_drug and OS in germline patients.pdf"))



patient <- readxl::read_xlsx(paste0(path, "/Nancy's working files/MM Avatar_Sequenced subset.xlsx"),
                             sheet = "Sequenced") %>% 
  select(avatar_id) %>% distinct()
id <- paste(patient$avatar_id, collapse = "|")
stat_data_sequeenced <- stat_data[ grepl(id, stat_data$avatar_id) , ]

write_csv(stat_data_sequeenced, paste0(path, "/Sequenced patients data for stats.csv"))

# tbl <- 
  stat_data_sequeenced %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Sequenced patients") %>% 
  select(Gender, Race, Ethnicity, Whole, ISS, Disease_Status_germline) %>%
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric")) %>% 
  bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Demographics in sequenced patients.pdf"))

# # tbl <- stat_data_sequeenced %>% 
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   mutate(Whole = "Sequenced patients") %>% 
#   select(Disease_Status_germline, Whole) %>%
#   tbl_summary(by = Whole, 
#               sort = list(everything() ~ "frequency")) %>% 
#   bold_labels() %>% as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/Disease_Status_germline in sequenced patients.pdf"))

# tbl <- 
  stat_data_sequeenced %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Sequenced patients") %>% 
  select(month_at_progression_drug, month_at_os, first_regimen_name, regimen_duration, Whole) %>%
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency"),
              digits = list(c(month_at_progression_drug, month_at_os) ~ 2)) %>% 
  bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/regimen_name in sequenced patients.pdf"))

# # tbl <- stat_data_sequeenced %>% 
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   mutate(Whole = "Sequenced patients") %>% 
#   select(regimen_duration, Whole) %>%
#   tbl_summary(by = Whole, 
#               sort = list(everything() ~ "frequency")) %>% 
#   bold_labels() %>% as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/regimen_duration in sequenced patients.pdf"))
# 
# # tbl <- stat_data_sequeenced %>% 
#   distinct(avatar_id, .keep_all = TRUE) %>% 
#   mutate(Whole = "Sequenced patients") %>% 
#   select(month_at_progression_drug, month_at_os, Whole) %>%
#   tbl_summary(by = Whole, 
#               sort = list(everything() ~ "frequency"),
#               digits = list(c(month_at_progression_drug, month_at_os) ~ 2)) %>% 
#   bold_labels() %>% as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Demographics/month_at_progression_drug and OS in sequenced patients.pdf"))











# How many time patients had KRd, VRd, Rd, DRd, Len, Len+dex in the first regimen by disease status?
# germline_patient_treat <- germline_patient_data %>%
#   mutate(drug_name__1 = 
#            str_remove_all(drug_name__1, "given with investigational therapy: |clinical trial: |investigational agent: clinical trial|investigational agent: | -|-|; $| sulfate|liposomal ")) %>% 
#   mutate(drugs_first_regimen_ = case_when(
#     str_detect(drug_name__1, "car") &
#       str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ NA_character_,
#     
#     str_detect(drug_name__1, "daratu") &
#       str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ NA_character_,
# 
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ NA_character_,
#     str_detect(drug_name__1, "vincristine") &
#       str_detect(drug_name__1, "cyclophos") &
#       str_detect(drug_name__1, "doxo") &
#       str_detect(drug_name__1, "dex")                  ~ "ViCXd",
#     str_detect(drug_name__1, "vincristine") &
#       str_detect(drug_name__1, "doxo") &
#       str_detect(drug_name__1, "dex")                  ~ "ViXd",
# 
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ NA_character_,
#     str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ "Rd",
#     str_detect(drug_name__1, "lenalidomide")           ~ "Len",
#     
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "cyclophos") &
#       str_detect(drug_name__1, "dex")                  ~ NA_character_,
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "doxo") &
#       str_detect(drug_name__1, "dex")                  ~ NA_character_,
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "thal") &
#       str_detect(drug_name__1, "dex")                  ~ "Vtd",
#     str_detect(drug_name__1, "bortezomib") &
#         str_detect(drug_name__1, "dex")                ~ "Vd",
#     str_detect(drug_name__1, "thal") &
#         str_detect(drug_name__1, "dex")                ~ "Td",
#     str_detect(drug_name__1, "dex")                    ~ "Dex",
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "melph")                ~ "Vm",
#     str_detect(drug_name__1, "bortezomib")             ~ "V",
#     str_detect(drug_name__1, "melph")                  ~ "M"
#   )) %>% 
#   mutate(drugs_first_regimen_KRd = case_when(
#     str_detect(drug_name__1, "car") &
#       str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ "KRd"
#   )) %>% 
#   mutate(drugs_first_regimen_DRd = case_when(
#     str_detect(drug_name__1, "daratu") &
#       str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ "DRd"
#   )) %>% 
#   mutate(drugs_first_regimen_VRd = case_when(
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "lena") &
#       str_detect(drug_name__1, "dex")                  ~ "VRd"
#   )) %>% 
#   mutate(drugs_first_regimen_VCd = case_when(
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "cyclophos") &
#       str_detect(drug_name__1, "dex")                  ~ "VCd"
#   )) %>%
#   mutate(drugs_first_regimen_VXd = case_when(
#     str_detect(drug_name__1, "bortezomib") &
#       str_detect(drug_name__1, "doxo") &
#       str_detect(drug_name__1, "dex")                  ~ "VXd"
#   )) %>% 
#   
#   unite("drugs_first_regimen", starts_with("drugs_first_regimen_"), sep = ", ", remove = FALSE, na.rm = TRUE) %>% 
#   mutate(Drugs = ifelse(!is.na(drug_start_date_1), "Other Regimen", "No Regimen")) %>%
#   mutate(drugs_first_regimen = na_if(drugs_first_regimen, "")) %>% 
#   mutate(drugs_first_regimen = coalesce(drugs_first_regimen, Drugs))

# What are the drugs in the first regimen?
# tbl <- 
  germline_patient_data %>% select(`Drug name` = "drug_name__1") %>% 
  mutate(Whole = "All drugs in 1st regimen") %>% 
  tbl_summary(by = Whole,
              sort = list(everything() ~ "frequency")) %>% bold_labels() %>% as_gt()
gt::gtsave(tbl, paste0(path, "/Figures/Treatment/Drugs in 1st regimen germline population.pdf"))

# What are the regimen in the first regimen
germline_patient_data %>%
  mutate(Whole = "All regimen in 1st regimen") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  # mutate(first_regimen_name = str_replace_na(first_regimen_name, replacement = "No Drugs")) %>% 
  select(first_regimen_name, Whole) %>% 
  tbl_summary(by = Whole,
              sort = list(everything() ~ "frequency"),
              missing_text = "No Drugs") %>% as_gt() %>%
  gt::tab_options(column_labels.border.bottom.color = "#0099CC",
                  table_body.border.bottom.color = "#0099CC")

# What are the most regimen in the first regimen
a <- germline_patient_data %>% 
  # mutate(first_regimen_name = str_replace_na(first_regimen_name, replacement = "No Drugs")) %>% 
  group_by(first_regimen_name) %>% mutate(n = n()) %>%
  select(first_regimen_name, n) %>% 
  filter(n >= 10) %>% select(first_regimen_name) %>% 
  distinct()

common_regimen_name <- paste0(a$first_regimen_name, collapse = "|^")
germline_patient_data %>%
  mutate(Whole = "Most common regimen in 1st regimen") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  # mutate(first_regimen_name = str_replace_na(first_regimen_name, replacement = "No Drugs")) %>% 
  filter(str_detect(first_regimen_name, common_regimen_name) | is.na(first_regimen_name)) %>% 
  select(first_regimen_name, Whole) %>% 
  tbl_summary(by = Whole,
              sort = list(everything() ~ "frequency"),
              missing_text = "No Drugs") %>% as_gt() %>%
  gt::tab_source_note(gt::md("*Most common regimen = Regimen given at 10 patients or more*")) 
gt::gtsave(tbl, zoom = 1,
           paste0(path, "/Figures/Treatment/Regimen 1st germline patients.pdf"))

# What are the regimen in the first regimen by disease status +BMT, radiation?
# tbl <- 
  germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  filter(!str_detect(Disease_Status_germline, "Amyl|Myelo|Normal|Refrac|Solit|Wald")) %>%
    mutate(first_regimen_name = str_replace_na(first_regimen_name, replacement = "No Drugs")) %>% 
    filter(str_detect(first_regimen_name, common_regimen_name)) %>% 
  # mutate(Treatment_ever = case_when(
  #   is.na(drug_start_date_1) &
  #     is.na(rad_start_date_1) &
  #     is.na(date_of_bmt_1) ~ "No Treatment",
  #   TRUE                   ~ "Treatment Given"
  # )) %>% 
  select(Drugs, first_regimen_name, Disease_Status_germline, Radiation_ever, HCT_ever, Treatment_ever) %>% 
  tbl_summary(by = Disease_Status_germline,
              sort = list(everything() ~ "frequency")) %>% add_p() %>% bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Treatment/Table any treatment in germline patients.pdf"))

# What are the "other regimen"
# tbl <- 
  germline_patient_data %>%
  mutate(Whole = "Other drugs than most used regimen") %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(first_regimen_name = ifelse(!is.na(drug_start_date_1), first_regimen_name, "No Drugs")) %>% 
  mutate(first_regimen_nameb = case_when(
    !str_detect(first_regimen_name, 
                "No Drugs|VRd|Bor-Dex|^Rd|CyBorD or VCd|Dexamethasone|Lenalidomide|^Td|^KRd|Bortezomib|Melphalan|VAd|ABCD|D-RVd or dara-RVd|IRd") 
                                                     ~ "Other Regimen",
    TRUE                                             ~ first_regimen_name
  )) %>% 
  filter(first_regimen_nameb == "Other Regimen") %>% 
  select(first_regimen_name, Whole) %>% 
  tbl_summary(by = Whole,
              sort = list(everything() ~ "frequency")) %>% bold_labels() %>% as_gt()
gt::gtsave(tbl, zoom = 1, 
           paste0(path, "/Figures/Treatment/Other drugs than most used regimen.pdf"))


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

# TREATM <- separate(treatment, drug_name_, paste("drug_name_", 1:7, sep="_"), sep = "; ", extra = "warn")
# TREATME <- TREATM %>% 
#   pivot_longer(cols = drug_name__1:ncol(TREATM),
#                names_to = "line", values_to = "drug_name_", values_drop_na = TRUE)
# drug_table_2 <- as.data.table(table(TREATME$drug_name_)) %>% 
#   arrange(desc(N))
# write.csv(drug_table_2, paste0(path, "/table alldrugs used at all time.csv"))

# TREATMEN <- TREATME %>% 
#   distinct(avatar_id, drug_name_, .keep_all = TRUE)
# drug_table_3 <- as.data.table(table(TREATMEN$drug_name_)) %>% 
#   arrange(desc(N))
# write.csv(drug_table_3, paste0(path, "/table alldrugs single used per patient.csv"))


regimen1 <- Treatment[, c("avatar_id", "drug_name__1")] %>% 
  separate(drug_name__1, paste("drug_name_", 1:7, sep="_"), sep = "; ", extra = "warn")

# Separate drugs by Pre-Post-Early-Late...
Pre_Treat_ <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma") %>% 
  select("avatar_id")
Pre_Treat_ <- merge.data.frame(Pre_Treat_, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Pre_Treat_$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as pre-treat in first regimen.csv"))

Post_Treat_ <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma") %>% 
  select("avatar_id")
Post_Treat_ <- merge.data.frame(Post_Treat_, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Post_Treat_$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as post-treat in first regimen.csv"))

Early_Relapse_ <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Early Relapse Multiple Myeloma") %>% 
  select("avatar_id")
Early_Relapse_ <- merge.data.frame(Early_Relapse_, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Early_Relapse_$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as early relapse in first regimen.csv"))

Late_Relapse_ <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Late Relapse Multiple Myeloma") %>% 
  select("avatar_id")
Late_Relapse_ <- merge.data.frame(Late_Relapse_, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Late_Relapse$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as late relapse in first regimen.csv"))

Smoldering_ <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma") %>% 
  select("avatar_id")
Smoldering_ <- merge.data.frame(Smoldering_, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Smoldering_$drug_name_)) %>% 
  arrange(desc(N))
# write.csv(drug_table_1, paste0(path, "/table drugs single used per patient classified as smoldering in first regimen.csv"))

Mgus_ <- germline_patient_data %>% 
  filter(Disease_Status_germline == "Mgus") %>% 
  select("avatar_id")
Mgus_ <- merge.data.frame(Mgus_, regimen1, all.x = TRUE, all.y = FALSE) %>% 
  pivot_longer(cols = drug_name__1:ncol(.),
               names_to = "line", values_to = "drug_name_", values_drop_na = TRUE) %>% 
  distinct(avatar_id, drug_name_, .keep_all = TRUE)
drug_table_1 <- as.data.table(table(Mgus_$drug_name_)) %>% 
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
rm(Pre_Treat_, Post_Treat_, Early_Relapse_, Late_Relapse_, Smoldering_, Mgus_,
   regimen1, drug_table_)
rm(Germline, a, treatment_number, germ_before_treatment)
