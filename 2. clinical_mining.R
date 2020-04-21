# We have 512 unique patient IDs in Sequencing, does they match the treatment
Treatment$avatar_id == Germline$avatar_id # No

#library(UpSetR)

color_vir <- list(p2 <- c(viridis::plasma(n = 2)),
                  p3 <- c(viridis::plasma(n = 3)),
                  p4 <- c(viridis::plasma(n = 4)),
                  m2 <- c(viridis::magma(n = 2)),
                  m3 <- c(viridis::magma(n = 3)),
                  m4 <- c(viridis::magma(n = 4)),
                  i3 <- c(viridis::inferno(n = 3)),
                  i4 <- c(viridis::inferno(n = 4)),
                  c3 <- c(viridis::cividis(n = 3)),
                  c4 <- c(viridis::cividis(n = 4)),
                  c5 <- c(viridis::cividis(n = 5)))

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
  fill = c3,
  margin = 0.2,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  ext.percent = 5
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
  category.names = c("Clinical data" , "Treatment" , "BMT"),
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
  fill = c("#F0F921FF", "#0D0887FF", "#CC4678FF"),
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.015),
  cat.fontfamily = "sans",
  rotation = 1
)

# Patient who had Drugs and BMT
venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, Radiation$avatar_id),
  category.names = c("Clinical data" , "Treatment" , "BMT", "Radiation"),
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
  fill = p4,
  margin = 0.2,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  ext.percent = 5,
  cat.pos = c(-38, 30, -30, 30),
  cat.dist = c(0.28, 0.25, 0.15, 0.15)
)

venn.diagram(
  x = list(MM_history$avatar_id, Germline$avatar_id),
  category.names = c("Clinical data" , "Germline data"),
  filename = 'Patient who had germlime sequenced.png',
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
  fill = p2,
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
  category.names = c("Clinical data" , "Treatment" , "BMT", "Germline data"),
  filename = 'Patient who had Drugs, BMT and Germlime sequenced.png',
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
  fill = i4,
  margin = 0.2,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  ext.percent = 5,
  cat.pos = c(-38, 30, -30, 30),
  cat.dist = c(0.28, 0.25, 0.15, 0.15)
  
)

venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, Germline$avatar_id, Radiation$avatar_id),
  category.names = c("Clinical data" , "Treatment" , "BMT", "Germline data", "Radiation"),
  filename = 'Patient who had Drugs, BMT, Radiation and Germlime sequenced.png',
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
  fill = c5,
  margin = 0.2,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  ext.percent = 5,
  #cat.pos = c(-38, 30, -30, 30),
  #cat.dist = c(0.28, 0.25, 0.15, 0.15)
)

venn.diagram(
  x = list(Treatment$avatar_id, SCT$avatar_id, Germline$avatar_id),
  category.names = c("Treatment" , "BMT", "Germline data"),
  filename = 'Patient who had Drugs, BMT and Germlime sequenced 2.png',
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
  fill = c3,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(200, 165, 0), # germ treat
  cat.dist = c(0.020, -0.035, 0.045), # x BMT germ
  cat.fontfamily = "sans"
)
################################################################################# TABLE disease status year ####
head(Combined_data_MM)
Combined_data_MM$Disease_Status_germline
Disease_status_table <- table(Combined_data_MM$Disease_Status_germline)
write.csv(Disease_status_table, paste0(path, "/Table germline disease status.csv"))

################################################################################# TABLE Year of germline sample collection ####
Amyloidosis_Diagnostic <- which(Combined_data_MM$Disease_Status_germline == "Amyloidosis- Diagnostic marrow")#1  
Early_Relapse <- which(Combined_data_MM$Disease_Status_germline == "Early Relapse Multiple Myeloma") # 208 
Late_Relapse <- which(Combined_data_MM$Disease_Status_germline == "Late Relapse Multiple Myeloma")  #66  
Mgus <- which(Combined_data_MM$Disease_Status_germline == "Mgus") #50
Myelofib <- which(Combined_data_MM$Disease_Status_germline == "MYELOFIBROSIS")    #1                                 
Normal_marrow <- which(Combined_data_MM$Disease_Status_germline == "Normal marrow") #1
Post_Treat <-which(Combined_data_MM$Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma")  #9 
Refractory_anemia <- which(Combined_data_MM$Disease_Status_germline == "Refractory anemia with ring sideroblasts")  #1                  
SmolderingMM <- which(Combined_data_MM$Disease_Status_germline == "Smoldering Multiple Myeloma") #47
Solitary_Plasmacytoma <- which(Combined_data_MM$Disease_Status_germline == "Solitary Plasmacytoma")    #4              
Walderstrom <- which(Combined_data_MM$Disease_Status_germline == "WALDENSTROM MACROGLOBULINEMIA") #1
Pre_Treat <- which(Combined_data_MM$Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma") #117

Pre_Treat <- Combined_data_MM[Pre_Treat, ]
Post_Treat <- Combined_data_MM[Post_Treat, ]
Amyloidosis_Diagnostic <- Combined_data_MM[Amyloidosis_Diagnostic, ]
Early_Relapse <- Combined_data_MM[Early_Relapse, ]
Late_Relapse <- Combined_data_MM[Late_Relapse, ]
Mgus <- Combined_data_MM[Mgus, ]
Myelofib <- Combined_data_MM[Myelofib, ]
Normal_marrow <- Combined_data_MM[Normal_marrow, ]
Refractory_anemia <- Combined_data_MM[Refractory_anemia, ]
SmolderingMM <- Combined_data_MM[SmolderingMM, ]
Solitary_Plasmacytoma <- Combined_data_MM[Solitary_Plasmacytoma, ]
Walderstrom <- Combined_data_MM[Walderstrom, ]

disease_staus_by_year <- matrix(
  c("group", "nbr of patients", "earliest date", "latest date","2011", "2012", "2013","2014","2015","2016","2017","2018","2019",
    
    "General", nrow(Combined_data_MM), as.character(min(Combined_data_MM$collectiondt_germline)), as.character(max(Combined_data_MM$collectiondt_germline)), 
    sum(str_count(Combined_data_MM$collectiondt_germline, "2011")),sum(str_count(Combined_data_MM$collectiondt_germline, "2012")),sum(str_count(Combined_data_MM$collectiondt_germline, "2013")),
    sum(str_count(Combined_data_MM$collectiondt_germline, "2014")),sum(str_count(Combined_data_MM$collectiondt_germline, "2015")),sum(str_count(Combined_data_MM$collectiondt_germline, "2016")),
    sum(str_count(Combined_data_MM$collectiondt_germline, "2015")),sum(str_count(Combined_data_MM$collectiondt_germline, "2018")),sum(str_count(Combined_data_MM$collectiondt_germline, "2019")),
    "Pre Treatment Newly Diagnosed", nrow(Pre_Treat), as.character(min(Pre_Treat$collectiondt_germline)),as.character(max(Pre_Treat$collectiondt_germline)),
    sum(str_count(Pre_Treat$collectiondt_germline, "2011")),sum(str_count(Pre_Treat$collectiondt_germline, "2012")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2013")),sum(str_count(Pre_Treat$collectiondt_germline, "2014")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2015")),sum(str_count(Pre_Treat$collectiondt_germline, "2016")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2017")),sum(str_count(Pre_Treat$collectiondt_germline, "2018")),
    sum(str_count(Pre_Treat$collectiondt_germline, "2019")),
    "Post Treatment Newly Diagnosed", nrow(Normal_marrow), as.character(min(Normal_marrow$collectiondt_germline)),as.character(max(Normal_marrow$collectiondt_germline)),
    sum(str_count(Normal_marrow$collectiondt_germline, "2011")),sum(str_count(Normal_marrow$collectiondt_germline, "2012")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2013")),sum(str_count(Normal_marrow$collectiondt_germline, "2014")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2015")),sum(str_count(Normal_marrow$collectiondt_germline, "2016")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2017")),sum(str_count(Normal_marrow$collectiondt_germline, "2018")),
    sum(str_count(Normal_marrow$collectiondt_germline, "2019")),
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
    "Smoldering Multiple Myeloma", nrow(SmolderingMM), as.character(min(SmolderingMM$collectiondt_germline)),as.character(max(SmolderingMM$collectiondt_germline)),
    sum(str_count(SmolderingMM$collectiondt_germline, "2011")),sum(str_count(SmolderingMM$collectiondt_germline, "2012")),
    sum(str_count(SmolderingMM$collectiondt_germline, "2013")),sum(str_count(SmolderingMM$collectiondt_germline, "2014")),
    sum(str_count(SmolderingMM$collectiondt_germline, "2015")),sum(str_count(SmolderingMM$collectiondt_germline, "2016")),
    sum(str_count(SmolderingMM$collectiondt_germline, "2017")),sum(str_count(SmolderingMM$collectiondt_germline, "2018")),
    sum(str_count(SmolderingMM$collectiondt_germline, "2019")),
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

write.csv(disease_staus_by_year,paste0(path, "/Germline Disease status classified by year of collection.csv"))
# disease_staus_by_year <- as.table(disease_staus_by_year)
# write.csv(disease_staus_by_year,paste0(path, "/Year of germline sample collection.csv"))

rm(
  Amyloidosis_Diagnostic,
  Early_Relapse,
  Late_Relapse,
  Mgus,
  Myelofib,
  Normal_marrow,
  Post_Treat,
  Pre_Treat,
  Refractory_anemia,
  SmolderingMM,
  Solitary_Plasmacytoma,
  Walderstrom,
  Yearofsamplecollection,
  col_date
)

