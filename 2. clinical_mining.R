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
  fill = m3,
  
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
  filename = 'Patient who had Drugs, BMT, Radiation and Germline sequenced.png',
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
  fill = m3,
  
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

library(RColorBrewer)
###################################################################################################  I  ## Venn 1
# Restart from the Global_data
# Who had BMT or/and drugs in the Germline available patient samples

# nbr of germline collection
NROW(gerrmline_patient_data) #512
# nbr tcc id
NROW(which(!is.na(gerrmline_patient_data$TCC_ID))) # 512
# nbr birth
NROW(which(!is.na(gerrmline_patient_data$Date_of_Birth))) # 512
# nbr death
NROW(which(!is.na(gerrmline_patient_data$date_death))) # 83
# nbr diag
NROW(which(!is.na(gerrmline_patient_data$date_of_diagnosis_1))) # 509

# nbr had bmt1 
NROW(which(!is.na(gerrmline_patient_data$date_of_first_bmt))) # 240
bmtINgerm <- gerrmline_patient_data[!is.na(gerrmline_patient_data$date_of_first_bmt),]
# nbr had drug1
NROW(which(!is.na(gerrmline_patient_data$drug_start_date_1))) # 416
drugINgerm <- gerrmline_patient_data[!is.na(gerrmline_patient_data$drug_start_date_1),]
# nbr commun in bmt1 and drug
had_GERM_BMT_DRUGS <- gerrmline_patient_data[!is.na(bmtINgerm$drug_start_date_1),] # 240
NROW(which(!is.na(drugINgerm$date_of_first_bmt))) # same
NROW(which(!is.na(bmtINgerm$drug_start_date_1)))


myCol1 <- brewer.pal(3, "Pastel1")
myCol2 <- brewer.pal(3, "Pastel2")

draw.triple.venn(nrow(gerrmline_patient_data), 
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

