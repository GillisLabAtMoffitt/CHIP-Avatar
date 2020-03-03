library(VennDiagram)
library(UpSetR)
colors3 <- c(viridis::plasma(n = 3))
colors2 <- c(viridis::plasma(n = 2))
colors4 <- c(viridis::inferno(n = 4))

# Patient who had Drugs and BMT
venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id),
  category.names = c("MM_history" , "Treatment" , "BMT"),
  filename = 'Drugs and BMT.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = colors3,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)

venn.diagram(
  x = list(MM_history$avatar_id, Germ$avatar_id),
  category.names = c("Clinical patients" , "Germline Patients"),
  filename = 'Patient who had germlime sequenced.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = colors2
)

venn.diagram(
  x = list(MM_history$avatar_id, Treatment$avatar_id, SCT$avatar_id, Germ$avatar_id),
  category.names = c("MM_history" , "Treatment" , "BMT", "Germline Patients"),
  filename = 'Patient who had Drugs, BMT and Germlime sequenced.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = colors4
)

