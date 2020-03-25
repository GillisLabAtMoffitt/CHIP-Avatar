# Seq

path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Cassin", "CHIP in Avatar")

Sequencing <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c(
    "SLID_germline", "SLID_tumor" , "moffitt_sample_id_tumor", "moffitt_sample_id_germline",
    "BaitSet", "SpecimenDetail_DiseaseType", "ClinicalSpecimenLinkage_WES.Batch", "moffitt_sample_id"))
Sequencing2 <- 
  read.delim(paste0(path, "/Jamie/wes_somatic_mutations_metadata_v0.4.4.txt")) %>% 
  select(c(
    "SLID_germline", "SLID_tumor" , "moffitt_sample_id_tumor", "moffitt_sample_id_germline",
    "BaitSet", "SpecimenDetail_DiseaseType", "moffitt_sample_id"))
table(Sequencing2$SpecimenDetail_DiseaseType)

a <- Sequencing2[Sequencing2$SpecimenDetail_DiseaseType == "HEM - Myeloma Spectrum",]
d$SLID_tumor.x == d$SLID_tumor.x
identical(Sequencing$moffitt_sample_id_germline == Sequencing2$moffitt_sample_id_germline)

b <- merge.data.frame(a, Sequencing, 
                 by.x = "moffitt_sample_id_tumor", 
                 by.y = "moffitt_sample_id_tumor",
                 all.x = TRUE,
                 all.y = TRUE)
d <- b[,c("SLID_germline.y", "SLID_germline.x", "moffitt_sample_id_tumor", 
          "SLID_tumor.y", "SLID_tumor.x")]

a <- a[order(a$moffitt_sample_id_tumor),]
WES <- WES[order(WES$moffitt_sample_id),]
a$moffitt_sample_id == WES$moffitt_sample_id # =>>>>>>> YES



