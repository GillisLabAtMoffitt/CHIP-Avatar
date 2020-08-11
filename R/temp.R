#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar")


# 1.2.Load Germline with disease status --------------------------------------------------------------------------
Germ <- 
  readxl::read_xlsx(paste0(path, 
                           "/Raghu MM/Moffitt_Germl_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "collectiondt", "WES_HUDSON_ALPHA", "Disease_Status")) #%>% 
  # `colnames<-`(c("avatar_id", "collectiondt_germline", "WES_HUDSON_ALPHA_germline",
  #                "Disease_Status_germline"))
Germ2 <-
  readxl::read_xlsx(paste0(path,
                           "/Raghu MM/Moffitt_Germl_Disease_Classification_2patient_from_2nd_sequencingfile.xlsx")) %>%
  select(-moffitt_sample_id) #%>% 
  # `colnames<-`(c("avatar_id", "collectiondt_germline", "WES_HUDSON_ALPHA_germline", "Disease_Status_germline"))
Germ3 <-
  readxl::read_xlsx(paste0(path,
                           "/Raghu MM/Germline_MM_Disease_Status_05052020_OUT .xlsx")) %>% 
  select(c(avatar_id = "Avatar_id", "collectiondt", "WES_HUDSON_ALPHA", "Disease_Status")) #%>% 
  # `colnames<-`(c(c("avatar_id", "collectiondt_germline", "WES_HUDSON_ALPHA_germline", "Disease_Status_germline")))
Germ4 <-
  readxl::read_xlsx(paste0(path,
                           "/Raghu MM/Moffitt_Germl_v0.4.5_Disease_Classification_OUT_07272020.xlsx")) %>% 
  select(c("avatar_id", "SLID_germline", "Disease_Status")) #%>% 
  # `colnames<-`(c(c("avatar_id", "SLID_germline", "Disease_Status_germline")))


# 1.3.Load Sequencing data ---------------------------------------------------------------------------------------
WES_tumor <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", "collectiondt")) %>% 
  `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", "collectiondt_tumor"))

#---
Sequencing <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c(
    "SLID_germline", 
    "SLID_tumor" , "moffitt_sample_id_tumor", 
    "moffitt_sample_id_germline",
    "BaitSet"))

#---
Seq_WES_Raghu <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V044.xlsx")) %>% 
  select(c(avatar_id = "subject",
           "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline",
           "SLID_tumor" , "moffitt_sample_id_tumor", "collectiondt_tumor",
           "BaitSet"))

# Keep
# Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
#---
Sequencing2 <- # warning message due to a TRUE added in a num var by Raghu (he copy paste an extra patient)
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V0441.xlsx")) %>% 
  select(c(avatar_id = "subject",
           "SLID_germline", moffitt_sample_id_germline = "moffittSampleId_germline",
           "collectiondt_germline",
           "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor",
           BaitSet = "baitSet"))
# Sequencing2 <- dcast(setDT(Sequencing2), avatar_id+SLID_germline+moffitt_sample_id_germline ~ rowid(avatar_id),
#                      value.var = c(
#                        "SLID_tumor",
#                        "moffitt_sample_id_tumor",
#                        "BaitSet", "clinicalSpecimenLinkageDiseaseTy")) 
#---
Seq_WES_Raghu2 <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V045.xlsx")) %>% 
  select(c(avatar_id = "subject",
           "SLID_germline", moffitt_sample_id_germline = "moffittSampleId_germline", "collectiondt_germline",
           "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor",
           "BaitSet"))


### Binda
Germline <- bind_rows(Germ, Germ2, Germ3, Germ4) %>% 
  `colnames<-`(c("avatar_id", "collectiondt_germline", 
                 "WES_HUDSON_ALPHA_germline", "Disease_Status_germline", "SLID_germline")) %>% 
  arrange(SLID_germline) %>% 
  distinct(avatar_id, Disease_Status_germline, .keep_all = TRUE)

# One of the seq is in 2 part so merge that first
Sequencing <-
  full_join(
    Sequencing,
    WES_tumor,
    by = "moffitt_sample_id_tumor")
# Binds
Seq_WES <- bind_rows(Seq_WES_Raghu, Sequencing2, Seq_WES_Raghu2, Sequencing, .id = "vers") %>% 
  arrange(collectiondt_germline) %>% 
  distinct(SLID_tumor, moffitt_sample_id_tumor, SLID_germline, .keep_all = TRUE)
# pivot wider
WES_seq <-
  dcast(setDT(Seq_WES), avatar_id+SLID_germline+moffitt_sample_id_germline+collectiondt_germline ~ rowid(avatar_id),
        value.var = c(
          "SLID_tumor",
          "moffitt_sample_id_tumor",
          "collectiondt_tumor",
          "BaitSet"
        )
  )

colnames(Seq_WES)
colnames(Germline)
duplicated(WES_seq$avatar_id)
Germline[which(duplicated(Germline$avatar_id)),]

# Merge all

Germline1 <- left_join(WES_seq, Germline, by = "avatar_id") %>% 
  rename(SLID_germline = "SLID_germline.x", collectiondt_germline = "collectiondt_germline.x") %>% 
  distinct(avatar_id, SLID_germline.y, .keep_all = TRUE) %>% 
  mutate(collectiondt_germline = coalesce(collectiondt_germline, collectiondt_germline.y)) %>% 
  select(-SLID_germline.y, -collectiondt_germline.y)






b <- full_join(Germline1[, c("avatar_id", "moffitt_sample_id_germline", "SLID_germline",
                            "collectiondt_germline", "Disease_Status_germline", 
                            "collectiondt_tumor_1", "WES_HUDSON_ALPHA_germline")],
               MM_history, by = "avatar_id") %>% 
  full_join(., Vitals, by = "avatar_id")

d <- full_join(b, SCT, by = "avatar_id")

e <- full_join(d, Treatment, by = "avatar_id")

f <- full_join(e, Radiation, by = "avatar_id")






