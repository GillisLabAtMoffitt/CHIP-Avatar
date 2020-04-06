####################################################
# Let's restart!
Seq <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c("SLID_germline", "moffitt_sample_id_germline",
           "SLID_tumor" , "moffitt_sample_id_tumor"))
WESS <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", "Disease_Status", "collectiondt"))
WESSS <-
  merge.data.frame(
    WESS,
    Seq,
    by.x = "moffitt_sample_id",
    by.y = "moffitt_sample_id_tumor",
    all.x = TRUE,
    all.y = TRUE
  )
Germ <- Germ[, c("avatar_id","moffitt_sample_id","collectiondt", "WES_HUDSON_ALPHA",
                 "Disease_Status")] %>% 
  `colnames<-`(c("avatar_id","moffitt_sample_id.germline","collectiondt.germline", "WES_HUDSON_ALPHA.germline",
                 "Disease_Status.germline"))
Combined_data_MMS <- merge.data.frame(WESSS, Germ, 
                                     by.x = "avatar_id", by.y = "avatar_id", 
                                     all.x = TRUE, all.y = TRUE)




# WESSS$subject == WESSS$avatar_id
print(paste("We have", length(Seq$subject) ,"subject-id in Seq with", 
            length(unique(Seq$subject)) ,"unique id"))

Seq_WES_R <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V044.xlsx")) %>% 
  select(c("subject", 
           "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline",
           "SLID_tumor", "moffitt_sample_id_tumor", "collectiondt_tumor"))

# Keep
# Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
colnames(Seq_WES_Raghu)

Seq_WES_Raghu$SpecimenDetail_DiseaseType
print(paste("We have", length(Seq_WES_Raghu$subject) ,"subject-id in Seq_WES_R with", 
            length(unique(Seq_WES_Raghu$subject)) ,"unique id"))

b[3,] # is unique SL225832 A000353

unique(Seq_WES_R$moffitt_sample_id_tumor)
unique(Seq_WES_R)
d <- bind_rows(Seq, Seq_WES_R)

unique(d$subject) # 512
unique(d$SLID_germline) #512
unique(d$moffitt_sample_id_germline)
unique(d$SLID_tumor) #617
unique(d$moffitt_sample_id_tumor)
unique(d$moffitt_sample_id) #617

# remove the duplicated lines
duplicated(d)
udf <- unique(d)
print(paste("In udf we have", length(unique(udf$moffitt_sample_id_tumor)) ,"unique tumor-id and", 
            length(unique(udf$subject)) ,"unique avatar id"))
# In udf we have 617 unique tumor-id and 512 unique avatar id

# will do dcast... but need to add date first
