####################################################
# Let's restart!

# WESSS$subject == WESSS$avatar_id

Seq <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c("SLID_germline",
           "SLID_tumor" , "moffitt_sample_id_tumor"))
WESS <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", "Disease_Status", "collectiondt")) %>% 
  `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", "Disease_Status_tumor", "collectiondt_tumor"))
WESSS <-
  merge.data.frame(
    WESS,
    Seq,
    by.x = "moffitt_sample_id_tumor",
    by.y = "moffitt_sample_id_tumor",
    all.x = TRUE,
    all.y = TRUE
  )
Germ <- Germ[, c("avatar_id","moffitt_sample_id","collectiondt",
                 "Disease_Status")] %>% 
  `colnames<-`(c("avatar_id","moffitt_sample_id_germline","collectiondt_germline",
                 "Disease_Status_germline"))
Combined_data_MMS <- merge.data.frame(WESSS, Germ, 
                                     by.x = "avatar_id", by.y = "avatar_id", 
                                     all.x = TRUE, all.y = TRUE)
# Combined_data_MMS$Disease_Status_germline == Combined_data_MMS$Disease_Status_tumor # not all -> good
colnames(Seq_WES_R)
Seq_WES_R <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V044.xlsx")) %>% 
  select(c("subject", 
           "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline",
           "SLID_tumor", "moffitt_sample_id_tumor", "collectiondt_tumor"))
colnames(Seq_WES_R)[which(names(Seq_WES_R) == "subject")] <- "avatar_id" 
# Keep
# Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
colnames(Seq_WES_Raghu)
b[3,] # is unique SL225832 A000353

unique(Seq_WES_R$moffitt_sample_id_tumor)
unique(Seq_WES_R)
d <- bind_rows(Combined_data_MMS, Seq_WES_R)




unique(d$avatar_id) # 512
unique(d$SLID_germline) #512
unique(d$moffitt_sample_id_germline)
unique(d$SLID_tumor) #617
unique(d$moffitt_sample_id_tumor)




df <- d[duplicated(d[, c("avatar_id", "SLID_tumor")]),]

duplicated(d[, c("avatar_id", "SLID_tumor")])
udf <- d[unique(d[, c("avatar_id", "SLID_tumor")])]


which(d[unique(d$SLID_tumor),])
which(d$SLID_tumor=="SL399811")
uudf <- d[which(d$moffitt_sample_id_tumor==unique(d$moffitt_sample_id_tumor)),]
d[unique(d$moffitt_sample_id_tumor),]
unique(uudf$avatar_id)
#############
uudf <- d[!duplicated(d[,"moffitt_sample_id_tumor"]),]


print(paste("We have", length(Seq_WES_Raghu$subject) ,"subject-id in Seq_WES_R with", 
            length(unique(Seq_WES_Raghu$subject)) ,"unique id"))
# remove the duplicated lines

print(paste("In udf we have", length(unique(udf$moffitt_sample_id_tumor)) ,"unique tumor-id and", 
            length(unique(udf$subject)) ,"unique avatar id"))
# In udf we have 617 unique tumor-id and 512 unique avatar id

# will do dcast... but need to add date first
