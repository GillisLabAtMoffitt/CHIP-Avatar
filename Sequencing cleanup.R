####################################################
# Let's restart!

# WESSS$subject == WESSS$avatar_id

# Seq <-
#   read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
#   select(c("SLID_germline",
#            "SLID_tumor" , "moffitt_sample_id_tumor"))
# # WESS <-
#   readxl::read_xlsx(paste0(path, "/Raghu MM/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
#   select(c("avatar_id", "moffitt_sample_id", "Disease_Status", "collectiondt")) %>% 
#   `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", "Disease_Status_tumor", "collectiondt_tumor"))


Combined_data_MMS <- merge.data.frame(WESSS, Germ, 
                                     by.x = "avatar_id", by.y = "avatar_id", 
                                     all.x = TRUE, all.y = TRUE)
# Combined_data_MMS$Disease_Status_germline == Combined_data_MMS$Disease_Status_tumor # not all -> good
# colnames(Seq_WES_R)
# Seq_WES_R <- 
#   readxl::read_xlsx(paste0(path, "/Raghu MM/MM_Metadata_WES_V044.xlsx")) %>% 
#   select(c("subject", 
#            "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline",
#            "SLID_tumor", "moffitt_sample_id_tumor", "collectiondt_tumor"))
# colnames(Seq_WES_R)[which(names(Seq_WES_R) == "subject")] <- "avatar_id" 

colnames(Seq_WES_Raghu)





d <- bind_rows(Combined_data_MMS, Seq_WES_R)


unique(d$avatar_id) # 512
unique(d$SLID_germline) #512
unique(d$moffitt_sample_id_germline)
unique(d$SLID_tumor) #617
unique(d$moffitt_sample_id_tumor)
#############


a <- d %>% distinct(avatar_id, moffitt_sample_id_tumor, collectiondt_tumor, SLID_germline,
                    SLID_tumor, moffitt_sample_id_germline, collectiondt_germline, .keep_all = TRUE)


print(paste("We have", length(Seq_WES_Raghu$subject) ,"subject-id in Seq_WES_R with", 
            length(unique(Seq_WES_Raghu$subject)) ,"unique id"))
# remove the duplicated lines

print(paste("In udf we have", length(unique(udf$moffitt_sample_id_tumor)) ,"unique tumor-id and", 
            length(unique(udf$subject)) ,"unique avatar id"))
# In udf we have 617 unique tumor-id and 512 unique avatar id

# will do dcast... but need to add date first
