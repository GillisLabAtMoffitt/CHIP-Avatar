# In the WES file (WES Disease classification from Raghu)
colnames(WES)
length(WES$avatar_id)
# We have 510 avatar-id which are unique




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


# In the first Sequencing file (WES samples from Jamie)
colnames(Sequencing)
length(Sequencing$subject)
unique(Sequencing$subject)
# We have 614 subject-id with 510 unique id
print(paste("We have", length(Sequencing$subject) ,"subject-id with", 
            length(unique(Sequencing$subject)) ,"unique id"))
# Need to merge WES and Sequencing by "moffitt_sample_id"
# to make sure the date we get in WES align with the good sample

# In the second Sequencing file (WES samples from Jamie)
colnames(Sequencing2)
length(Sequencing2$subject)
unique(Sequencing2$subject)
# We have 612 subject-id with 507 unique id
print(paste("We have", length(Sequencing2$subject) ,"subject-id with", 
            length(unique(Sequencing2$subject)) ,"unique id"))
#-----------------------------------------------------------------------------------------------------------------

# In the Seq file selected MM from Raghu
# We have 612 subject-id with 507 unique id
print(paste("We have", length(Seq_WES_Raghu$subject) ,"subject-id with", 
            length(unique(Seq_WES_Raghu$subject)) ,"unique id"))
UniqueRaghu <- as.data.frame(unique(Seq_WES_Raghu$subject))
UniqueRaghu$listR <- "Raghu"

Nancy_unique_id <- read_csv(paste0(path, "/Raghu MM/Unique Pts with WES_v4.4.csv"))
Nancy_unique_id$list <- "Nancy"
unique(Nancy_unique_id$`Unique Patients with WES`)
a <- merge.data.frame(Nancy_unique_id, UniqueRaghu, 
                      by.x = "Unique Patients with WES", by.y = "unique(Seq_WES_Raghu$subject)",
                      all.x = TRUE, all.y = TRUE)
a <- as.data.table(a)
a <- arrange(a, a$listR)
#write.csv(a, paste0(path, "/a.csv"))
b <- merge.data.frame(Nancy_unique_id, Sequencing2,
                      by.x = "Unique Patients with WES", by.y = "subject",
                      all.x = TRUE, all.y = TRUE)
b <- as.data.table(b)
b <- arrange(b, b$SLID_germline)
write.csv(b, paste0(path, "/b.csv"))
c <- merge.data.frame(Nancy_unique_id, Sequencing,
                      by.x = "Unique Patients with WES", by.y = "subject",
                      all.x = TRUE, all.y = TRUE)
c <- as.data.table(c)
c <- arrange(c, c$SLID_germline)
write.csv(c, paste0(path, "/idwithsequncing1.csv"))
