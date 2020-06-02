
colnames(Additional)

Additional <-
  readxl::read_xlsx((paste0(path, "/Raghu MM/Additional_Neoplasms_OUT05222020.xlsx")))

Additional <- 
  merge.data.frame(Additional, germline_patient_data[, c("avatar_id", "Disease_Status_germline",
                                                         "collectiondt_germline", "germlineBFdrugs", "drug_start_date_1",
                                                         "germlineBFbmt1", "date_of_first_bmt")],
                   by.x = "Avatar_id", by.y = "avatar_id",
                   all.x = TRUE, all.y = FALSE)     
write.csv(Additional, paste0(path, "/Additional neoplasms with disease status.csv"))