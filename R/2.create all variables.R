Age_data <- Global_data

enddate <- today()
Age_data$Age <- interval(start= Global_data$Date_of_Birth, end= enddate)/                      
  duration(n=1, unit="years")
Age_data$Age <- round(Age_data$Age, 3)
# summary(Age_data$Age)

Age_data$Age_at_diagosis <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_diagnosis_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_diagosis <- round(Age_data$Age_at_diagosis, 3)
# summary(Age_data$Age_at_diagosis, na.rm = TRUE)

Age_data$Age_at_death <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_death)/                      
  duration(n=1, unit="years")
Age_data$Age_at_death <- round(Age_data$Age_at_death, 3)
# summary(Age_data$Age_at_death, na.rm = TRUE)

Age_data$Age_at_lastfollowup <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_last_follow_up)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastfollowup <- round(Age_data$Age_at_lastfollowup, 3)
# summary(Age_data$Age_at_lastfollowup, na.rm = TRUE)

Age_data$Age_at_lastdate <- interval(start= Global_data$Date_of_Birth, end= Global_data$last_date_available)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastdate <- round(Age_data$Age_at_lastdate, 3)
# summary(Age_data$Age_at_lastdate, na.rm = TRUE)

Age_data$Age_at_firstdrug <- interval(start= Global_data$Date_of_Birth, end= Global_data$drug_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstdrug <- round(Age_data$Age_at_firstdrug, 3)
# summary(Age_data$Age_at_firstdrug, na.rm = TRUE)

Age_data$Age_at_firstbmt <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_first_bmt)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstbmt <- round(Age_data$Age_at_firstbmt, 3)
# summary(Age_data$Age_at_firstbmt, na.rm = TRUE)

Age_data$Age_at_firstrad <- interval(start= Global_data$Date_of_Birth, end= Global_data$rad_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstrad <- round(Age_data$Age_at_firstrad, 3)
# summary(Age_data$Age_at_firstrad, na.rm = TRUE)

Age_data$Age_at_germcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt_germline)/                      
  duration(n=1, unit="years")
Age_data$Age_at_germcollect <- round(Age_data$Age_at_germcollect, 3)
# summary(Age_data$Age_at_germcollect, na.rm = TRUE)

Age_data$Age_at_tumorcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt_tumor_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_tumorcollect <- round(Age_data$Age_at_tumorcollect, 3)
# summary(Age_data$Age_at_tumorcollect, na.rm = TRUE)


##################################################################################################  IV  ## Germline
# Create dataframe for only the patients who had germline sequenced
germline_patient_data <- Age_data[!is.na(Age_data$moffitt_sample_id_germline),]


germline_patient_data <- germline_patient_data %>% 
  mutate(Disease_Status_facet = case_when(
    Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
      Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myelom" |
      Disease_Status_germline == "Early Relapse Multiple Myeloma" |
      Disease_Status_germline == "Late Relapse Multiple Myeloma"                      ~ "MM",
    Disease_Status_germline == "Mgus"                                                 ~ "MGUS",
    Disease_Status_germline == "Smoldering Multiple Myeloma"                          ~ "Smoldering"
  ))

write.csv(germline_patient_data, paste0(path, "/germline_patient_data.csv"))




















# Create dataframe for all start dates 

all_dates <- germline_patient_data %>% 
  select("avatar_id", "Date_of_Birth", "collectiondt_germline", "collectiondt_tumor_1", 
         "date_of_diagnosis_1", "date_of_diagnosis_2", "date_of_diagnosis_3", "date_of_diagnosis_4",
         "date_death", "date_last_follow_up", 
         "date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt",
         "drug_start_date_1", "drug_start_date_2", "drug_start_date_3", "drug_start_date_4", "drug_start_date_5", 
         "drug_start_date_6", "drug_start_date_7", "drug_start_date_8", "drug_start_date_9", "drug_start_date_10", 
         "drug_start_date_11", "drug_start_date_12", "drug_start_date_13", "drug_start_date_14", "drug_start_date_15",
         "drug_start_date_16",
         "drug_start_date_17",
         "drug_stop_date_1", "drug_stop_date_2", "drug_stop_date_3", "drug_stop_date_4", "drug_stop_date_5", 
         "drug_stop_date_6", "drug_stop_date_7", "drug_stop_date_8", "drug_stop_date_9", "drug_stop_date_10",
         "drug_stop_date_11", "drug_stop_date_12", "drug_stop_date_13", "drug_stop_date_14", "drug_stop_date_15",
         "drug_stop_date_16",
         "drug_stop_date_17",
         "rad_start_date_1", "rad_start_date_2", "rad_start_date_3", "rad_start_date_4", "rad_stop_date_1",
         "rad_stop_date_2", "rad_stop_date_3", "rad_stop_date_4")
all_date <- all_dates %>% 
  gather(key = "event", value = "date", -1) %>% 
  arrange(avatar_id)
write.csv(all_date, paste0(path, "/all_dates.csv"))








