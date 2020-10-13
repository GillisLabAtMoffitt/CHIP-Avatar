########################################### I ### Create dataframe for all start dates, will use that for timeline ----
# Need to separate in 2 df then bind because of date_of_diag = date of treatment
all_dates <- Global_data %>% 
  select("avatar_id", "date_of_diagnosis_1",
         #"date_of_diagnosis_2", "date_of_diagnosis_3", "date_of_diagnosis_4",
         "collectiondt_germline", starts_with("collectiondt_tumor_"),
         "date_death", "date_last_follow_up", "date_contact_lost",
         starts_with("date_of_bmt_"),
         starts_with("drug_start_date_"), starts_with("drug_stop_date_"),
         starts_with("rad_start_date_"), starts_with("rad_stop_date_"))
all_dates1 <- Global_data %>% 
  select("avatar_id", "Date_of_Birth", "date_of_diagnosis")
# pivot both
all_dates <- all_dates %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "event", values_to = "date") %>% 
  drop_na("date") %>% 
  bind_rows(., Labs_dates) %>% 
  arrange(date, desc(event))
all_dates1 <- all_dates1 %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "event", values_to = "date") %>% 
  drop_na("date") %>% 
  arrange(date, desc(event))
# and bind
all_dates <- bind_rows(all_dates1, all_dates)


# Get the last event and corresponding date----
last_event <- dcast(setDT(all_dates), avatar_id ~ rowid(avatar_id), 
                    value.var = c("event", "date"))

last_event <- last_event %>%  select(ncol(last_event):1) %>% 
  mutate(last_date_available = coalesce(!!! select(., starts_with("date_"))
         )) %>% 
  mutate(last_event_available = coalesce(!!! select(., starts_with("event_"))
    ))
# write.csv(last_event, paste0(path, "/last_event.csv"))

Global_data <- left_join(Global_data, 
               last_event %>% select(c("avatar_id", "last_date_available", "last_event_available")),
               by = "avatar_id") %>% 
  mutate(progression_date_surv = coalesce(progression_date, last_date_available))
write.csv(Global_data, paste0(path, "/Global_data updated.csv"))


########################################### II ### Create all the age from dates ----
Age_data <- Global_data

enddate <- today()
Age_data$Age <- interval(start= Global_data$Date_of_Birth, end= enddate)/                      
  duration(n=1, unit="years")
Age_data$Age <- round(Age_data$Age, 3)
# summary(Age_data$Age)

Age_data$Age_at_diagosis <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_diagnosis)/                      
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

Age_data$Age_at_firstbmt <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_bmt_1)/                      
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

Age_data$month_at_progression <- interval(start= Global_data$date_of_diagnosis, end= Global_data$progression_date_surv)/                      
  duration(n=1, unit="months")
Age_data$month_at_progression <- round(Age_data$month_at_progression, 3)

################################################################################################## III ## Germline ----
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



# Cleaning
rm(Global_data, enddate)

# Do a check on dates
germline_patient_data <- germline_patient_data %>% 
  mutate(germlineBFdrugs = case_when(
    collectiondt_germline > drug_start_date_1 ~ "No",
    collectiondt_germline <= drug_start_date_1 ~ "OK",
    is.na(drug_start_date_1) ~ "OK"
  )) %>% 
  mutate(germlineBFbmt1 = case_when(
    collectiondt_germline > date_of_bmt_1  ~ "No",
    collectiondt_germline <= date_of_bmt_1  ~ "OK",
    is.na(date_of_bmt_1) ~ "OK"
  )) %>% 
  mutate(germlineBFbmt2 = case_when(
    collectiondt_germline > date_of_bmt_2  ~ "No",
    collectiondt_germline <= date_of_bmt_2  ~ "OK"
  )) %>% 
  mutate(germlineBFbmt3 = case_when(
    collectiondt_germline > date_of_bmt_3 ~ "No",
    collectiondt_germline <= date_of_bmt_3 ~ "OK"
  )) %>%
  mutate(germlineBFrad1 = case_when(
    collectiondt_germline <= rad_start_date_1 ~ "OK",
    collectiondt_germline > rad_start_date_1 ~ "No",
    is.na(rad_start_date_1) ~ "OK"
  )) %>% 
  mutate(germlineBFrad2 = case_when(
    collectiondt_germline <= rad_start_date_2 ~ "OK",
    collectiondt_germline > rad_start_date_2 ~ "No"
  )) %>% 
  mutate(germlineBFrad3 = case_when(
    collectiondt_germline <= rad_start_date_3 ~ "OK",
    collectiondt_germline > rad_start_date_3 ~ "No"
  )) %>% 
  mutate(germlineBFrad4 = case_when(
    collectiondt_germline <= rad_start_date_4 ~ "OK",
    collectiondt_germline > rad_start_date_4 ~ "No"
  )) %>% 
  mutate(germBFdrugsbmt = case_when(
    germlineBFdrugs == "OK" &
      germlineBFbmt1 == "OK"                  ~ "OK"
  )) %>% 
  mutate(germBFdbr = case_when(
    germlineBFdrugs == "OK" &
      germlineBFbmt1 == "OK" &
      germlineBFrad1 == "OK"                  ~ "OK"
  )) %>% 
  mutate(GermBFtumorWES = case_when(
    collectiondt_germline < collectiondt_tumor_1 ~ "Germ first",
    collectiondt_germline > collectiondt_tumor_1 ~ "tumorWES first",
    collectiondt_germline == collectiondt_tumor_1 ~ "same date"
  # )) %>% 
  # mutate(progressed = case_when(
  #   is.na(progressed) ~ 0,
  #   TRUE ~ progressed
  )) %>% 
  mutate(progression_surv = coalesce(progression_surv, 0)) %>%  # %>% # missing value because of merging
  mutate(progressed_surv = case_when( # remove censored patient when progression_date_surv > date_contact_lost
    progression_surv == 1 &
      progression_date_surv > date_contact_lost ~ 0,
    TRUE ~ progression_surv
  ))

# write.csv(germline_patient_data, paste0(path, "/compared germline dates and Demographics.csv"))
tab <- table(germline_patient_data$GermBFtumorWES)
# jpeg("barplot3.jpg", width = 350, height = 350)
barplot(tab, main = "Frequency of collection date first observed", ylim = c(0,500))
# dev.off()
tab

# Cleaning
rm(tab, all_dates, all_dates1, last_event)


# Request info from Raghu
# get_info <- germline_patient_data$avatar_id[is.na(germline_patient_data$WES_HUDSON_ALPHA_germline)]
# write.csv(get_info, paste0(path, "/get_info.csv"))
# get_info2 <- germline_patient_data$avatar_id[is.na(germline_patient_data$TCC_ID)]
# write.csv(get_info2, paste0(path, "/get_info2.csv"))





