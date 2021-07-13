########################################### I ### Create dataframe for all start dates, will use that for timeline ----
# Global_data <- readRDS("Global_data_pre.rds")
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Need to separate in 2 df ,do diag separately to make sure it appear before germ or tumor date, date of treatment
all_dates <- Global_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select("avatar_id", # "date_of_diagnosis_1",
         # starts_with("date_of_diagnosis_"),
         # "date_of_diagnosis_2", "date_of_diagnosis_3", "date_of_diagnosis_4",
         "collectiondt_germline", starts_with("collectiondt_tumor_"),
         starts_with("date_of_bmt_"),
         starts_with("line_start_date_"), starts_with("line_stop_date_"),
         starts_with("rad_start_date_"), starts_with("rad_stop_date_"), 
         starts_with("progression_"),
         "labs_last_date", "date_last_follow_up", "date_contact_lost", "date_death")
all_dates1 <- Global_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>%
  select("avatar_id", "Date_of_Birth", "Dx_date_closest_germline", "date_of_MM_diagnosis", "date_of_MMSMMGUSdiagnosis")
# pivot both
all_dates <- all_dates %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "event", values_to = "date") %>% 
  drop_na("date") %>% 
  arrange(date)
all_dates1 <- all_dates1 %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "event", values_to = "date") %>% 
  drop_na("date") %>% 
  arrange(date)
# and bind
all_dates <- bind_rows(all_dates1, all_dates) %>% 
  # left_join(., Contact_lost %>% select(c("avatar_id", "date_contact_lost")), by = "avatar_id") %>%
  left_join(., Vitals %>% 
              select(c("avatar_id", "date_death", "date_contact_lost", "date_last_follow_up")), by = "avatar_id") %>%
  mutate(date_sameas_last = case_when(
    date > date_contact_lost |
    date > date_death |
    date > date_last_follow_up              ~ "removed"
    )) %>% 
  filter(is.na(date_sameas_last)) %>%
  select(-c("date_contact_lost", "date_death", "date_sameas_last", "date_last_follow_up"))

# Get the last event and corresponding date----
last_event <- dcast(setDT(all_dates), avatar_id ~ rowid(avatar_id), 
                    value.var = c("event", "date"))

# a <- last_event %>% 
#   filter(str_detect(event_3, "bmt|stop")) %>% 
#   right_join(Global_data[c("avatar_id", "Disease_Status_germline")], ., by = "avatar_id") %>% 
#   select(c("avatar_id", "Disease_Status_germline", "event_1", "date_1", "event_2", "date_2", "event_3", "date_3", 
#            "event_4", "date_4", "event_5", "date_5", "event_6", "date_6"))
# b <- last_event %>% 
#   filter(str_detect(event_3, "collectiondt_germline|collectiondt_tumor_1")) %>% 
#   filter(str_detect(event_4, "rad")) %>% 
#   right_join(Global_data[c("avatar_id", "Disease_Status_germline")], ., by = "avatar_id") %>% 
#   select(c("avatar_id", "Disease_Status_germline", "event_1", "date_1", "event_2", "date_2", "event_3", "date_3", 
#            "event_4", "date_4", "event_5", "date_5", "event_6", "date_6"))


last_event <- last_event %>%  select(ncol(last_event):1) %>% 
  mutate(last_date_available = coalesce(!!! select(., starts_with("date_"))
         )) %>% 
  mutate(last_event_available = coalesce(!!! select(., starts_with("event_"))
    ))
# table(last_event$last_event_available) # Check why date_contact_lost are for only 3 patients
# table(Global_data$date_contact_lost) # 
id <- paste(unique( Global_data$avatar_id[which(!is.na(Global_data$was_contact_lost))] ), collapse = '|')

# a <- last_event[which(!is.na(Global_data$date_contact_lost)),] %>% 
#   purrr::keep(~!all(is.na(.)))

# a <- last_event %>% 
#   filter(str_detect(avatar_id , id)) %>% 
#   purrr::keep(~!all(is.na(.)))

# write.csv(last_event, paste0(path, "/last_event.csv"))


Global_data <- Global_data %>% 
  mutate(diagnosis_MM_year = year(date_of_MM_diagnosis)) %>% 
  mutate(diagnosis_MM_year = case_when(
    diagnosis_MM_year < 2009     ~ "previous 2009",
    diagnosis_MM_year >= 2009 &
      diagnosis_MM_year <= 2011  ~ "2009-2011",
    diagnosis_MM_year >= 2015 &
      diagnosis_MM_year <= 2016  ~ "2015-2016",
    diagnosis_MM_year >= 2017    ~ "later of 2017",
    TRUE                         ~ as.character(diagnosis_MM_year)
  )) %>%
  
  # Code progression from SM to MM
  mutate(sm_progression_event = case_when(
    smoldering_status == "Progressed from Smoldering"       ~ 1,
    smoldering_status == "Is Smoldering"                    ~ 0
    )) %>% 
  mutate(pfs_sm_MM_date = round(interval(start = sm_date_diagnosis, end = date_of_MM_diagnosis)/                      
           duration(n=1, unit="days"), 3)
  ) %>% 

  # Add date_death as progression_date when no previous progression_date
  mutate(pfs_date_death= case_when(
    date_death > date_last_follow_up |
      is.na(date_death)                          ~ NA_POSIXct_,
    date_death <= date_last_follow_up |
        is.na(date_last_follow_up)               ~ date_death # remove 36 dates
    )) %>%
  # PFS FROM Dx DATE
  mutate(pfs_progression_date = coalesce(progression_date, pfs_date_death)) %>% 
  
  mutate(Progression_event = case_when(
    !is.na(pfs_progression_date)              ~ 1,
    is.na(pfs_progression_date)               ~ 0
  )) %>% 
  # Add last_date_available
  left_join(., last_event %>% select(c("avatar_id", "last_date_available", "last_event_available")),
               by = "avatar_id") %>% 
  mutate(pfs_progression_date = coalesce(pfs_progression_date, last_date_available)) %>% 
  
  # PFS FROM DRUG DATE
  mutate(pfs_drug_progression_date = coalesce(progression_drug_date, pfs_date_death)) %>% 
  mutate(drug_progression_event = case_when(
    !is.na(pfs_drug_progression_date)          ~ 1,
    is.na(pfs_drug_progression_date)           ~ 0
  )) %>% 
  # Add last_date_available
  mutate(pfs_drug_progression_date = coalesce(pfs_drug_progression_date, last_date_available)) %>% 
  # Remove the drug dates after last date available
  mutate(pfs_line_start_date = case_when(
    line_start_date_1 >= last_date_available        ~ NA_POSIXct_, # doesn't remove date
    TRUE                                            ~ line_start_date_1
  )) %>%
  mutate(Drugs_ever = ifelse(!is.na(line_start_date_1), "Drug", "No Drug")) %>% 
  mutate(Drugs_ever = factor(Drugs_ever, levels = c("No Drug", "Drug"))) %>% 
  mutate(drugs_before_germline = case_when(
    pfs_line_start_date < collectiondt_germline         ~ "Blood after Drugs",
    pfs_line_start_date >= collectiondt_germline |
      is.na(pfs_line_start_date)                         ~ "Blood before Drugs"
  )) %>% 
  
  mutate(rad_bf_germline = if_else(rad_start_date_1 < collectiondt_germline, "Blood after Radiation", "No")) %>% 
  # PFS FROM RAD DATE
  mutate(pfs_rad_progression_date = coalesce(progression_rad_date, pfs_date_death)) %>% 
  mutate(rad_progression_event = case_when(
    !is.na(pfs_rad_progression_date)           ~ 1,
    is.na(pfs_rad_progression_date)            ~ 0
  )) %>% 
  # Add last_date_available
  mutate(pfs_rad_progression_date = coalesce(pfs_rad_progression_date, last_date_available)) %>% 
  # Remove the rad dates after last date available
  mutate(pfs_rad_start_date = case_when(
    rad_start_date_1 >= last_date_available        ~ NA_POSIXct_, # doesn't remove date
    TRUE                                           ~ rad_start_date_1
  )) %>%
  mutate(Radiation_ever = ifelse(!is.na(rad_start_date_1), "Radiation", "No Radiation")) %>% 
  mutate(interval_radiation_germline = interval(start = pfs_rad_start_date, collectiondt_germline)/
           duration(n=1, units = "days")) %>% 
  mutate(radiation_before_germline = case_when(
    pfs_rad_start_date < collectiondt_germline           ~ "Blood after Radiation",
    pfs_rad_start_date >= collectiondt_germline |
      is.na(pfs_rad_start_date)                          ~ "Blood before Radiation or no Rad"
  )) %>% 
  
  
  # PFS FROM HCT DATE
  mutate(pfs_hct_progression_date = coalesce(progression_hct_date, pfs_date_death)) %>% 
  mutate(hct_progression_event = case_when(
    !is.na(pfs_hct_progression_date)           ~ 1,
    is.na(pfs_hct_progression_date)            ~ 0
  )) %>% 
  # Add last_date_available
  mutate(pfs_hct_progression_date = coalesce(pfs_hct_progression_date, last_date_available)) %>% 
  # Remove the hct dates after last date available
  mutate(pfs_hct_start_date = case_when(
    date_of_bmt_1 >= last_date_available           ~ NA_POSIXct_, # doesn't remove date
    TRUE                                           ~ date_of_bmt_1
  )) %>%
  mutate(HCT_ever = ifelse(is.na(date_of_bmt_1), "No HCT", "HCT")) %>% 
  mutate(HCT_ever = factor(HCT_ever, levels = c("No HCT", "HCT"))) %>% 
  mutate(HCT_before_germline = case_when(
    pfs_hct_start_date < collectiondt_germline          ~ "Blood after HCT",
    pfs_hct_start_date >= collectiondt_germline |
      is.na(pfs_hct_start_date)                         ~ "Blood before HCT or no HCT"
  )) %>%
  mutate(HCT_vs_germline = case_when(
    pfs_hct_start_date < collectiondt_germline          ~ "Blood after HCT",
    pfs_hct_start_date >= collectiondt_germline         ~ "Blood before HCT",
      is.na(pfs_hct_start_date)                         ~ "No HCT"
  )) %>%
  mutate(interval_HCT_germline = interval(start = pfs_hct_start_date, end = collectiondt_germline)/
         duration(n= 1, units = 'days')) %>% 
  
  # # PFS FROM Drugs/HCT DATE
  # mutate(pfs_treatment_progression_date = coalesce(progression_treatment_date, pfs_date_death)) %>% 
  # mutate(treatment_progression_event = case_when(
  #   !is.na(pfs_treatment_progression_date)           ~ 1,
  #   is.na(pfs_treatment_progression_date)            ~ 0
  # )) %>% 
  # # Add last_date_available
  # mutate(pfs_treatment_progression_date = coalesce(pfs_treatment_progression_date, last_date_available)) %>% 
  # # Take first treatment dates after last date available from pfs_hct_start_date and pfs_line_start_date
  # mutate(pfs_treatment_start_date = case_when(
  #   pfs_line_start_date >= pfs_hct_start_date         ~ NA_POSIXct_,
  #   TRUE                                              ~ pfs_line_start_date
  # )) %>%
  # mutate(pfs_treatment_start_date = coalesce(pfs_treatment_start_date, pfs_hct_start_date)) %>% 
  mutate(pfs_treatment = case_when(
    !is.na(pfs_hct_start_date) &
      !is.na(pfs_line_start_date)                     ~ "Drug + HCT",
    !is.na(pfs_hct_start_date) &
      is.na(pfs_line_start_date)                      ~ "HCT only (before or after blood)",
    is.na(pfs_hct_start_date) &
      !is.na(pfs_line_start_date)                     ~ "Drug only"
  )) %>% 

  
  # OS FROM Dx DATE
  mutate(os_date_surv = Vital_Status_Date) %>% 
  mutate(os_event =  case_when(
    final_vitals == "Dead"                    ~ 1,
    final_vitals != "Dead"                    ~ 0
    )) %>% 
  
  
  mutate(ISS = case_when(
    !is.na(date_of_MM_diagnosis)            ~ ISS,
    TRUE                                        ~ NA_character_
  )) %>% 
  mutate(Disease_Status_germline = tolower(Disease_Status_germline)) %>% 
  mutate(Disease_Status_germline = capwords(Disease_Status_germline)) %>% 
  mutate(Disease_Status_germline = na_if(Disease_Status_germline, "NANA")) %>% 
  mutate(Disease_Status_germline = str_replace(Disease_Status_germline, "Amyloidosis- Diagnostic Marrow", "Amyloidosis")) %>% 
  mutate(Disease_Status_germline = str_replace(Disease_Status_germline, "Refractory anemia with ring sideroblasts", "Refractory Anemia with Ring Sideroblasts (RARS)")) %>% 
  mutate(Disease_Status_germline =
           factor(Disease_Status_germline, levels = c("Pre Treatment Newly Diagnosed Multiple Myeloma",
                                                      "Post Treatment Newly Diagnosed Multiple Myeloma",
                                                      "Early Relapse Multiple Myeloma",
                                                      "Late Relapse Multiple Myeloma",
                                                      "Smoldering Multiple Myeloma", "Mgus",
                                                      "Solitary Plasmacytoma", "Normal Marrow", 
                                                      "Amyloidosis", "Waldenstrom Macroglobulinemia", 
                                                      "Myelofibrosis", "Refractory Anemia with Ring Sideroblasts (RARS)",
                                                      "Polyclonal Gammopathy"))) %>% 
  mutate(Disease_Status_facet = case_when(
    Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
      Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma" |
      Disease_Status_germline == "Early Relapse Multiple Myeloma" |
      Disease_Status_germline == "Late Relapse Multiple Myeloma"                      ~ "MM",
    Disease_Status_germline == "Mgus"                                                 ~ "MGUS",
    Disease_Status_germline == "Smoldering Multiple Myeloma"                          ~ "Smoldering"
  )) %>% 
  mutate(Disease_Status_facet = factor(Disease_Status_facet, levels=c("MM", "Smoldering", "MGUS"))) %>% 
  mutate(Treatment_ever = case_when(
    is.na(drug_start_date_1) &
      is.na(rad_start_date_1) &
      is.na(date_of_bmt_1) ~ "No Treatment",
    TRUE                   ~ "Treatment Given"
  )) %>% 
  mutate(ISS_grp = ifelse(str_detect(ISS, "II"), "II-III", ISS)) %>%
  mutate(ISSdx_grp = ifelse(str_detect(ISS_at_MMdx, "II"), "II-III", ISS_at_MMdx))
  
# Cleaning
rm(all_dates, all_dates1, last_event, Last_labs_dates, Contact_lost, OS_data, Staging_ISS, capwords)

# write.csv(Global_data, paste0(path, "/Global_data updated.csv"))


########################################### II ### Create all the age from dates ----
enddate <- today()
Global_data$Age <- interval(start= Global_data$Date_of_Birth, end= enddate)/                      
  duration(n=1, unit="years")
Global_data$Age <- round(Global_data$Age, 3)
# summary(Global_data$Age)

Global_data$Age_at_diagnosis_closest_germline <- interval(start= Global_data$Date_of_Birth, end= Global_data$Dx_date_closest_germline)/                      
  duration(n=1, unit="years")
Global_data$Age_at_diagnosis_closest_germline <- round(Global_data$Age_at_diagnosis_closest_germline, 3)
# summary(Global_data$Age_at_diagnosis, na.rm = TRUE)
Global_data$Age_at_MMonly_diagnosis <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_MM_diagnosis)/                      
  duration(n=1, unit="years")
Global_data$Age_at_MMonly_diagnosis <- round(Global_data$Age_at_MMonly_diagnosis, 3)
Global_data$MM_age_cat <- as.factor(findInterval(Global_data$Age_at_MMonly_diagnosis, c(0,50,60,70)))
levels(Global_data$MM_age_cat) <- c("<50", "50-59", "60-69", ">70")

Global_data <- Global_data %>% 
  mutate(MM_age_cat5 = case_when(
    Age_at_MMonly_diagnosis < 50    ~ "<50",
    Age_at_MMonly_diagnosis < 55    ~ "50-54",
    Age_at_MMonly_diagnosis < 60    ~ "55-59",
    Age_at_MMonly_diagnosis < 65    ~ "60-64",
    Age_at_MMonly_diagnosis < 70    ~ "65-69",
    # Age_at_MMonly_diagnosis < 75    ~ "70-74",
    # Age_at_MMonly_diagnosis < 80    ~ "75-79",
    Age_at_MMonly_diagnosis >= 70   ~ ">=70"
  ), 
  MM_age_cat5 = factor(MM_age_cat5, levels = c("<50", "50-54", "55-59", "60-64", "65-69",
                                               ">=70"))
  ) %>% 
  mutate(MM_age_cat3 = case_when(
    Age_at_MMonly_diagnosis < 50    ~ "<50",
    Age_at_MMonly_diagnosis < 53    ~ "50-52",
    Age_at_MMonly_diagnosis < 56    ~ "53-55",
    Age_at_MMonly_diagnosis < 59    ~ "56-58",
    Age_at_MMonly_diagnosis < 62    ~ "59-61",
    Age_at_MMonly_diagnosis < 65    ~ "62-64",
    Age_at_MMonly_diagnosis < 68    ~ "65-67",
    Age_at_MMonly_diagnosis < 71    ~ "68-70",
    # Age_at_MMonly_diagnosis < 74    ~ "71-73",
    # Age_at_MMonly_diagnosis < 77    ~ "74-77",
    # Age_at_MMonly_diagnosis < 80    ~ "78-80",
    
    
    Age_at_MMonly_diagnosis >= 71   ~ ">=71"
  ), 
  MM_age_cat3 = factor(MM_age_cat3, levels = c("<50", "50-52", "53-55", "56-58", "59-61", "62-64", "65-67", "68-70", 
                                               ">=71"))
  )

# Global_data$MM_age_cat5 <- as.factor(findInterval(Global_data$Age_at_MMonly_diagnosis, c(0,50, 55,60, 65, 70, 75, 80)))
# levels(Global_data$MM_age_cat5) <- c("<50", "50-54", "55-59", "60-64", "65-69", "70-64", "75-79",
#                                      ">=80")

# Global_data$MM_age_cat3 <- as.factor(findInterval(Global_data$Age_at_MMonly_diagnosis, c(0,50, 53, 56, 59, 62,65, 68,71, 74, 77, 80)))
# levels(Global_data$MM_age_cat3) <- c("<50", "50-52", "53-55", "56-58", "59-61", "62-64", "65-67", "68-70", 
#                                      "71-73", "74-77", "78-80", 
#                                      ">=80")

Global_data$Age_at_MMSMMGUSdiagnosis <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_MMSMMGUSdiagnosis)/                      
  duration(n=1, unit="years")
Global_data$Age_at_MMSMMGUSdiagnosis <- round(Global_data$Age_at_MMSMMGUSdiagnosis, 3)

Global_data$Age_at_death <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_death)/                      
  duration(n=1, unit="years")
Global_data$Age_at_death <- round(Global_data$Age_at_death, 3)
# summary(Global_data$Age_at_death, na.rm = TRUE)

Global_data$Age_at_lastfollowup <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_last_follow_up)/                      
  duration(n=1, unit="years")
Global_data$Age_at_lastfollowup <- round(Global_data$Age_at_lastfollowup, 3)
# summary(Global_data$Age_at_lastfollowup, na.rm = TRUE)

Global_data$Age_at_lastdate <- interval(start= Global_data$Date_of_Birth, end= Global_data$last_date_available)/                      
  duration(n=1, unit="years")
Global_data$Age_at_lastdate <- round(Global_data$Age_at_lastdate, 3)
# summary(Global_data$Age_at_lastdate, na.rm = TRUE)

Global_data$Age_at_firstdrug <- interval(start= Global_data$Date_of_Birth, end= Global_data$line_start_date_1)/                      
  duration(n=1, unit="years")
Global_data$Age_at_firstdrug <- round(Global_data$Age_at_firstdrug, 3)
# summary(Global_data$Age_at_firstdrug, na.rm = TRUE)

Global_data$Age_at_firstbmt <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_bmt_1)/                      
  duration(n=1, unit="years")
Global_data$Age_at_firstbmt <- round(Global_data$Age_at_firstbmt, 3)
# summary(Global_data$Age_at_firstbmt, na.rm = TRUE)

Global_data$days_at_firsthct <- interval(start= Global_data$date_of_MM_diagnosis, end= Global_data$date_of_bmt_1)/                      
  duration(n=1, unit="days")
Global_data$days_at_firsthct <- round(Global_data$days_at_firsthct, 3)

Global_data$Age_at_firstrad <- interval(start= Global_data$Date_of_Birth, end= Global_data$rad_start_date_1)/                      
  duration(n=1, unit="years")
Global_data$Age_at_firstrad <- round(Global_data$Age_at_firstrad, 3)
# summary(Global_data$Age_at_firstrad, na.rm = TRUE)

Global_data$Age_at_germcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt_germline)/                      
  duration(n=1, unit="years")
Global_data$Age_at_germcollect <- round(Global_data$Age_at_germcollect, 3)
# summary(Global_data$Age_at_germcollect, na.rm = TRUE)

Global_data$Age_at_tumorcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt_tumor_1)/                      
  duration(n=1, unit="years")
Global_data$Age_at_tumorcollect <- round(Global_data$Age_at_tumorcollect, 3)
# summary(Global_data$Age_at_tumorcollect, na.rm = TRUE)

Global_data$age_at_progression <- interval(start= Global_data$Date_of_Birth, end= Global_data$pfs_progression_date)/                      
  duration(n=1, unit="years")
Global_data$age_at_progression <- round(Global_data$age_at_progression, 3)

Global_data$month_at_progression_Dx <- interval(start= Global_data$date_of_diagnosis_1, end= Global_data$pfs_progression_date)/                      
  duration(n=1, unit="months")
Global_data$month_at_progression_Dx <- round(Global_data$month_at_progression_Dx, 3)
# b <- Global_data[,c("avatar_id", "month_at_progression_Dx", "date_of_diagnosis_1", "pfs_progression_date", "last_date_available", "progression_date", 
#                  "last_event_available")]

Global_data$month_at_progression_drug <- interval(start= Global_data$pfs_line_start_date, end= Global_data$pfs_drug_progression_date)/                      
  duration(n=1, unit="months")
Global_data$month_at_progression_drug <- round(Global_data$month_at_progression_drug, 3)
# b <- Global_data[,c("avatar_id", "month_at_progression_drug", "pfs_line_start_date", "pfs_drug_progression_date", "last_date_available", "progression_date", 
#                  "last_event_available")]


Global_data$month_at_progression_rad <- interval(start= Global_data$pfs_rad_start_date, end= Global_data$pfs_rad_progression_date)/                      
  duration(n=1, unit="months")
Global_data$month_at_progression_rad <- round(Global_data$month_at_progression_rad, 3)

Global_data$month_at_progression_hct <- interval(start= Global_data$pfs_hct_start_date, end= Global_data$pfs_hct_progression_date)/                      
  duration(n=1, unit="months")
Global_data$month_at_progression_hct <- round(Global_data$month_at_progression_hct, 3)

# Global_data$month_at_progression_treat <- interval(start= Global_data$pfs_treatment_start_date, end= Global_data$pfs_treatment_progression_date)/                      
#   duration(n=1, unit="months")
# Global_data$month_at_progression_treat <- round(Global_data$month_at_progression_treat, 3)

Global_data$month_at_os <- interval(start= Global_data$date_of_MM_diagnosis, end= Global_data$os_date_surv)/                      
  duration(n=1, unit="months")
Global_data$month_at_os <- round(Global_data$month_at_os, 3)
# b <- Global_data[,c("avatar_id", "month_at_os", "date_death", "date_of_diagnosis_1", "os_date_surv", "os_event", "last_date_available"
#                  )]
Global_data$age_at_os <- interval(start= Global_data$Date_of_Birth, end= Global_data$os_date_surv)/                      
  duration(n=1, unit="years")
Global_data$age_at_os <- round(Global_data$age_at_os, 3)

Global_data$time_to_hct <- interval(start= Global_data$date_of_MM_diagnosis, end= Global_data$date_of_bmt_1)/                      
  duration(n=1, unit="days")
Global_data$time_to_hct <- round(Global_data$time_to_hct, 3)

Global_data$time_to_drug <- interval(start= Global_data$date_of_MM_diagnosis, end= Global_data$line_start_date_1)/                      
  duration(n=1, unit="days")
Global_data$time_to_drug <- round(Global_data$time_to_drug, 3)

Global_data$days_blood_drugs <- interval(start= Global_data$collectiondt_germline, end= Global_data$line_start_date_1)/                      
  duration(n=1, unit="days")

Global_data$days_blood_hct <- interval(start= Global_data$collectiondt_germline, end= Global_data$date_of_bmt_1)/                      
  duration(n=1, unit="days")

# rm(b)
write_rds(Global_data, file = "Global_data.rds")

################################################################################################## III ## Germline ----
# Create dataframe for only the patients who had germline sequenced
germline_patient_data <- Global_data[!is.na(Global_data$Disease_Status_germline),]
# write.csv(germline_patient_data, paste0(path, "/germline_patient_data.csv"))



# Cleaning
# rm(Global_data, enddate)

# Do a check on dates
germline_patient_data <- germline_patient_data %>% 
  mutate(CH_status = factor(CH_status, levels = c("No CH", "CH"))) %>% 
  mutate(CH_status_TS = factor(CH_status_TS, levels = c("No CHIP", "CHIP"))) %>% 
  mutate(germlineBFdrugs = case_when(
    collectiondt_germline > line_start_date_1 ~ "No",
    collectiondt_germline <= line_start_date_1 ~ "OK",
    is.na(line_start_date_1) ~ "OK"
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
  )) %>% # remove censored patient when pfs_progression_date > date_contact_lost - remove 1 patient
  mutate(Progression_event = case_when( 
    Progression_event == 1 & 
      pfs_progression_date > date_contact_lost              ~ 0,
    TRUE ~ Progression_event
  )) %>% # remove censored patient when pfs_drug_progression_date > date_contact_lost - remove 1 patient
  mutate(drug_progression_event = case_when( 
    drug_progression_event == 1 & 
      pfs_drug_progression_date > date_contact_lost         ~ 0,
    TRUE ~ drug_progression_event
  )) %>% # remove censored patient when date_death > date_contact_lost (just in case)
  
  mutate(rad_progression_event = case_when( 
    rad_progression_event == 1 & 
      pfs_rad_progression_date > date_contact_lost          ~ 0,
    TRUE ~ rad_progression_event
  )) %>% 
  mutate(hct_progression_event = case_when( 
    hct_progression_event == 1 & 
      pfs_hct_progression_date > date_contact_lost          ~ 0,
    TRUE ~ hct_progression_event
  )) %>% 
  # mutate(treatment_progression_event = case_when( 
  #   treatment_progression_event == 1 & 
  #     pfs_treatment_progression_date > date_contact_lost    ~ 0,
  #   TRUE ~ treatment_progression_event
  # )) %>% 
  
  mutate(os_event = case_when( 
    os_event == 1 & 
      date_death > date_contact_lost                        ~ 0,
    TRUE ~ os_event
  )) %>% 
  mutate(days_at_firsthct = ifelse(days_at_firsthct < 0, NA_real_, days_at_firsthct)) %>% 
  mutate(days_hct_cat = case_when(
    days_at_firsthct >= 800            ~ "> 800",
    # days_at_firsthct > 800 &
    #   days_at_firsthct < 1500          ~ "> 800-1500",
    # days_at_firsthct > 300 &
    #   days_at_firsthct < 800           ~ "> 300-800",
    days_at_firsthct > 0 &
      days_at_firsthct < 800           ~ "> 0-800"
  )) %>% 
  mutate(ntile_at_firsthct = ntile(days_at_firsthct, 2)) %>% 
  group_by(ntile_at_firsthct) %>% 
  mutate(min_ntile = min(days_at_firsthct)) %>% ungroup()

write_rds(germline_patient_data, file = "germline_patient_data.rds")
# # write.csv(germline_patient_data, paste0(path, "/compared germline dates and Demographics.csv"))
# End create all variables

# tab <- table(germline_patient_data$GermBFtumorWES)
# # jpeg("barplot3.jpg", width = 350, height = 350)
# barplot(tab, main = "Frequency of collection date first observed", ylim = c(0,500))
# # dev.off()
# tab
# 
# # Cleaning
# rm(tab)

