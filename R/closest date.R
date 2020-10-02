# Find tumor date before germline

data <- germline_patient_data %>% select(c(avatar_id, SLID_germline, collectiondt_germline, drug_start_date_1, date_of_bmt_1,
                                            starts_with("SLID_tumor"), starts_with("collectiondt_tumor_")))
data <- pivot_longer(data, 
                      cols = -c(avatar_id, SLID_germline, collectiondt_germline, drug_start_date_1, date_of_bmt_1), 
                      names_to = c(".value", "tumor"),
                      names_sep = "_tumor_",
                      values_drop_na = TRUE)
data$interv <- interval(start= data$collectiondt_germline, end= data$collectiondt)/
  duration(n=1, unit="days")


data <- data %>% 
  mutate(GequalT_BFtreat = case_when(
    interv == 0 &
      collectiondt_germline <= drug_start_date_1 &
      collectiondt_germline <= date_of_bmt_1         ~ "Perfect",
    interv == 0 &
      is.na(drug_start_date_1) &
      is.na(date_of_bmt_1)                           ~ "Perfect",
    interv == 0 &
      collectiondt_germline <= drug_start_date_1 &
      is.na(date_of_bmt_1)                           ~ "Perfect",
    interv == 0 &
      is.na(drug_start_date_1) &
      collectiondt_germline <= date_of_bmt_1         ~ "Perfect"
  ))
# Keep only the perfect slid
data_perfect <- data %>% filter(GequalT_BFtreat == "Perfect") %>% 
  distinct() # But keep 4 patients who had 2 tumor collected the same day

data1 <- data %>% 
  distinct(avatar_id, SLID_germline, .keep_all = TRUE)
uid <- data1$avatar_id[duplicated(data1$avatar_id)]
uid1 <- data_perfect$SLID_germline[data_perfect$avatar_id == uid]

data_perfect$avatar_id == uid

data1 <- data %>% filter(avatar_id == uid, SLID_germline != uid1) %>%
  mutate(abs_interv = abs(interv)) %>% 
  arrange(abs_interv) %>% 
  distinct(avatar_id, .keep_all = TRUE)


# remove the id from data to find the closest date  but before treatment
uid <- paste(unique(data_perfect$avatar_id), collapse = '|')
data <- data[(!grepl(uid, data$avatar_id)),]

data <- data %>% 
  mutate(treatment_BF_germtumor = case_when(
    (drug_start_date_1 <= collectiondt_germline |
      drug_start_date_1 <= collectiondt) |
      (date_of_bmt_1 <= collectiondt_germline |
         date_of_bmt_1 <= collectiondt)              ~ "before",
    (drug_start_date_1 > collectiondt_germline |
       drug_start_date_1 > collectiondt) |
      (date_of_bmt_1 > collectiondt_germline |
         date_of_bmt_1 > collectiondt)              ~ "after"
  )) %>%
  mutate(interv_germ_BF_treatment = case_when(
    treatment_BF_germtumor == "after" |
      is.na(treatment_BF_germtumor)                 ~ abs(interv)
  ))

data_perfect1 <- data %>% filter(!is.na(interv_germ_BF_treatment)) %>% 
  arrange(interv) %>% 
  distinct()
# remove the id from data5 to find the closest date when germ,tumor after treatment
uid <- paste(unique(data_perfect1$avatar_id), collapse = '|')
data <- data[(!grepl(uid, data$avatar_id)),]

data_perfect2 <- data %>% 
  select(-c()) %>%
  mutate(abs_interv = abs(interv)) %>% 
  arrange(abs_interv) %>% 
  distinct(avatar_id, tumor, .keep_all = TRUE)


DATA <- bind_rows(data_perfect, data1, data_perfect1, data_perfect2) %>% 
  select(c("avatar_id", "SLID_germline", "collectiondt_germline", "drug_start_date_1", "date_of_bmt_1", "tumor",
           SLID_tumor = "SLID", collectiondt_tumor = "collectiondt", "interv"))
  
write.csv(DATA, paste0(path, "/List tumor SLID earliest or closest to germline.csv"))

  
