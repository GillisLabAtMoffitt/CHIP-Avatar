# Hispanic
germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, Gender, Race, Ethnicity, Whole, ISS) %>%
  tbl_summary(by = Ethnicity, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Race) ~ 2)) %>% 
  bold_labels() %>% add_p()

# Does CH status affect PFS in month?

germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, month_at_os, month_at_progression_drug, month_at_progression_rad, CH_status, ISS) %>%
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline) ~ 2)) %>% 
  bold_labels() %>% add_p()

tbl1 <- germline_patient_data %>% 
  filter(CH_status == "CH") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, month_at_os, month_at_progression_drug, month_at_progression_rad, CH_status, ISS) %>%
  tbl_summary(by = ISS, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline) ~ 2)) %>% 
  bold_labels() %>% add_p()

tbl2 <- germline_patient_data %>% 
  filter(CH_status == "No_CH") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, month_at_os, month_at_progression_drug, month_at_progression_rad, CH_status, ISS) %>%
  tbl_summary(by = ISS, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline) ~ 2)) %>% 
  bold_labels() %>% add_p()

tbl_merge(list(tbl1, tbl2), tab_spanner = c("**CH**", "**No_CH**"))

germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(CH_status, ISS) %>%
  tbl_summary(by = ISS, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric")) %>% 
  bold_labels() %>% add_p()

tbl1 <- germline_patient_data %>% 
  filter(ISS == "I") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, month_at_os, month_at_progression_drug, month_at_progression_rad, CH_status, ISS) %>%
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline) ~ 2)) %>% 
  bold_labels() %>% add_p()

tbl2 <- germline_patient_data %>% 
  filter(ISS == "II") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, month_at_os, month_at_progression_drug, month_at_progression_rad, CH_status, ISS) %>%
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline) ~ 2)) %>% 
  bold_labels() %>% add_p()

tbl3 <- germline_patient_data %>% 
  filter(ISS == "III") %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, month_at_os, month_at_progression_drug, month_at_progression_rad, CH_status, ISS) %>%
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline) ~ 2)) %>% 
  bold_labels() %>% add_p()

tbl_merge(list(tbl1, tbl2, tbl3), tab_spanner = c("**ISS I**", "**ISS II**", "**ISS III**"))


# Metastasis----

germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(have_metastasis) %>%
  tbl_summary() %>% 
  bold_labels() 

germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(have_metastasis, CH_status) %>%
  tbl_summary(by = CH_status,
              missing = "no") %>% 
  bold_labels() %>% add_p() %>% bold_p(t = .05)





















