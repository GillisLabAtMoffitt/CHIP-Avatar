# Treatment id date date drug drug drug

# MM_history <- dcast(setDT(mm_history), avatar_id ~ rowid(avatar_id), 
#                     value.var = c("date_of_diagnosis", "disease_stage", "versionMM"))
  

V2 <- dcast(setDT(TreatmentV2), avatar_id+drug_start_date+drug_stop_date ~ rowid(avatar_id),
            value.var = c("drug_name_"),fun.aggregate = )

V1 <- Treatment
Treatment <- Treatment %>% pivot_longer(c(5:11), names_to = "drug_regimen", values_to = "drug_name_",
                          values_drop_na = TRUE)

v1 <- dcast(setDT(treatment), avatar_id+drug_start_date+drug_stop_date ~ rowid(avatar_id), 
                   value.var = c("drug_name_", "treatment_line_"))


v11 <- apply(v1, 1, function(x) unique(na.omit(x)))
df <- as.data.frame.list(v11,row.names = NULL, check.rows = FALSE, check.names = TRUE)

v2 <- TreatmentV2 %>% pivot_wider(values_from = c("drug_name_", "treatment_line_"),
                                values_fill = list(drug_name_ = 0))



%>% 
  mutate(drug_name = coalesce(v2$drug_name__1, v2$drug_name__2, v2$drug_name__3))


bb <- for (id in v1$avatar_id) {
  apply(v1[5:50], 1, function(x) unique(na.omit(x)))
}
combine(na.omit(v1[,4:50]))
##############################################
colnames(TreatmentV2)


# TV2 <- dcast(setDT(TreatmentV2), avatar_id ~ rowid(avatar_id), 
#                              value.var = c("treatment_line_", "drug_start_date", "drug_name_", "drug_stop_date"))
TV22 <- dcast(setDT(TreatmentV2), avatar_id+drug_start_date ~ rowid(avatar_id), 
             value.var = c("drug_name_", "drug_stop_date"),
             na.rm = TRUE)
TVV22 <- dcast.data.table((TreatmentV2), avatar_id+drug_start_date ~ rowid(avatar_id), 
                          fun.aggregate = sum,
                          value.var = c("drug_name_"))
TVv2 <- TreatmentV2 %>% pivot_wider(id_cols = avatar_id, names_from = NULL,
                                    values_from = "drug_name_")
TreatmentV2 %>%  plyr::ddply(TreatmentV2, avatar_id, summarize, text=paste(drug_name_, collapse=""))

TreatmentV2 %>%
  group_by(avatar_id,drug_start_date) %>%
  summarise(text=paste(drug_name_,collapse=','))


TV222 <- dcast(setDT(TreatmentV2), avatar_id+drug_start_date+drug_stop_date ~ rowid(avatar_id), 
               drop = TRUE,
              value.var = c("treatment_line_", "drug_name_"))



# We want to have 1 row per regiment/line so we wouldn't need to dcast at the end..

treatment <- bind_rows(Treatment, TreatmentV2, TreatmentV4, .id = "versionTreat") %>% 
  distinct() %>% 
  arrange(drug_start_date)
Treatment <- dcast(setDT(treatment), avatar_id ~ rowid(avatar_id), 
                   value.var = c("drug_start_date", "drug_stop_date", "drug_name_"))

Treat <- TreatmentV2 %>% unite(drug_name_, c(drug_name_,drug_name_other), 
                                 sep = "; ", na.rm = TRUE, remove = FALSE)





Treat <- bind_rows(Treatment, Qcd_Treatment, .id = "Treatment") %>% 
  distinct(avatar_id, drug_start_date, drug_stop_date, drug_name_) # remove duplicated rows
TreatV2 <- bind_rows(TreatmentV2, Qcd_TreatmentV2, .id = "Treatment") %>% 
  distinct(avatar_id, drug_start_date, drug_stop_date, drug_name_) # remove duplicated rows
# Cleanup
rm(Qcd_Treatment, Qcd_TreatmentV2)
# Collapse drug_name_ V1

# Widen V2 and V4
TreatmV2 <- TreatV2 %>%
  group_by(avatar_id,drug_start_date, drug_stop_date) %>%
  summarise(drug_name_=paste(drug_name_,collapse='; ')) 
TreatmV4 <- TreatV4 %>%
  group_by(avatar_id,drug_start_date, drug_stop_date) %>%
  summarise(drug_name_=paste(drug_name_,collapse='; ')) 
colnames(Treatment)
# ready to bind
treat <- bind_rows(Treat, TreatmV2, TreatmV4, .id = "versionTreat") %>% 
  distinct(avatar_id, drug_start_date, drug_stop_date, drug_name_) %>% 
  arrange(drug_start_date)
TREATMENT <- dcast(setDT(treat), avatar_id ~ rowid(avatar_id), 
                   value.var = c("drug_start_date", "drug_name_", "drug_stop_date"))
# write.csv(Treatment,paste0(path, "/Treatment simplify.csv"))


