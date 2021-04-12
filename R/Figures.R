# Figures
analysis_data <- germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% filter(!is.na(date_of_MMonly_diagnosis))
  
# Demo
tbl <- analysis_data %>% 
  mutate(Whole = "MM patients") %>% 
  mutate(Race = str_replace(Race, "Unknown", NA_character_)) %>% 
  mutate(Ethnicity = str_replace(Ethnicity, "Unknown", NA_character_)) %>% 
  select(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis, 
         Gender, Race, Ethnicity, Whole, ISS, CH_status) %>%
  tbl_summary(by = Whole, label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis) ~ 2),
              missing = "no") %>% 
  bold_labels() %>% as_gt() %>%  
  # gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Demographics in germline MM Avatar patients.pdf"))

tbl <- germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Disease_Status_germline, Whole) %>% 
  tbl_summary(by = Whole, label = list(Disease_Status_germline ~ "Disease Status at Germline")) %>% 
  bold_labels() %>% as_gt()  %>%
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Disease status in germline MM Avatar patients.pdf"))

jpeg(paste0(path, "/Figures/Moffitt Symposium/Disease_Status_germline.jpeg"), height = 350, width = 600)
germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>%
  group_by(Disease_Status_germline) %>% count(Disease_Status_germline) %>% 
  mutate(Disease_Status_germline = fct_reorder(as.character(Disease_Status_germline), desc(n))) %>%
  arrange(n) %>% 
  ggplot(aes(x="", y=n, fill=Disease_Status_germline)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual("", values = c("hotpink", "orchid1", "mediumorchid1", "mediumpurple1", "lemonchiffon", "peachpuff", 
                                                               "lightcyan", "lightblue", "cyan", "steelblue1", "blue", "darkblue")) +
  theme_void(base_size = 16) +
  theme(plot.title = element_text(hjust = -0.15))+
  coord_polar("y", start=0, direction=-1) +
  labs(x=NULL, y=NULL, title="Disease Status at germline collection in MM Avatar data")
dev.off()

jpeg(paste0(path, "/Figures/Moffitt Symposium/Age at diagnosis repartition in MM Avatar.jpeg"), height = 400, width = 600)
p <- qplot(x =Age_at_MMonly_diagnosis, data=subset(germline_patient_data,!is.na(Age_at_MMonly_diagnosis)), fill=..count.., geom="histogram") 
p + scale_fill_viridis_c(
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1,
  option = "D",
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
) +
  theme_minimal(base_size = 16) +
  labs(x="Age at Diagnosis", y="Number of Patient", title="Repartition of Age at Diagnosis in MM Avatar")
dev.off()

# CH 
tbl <- analysis_data %>% 
  select(Age_at_MMonly_diagnosis, Gender, Race, Ethnicity, CH_status, ISS, ISS_grp,
         Age_at_firstbmt, Age_at_firstdrug, delay_to_treatment, Age_at_firstrad) %>% 
  mutate(Race = str_replace(Race, "Unknown", NA_character_)) %>%
  mutate(Ethnicity = str_replace(Ethnicity, "Unknown", NA_character_)) %>%
  mutate(Race = str_replace(Race, "Asian|More than one race|Am Indian", "Others")) %>% 
  tbl_summary(by = CH_status, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_MMonly_diagnosis, Race) ~ 2),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*ISS is reported for MM patients only*")) %>%  
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Demographics in CH patients.pdf"))

# CH IMIDS----

germline_patient_data_imids <- analysis_data %>% filter(imids_maintenance != "not qc'd")

IMIDs <- germline_patient_data_imids %>% 
  filter(imids_maintenance == "IMIDs as maintenance")
No_IMIDs <- germline_patient_data_imids %>% 
  filter(imids_maintenance == "no IMIDs as maintenance")

mysurv <- Surv(time = germline_patient_data_imids$month_at_os, event = germline_patient_data_imids$os_event)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = germline_patient_data_imids)

ggsurv <- ggsurvplot(myplot, data = germline_patient_data_imids,
                     title = "OS in germline patients data",
                     font.main = c(24, "bold", "black"),
                     font.x = c(20, "bold", "black"),
                     font.y = c(20, "bold", "black"),
                     font.legend = c(20, "bold", "black"),
                     font.tickslab = c(18, "bold", "black"),
                     size = 1.5, # line thickness default = 1
                     
                     xlab = "Time in months", 
                     legend = "top",
                     legend.title = "", 
                     # legend.labs = c("CH", "No CH"),
                     color = "CH_status",
                     palette = c("cornflowerblue", "firebrick1"),
                     linetype = "imids_maintenance", 
                     # pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.3,
                     risk.table.title = "Risk table (count(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     # risk.table.fontsize = 4,
                     tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                                    font.x = c(16, "bold", "black"),
                                                    font.y = c(16, "bold", "transparent"),
                                                    font.tickslab = c(19, "bold", "black")
                     )) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 215, y = 0.2, # x and y coordinates of the text
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs$month_at_os, event = IMIDs$os_event))~CH_status, 
                            data = IMIDs))$chisq, 
                            length((survdiff((Surv(time = IMIDs$month_at_os, event = IMIDs$os_event))~CH_status, 
                                             data = IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 180, xend = 180, y = 0.05, yend = 0.25,
           size = 2.5, colour = "black")+
  annotate("text", x = 222, y = 0.7, # x and y coordinates of the text
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_event))~CH_status, 
                            data = No_IMIDs))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_event))~CH_status, 
                                             data = No_IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 180, xend = 180, y = 0.35, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS CH IMIDs.jpeg"), height = 600, width = 600)
ggsurv
dev.off()

mysurv <- Surv(time = germline_patient_data_imids$month_at_progression_drug, event = germline_patient_data_imids$drug_progression_event)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = germline_patient_data_imids)
ggsurv <- ggsurvplot(myplot, data = germline_patient_data_imids,
                     title = "PFS in germline patients data",
                     font.main = c(24, "bold", "black"),
                     font.x = c(20, "bold", "black"),
                     font.y = c(20, "bold", "black"),
                     font.legend = c(20, "bold", "black"),
                     font.tickslab = c(18, "bold", "black"),
                     size = 1.5, # line thickness default = 1
                     
                     xlab = "Time in months", 
                     legend = "top",
                     legend.title = "", 
                     # legend.labs = c("CH", "No CH"),
                     color = "CH_status",
                     palette = c("cornflowerblue", "firebrick1"),
                     linetype = "imids_maintenance",
                     # pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.3,
                     risk.table.title = "Risk table (count(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                                    font.x = c(16, "bold", "black"),
                                                    font.y = c(16, "bold", "transparent"),
                                                    font.tickslab = c(19, "bold", "black")
                     )) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 150, y = 0.2, # x and y coordinates of the text
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs$month_at_progression_drug, event = IMIDs$drug_progression_event))~CH_status, 
                            data = IMIDs))$chisq, 
                            length((survdiff((Surv(time = IMIDs$month_at_progression_drug, event = IMIDs$drug_progression_event))~CH_status, 
                                             data = IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 100, xend = 100, y = 0.05, yend = 0.25,
           size = 2.5, colour = "black")+
  annotate("text", x = 150, y = 0.7, # x and y coordinates of the text
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs$month_at_progression_drug, event = No_IMIDs$drug_progression_event))~CH_status, 
                            data = No_IMIDs))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs$month_at_progression_drug, event = No_IMIDs$drug_progression_event))~CH_status, 
                                             data = No_IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 100, xend = 100, y = 0.35, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS CH IMIDs.jpeg"), height = 600, width = 600)
ggsurv
dev.off()

# In pre-treatment

PreT <- germline_patient_data_imids %>% filter(Disease_Status_facet == "MM")

IMIDs_MM <- PreT %>% 
  filter(imids_maintenance == "IMIDs as maintenance")

No_IMIDs_MM <- PreT %>% 
  filter(imids_maintenance == "no IMIDs as maintenance")


mysurv <- Surv(time = PreT$month_at_os, event = PreT$os_event)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = PreT)


jpeg(paste0(path, "/Figures/Moffitt Symposium/OS CH IMIDs in PreT.jpeg"), height = 600, width = 600)
ggsurv <- ggsurvplot(myplot, data = PreT,
                     title = "OS in Pre-Treatment patients",
                     font.main = c(24, "bold", "black"),
                     font.x = c(20, "bold", "black"),
                     font.y = c(20, "bold", "black"),
                     font.legend = c(20, "bold", "black"),
                     font.tickslab = c(18, "bold", "black"),
                     size = 1.5, # line thickness default = 1
                     
                     xlab = "Time in months", 
                     legend = "top",
                     legend.title = "", 
                     # legend.labs = c("CH", "No CH"),
                     color = "CH_status",
                     # palette = c("red", "blue", "#00BA38", "#00BA38"),
                     linetype = "imids_maintenance",
                     # pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.17,
                     risk.table.title = "Risk table (count(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                                    font.x = c(16, "bold", "black"),
                                                    font.y = c(16, "bold", "transparent"),
                                                    font.tickslab = c(19, "bold", "black")
                     )) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 215, y = 0.2,
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs_MM$month_at_os, event = IMIDs_MM$os_event))~CH_status, 
                            data = IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = IMIDs_MM$month_at_os, event = IMIDs_MM$os_event))~CH_status, 
                                             data = IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 180, xend = 180, y = 0.05, yend = 0.25,
           size = 2.5, colour = "black")+
  annotate("text", x = 222, y = 0.7,
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs_MM$month_at_os, event = No_IMIDs_MM$os_event))~CH_status, 
                            data = No_IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs_MM$month_at_os, event = No_IMIDs_MM$os_event))~CH_status, 
                                             data = No_IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 180, xend = 180, y = 0.35, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
ggsurv
dev.off()


mysurv <- Surv(time = PreT$month_at_progression_drug, event = PreT$drug_progression_event)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = PreT)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS CH IMIDs in PreT.jpeg"), height = 600, width = 600)
ggsurv <- ggsurvplot(myplot, data = PreT,
                     title = "PFS in Pre-Treatment patients",
                     font.main = c(24, "bold", "black"),
                     font.x = c(20, "bold", "black"),
                     font.y = c(20, "bold", "black"),
                     font.legend = c(20, "bold", "black"),
                     font.tickslab = c(18, "bold", "black"),
                     size = 1.5, # line thickness default = 1
                     
                     xlab = "Time in months", 
                     legend = "top",
                     legend.title = "", 
                     # legend.labs = c("CH", "No CH"),
                     color = "CH_status",
                     linetype = "imids_maintenance",
                     pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.17,
                     risk.table.title = "Risk table (count(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                                    font.x = c(16, "bold", "black"),
                                                    font.y = c(16, "bold", "transparent"),
                                                    font.tickslab = c(19, "bold", "black")
                     )) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 150, y = 0.2,
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs_MM$month_at_progression_drug, event = IMIDs_MM$drug_progression_event))~CH_status, 
                            data = IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = IMIDs_MM$month_at_progression_drug, event = IMIDs_MM$drug_progression_event))~CH_status, 
                                             data = IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 100, xend = 100, y = 0.05, yend = 0.25,
           size = 2.5, colour = "black")+
  annotate("text", x = 150, y = 0.7,
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs_MM$month_at_progression_drug, event = No_IMIDs_MM$drug_progression_event))~CH_status, 
                            data = No_IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs_MM$month_at_progression_drug, event = No_IMIDs_MM$drug_progression_event))~CH_status, 
                                             data = No_IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 100, xend = 100, y = 0.35, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
ggsurv
dev.off()

rm(germline_patient_data_imids)


# Disparities----
# Hisp
jpeg(paste0(path, "/Figures/Moffitt Symposium/Age by Eth.jpeg"), height = 600, width = 600)
p <- ggplot(analysis_data %>% filter(!is.na(Age_at_MMonly_diagnosis), 
                                             (Ethnicity == "Hispanic" | Ethnicity == "Non-Hispanic") ), 
            aes(x=Ethnicity, y=Age_at_MMonly_diagnosis), fill=Ethnicity) + 
  geom_boxplot(color = c("darkred", "darkgreen")) + 
  theme_minimal(base_size = 23) +
  labs(x=NULL, y="Age at Diagnosis", title="Age at Multiple Myeloma diagnosis")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(size = 7)
dev.off()

tbl <- analysis_data %>% 
  filter(str_detect(Ethnicity, "Hispanic")) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis,
         Drugs_ever, delay_to_treatment, Age_at_firstdrug, HCT_ever, Age_at_firstbmt, Radiation_ever, Age_at_firstrad,
         Gender, Ethnicity, ISS, CH_status) %>%
  tbl_summary(by = Ethnicity, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              missing = "no",
              digits = list(c(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis) ~ 2)) %>% bold_labels() %>% add_overall() %>% add_p() %>% bold_p(t = .05) %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Ethnicity table summary.pdf"))

tbl <- analysis_data %>% 
  filter(str_detect(Ethnicity, "Hispanic")) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(Drugs_ever, delay_to_treatment, Age_at_firstdrug, Ethnicity) %>%
  tbl_summary(by = Ethnicity, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency"),
              missing = "no") %>% bold_labels() %>% add_overall() %>% add_p() %>% bold_p(t = .05) %>% as_gt() %>%  
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Ethnicity delay summary.pdf"))



# Surv
ethnicity_surv <- analysis_data %>% 
  mutate(Ethnicity = factor(Ethnicity, levels= c("Hispanic", "Non-Hispanic"))) %>% filter(!is.na(Ethnicity))

mysurv <- Surv(time = ethnicity_surv$month_at_os, event = ethnicity_surv$os_event)
myplot <- survfit(mysurv~Ethnicity, data = ethnicity_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by Eth.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = ethnicity_surv,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("darkred", "darkgreen"),
           xlim = c(-25, 400),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
dev.off()

tbl1 <- ethnicity_surv %>% select(Ethnicity, Age_at_MMonly_diagnosis, Gender, ISS, Drugs_ever, HCT_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = ethnicity_surv$month_at_os, 
                             event = ethnicity_surv$os_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = ethnicity_surv$month_at_os, 
                   event = ethnicity_surv$os_event) ~ Ethnicity + Age_at_MMonly_diagnosis + Gender + ISS + Drugs_ever + HCT_ever, data =  ethnicity_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*ISS calculated for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Ethnicity coxph.pdf"))





mysurv <- Surv(time = ethnicity_surv$month_at_progression_drug, event = ethnicity_surv$drug_progression_event)
myplot <- survfit(mysurv~Ethnicity, data = ethnicity_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by Eth.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = ethnicity_surv,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("darkred", "darkgreen"),
           xlim = c(-25, 400),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
dev.off()

tbl1 <- ethnicity_surv %>% select(Ethnicity, Age_at_MMonly_diagnosis, Gender, ISS, HCT_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = ethnicity_surv$month_at_progression_drug, 
                             event = ethnicity_surv$drug_progression_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = ethnicity_surv$month_at_progression_drug, 
                   event = ethnicity_surv$drug_progression_event) ~ Ethnicity + Age_at_MMonly_diagnosis + Gender + ISS + HCT_ever, data =  ethnicity_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*ISS calculated for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Ethnicity PFS coxph.pdf"))


mysurv <- Surv(time = ethnicity_surv$month_at_os, event = ethnicity_surv$os_event)
myplot <- survfit(mysurv~Ethnicity + CH_status, data = ethnicity_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by Eth CH.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = ethnicity_surv,
           title = "OS in germline patients",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           color = "Ethnicity",
           linetype = "CH_status",
           palette = c("darkred", "darkgreen"),
           xlim = c(-25, 400),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
dev.off()

tbl1 <- ethnicity_surv %>% select(Ethnicity, Age_at_MMonly_diagnosis, Gender, ISS, Drugs_ever, HCT_ever, CH_status) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = ethnicity_surv$month_at_os, 
                             event = ethnicity_surv$os_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = ethnicity_surv$month_at_os, 
                   event = ethnicity_surv$os_event) ~ Ethnicity + Age_at_MMonly_diagnosis + Gender + ISS + Drugs_ever + HCT_ever + CH_status, data =  ethnicity_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*Data on active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Ethnicity CH coxph.pdf"))


mysurv <- Surv(time = ethnicity_surv$month_at_progression_drug, event = ethnicity_surv$drug_progression_event)
myplot <- survfit(mysurv~Ethnicity + CH_status, data = ethnicity_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by Eth CH.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = ethnicity_surv,
           title = "PFS in germline patients",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           color = "Ethnicity",
           linetype = "CH_status",
           palette = c("darkred", "darkgreen"),
           xlim = c(-25, 400),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
dev.off()

tbl1 <- ethnicity_surv %>% select(Ethnicity, Age_at_MMonly_diagnosis, Gender, ISS, HCT_ever, CH_status) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = ethnicity_surv$month_at_progression_drug, 
                             event = ethnicity_surv$drug_progression_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = ethnicity_surv$month_at_progression_drug, 
                   event = ethnicity_surv$drug_progression_event) ~ Ethnicity + Age_at_MMonly_diagnosis + Gender + ISS + HCT_ever + CH_status, data =  ethnicity_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*Data on active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Ethnicity PFS CH coxph.pdf"))

rm(ethnicity_surv)

# Black----
jpeg(paste0(path, "/Figures/Moffitt Symposium/Age by Race.jpeg"), height = 600, width = 600)
p <- analysis_data %>% 
  mutate(Race = factor(Race, levels=c("White", "Black"))) %>% filter(!is.na(Race)) %>%
  ggplot(aes(x=Race, y=Age_at_MMonly_diagnosis), fill=Race) + 
  geom_boxplot(color= c("blue", "lightsalmon1")) + # c(rep(c("#A92E5EFF", "#E65D2FFF", "grey"),3))
  theme_minimal(base_size = 22) +
  labs(x=NULL, y="Age at Diagnosis", title="Age of Multiple Myeloma diagnosis")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(size = 7)
dev.off()

tbl <- analysis_data %>% 
  mutate(Race = factor(Race, levels=c("White", "Black"))) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis,
         Drugs_ever, delay_to_treatment, Age_at_firstdrug, HCT_ever, Age_at_firstbmt, Radiation_ever, Age_at_firstrad,
         Gender, Race, ISS, CH_status) %>%
  tbl_summary(by = Race, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis) ~ 2)) %>% bold_labels() %>% add_overall() %>% add_p() %>% bold_p(t = .05) %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Race table summary.pdf"))

tbl <- analysis_data %>% 
  mutate(Race = factor(Race, levels=c("White", "Black"))) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(Drugs_ever, delay_to_treatment, Race, CH_status) %>%
  tbl_summary(by = Race, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency")) %>% bold_labels() %>% add_overall() %>% add_p() %>% bold_p(t = .05) %>% as_gt() %>%  
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Race delay summary.pdf"))

race_surv <- analysis_data %>% 
  mutate(Race1 = factor(Race, levels=c("White", "Black"))) %>% filter(!is.na(Race))

mysurv <- Surv(time = race_surv$month_at_os, event = race_surv$os_event)
myplot <- survfit(mysurv~Race1, data = race_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by Race.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = race_surv,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           legend.labs = c("White", "Black"),
           palette = c("blue", "lightsalmon1"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
dev.off()

tbl1 <- race_surv %>% select(Race1, Age_at_MMonly_diagnosis, Gender, ISS, Drugs_ever, HCT_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = race_surv$month_at_os, 
                             event = race_surv$os_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = race_surv$month_at_os, 
                   event = race_surv$os_event) ~ Race1 + Age_at_MMonly_diagnosis + Gender + ISS + Drugs_ever + HCT_ever, data =  race_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*ISS calculated for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Race coxph.pdf"))





mysurv <- Surv(time = race_surv$month_at_progression_drug, event = race_surv$drug_progression_event)
myplot <- survfit(mysurv~Race1, data = race_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by Race.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = race_surv,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           legend.labs = c("White", "Black"),
           palette = c("blue", "lightsalmon1"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
dev.off()

tbl1 <- race_surv %>% select(Race1, Age_at_MMonly_diagnosis, Gender, ISS, HCT_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = race_surv$month_at_progression_drug, 
                             event = race_surv$drug_progression_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = race_surv$month_at_progression_drug, 
                   event = race_surv$drug_progression_event) ~ Race1 + Age_at_MMonly_diagnosis + Gender + ISS + HCT_ever, data =  race_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*ISS calculated for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Race PFS coxph.pdf"))


mysurv <- Surv(time = race_surv$month_at_os, event = race_surv$os_event)
myplot <- survfit(mysurv~Race1 + CH_status, data = race_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by Race CH.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = race_surv,
           title = "OS in germline patients",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           color = "Race1",
           linetype = "CH_status",
           palette = c("lightsalmon1", "blue"),
           xlim = c(-25, 400),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
dev.off()

tbl1 <- race_surv %>% select(Race1, Age_at_MMonly_diagnosis, Gender, ISS, Drugs_ever, HCT_ever, CH_status) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = race_surv$month_at_os, 
                             event = race_surv$os_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = race_surv$month_at_os, 
                   event = race_surv$os_event) ~ Race1 + Age_at_MMonly_diagnosis + Gender + ISS + Drugs_ever + HCT_ever + CH_status, data =  race_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Race CH coxph.pdf"))

mysurv <- Surv(time = race_surv$month_at_progression_drug, event = race_surv$drug_progression_event)
myplot <- survfit(mysurv~Race1 + CH_status, data = race_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by Race CH.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = race_surv,
           title = "PFS in germline patients",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           color = "Race1",
           linetype = c("CH_status"),
           palette = c("lightsalmon1", "blue"),
           pval = TRUE,
           conf.int = FALSE,
           xlim = c(-25, 400),
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
dev.off()

tbl1 <- race_surv %>% select(Race1, Age_at_MMonly_diagnosis, Gender, ISS, HCT_ever, CH_status) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = race_surv$month_at_progression_drug, 
                             event = race_surv$drug_progression_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = race_surv$month_at_progression_drug, 
                   event = race_surv$drug_progression_event) ~ Race1 + Age_at_MMonly_diagnosis + Gender + ISS + HCT_ever + CH_status, data =  race_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Race CH PFS coxph.pdf"))

rm(race_surv)

# HCT----
analysis_data %>% 
  select(HCT_ever, Age_at_firstbmt, days_at_firsthct) %>% 
  mutate(Whole = "Germline patients") %>% 
  tbl_summary(by = Whole, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency"),
              digits = list(c(Age_at_firstbmt) ~ 2),
              missing = "no") %>% 
  bold_labels() %>% as_gt() %>%  
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

tbl <- analysis_data %>% 
  select(HCT_ever, CH_status, ISS, Ethnicity, Race, Gender, Drugs_ever) %>% 
  tbl_summary(by = HCT_ever, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency"),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/HCT summary.pdf"))

tbl <- analysis_data %>% 
  mutate(HCT_vs_germline = str_replace(HCT_vs_germline, "No HCT", NA_character_)) %>% 
  select(HCT_ever, HCT_before_germline, HCT_vs_germline, Age_at_firstbmt, days_at_firsthct, CH_status) %>% 
  tbl_summary(by = CH_status, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency"),
              digits = list(c(Age_at_firstbmt) ~ 2),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/HCT by CH.pdf"))

tbl <- analysis_data %>% 
  mutate(HCT_vs_germline = str_replace(HCT_vs_germline, "No HCT", NA_character_)) %>% 
  select(HCT_ever, HCT_before_germline, HCT_vs_germline, Age_at_firstbmt, days_at_firsthct, CH_status, ISS) %>% 
  tbl_summary(by = ISS, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_firstbmt) ~ 2), 
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/HCT by ISS.pdf"))

tbl <- analysis_data %>% 
  mutate(HCT_vs_germline = str_replace(HCT_vs_germline, "No HCT", NA_character_)) %>% 
  select(HCT_before_germline, Age_at_firstbmt, days_at_firsthct, CH_status, ISS) %>% 
  tbl_summary(by = HCT_before_germline, 
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_firstbmt) ~ 2), 
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/HCT vs germline summary.pdf"))

# Surv

mysurv <- Surv(time = analysis_data$month_at_os, event = analysis_data$os_event)
myplot <- survfit(mysurv~HCT_ever, data = analysis_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = analysis_data,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("tan1", "turquoise4"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
dev.off()

tbl1 <- analysis_data %>% select(HCT_ever, CH_status, ISS, Drugs_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = analysis_data$month_at_os, 
                             event = analysis_data$os_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = analysis_data$month_at_os, 
                   event = analysis_data$os_event) ~ HCT_ever + CH_status + ISS + Drugs_ever, data =  analysis_data) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/HCT coxph.pdf"))





mysurv <- Surv(time = analysis_data$month_at_progression_drug, event = analysis_data$drug_progression_event)
myplot <- survfit(mysurv~HCT_ever, data = analysis_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = analysis_data,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("tan1", "turquoise4"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
dev.off()

tbl1 <- analysis_data %>% select(HCT_ever, CH_status, ISS, Drugs_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = analysis_data$month_at_progression_drug, 
                             event = analysis_data$drug_progression_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = analysis_data$month_at_progression_drug, 
                   event = analysis_data$drug_progression_event) ~ HCT_ever + CH_status + ISS + Drugs_ever, data =  analysis_data) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
  gt::tab_source_note(gt::md("*Data on active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/HCT PFS coxph.pdf"))


# HCT CH

had_HCT <- analysis_data %>% 
  filter(HCT_ever == "HCT")
Not_had_HCT <- analysis_data %>% 
  filter(HCT_ever == "No HCT")

mysurv <- Surv(time = analysis_data$month_at_os, event = analysis_data$os_event)
myplot <- survfit(mysurv~CH_status+HCT_ever, data = analysis_data)

ggsurv <- ggsurvplot(myplot, data = analysis_data,
                     title = "OS in germline patients data",
                     font.main = c(24, "bold", "black"),
                     font.x = c(20, "bold", "black"),
                     font.y = c(20, "bold", "black"),
                     font.legend = c(20, "bold", "black"),
                     font.tickslab = c(18, "bold", "black"),
                     size = 1.5, # line thickness default = 1
                     
                     xlab = "Time in months", 
                     legend = "top",
                     legend.title = "", 
                     # legend.labs = c("CH", "No CH"),
                     color = "CH_status",
                     # palette = c("red", "blue", "#00BA38", "#00BA38"),
                     linetype = "HCT_ever", 
                     # pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.17,
                     risk.table.title = "Risk table (count(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     # risk.table.fontsize = 4,
                     tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                                    font.x = c(16, "bold", "black"),
                                                    font.y = c(16, "bold", "transparent"),
                                                    font.tickslab = c(19, "bold", "black")
                     )) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 215, y = 0.2, # x and y coordinates of the text
           label = paste0("P had_HCT = ", 
                          round(pchisq((survdiff(
                            (Surv(time = had_HCT$month_at_os, event = had_HCT$os_event))~CH_status, 
                            data = had_HCT))$chisq, 
                            length((survdiff((Surv(time = had_HCT$month_at_os, event = had_HCT$os_event))~CH_status, 
                                             data = had_HCT))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 180, xend = 180, y = 0.05, yend = 0.25,
           size = 2.5, colour = "black")+
  annotate("text", x = 222, y = 0.7, # x and y coordinates of the text
           label = paste0("P no HCT = ", 
                          round(pchisq((survdiff(
                            (Surv(time = Not_had_HCT$month_at_os, event = Not_had_HCT$os_event))~CH_status, 
                            data = Not_had_HCT))$chisq, 
                            length((survdiff((Surv(time = Not_had_HCT$month_at_os, event = Not_had_HCT$os_event))~CH_status, 
                                             data = Not_had_HCT))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 180, xend = 180, y = 0.35, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS CH HCT.jpeg"), height = 600, width = 600)
ggsurv
dev.off()

mysurv <- Surv(time = analysis_data$month_at_progression_drug, event = analysis_data$drug_progression_event)
myplot <- survfit(mysurv~CH_status+HCT_ever, data = analysis_data)
ggsurv <- ggsurvplot(myplot, data = analysis_data,
                     title = "PFS in germline patients data",
                     font.main = c(24, "bold", "black"),
                     font.x = c(20, "bold", "black"),
                     font.y = c(20, "bold", "black"),
                     font.legend = c(20, "bold", "black"),
                     font.tickslab = c(18, "bold", "black"),
                     size = 1.5, # line thickness default = 1
                     
                     xlab = "Time in months", 
                     legend = "top",
                     legend.title = "", 
                     # legend.labs = c("CH", "No CH"),
                     color = "CH_status",
                     # palette = c("red", "blue", "#00BA38", "#00BA38"),
                     linetype = "HCT_ever",
                     # pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.17,
                     risk.table.title = "Risk table (count(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                                    font.x = c(16, "bold", "black"),
                                                    font.y = c(16, "bold", "transparent"),
                                                    font.tickslab = c(19, "bold", "black")
                     )) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 150, y = 0.2, # x and y coordinates of the text
           label = paste0("P had_HCT = ", 
                          round(pchisq((survdiff(
                            (Surv(time = had_HCT$month_at_progression_drug, event = had_HCT$drug_progression_event))~CH_status, 
                            data = had_HCT))$chisq, 
                            length((survdiff((Surv(time = had_HCT$month_at_progression_drug, event = had_HCT$drug_progression_event))~CH_status, 
                                             data = had_HCT))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 100, xend = 100, y = 0.05, yend = 0.25,
           size = 2.5, colour = "black")+
  annotate("text", x = 150, y = 0.7, # x and y coordinates of the text
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = Not_had_HCT$month_at_progression_drug, event = Not_had_HCT$drug_progression_event))~CH_status, 
                            data = Not_had_HCT))$chisq, 
                            length((survdiff((Surv(time = Not_had_HCT$month_at_progression_drug, event = Not_had_HCT$drug_progression_event))~CH_status, 
                                             data = Not_had_HCT))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 100, xend = 100, y = 0.35, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS CH HCT.jpeg"), height = 600, width = 600)
ggsurv
dev.off()


# Drugs----
analysis_data %>% 
  select(Drugs_ever, Age_at_firstdrug, days_at_firstdrugs) %>% 
  mutate(Whole = "Germline patients") %>% 
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency"),
              digits = list(c(Age_at_firstbmt) ~ 2),
              missing = "no") %>% 
  bold_labels() %>% as_gt() %>%  
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

tbl <- analysis_data %>% 
  select(Drugs_ever, CH_status, ISS, Ethnicity, Race, Gender, HCT_ever) %>% 
  tbl_summary(by = Drugs_ever, 
              sort = list(everything() ~ "frequency"),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Drugs summary.pdf"))

tbl <- analysis_data %>% 
  select(Drugs_ever, Age_at_firstdrug, Age_at_MMonly_diagnosis, delay_to_treatment, CH_status) %>% 
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency"),
              digits = list(c(Age_at_firstdrug, Age_at_MMonly_diagnosis) ~ 2),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Drugs by CH.pdf"))

tbl <- analysis_data %>% 
  select(Drugs_ever, Age_at_firstdrug, Age_at_MMonly_diagnosis, delay_to_treatment, CH_status, ISS) %>% 
  tbl_summary(by = ISS, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_firstdrug) ~ 2), 
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Drugs by ISS.pdf"))

tbl <- analysis_data %>% 
  select(drugs_before_germline, CH_status, ISS) %>% 
  tbl_summary(by = drugs_before_germline, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Drugs vs germline summary.pdf"))

# Surv

mysurv <- Surv(time = analysis_data$month_at_os, event = analysis_data$os_event)
myplot <- survfit(mysurv~Drugs_ever, data = analysis_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by Drugs.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = analysis_data,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
dev.off()

# tbl1 <- analysis_data %>% select(Drugs_ever, CH_status, ISS, HCT_ever) %>% 
#   tbl_uvregression(method = survival::coxph, 
#                    y = (Surv(time = analysis_data$month_at_os, 
#                              event = analysis_data$os_event)),
#                    exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
#   bold_labels() %>% italicize_levels()
# tbl2 <- coxph(Surv(time = analysis_data$month_at_os, 
#                    event = analysis_data$os_event) ~ Drugs_ever + CH_status + ISS + HCT_ever, data =  analysis_data) %>%
#   tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
# tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
#   gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
#   gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))
# 
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Drugs coxph.pdf"))





mysurv <- Surv(time = analysis_data$month_at_progression_drug, event = analysis_data$drug_progression_event)
myplot <- survfit(mysurv~Drugs_ever, data = analysis_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by Drugs.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = analysis_data,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
dev.off()

# tbl1 <- analysis_data %>% select(Drugs_ever, CH_status, ISS, Drugs_ever) %>% 
#   tbl_uvregression(method = survival::coxph, 
#                    y = (Surv(time = analysis_data$month_at_progression_drug, 
#                              event = analysis_data$drug_progression_event)),
#                    exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
#   bold_labels() %>% italicize_levels()
# tbl2 <- coxph(Surv(time = analysis_data$month_at_progression_drug, 
#                    event = analysis_data$drug_progression_event) ~ Drugs_ever + CH_status + ISS + Drugs_ever, data =  analysis_data) %>%
#   tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
# tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**")) %>% as_gt() %>% 
#   gt::tab_source_note(gt::md("*Data on active MM patients only*")) %>% 
#   gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))
# 
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Drugs PFS coxph.pdf"))


# By number of regimen
regimen_data <- analysis_data %>%
  select(avatar_id, starts_with("treatment_line_"), CH_status, os_event, month_at_os, month_at_progression_drug, drug_progression_event) %>% 
  pivot_longer(cols = starts_with("treatment_line_"),
               names_to = "regimen_number", values_to = "treatment_line_", values_drop_na = TRUE) %>% 
  filter(treatment_line_ < 90) %>% 
  arrange(desc(treatment_line_)) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(treatment_line_ = case_when(
    treatment_line_ > 5         ~ "more than 5",
    TRUE                        ~ as.character(treatment_line_)
  )) 
regimen_data %>% 
  select(treatment_line_, CH_status) %>% 
  tbl_summary(by = CH_status,
              label = list(Age_at_MMonly_diagnosis ~ "Age at MM diagnosis", CH_status ~ "CH status"),
              missing_text = "No Drugs") %>% bold_labels() %>% add_p() %>% as_gt()

mysurv <- Surv(time = regimen_data$month_at_os, event = regimen_data$os_event)
myplot <- survfit(mysurv~treatment_line_, data = regimen_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by regimen number.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_data,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()

mysurv <- Surv(time = regimen_data$month_at_progression_drug, event = regimen_data$drug_progression_event)
myplot <- survfit(mysurv~treatment_line_, data = regimen_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by regimen number.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_data,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()


# Regimen

a <- analysis_data %>% 
  group_by(first_regimen_name) %>% mutate(n = n()) %>% filter(n >= 10) %>% select(first_regimen_name) %>% distinct()
common_regimen_name <- paste0(a$first_regimen_name, collapse = "|^")

tbl <- analysis_data %>%
  mutate(Whole = "Most common regimen in 1st regimen") %>% 
  filter(str_detect(first_regimen_name, common_regimen_name) | is.na(first_regimen_name)) %>% 
  select(first_regimen_name, Whole) %>% 
  tbl_summary(by = Whole,
              label = list(first_regimen_name ~ "First Regimen Name", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency"),
              missing_text = "No Drugs") %>% bold_labels() %>%  as_gt() %>%  
  gt::tab_source_note(gt::md("*Most common regimen = Regimen given at 10 patients or more*")) %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Regimen summary.pdf"))

tbl <- analysis_data %>%
  filter(str_detect(first_regimen_name, common_regimen_name)) %>% 
  select(CH_status, first_regimen_name) %>% 
  tbl_summary(by = CH_status, 
              label = list(first_regimen_name ~ "First Regimen Name", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency"),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Regimen by CH.pdf"))

tbl <- analysis_data %>%
  filter(str_detect(first_regimen_name, common_regimen_name)) %>% 
  select(ISS, first_regimen_name) %>% 
  tbl_summary(by = ISS, 
              label = list(first_regimen_name ~ "First Regimen Name", CH_status ~ "CH status"),
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*Data for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Regimen by ISS.pdf"))

regimen_data <- analysis_data %>%
  filter(str_detect(first_regimen_name, common_regimen_name))

mysurv <- Surv(time = regimen_data$month_at_os, event = regimen_data$os_event)
myplot <- survfit(mysurv~first_regimen_name, data = regimen_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by first_regimen_name.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_data,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
dev.off()


mysurv <- Surv(time = regimen_data$month_at_progression_drug, event = regimen_data$drug_progression_event)
myplot <- survfit(mysurv~first_regimen_name, data = regimen_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by first_regimen_name.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_data,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
dev.off()

# CH 
mysurv <- Surv(time = regimen_data$month_at_os, event = regimen_data$os_event)
myplot <- survfit(mysurv~first_regimen_name + CH_status, data = regimen_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by regimen CH.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_data,
           title = "OS in germline patients",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           color = "first_regimen_name",
           linetype = "CH_status",
           # palette = c("skyblue2", "khaki3", "palegreen3", "purple", "pink", "turquoise", "seashell2"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
dev.off()

mysurv <- Surv(time = regimen_data$month_at_progression_drug, event = regimen_data$drug_progression_event)
myplot <- survfit(mysurv~first_regimen_name + CH_status, data = regimen_data)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by regimen CH.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_data,
           title = "PFS in germline patients",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           color = "first_regimen_name",
           linetype = c("CH_status"),
           # palette = c("blue", "lightsalmon1"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
dev.off()

rm(regimen_data)


# First regimen before hct----
a <- analysis_data %>% 
  group_by(first_regimen_name) %>% mutate(n = n()) %>% filter(n >= 10) %>% select(first_regimen_name) %>% distinct()
common_regimen_name <- paste0(a$first_regimen_name, collapse = "|^")

regimen_in_hct <- analysis_data %>% filter(HCT_ever == "HCT") %>% 
  filter(str_detect(first_regimen_name, common_regimen_name)) %>% 
  mutate(first_regimen_before_hct = case_when(
    line_stop_date_1 < date_of_bmt_1              ~ "1st regimen before HCT",
    TRUE                                          ~ "HCT before 1st regimen"
  )) %>% 
  mutate(number_regimen_before_hct = case_when(
    line_stop_date_1 < date_of_bmt_1              ~ "1",
    line_stop_date_2 < date_of_bmt_1              ~ "2",
    line_stop_date_3 < date_of_bmt_1              ~ "3",
    line_stop_date_4 < date_of_bmt_1              ~ "4",
    TRUE                                          ~ "more than 4"
  ))


mysurv <- Surv(time = regimen_in_hct$month_at_os, event = regimen_in_hct$os_event)
myplot <- survfit(mysurv~first_regimen_before_hct, data = regimen_in_hct)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by regimen vs HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_in_hct,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()

mysurv <- Surv(time = regimen_in_hct$month_at_progression_drug, event = regimen_in_hct$drug_progression_event)
myplot <- survfit(mysurv~first_regimen_before_hct, data = regimen_in_hct)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by regimen vs HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_in_hct,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()

# regimen number bf HCT
mysurv <- Surv(time = regimen_in_hct$month_at_os, event = regimen_in_hct$os_event)
myplot <- survfit(mysurv~number_regimen_before_hct, data = regimen_in_hct)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by regimen number bf HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_in_hct,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()

mysurv <- Surv(time = regimen_in_hct$month_at_progression_drug, event = regimen_in_hct$drug_progression_event)
myplot <- survfit(mysurv~number_regimen_before_hct, data = regimen_in_hct)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by regimen number bf HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_in_hct,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()


regimen_in_hct_2 <- regimen_in_hct %>% 
  filter(first_regimen_before_hct == "1st regimen before HCT")

mysurv <- Surv(time = regimen_in_hct_2$month_at_os, event = regimen_in_hct_2$os_event)
myplot <- survfit(mysurv~first_regimen_name, data = regimen_in_hct_2)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by regimen name bf HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_in_hct_2,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()

mysurv <- Surv(time = regimen_in_hct_2$month_at_progression_drug, event = regimen_in_hct_2$drug_progression_event)
myplot <- survfit(mysurv~first_regimen_name, data = regimen_in_hct_2)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by regimen name bf HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = regimen_in_hct_2,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()

# In no HCT pop----
a <- analysis_data %>% 
  group_by(first_regimen_name) %>% mutate(n = n()) %>% filter(n >= 10) %>% select(first_regimen_name) %>% distinct()
common_regimen_name <- paste0(a$first_regimen_name, collapse = "|^")

no_HCT_pop <- analysis_data %>% filter(HCT_ever == "HCT") %>% 
  filter(str_detect(first_regimen_name, common_regimen_name))

mysurv <- Surv(time = no_HCT_pop$month_at_os, event = no_HCT_pop$os_event)
myplot <- survfit(mysurv~first_regimen_name, data = no_HCT_pop)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by regimen name no HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = no_HCT_pop,
           title = "OS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()

mysurv <- Surv(time = no_HCT_pop$month_at_progression_drug, event = no_HCT_pop$drug_progression_event)
myplot <- survfit(mysurv~first_regimen_name, data = no_HCT_pop)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by regimen name no HCT.jpeg"), height = 600, width = 600)
ggsurvplot(myplot, data = no_HCT_pop,
           title = "PFS in germline patient data",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           # palette = c("blue", "red"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)+ guides(colour = guide_legend(ncol = 2))
dev.off()


# For Brianna
b <- regimen_in_hct %>% filter(first_regimen_before_hct == "HCT before 1st regimen") %>% 
  select(avatar_id, date_of_bmt_1, line_start_date_1, line_stop_date_1, first_regimen_name, drug_name__1)
