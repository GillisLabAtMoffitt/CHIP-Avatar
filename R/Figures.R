# Figures

# Demo
tbl <- germline_patient_data %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  mutate(Race = str_replace(Race, "Unknown", NA_character_)) %>% 
  mutate(Ethnicity = str_replace(Ethnicity, "Unknown", NA_character_)) %>% 
  select(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis, 
         Gender, Race, Ethnicity, Whole, ISS, CH_status) %>%
  tbl_summary(by = Whole, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis) ~ 2),
              missing = "no") %>% 
  bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*ISS is reported for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Demographics in germline MM Avatar patients.pdf"))

tbl <- germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Disease_Status_germline, Whole) %>% 
  tbl_summary(by = Whole) %>% bold_labels() %>% as_gt()  %>%
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Disease status in germline MM Avatar patients.pdf"))




# CH 
tbl <- germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(Age_at_MMonly_diagnosis, Gender, Race, Ethnicity, CH_status, ISS, ISS_grp,
         Age_at_firstbmt, Age_at_firstdrug, delay_to_treatment, Age_at_firstrad) %>% 
  mutate(Race = str_replace(Race, "Unknown", NA_character_)) %>%
  mutate(Ethnicity = str_replace(Ethnicity, "Unknown", NA_character_)) %>%
  mutate(Race = str_replace(Race, "Asian|More than one race|Am Indian", "Others")) %>% 
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_MMonly_diagnosis, Race) ~ 2),
              missing = "no") %>% 
  add_p() %>% bold_p(t = .05) %>% bold_labels() %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*ISS is reported for MM patients only*")) %>%  
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Demographics in CH patients.pdf"))

# CH IMIDS

germline_patient_data_imids <- germline_patient_data %>% 
  filter(!is.na(Age_at_MMonly_diagnosis)) %>% filter(imids_maintenance != "not qc'd")

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
                     # palette = c("red", "blue", "#00BA38", "#00BA38"),
                     linetype = "imids_maintenance", 
                     # pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.17,
                     risk.table.title = "Risk table (number(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     risk.table.fontsize = 4,
                     tables.theme = theme_survminer(base_size = 5,
                                                    font.main = c(16, "bold", "black"),
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
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS CH IMIDs.jpeg"), height = 1200, width = 1200)
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
                     # palette = c("red", "blue", "#00BA38", "#00BA38"),
                     linetype = "imids_maintenance",
                     # pval = TRUE,
                     conf.int = FALSE,
                     # Add risk table
                     tables.height = 0.17,
                     risk.table.title = "Risk table (number(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     risk.table.fontsize = 4,
                     tables.theme = theme_survminer(base_size = 5,
                                                    font.main = c(16, "bold", "black"),
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
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS CH IMIDs.jpeg"), height = 1200, width = 1200)
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


jpeg(paste0(path, "/Figures/Moffitt Symposium/OS CH IMIDs in PreT.jpeg"), height = 1200, width = 1200)
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
                     risk.table.title = "Risk table (number(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     risk.table.fontsize = 4,
                     tables.theme = theme_survminer(base_size = 5,
                                                    font.main = c(16, "bold", "black"),
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
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS CH IMIDs in PreT.jpeg"), height = 1200, width = 1200)
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
                     risk.table.title = "Risk table (number(%))",
                     risk.table = "abs_pct",
                     risk.table.y.text = FALSE,
                     risk.table.fontsize = 4,
                     tables.theme = theme_survminer(base_size = 5,
                                                    font.main = c(16, "bold", "black"),
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




# Disparities
# Hisp
jpeg(paste0(path, "/Figures/Moffitt Symposium/Age by Eth.jpeg"), height = 1200, width = 1200)
p <- ggplot(germline_patient_data %>% filter(!is.na(Age_at_MMonly_diagnosis), (Ethnicity == "Hispanic" | Ethnicity == "Non-Hispanic") ), 
            aes(x=Ethnicity, y=Age_at_MMonly_diagnosis), fill=Ethnicity) + 
  geom_boxplot(color = c("darkred", "darkgreen")) + 
  theme_minimal(base_size = 23) +
  labs(x=NULL, y="Age at Diagnosis", title="Age at Multiple Myeloma diagnosis repartition")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means()
dev.off()

tbl <- germline_patient_data %>% 
  filter(str_detect(Ethnicity, "Hispanic")) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis,
         Drugs_ever, delay_to_treatment, Age_at_firstdrug, HCT_ever, Age_at_firstbmt, Radiation_ever, Age_at_firstrad,
         Gender, Ethnicity, ISS, CH_status) %>%
  tbl_summary(by = Ethnicity, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis) ~ 2)) %>% bold_labels() %>% add_overall() %>% add_p() %>% bold_p(t = .05) %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*ISS is reported for active MM patients only*")) %>% 
  gt::tab_style(style = gt::cell_text(color = "#0099CC"), locations = gt::cells_column_labels(everything()))

gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/Moffitt Symposium/Ethnicity table summary.pdf"))






germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)
ethnicity_surv <- germline_patient_surv %>% 
  mutate(Ethnicity = factor(Ethnicity, levels= c("Hispanic", "Non-Hispanic"))) %>% filter(!is.na(Ethnicity))

mysurv <- Surv(time = ethnicity_surv$month_at_os, event = ethnicity_surv$os_event)
myplot <- survfit(mysurv~Ethnicity, data = ethnicity_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/OS by Eth.jpeg"), height = 1200, width = 1200)
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
           legend.title = "Ethnicity",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("darkred", "darkgreen"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 4,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
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
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by Eth.jpeg"), height = 1200, width = 1200)
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
           legend.title = "Ethnicity",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           palette = c("darkred", "darkgreen"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 4,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
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




mysurv <- Surv(time = ethnicity_surv$month_at_progression_drug, event = ethnicity_surv$drug_progression_event)
myplot <- survfit(mysurv~Ethnicity + CH_status, data = ethnicity_surv)
jpeg(paste0(path, "/Figures/Moffitt Symposium/PFS by Eth CH.jpeg"), height = 1200, width = 1200)
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
           legend.title = "Ethnicity",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           color = "Ethnicity",
           linetype = "CH_status",
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 3,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))
dev.off()

# Black
p <- germline_patient_data %>% 
  mutate(Race = factor(Race, levels=c("White", "Black"))) %>% filter(!is.na(Race), !is.na(Disease_Status_facet)) %>%   ggplot(aes(x=Race, y=Age_at_MMonly_diagnosis), fill=Race) + 
  geom_boxplot(color= c("#A92E5EFF", "#E65D2FFF")) + # c(rep(c("#A92E5EFF", "#E65D2FFF", "grey"),3))
  theme_minimal(base_size = 22) +
  labs(x=NULL, y="Age at Diagnosis", title="Age of MM diagnosis repartition")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means()

germline_patient_data %>% 
  mutate(Race = factor(Race, levels=c("White", "Black"))) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(Whole = "Germline patients") %>% 
  select(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis, diagnosis_MM_year,
         delay_to_treatment, Drugs_ever, HCT_ever, Gender, Race, Ethnicity, ISS, CH_status) %>%
  tbl_summary(by = Race, 
              sort = list(everything() ~ "frequency", ISS ~ "alphanumeric"),
              digits = list(c(Age_at_diagnosis_closest_germline, Age_at_MMonly_diagnosis) ~ 2)) %>% bold_labels() %>% add_overall() %>% add_p() %>% bold_p(t = .05) %>% as_gt() %>%  
  gt::tab_source_note(gt::md("*ISS is reported for active MM patients only*"))

race_surv <- germline_patient_surv %>% 
  mutate(Race1 = factor(Race, levels=c("White", "Black")))  %>% filter(!is.na(Race))

mysurv <- Surv(time = race_surv$month_at_os, event = race_surv$os_event)
myplot <- survfit(mysurv~Race1, data = race_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/OS Race1.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = race_surv,
           title = "OS from MM diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Race",
           # # legend.labs = c("White", "Black"),
           # palette = c("#A92E5EFF", "#E65D2FFF"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 4,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)

tbl1 <- race_surv %>% select(Race1, Age_at_MMonly_diagnosis, Gender, ISS, Drugs_ever, HCT_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = race_surv$month_at_os, 
                             event = race_surv$os_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = race_surv$month_at_os, 
                   event = race_surv$os_event) ~ Race1 + Age_at_MMonly_diagnosis + Gender + ISS + Drugs_ever + HCT_ever, data =  race_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**"))

mysurv <- Surv(time = race_surv$month_at_progression_drug, event = race_surv$drug_progression_event)
myplot <- survfit(mysurv~Race1, data = race_surv)
ggsurvplot(myplot, data = race_surv,
           title = "PFS by Race W/B from drug",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Race",
           # # legend.labs = c("White", "Black"),
           # palette = c("#A92E5EFF", "#E65D2FFF"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 4,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)

tbl1 <- race_surv %>% select(Race1, Age_at_MMonly_diagnosis, Gender, ISS, HCT_ever) %>% 
  tbl_uvregression(method = survival::coxph, 
                   y = (Surv(time = race_surv$month_at_progression_drug, 
                             event = race_surv$drug_progression_event)),
                   exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
  bold_labels() %>% italicize_levels()
tbl2 <- coxph(Surv(time = race_surv$month_at_progression_drug, 
                   event = race_surv$drug_progression_event) ~ Race1 + Age_at_MMonly_diagnosis + Gender + ISS + HCT_ever, data =  race_surv) %>%
  tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**"))

mysurv <- Surv(time = race_surv$month_at_os, event = race_surv$os_event)
myplot <- survfit(mysurv~Race1+ CH_status, data = race_surv)

ggsurvplot(myplot, data = race_surv,
           title = "OS by Race/CH",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Race",
           # # legend.labs = c("Hipanic", "Non-Hispanic"),
           color = "Race1",
           linetype = "CH_status",
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 4,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
) + guides(linetype = guide_legend(nrow = 2, title = "")) + guides(colour = guide_legend(nrow = 2))








