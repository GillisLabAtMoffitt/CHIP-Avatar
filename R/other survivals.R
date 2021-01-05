################################################################################### IV ### PFS/OS drug date by demo----
# PFS
germline_demo <- germline_patient_data %>% 
  mutate(Ethnicity = factor(Ethnicity, levels= c("Hispanic", "Non-Hispanic"))) %>% filter(!is.na(Ethnicity)) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black"))) %>% filter(!is.na(Race))
# Ethnicity
mysurv <- Surv(time = germline_demo$month_at_progression_drug, event = germline_demo$progression_drug_surv)
myplot <- survfit(mysurv~Ethnicity, data = germline_demo)
# jpeg(paste0(path, "/Output Survivals/PFS Ethnicity from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_demo,
           title = "PFS Ethnicity",
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
           # palette = c("darkred", "darkgreen"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
# dev.off()

# Race
mysurv <- Surv(time = germline_demo$month_at_progression_drug, event = germline_demo$progression_drug_surv)
myplot <- survfit(mysurv~Race, data = germline_demo)
# jpeg(paste0(path, "/Output Survivals/PFS Race from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_demo,
           title = "PFS Race",
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
           palette = c("#A92E5EFF", "#E65D2FFF"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
# dev.off()
# OS
mysurv <- Surv(time = germline_demo$month_at_os, event = germline_demo$os_surv_cor)
myplot <- survfit(mysurv~Ethnicity, data = germline_demo)
# jpeg(paste0(path, "/Output Survivals/OS Ethnicity from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_demo,
           title = "OS from date of diagnosis",
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
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()


# Race
mysurv <- Surv(time = germline_demo$month_at_os, event = germline_demo$os_surv_cor)
myplot <- survfit(mysurv~Race, data = germline_demo)
# jpeg(paste0(path, "/Output Survivals/OS Race from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_demo,
           title = "OS from date of diagnosis",
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
           palette = c("#A92E5EFF", "#E65D2FFF"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()

################################################################################### V ### PFS/OS drug date by regimen----
# PFS
germline_patient_treatment <- germline_patient_data %>% 
  mutate(first_regimen_name_MM = str_replace_na(first_regimen_name_MM, replacement = "No Drugs")) %>% 
  mutate(regimen_name = factor(first_regimen_name_MM, levels = c("VRd", "No Drugs", "Bor-Dex", "CyBorD or VCd", "Rd", "Dex",
                                                                 "Len", "Td", "KRd", "V", "NonMM drugs", "M", "VAd", "ABCD", "IRD",
                                                                 "D-RVd or dara-RVd"))) %>%
  filter(!is.na(regimen_name))

mysurv <- Surv(time = germline_patient_treatment$month_at_progression_drug, event = germline_patient_treatment$progression_drug_surv)
myplot <- survfit(mysurv~regimen_name, data = germline_patient_treatment)
# jpeg(paste0(path, "/Output Survivals/PFS Regimen from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_treatment,
           title = "PFS drugs_first_regimen",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "regimen_name",
           # # legend.labs = c("Hipanic", "Non-Hispanic", "Unknown"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
# dev.off()
# OS
mysurv <- Surv(time = germline_patient_treatment$month_at_os, event = germline_patient_treatment$os_surv_cor)
myplot <- survfit(mysurv~regimen_name, data = germline_patient_treatment)
# jpeg(paste0(path, "/Output Survivals/OS Regimen from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_treatment,
           title = "OS drugs_first_regimen from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "regimen_name",
           # # legend.labs = c("Hipanic", "Non-Hispanic", "Unknown"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()

################################################################################### V ### PFS/OS drug date by BMT----
germline_patient_data <- germline_patient_data %>% 
  mutate(HCT = ifelse(!is.na(date_of_bmt_1), "HCT", "No HCT"))
# PFS
mysurv <- Surv(time = germline_patient_surv$month_at_progression_drug, event = germline_patient_surv$progression_drug_surv)
myplot <- survfit(mysurv~HCT, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/PFS HCT from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic", "Unknown"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
# dev.off()
# OS
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
myplot <- survfit(mysurv~HCT, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/OS HCT from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS HCT from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("Hipanic", "Non-Hispanic", "Unknown"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()


################################################################################### I ### PFS/OS drug date by Gender----
# PFS
mysurv <- Surv(time = germline_patient_data$month_at_progression_drug, event = germline_patient_data$progression_drug_surv)
myplot <- survfit(mysurv~Gender, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/PFS Gender from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data,
           title = "PFS Gender",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Gender",
           # # legend.labs = c("No Gender", "Gender"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
# dev.off()
# OS
mysurv <- Surv(time = germline_patient_data$month_at_os, event = germline_patient_data$os_surv_cor)
myplot <- survfit(mysurv~Gender, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/OS Gender from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data,
           title = "OS Gender from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Gender",
           # # legend.labs = c("No Gender", "Gender"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()

################################################################################### V ### PFS/OS drug date by Drugs----
germline_patient_treatment <- germline_patient_treatment %>% 
  mutate(Drugs = ifelse(!is.na(regimen_name), "Drugs", "No Drugs"))

# OS
mysurv <- Surv(time = germline_patient_data$month_at_os, event = germline_patient_data$os_surv_cor)
myplot <- survfit(mysurv~Drugs, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/OS Drugs from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data,
           title = "OS Drugs from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # legend.labs = c("No Drugs", "Drugs"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()

################################################################################### V ### PFS/OS drug date by ISS----
# PFS
mysurv <- Surv(time = germline_patient_data$month_at_progression_drug, event = germline_patient_data$progression_drug_surv)
myplot <- survfit(mysurv~ISS, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/PFS ISS from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data,
           title = "PFS ISS",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "ISS",
           # # legend.labs = c("No ISS", "ISS"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
# dev.off()
# OS
mysurv <- Surv(time = germline_patient_data$month_at_os, event = germline_patient_data$os_surv_cor)
myplot <- survfit(mysurv~ISS, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/OS ISS from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data,
           title = "OS ISS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "ISS",
           # # legend.labs = c("No ISS", "ISS"),
           # palette = c("darkred", "darkgreen", "grey"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()








