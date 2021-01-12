germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)
################################################################################### I ### PFS/OS by demo----
ethnicity_surv <- germline_patient_surv %>% 
  mutate(Ethnicity = factor(Ethnicity, levels= c("Hispanic", "Non-Hispanic"))) %>% filter(!is.na(Ethnicity))
  

race_surv <- germline_patient_surv %>% 
  mutate(Race1 = factor(Race, levels=c("White", "Black")))  %>% filter(!is.na(Race))

# Gender----
# From Dx
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$progression_surv)
myplot <- survfit(mysurv~Gender, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/PFS Gender from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Gender from Dx",
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
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
myplot <- survfit(mysurv~Gender, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/OS Gender.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
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

# Ethnicity----
# PFS
mysurv <- Surv(time = ethnicity_surv$month_at_progression_Dx, event = ethnicity_surv$progression_surv)
myplot <- survfit(mysurv~Ethnicity, data = ethnicity_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/PFS Ethnicity from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = ethnicity_surv,
           title = "PFS Ethnicity from Dx",
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
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
# dev.off()

# OS
mysurv <- Surv(time = ethnicity_surv$month_at_os, event = ethnicity_surv$os_surv_cor)
myplot <- survfit(mysurv~Ethnicity, data = ethnicity_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/OS Ethnicity.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = ethnicity_surv,
           title = "OS Ethnicity",
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

# Race----
# PFS
mysurv <- Surv(time = race_surv$month_at_progression_Dx, event = race_surv$progression_surv)
myplot <- survfit(mysurv~Race, data = race_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/PFS Race Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = race_surv,
           title = "PFS Race from Dx",
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
myplot <- survfit(mysurv~Race1, data = race_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/PFS Race1 Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = race_surv,
           title = "PFS Race1 from Dx",
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
mysurv <- Surv(time = race_surv$month_at_os, event = race_surv$os_surv_cor)
myplot <- survfit(mysurv~Race, data = race_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/OS Race.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = race_surv,
           title = "OS Race",
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
myplot <- survfit(mysurv~Race1, data = race_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/OS Race1.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = race_surv,
           title = "OS Race1",
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


################################################################################### II ### PFS/OS by ISS----
# PFS
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$progression_surv)
myplot <- survfit(mysurv~ISS, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/PFS ISS from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS ISS from Dx",
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

mysurv <- Surv(time = germline_patient_surv$month_at_progression_drug, event = germline_patient_surv$progression_drug_surv)
myplot <- survfit(mysurv~ISS, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/PFS ISS from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS ISS from drug date",
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
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
myplot <- survfit(mysurv~ISS, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Demographic/OS ISS.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
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





