germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)

################################################################################### I ### General Survivals----
# Form Dx
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$progression_surv)
myplot <- survfit(mysurv~1)
plot(myplot)

# jpeg(paste0(path, "/Figures/Survivals/General PFS from Dx.jpeg"), width = 1000, height = 800)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS from Dx",
           font.main = c(16, "bold", "black"),
           xlab = "Time in months", 
           # surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table (number(%))",
           # risk.table.pos = "in",
           conf.int = FALSE,
           censor = TRUE
)
# dev.off()
Surv(germline_patient_surv$month_at_progression_Dx, germline_patient_surv$progression_surv)[1:10]
germline_patient_surv$progression_surv[1:10]

names(myplot)
myplot

summary(survfit(mysurv~1), times = 12) # probability of surviving (PFS) beyond 12 months 

table(germline_patient_surv$progression_surv)

# From drug
mysurv <- Surv(time = germline_patient_surv$month_at_progression_drug, event = germline_patient_surv$progression_drug_surv)
myplot <- survfit(mysurv~1)
# jpeg(paste0(path, "/Figures/Survivals/General PFS from drug.jpeg"), width = 1000, height = 800)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS from drug",
           font.main = c(16, "bold", "black"),
           xlab = "Time in months", 
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table (number(%))",
           conf.int = FALSE,
           censor = TRUE
)
# dev.off()

################################################################################### II ### PFS/OS by demo----
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

rm(race_surv, ethnicity_surv)

################################################################################### III ### PFS/OS by ISS----
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

################################################################################### III ### PFS/OS by Status and CH----
# From Dx---
# CH--
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$progression_surv)
myplot <- survfit(mysurv~CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()
# Summary of survival curves
res.sum <- surv_summary(myplot)
res.sum

summary(myplot)
a <- summary(myplot)$table
# write.csv(a, paste0(path, "/Figures/Survivals/CHIP/summary PFS by CH from Dx.csv"))

# Status--
germline_patient_data_simp <- germline_patient_surv %>% 
  filter(!str_detect(Disease_Status_germline, "Amyloidosis|MYELOFIBROSIS|Solitary|WALDENSTROM|Refractory|Normal|Poly"))

mysurv <- Surv(time = germline_patient_data_simp$month_at_progression_Dx, event = germline_patient_data_simp$progression_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20 when legend has names
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("MGUS", "MM", "SM"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
           # Censor
           # censor = TRUE,
           # ncensor.plot = TRUE
)
# dev.off()

# Summary of survival curves
res.sum <- surv_summary(myplot)
res.sum

summary(myplot)
a <- summary(myplot)$table
# write.csv(a, paste0(path, "/Figures/Survivals/summary PFS from Dx by simplify DS.csv"))

# Log-Rank test comparing survival curves for Significant Difference
survdiff(Surv(time = germline_patient_data_simp$month_at_progression_Dx, event = germline_patient_data_simp$progression_surv) ~
           Disease_Status_germline, data = germline_patient_data_simp)



# From drug---
# CH--
mysurv <- Surv(time = germline_patient_surv$month_at_progression_drug, event = germline_patient_surv$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()
summary(myplot)
a <- summary(myplot)$table
write.csv(a, paste0(path, "/Figures/Survivals/CHIP/summary PFS CH from drug date.csv"))

# Status--
mysurv <- Surv(time = germline_patient_data_simp$month_at_progression_drug, event = germline_patient_data_simp$progression_drug_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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
summary(myplot)
a <- summary(myplot)$table
# write.csv(a, paste0(path, "/Figures/Survivals/summary PFS by DS from drug date.csv"))

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("MM", "SM", "MGUS"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()
summary(myplot)
a <- summary(myplot)$table
# write.csv(a, paste0(path, "/Figures/Survivals/summary PFS by simplify DS from drugs date.csv"))


# OS---
# CH--
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS by CH.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS CH",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()
summary(myplot)
a <- summary(myplot)$table
# write.csv(a, paste0(path, "/Figures/Survivals/CHIP/summary OS by CH.csv"))

# Status--
mysurv <- Surv(time = germline_patient_data_simp$month_at_os, event = germline_patient_data_simp$os_surv_cor)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Figures/Survivals/OS by DS.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "OS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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
summary(myplot)
a <- summary(myplot)$table
# write.csv(a, paste0(path, "/Figures/Survivals/summary OS by DS.csv"))

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Figures/Survivals/OS by simplify DS.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "OS date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("MGUS", "MM", "SM"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()
summary(myplot)
a <- summary(myplot)$table
# write.csv(a, paste0(path, "/Figures/Survivals/summary OS by simplify DS.csv"))


################################################################################### IV ### Sequenced patients Status and CH----
patient <- readxl::read_xlsx(paste0(path, "/Nancy's working files/MM Avatar_Sequenced subset.xlsx"),
                             sheet = "Sequenced") %>% 
  select(avatar_id) %>% distinct()
id <- paste(patient$avatar_id, collapse = "|")
germline_patient_data_seqsimp <- germline_patient_data_simp[ grepl(id, germline_patient_data_simp$avatar_id) , ]

# From Dx---
# CH--
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_progression_Dx, event = germline_patient_data_seqsimp$progression_surv)
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from Dx in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_progression_Dx, event = germline_patient_data_seqsimp$progression_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS sequenced patients in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from Dx in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("MM", "SM"),
           palette = c("#00BA38", "blue"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
           # Censor
           # censor = TRUE,
           # ncensor.plot = TRUE
)
# dev.off()


# From Drug---
# CH--
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_progression_drug, event = germline_patient_data_seqsimp$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from drugs date in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "PFS from drug date",
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
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_progression_drug, event = germline_patient_data_seqsimp$progression_drug_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS from drugs date in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), #20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from drugs date in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # legend.labs = c("MM", "SM"),
           palette = c("#00BA38", "blue"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()

# OS---
# CH--
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_os, event = germline_patient_data_seqsimp$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS by CH in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "OS CH",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()

# Status--
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_os, event = germline_patient_data_seqsimp$os_surv_cor)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/OS by DS in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "OS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/OS by simplify DS in sequenced patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "OS date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("MGUS", "MM", "SM"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()

################################################################################### V ### For sequenced MM patients----
MM_surv <- germline_patient_data_seqsimp %>% filter(Disease_Status_facet == "MM")

# From Dx---
# CH--
mysurv <- Surv(time = MM_surv$month_at_progression_Dx, event = MM_surv$progression_surv)
myplot <- survfit(mysurv~CH_status, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from Dx in sequenced MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = MM_surv$month_at_progression_Dx, event = MM_surv$progression_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS in sequenced MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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


# From Drug---
# CH--
mysurv <- Surv(time = MM_surv$month_at_progression_drug, event = MM_surv$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from drugs date in sequenced MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "PFS from drug date",
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
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = MM_surv$month_at_progression_drug, event = MM_surv$progression_drug_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS from drugs date in sequenced MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), #20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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

# OS---
# CH--
mysurv <- Surv(time = MM_surv$month_at_os, event = MM_surv$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS by CH in sequenced MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "OS CH",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()

# Status--
mysurv <- Surv(time = MM_surv$month_at_os, event = MM_surv$os_surv_cor)
myplot <- survfit(mysurv~Disease_Status_germline, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/OS by DS in sequenced MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "OS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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


################################################################################### VI ### For sequenced<drugs patients----
patient1 <- readxl::read_xlsx(paste0(path, "/Nancy's working files/MM Avatar_Sequenced subset.xlsx"),
                             sheet = "Sequenced_before.drug.start") %>% 
  select(avatar_id) %>% distinct()
id <- paste(patient1$avatar_id, collapse = "|")
germline_patient_data_Dseqsimp <- germline_patient_data_simp[ grepl(id, germline_patient_data_simp$avatar_id) , ]

# From Dx---
# CH--
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_progression_Dx, event = germline_patient_data_Dseqsimp$progression_surv)
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from Dx in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_progression_Dx, event = germline_patient_data_Dseqsimp$progression_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS sequenced patients in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from Dx in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("MM", "SM"),
           palette = c("#00BA38", "blue"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
           # Censor
           # censor = TRUE,
           # ncensor.plot = TRUE
)
# dev.off()


# From Drug---
# CH--
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_progression_drug, event = germline_patient_data_Dseqsimp$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from drugs date in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "PFS from drug date",
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
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_progression_drug, event = germline_patient_data_Dseqsimp$progression_drug_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS from drugs date in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), #20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from drugs date in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # legend.labs = c("MM", "SM"),
           palette = c("#00BA38", "blue"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()

# OS---
# CH--
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_os, event = germline_patient_data_Dseqsimp$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS by CH in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "OS CH",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()

# Status--
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_os, event = germline_patient_data_Dseqsimp$os_surv_cor)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/OS by DS in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "OS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Figures/Survivals/OS by simplify DS in sequenced<drugs patients MM+SM.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "OS date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("MGUS", "MM", "SM"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()


################################################################################### VII ### For sequenced<drugs PreT patients----
PreT_surv <- germline_patient_data_Dseqsimp %>% filter(Disease_Status_facet == "MM")

# From Dx---
# CH--
mysurv <- Surv(time = PreT_surv$month_at_progression_Dx, event = PreT_surv$progression_surv)
myplot <- survfit(mysurv~CH_status, data = PreT_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from Dx in sequenced<drugs PreT patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT_surv,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = PreT_surv$month_at_progression_Dx, event = PreT_surv$progression_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = PreT_surv)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS in sequenced<drugs PreT patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT_surv,
           title = "PFS from diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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


# From Drug---
# CH--
mysurv <- Surv(time = PreT_surv$month_at_progression_drug, event = PreT_surv$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = PreT_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from drugs date in sequenced<drugs PreT patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT_surv,
           title = "PFS from drug date",
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
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           )
)
# dev.off()

# Status--
mysurv <- Surv(time = PreT_surv$month_at_progression_drug, event = PreT_surv$progression_drug_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = PreT_surv)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS from drugs date in sequenced<drugs PreT patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT_surv,
           title = "PFS from drugs date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), #20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "#00FF99", "#33FF33", "blue"),
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

# OS---
# CH--
mysurv <- Surv(time = PreT_surv$month_at_os, event = PreT_surv$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = PreT_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS by CH in sequenced<drugs PreT patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT_surv,
           title = "OS CH",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "", 
           # # legend.labs = c("CH", "No CH"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.17,
           risk.table.title = "Risk table (number(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black"))
)
# dev.off()

# Status--
mysurv <- Surv(time = PreT_surv$month_at_os, event = PreT_surv$os_surv_cor)
myplot <- survfit(mysurv~Disease_Status_germline, data = PreT_surv)
# jpeg(paste0(path, "/Figures/Survivals/OS by DS in sequenced<drugs PreT patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT_surv,
           title = "OS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM ", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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
















