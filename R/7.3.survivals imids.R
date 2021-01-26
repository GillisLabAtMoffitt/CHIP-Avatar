# We want mainteanance IMIDs on the MM sequenced population
germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)
germline_patient_data_simp <- germline_patient_surv %>% 
  filter(!str_detect(Disease_Status_germline, "Amyloidosis|MYELOFIBROSIS|Solitary|WALDENSTROM|Refractory|Normal|Poly"))
# patient <- readxl::read_xlsx(paste0(path, "/Nancy's working files/MM Avatar_Sequenced subset.xlsx"),
#                              sheet = "Sequenced") %>% 
#   select(avatar_id) %>% distinct()
# id <- paste(patient$avatar_id, collapse = "|")
# germline_patient_data_seqsimp <- germline_patient_data_simp[ grepl(id, germline_patient_data_simp$avatar_id) , ]

################################################################################### I ### PFS IMIDS ----
# In germline 651 patients
IMIDs <- germline_patient_data_simp %>% 
  filter(imids_maintenance == "received IMIDs as maintenance")
# PFS Dx
mysurv <- Surv(time = IMIDs$month_at_progression_Dx, event = IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH IMIDs from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
           title = "PFS from diagnosis in IMIDs patients",
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
           ))
# dev.off()
# PFS drug
mysurv <- Surv(time = IMIDs$month_at_progression_drug, event = IMIDs$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH IMIDs from drug.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
           title = "PFS from drug in IMIDs patients",
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
           ))
# dev.off()
# OS
mysurv <- Surv(time = IMIDs$month_at_os, event = IMIDs$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
           title = "OS from diagnosis in IMIDs patients",
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
           ))
# dev.off()

################################################################################### I ### PFS No IMIDS ----
No_IMIDs <- germline_patient_data_simp %>% 
  filter(imids_maintenance == "no IMIDs as maintenance")
# PFS Dx
mysurv <- Surv(time = No_IMIDs$month_at_progression_Dx, event = No_IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH noIMIDs from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
           title = "PFS from diagnosis in no IMIDs patients",
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
           ))
# dev.off()
# PFS drug
mysurv <- Surv(time = No_IMIDs$month_at_progression_drug, event = No_IMIDs$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH noIMIDs from drug.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
           title = "PFS from drug in no IMIDs patients",
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
           ))
# dev.off()
# OS
mysurv <- Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH noIMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
           title = "OS  in no IMIDs patients",
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
           ))
# dev.off()

# In MM----
IMIDs_MM <- germline_patient_data_simp %>% 
  filter(imids_maintenance == "received IMIDs as maintenance") %>% 
  filter(Disease_Status_facet == "MM")

mysurv <- Surv(time = IMIDs_MM$month_at_progression_Dx, event = IMIDs_MM$progression_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs_MM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH IMIDs MM patients from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs_MM,
           title = "PFS from diagnosis in IMIDs patients",
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
           ))
# dev.off()
# PFS drug
mysurv <- Surv(time = IMIDs_MM$month_at_progression_drug, event = IMIDs_MM$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs_MM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH IMIDs MM patients from drug.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs_MM,
           title = "PFS from drug in IMIDs patients",
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
           ))
# dev.off()
# OS
mysurv <- Surv(time = IMIDs_MM$month_at_os, event = IMIDs_MM$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = IMIDs_MM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH IMIDs MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs_MM,
           title = "OS in IMIDs patients",
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
           ))
# dev.off()


No_IMIDs_MM <- germline_patient_data_simp %>% 
  filter(imids_maintenance == "no IMIDs as maintenance") %>% 
  filter(Disease_Status_facet == "MM")
# PFS Dx
mysurv <- Surv(time = No_IMIDs_MM$month_at_progression_Dx, event = No_IMIDs_MM$progression_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs_MM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH noIMIDs MM patients from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs_MM,
           title = "PFS from diagnosis in no IMIDs patients",
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
           ))
# dev.off()
# PFS drug
mysurv <- Surv(time = No_IMIDs_MM$month_at_progression_drug, event = No_IMIDs_MM$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs_MM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH noIMIDs MM patients from drug.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs_MM,
           title = "PFS from drug in no IMIDs patients",
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
           ))
# dev.off()
# OS
mysurv <- Surv(time = No_IMIDs_MM$month_at_os, event = No_IMIDs_MM$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs_MM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH noIMIDs MM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs_MM,
           title = "OS in no IMIDs patients",
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
           ))
# dev.off()

# In Smoldering
# IMIDs_SM <- germline_patient_data_simp %>% 
#   filter(imids_maintenance == "received IMIDs as maintenance") %>% 
#   filter(Disease_Status_facet == "Smoldering")
# # PFS Dx
# mysurv <- Surv(time = IMIDs_SM$month_at_progression_Dx, event = IMIDs_SM$progression_surv)
# myplot <- survfit(mysurv~CH_status, data = IMIDs_SM)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH IMIDs SM patients from Dx.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = IMIDs_SM,
#            title = "PFS from diagnosis in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()
# # PFS drug
# mysurv <- Surv(time = IMIDs_SM$month_at_progression_drug, event = IMIDs_SM$progression_drug_surv)
# myplot <- survfit(mysurv~CH_status, data = IMIDs_SM)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH IMIDs SM patients from drug.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = IMIDs_SM,
#            title = "PFS from drug in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()
# # OS
# mysurv <- Surv(time = IMIDs_SM$month_at_os, event = IMIDs_SM$os_surv_cor)
# myplot <- survfit(mysurv~CH_status, data = IMIDs_SM)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH IMIDs SM patients.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = IMIDs_SM,
#            title = "OS in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()


No_IMIDs_SM <- germline_patient_data_simp %>% 
  filter(imids_maintenance == "no IMIDs as maintenance") %>% 
  filter(Disease_Status_facet == "Smoldering")
# PFS Dx
mysurv <- Surv(time = No_IMIDs_SM$month_at_progression_Dx, event = No_IMIDs_SM$progression_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs_SM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH noIMIDs SM patients from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs_SM,
           title = "PFS from diagnosis in IMIDs no patients",
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
           ))
# dev.off()
# PFS drug
mysurv <- Surv(time = No_IMIDs_SM$month_at_progression_drug, event = No_IMIDs_SM$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs_SM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH noIMIDs SM patients from drug.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs_SM,
           title = "PFS from drug in IMIDs no patients",
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
           ))
# dev.off()
# OS
mysurv <- Surv(time = No_IMIDs_SM$month_at_os, event = No_IMIDs_SM$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs_SM)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS  CH noIMIDs SM patients.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs_SM,
           title = "OS in IMIDs no patients",
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
           ))
# dev.off()

# # In MGUS
# IMIDs_Mgus <- germline_patient_data_simp %>% 
#   filter(imids_maintenance == "received IMIDs as maintenance") %>% 
#   filter(Disease_Status_facet == "MGUS")
# # PFS Dx
# mysurv <- Surv(time = IMIDs_Mgus$month_at_progression_Dx, event = IMIDs_Mgus$progression_surv)
# myplot <- survfit(mysurv~CH_status, data = IMIDs_Mgus)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS Dx CH status Mgus patients with IMIDs.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = IMIDs_Mgus,
#            title = "PFS from diagnosis in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()
# # PFS drug
# mysurv <- Surv(time = IMIDs_Mgus$month_at_progression_drug, event = IMIDs_Mgus$progression_drug_surv)
# myplot <- survfit(mysurv~CH_status, data = IMIDs_Mgus)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS Drug CH status Mgus patients with IMIDs.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = IMIDs_Mgus,
#            title = "PFS from drug in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()
# # OS
# mysurv <- Surv(time = IMIDs_Mgus$month_at_os, event = IMIDs_Mgus$os_surv_cor)
# myplot <- survfit(mysurv~CH_status, data = IMIDs_Mgus)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS Dx CH status Mgus patients with IMIDs.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = IMIDs_Mgus,
#            title = "OS in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()
# 
# 
# No_IMIDs_Mgus <- germline_patient_data_simp %>% 
#   filter(imids_maintenance == "no IMIDs as maintenance") %>% 
#   filter(Disease_Status_facet == "MGUS")
# # PFS Dx
# mysurv <- Surv(time = No_IMIDs_Mgus$month_at_progression_Dx, event = No_IMIDs_Mgus$progression_surv)
# myplot <- survfit(mysurv~CH_status, data = No_IMIDs_Mgus)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS Dx CH status Mgus patients with No IMIDs.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = No_IMIDs_Mgus,
#            title = "PFS from diagnosis",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()
# # PFS drug
# mysurv <- Surv(time = No_IMIDs_Mgus$month_at_progression_drug, event = No_IMIDs_Mgus$progression_drug_surv)
# myplot <- survfit(mysurv~CH_status, data = No_IMIDs_Mgus)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS Drug CH status Mgus patients with No IMIDs.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = No_IMIDs_Mgus,
#            title = "PFS from drug in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()
# # OS
# mysurv <- Surv(time = No_IMIDs_Mgus$month_at_os, event = No_IMIDs_Mgus$os_surv_cor)
# myplot <- survfit(mysurv~CH_status, data = No_IMIDs_Mgus)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS Dx CH status Mgus patients with No IMIDs.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = No_IMIDs_Mgus,
#            title = "OS in IMIDs patients",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(20, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5, # line thickness default = 1
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "", 
#            # legend.labs = c("CH", "No CH"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.17,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ))
# # dev.off()

###### Combined----
germline_patient_data_comb <- germline_patient_data_simp %>% filter(imids_maintenance != "not qc'd")
mysurv <- Surv(time = germline_patient_data_comb$month_at_os, event = germline_patient_data_comb$os_surv_cor)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = germline_patient_data_comb)

surv_diff <- survdiff(mysurv~CH_status+imids_maintenance, data = germline_patient_data_comb)
surv_diff
pchisq(surv_diff$chisq, length(surv_diff$n)-1, lower.tail = FALSE)

pchisq((survdiff((Surv(time = IMIDs$month_at_os, event = IMIDs$os_surv_cor))~CH_status, data = IMIDs))$chisq, 
       length((survdiff((Surv(time = IMIDs$month_at_os, event = IMIDs$os_surv_cor))~CH_status, data = IMIDs))$n)-1, 
       lower.tail = FALSE)
pchisq((survdiff((Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_surv_cor))~CH_status, data = No_IMIDs))$chisq, 
       length((survdiff((Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_surv_cor))~CH_status, data = No_IMIDs))$n)-1, 
       lower.tail = FALSE)

surv_pvalue(myplot)
coxfit <- coxph(
  Surv(time = germline_patient_data_comb$month_at_os, event = germline_patient_data_comb$os_surv_cor) ~ CH_status+imids_maintenance,
  data = germline_patient_data_comb,
  ties = 'exact')
summary(coxfit)

## jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new CH status vs IMIDs OS.jpeg"), width = 1200, height = 900)
ggsurv <- ggsurvplot(myplot, data = germline_patient_data_comb,
           title = "OS from diagnosis",
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
           ))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 225, y = 0.39, # x and y coordinates of the text
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs$month_at_os, event = IMIDs$os_surv_cor))~CH_status, 
                            data = IMIDs))$chisq, 
                            length((survdiff((Surv(time = IMIDs$month_at_os, event = IMIDs$os_surv_cor))~CH_status, 
                                             data = IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 200, xend = 200, y = 0.32, yend = 0.46,
             size = 2.5, colour = "black")+
  annotate("text", x = 230, y = 0.8, # x and y coordinates of the text
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_surv_cor))~CH_status, 
                            data = No_IMIDs))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_surv_cor))~CH_status, 
                                             data = No_IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 200, xend = 200, y = 0.58, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
ggsurv
## dev.off()
ggsave(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH vs IMIDs.jpeg"), print(ggsurv), width = 12.5, height = 9.4, dpi = 1000)

mysurv <- Surv(time = germline_patient_data_comb$month_at_progression_drug, event = germline_patient_data_comb$progression_drug_surv)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = germline_patient_data_comb)
## jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH vs IMIDs from drug.jpeg"), width = 1200, height = 900)
ggsurv <- ggsurvplot(myplot, data = germline_patient_data_comb,
           title = "PFS from drugs",
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
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 109, y = 0.36, # x and y coordinates of the text
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs$month_at_progression_drug, event = IMIDs$progression_drug_surv))~CH_status, 
                            data = IMIDs))$chisq, 
                            length((survdiff((Surv(time = IMIDs$month_at_progression_drug, event = IMIDs$progression_drug_surv))~CH_status, 
                                             data = IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 80, xend = 80, y = 0.22, yend = 0.505,
           size = 2.5, colour = "black")+
  annotate("text", x = 82, y = 0.78, # x and y coordinates of the text
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs$month_at_progression_drug, event = No_IMIDs$progression_drug_surv))~CH_status, 
                            data = No_IMIDs))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs$month_at_progression_drug, event = No_IMIDs$progression_drug_surv))~CH_status, 
                                             data = No_IMIDs))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 48, xend = 48, y = 0.54, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
ggsurv
## dev.off()
ggsave(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH vs IMIDs from drug.jpeg"), print(ggsurv), width = 12.5, height = 9.4, dpi = 1000)

# LPreT
PreT <- germline_patient_data_comb %>% filter(Disease_Status_facet == "MM")
mysurv <- Surv(time = PreT$month_at_os, event = PreT$os_surv_cor)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = PreT)


## jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH vs IMIDs in PreT patients.jpeg"), width = 1200, height = 900)
ggsurv <- ggsurvplot(myplot, data = PreT,
           title = "OS from diagnosis",
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
           ))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 192, y = 0.45,
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs_MM$month_at_os, event = IMIDs_MM$os_surv_cor))~CH_status, 
                            data = IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = IMIDs_MM$month_at_os, event = IMIDs_MM$os_surv_cor))~CH_status, 
                                             data = IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 165, xend = 165, y = 0.378, yend = 0.521,
           size = 2.5, colour = "black")+
  annotate("text", x = 243, y = 0.76,
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs_MM$month_at_os, event = No_IMIDs_MM$os_surv_cor))~CH_status, 
                            data = No_IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs_MM$month_at_os, event = No_IMIDs_MM$os_surv_cor))~CH_status, 
                                             data = No_IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 210, xend = 210, y = 0.515, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
ggsurv
## dev.off()
ggsave(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new OS CH vs IMIDs in MM patients.jpeg"), print(ggsurv), width = 12.5, height = 9.4, dpi = 1000)


mysurv <- Surv(time = PreT$month_at_progression_drug, event = PreT$progression_drug_surv)
myplot <- survfit(mysurv~CH_status+imids_maintenance, data = PreT)
## jpeg(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH vs IMIDs in PreT patients from drug.jpeg"), width = 1200, height = 900)
ggsurv <- ggsurvplot(myplot, data = PreT,
           title = "PFS from drugs",
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
           risk.table.fontsize = 6,
           tables.theme = theme_survminer(base_size = 5,
                                          font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ))
ggsurv$plot <- ggsurv$plot+
  annotate("text", x = 52, y = 0.45,
           label = paste0("P imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = IMIDs_MM$month_at_progression_drug, event = IMIDs_MM$progression_drug_surv))~CH_status, 
                            data = IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = IMIDs_MM$month_at_progression_drug, event = IMIDs_MM$progression_drug_surv))~CH_status, 
                                             data = IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 43, xend = 43, y = 0.37, yend = 0.54,
           size = 2.5, colour = "black")+
  annotate("text", x = 53, y = 0.89,
           label = paste0("P no imids = ", 
                          round(pchisq((survdiff(
                            (Surv(time = No_IMIDs_MM$month_at_progression_drug, event = No_IMIDs_MM$progression_drug_surv))~CH_status, 
                            data = No_IMIDs_MM))$chisq, 
                            length((survdiff((Surv(time = No_IMIDs_MM$month_at_progression_drug, event = No_IMIDs_MM$progression_drug_surv))~CH_status, 
                                             data = No_IMIDs_MM))$n)-1, 
                            lower.tail = FALSE),3)), 
           size = 5) +
  annotate("segment", x = 43, xend = 43, y = 0.77, yend = 1.0,
           size = 2.5, colour = "black", linetype=5)
ggsurv
## dev.off()
ggsave(paste0(path, "/Figures/Survivals/CHIP/IMIDs/new PFS CH vs IMIDs in MM patients from drug.jpeg"), print(ggsurv), width = 12.5, height = 9.4, dpi = 1000)
