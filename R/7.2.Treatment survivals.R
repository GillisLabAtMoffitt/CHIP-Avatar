germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)
################################################################################### I ### PFS/OS hct date by HCT----
# From Dx
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$Progression_event)
myplot <- survfit(mysurv~pfs_hct, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS HCT from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from Dx (pfs_hct is when HCT <or= germline date)",
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

myplot <- survfit(mysurv~pfs_hct+CH_status, data = germline_patient_surv)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from Dx (pfs_hct is when HCT <or= germline date)",
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


myplot <- survfit(mysurv~HCT_ever, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS HCT.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from Dx (general HCT yes/no)",
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

# From hct
mysurv <- Surv(time = germline_patient_surv$month_at_progression_hct, event = germline_patient_surv$hct_progression_event)
myplot <- survfit(mysurv~pfs_hct, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS HCT from hct date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from  HCT date (pfs_hct is when HCT <or= germline date)",
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
           # censor = TRUE
)
# dev.off()

myplot <- survfit(mysurv~pfs_hct+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH HCT from hct date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from  HCT date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(10, "bold", "black"), # 20
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
           # censor = TRUE
)
# dev.off()

myplot <- survfit(mysurv~HCT_ever, data = germline_patient_surv)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from  HCT date (general HCT yes/no)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(10, "bold", "black"), # 20
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
           # censor = TRUE
)

# OS
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_event)
myplot <- survfit(mysurv~pfs_hct, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/OS HCT.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS HCT (pfs_hct is when HCT <or= germline date)",
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

myplot <- survfit(mysurv~pfs_hct+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS HCT by CH.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS HCT (pfs_hct is when HCT <or= germline date)",
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

mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_event)
myplot <- survfit(mysurv~HCT_ever, data = germline_patient_surv)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS HCT (general HCT yes/no)",
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


# From hct
mysurv <- Surv(time = germline_patient_surv$month_at_progression_hct, event = germline_patient_surv$hct_progression_event)
myplot <- survfit(mysurv~pfs_hct, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS HCT from hct date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from  HCT date (pfs_hct is when HCT <or= germline date)",
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
           # censor = TRUE
)
# dev.off()
################################################################################### II ### PFS/OS treatment date by general treatment----
# From Dx
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$Progression_event)
myplot <- survfit(mysurv~pfs_treatment, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by Treatment from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Treatment from Dx",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
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

myplot <- survfit(mysurv~pfs_treatment+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH Treatment from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Treatment from Dx",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "pfs_treatment",
           linetype = "CH_status",
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

myplot <- survfit(mysurv~HCT_ever, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by HCT2.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from Dx (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
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

myplot <- survfit(mysurv~HCT_ever+CH_status, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by HCT2.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from Dx (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "HCT_ever",
           linetype = "CH_status",
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

myplot <- survfit(mysurv~HCT_ever+ISS, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by HCT2 ISS.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from Dx (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "ISS",
           linetype = "HCT_ever",
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



# from treatment date
mysurv <- Surv(time = germline_patient_surv$month_at_progression_treat, event = germline_patient_surv$drug_progression_event)
myplot <- survfit(mysurv~pfs_treatment, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by Treatment from treatment date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Treatment from treatment date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
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

myplot <- survfit(mysurv~pfs_treatment+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by Treatment CH from treatment date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Treatment from treatment date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "pfs_treatment",
           linetype = "CH_status",
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


myplot <- survfit(mysurv~HCT_ever, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by HCT2 from treatment date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Treatment from treatment date (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
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

myplot <- survfit(mysurv~HCT_ever+ISS, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by HCT2 ISS from treatment date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from treatment (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "ISS",
           linetype = "HCT_ever",
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

myplot <- survfit(mysurv~HCT_ever+CH_status, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by HCT2 CH from treatment date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from treatment (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "HCT_ever",
           linetype = "CH_status",
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



# OS----
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_event)
myplot <- survfit(mysurv~pfs_treatment, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/OS by Treatment.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS Treatment",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # legend.labs = c("No Treatment", "Treatment"),
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

myplot <- survfit(mysurv~pfs_treatment+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS Treatment CH.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS Treatment",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "pfs_treatment",
           linetype = "CH_status",
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

myplot <- survfit(mysurv~HCT_ever, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/OS HCT2.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS HCT2 (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
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

myplot <- survfit(mysurv~HCT_ever+ISS, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/OS HCT2 ISS.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS HCT2 (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "ISS",
           linetype = "HCT_ever",
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

myplot <- survfit(mysurv~HCT_ever+CH_status, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/CHIP/OS HCT2 CH.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS HCT2 (general HCT yes/no but no include drugs)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Treatment", "Treatment"),
           # palette = c("darkred", "darkgreen", "grey"),
           color = "HCT_ever",
           linetype = "CH_status",
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


################################################################################### III ### PFS/OS rad date by Radiation----
# From Dx
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$Progression_event)
myplot <- survfit(mysurv~pfs_radiation, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by Radiation from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Radiation from Dx (pfs_radiation is when rad <or= germline date)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Radiation", "Radiation"),
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

myplot <- survfit(mysurv~Radiation, data = germline_patient_surv)
## jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by Radiation.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Radiation from Dx (general HCT yes/no)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Radiation", "Radiation"),
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

# From rad date
mysurv <- Surv(time = germline_patient_surv$month_at_progression_rad, event = germline_patient_surv$rad_progression_event)
myplot <- survfit(mysurv~pfs_radiation, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS by Radiation from rad date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Radiation from rad date (pfs_radiation is when rad <or= germline date)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Radiation", "Radiation"),
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

myplot <- survfit(mysurv~pfs_radiation+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by Radiation CH from rad date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Radiation from rad date (pfs_radiation is when rad <or= germline date)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # # legend.labs = c("No Radiation", "Radiation"),
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

# OS----
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_event)
myplot <- survfit(mysurv~pfs_radiation, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/OS by Radiation.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS Radiation (pfs_radiation is when rad <or= germline date)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # legend.labs = c("No Radiation", "Radiation"),
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

myplot <- survfit(mysurv~pfs_radiation+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS Rad by CH.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS Radiation (pfs_radiation is when rad <or= germline date)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "",
           # legend.labs = c("No Radiation", "Radiation"),
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

# tbl <- 
germline_patient_surv %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(CH_status, Radiation, pfs_radiation) %>% 
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency")) %>% add_p() %>% 
  as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/CHIP/Radiation by CH.pdf"))

pdf(paste0(path, "/Figures/CHIP/Days repartition interval_radiation_vs_germ.pdf"), height = 6, width = 9)
p <- germline_patient_data %>% # filter(!is.na(Race)) %>% 
  # mutate(Race = factor(Race, levels=c("White", "Black", "Am Indian", "Asian", "More than one race", "Others", "Unknown"))) %>%
  ggplot(aes(y=interval_radiation_vs_germ)) + 
  geom_boxplot(alpha = 0.5) + # color= c("#60136EFF", "#A92E5EFF", "#E65D2FFF")
  theme_minimal() +
  # scale_fill_brewer(palette="BuPu") +
  labs(y="Days", title="interval_radiation_vs_germ")
# p + geom_jitter(shape=16, position=position_jitter(0.2)) #+
  # stat_compare_means()
dev.off()

######################################################################################################### By CH
# Dx
# mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$Progression_event)
# myplot <- survfit(mysurv~Radiation+CH_status, data = germline_patient_surv)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH Radiation from Dx.jpeg"), width = 1500, height = 900)
# ggsurvplot(myplot, data = germline_patient_surv,
#            title = "PFS Radiation from Dx",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(15, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5,
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "",
#            # # legend.labs = c("No Radiation", "Radiation"),
#            # palette = c("darkred", "darkgreen", "grey"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.3,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ),
#            # Censor
#            censor = TRUE
# )
# # dev.off()
# 
# myplot <- survfit(mysurv~Radiation_event+CH_status, data = germline_patient_surv)
# # jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH Radiation_event from Dx.jpeg"), width = 1200, height = 900)
# ggsurvplot(myplot, data = germline_patient_surv,
#            title = "PFS Radiation from Dx",
#            font.main = c(24, "bold", "black"),
#            font.x = c(20, "bold", "black"),
#            font.y = c(20, "bold", "black"),
#            font.legend = c(10, "bold", "black"),
#            font.tickslab = c(18, "bold", "black"),
#            size = 1.5,
#            
#            xlab = "Time in months", 
#            legend = "top",
#            legend.title = "",
#            # # legend.labs = c("No Radiation", "Radiation"),
#            # palette = c("darkred", "darkgreen", "grey"),
#            pval = TRUE,
#            conf.int = FALSE,
#            # Add risk table
#            tables.height = 0.3,
#            risk.table.title = "Risk table (number(%))",
#            risk.table = "abs_pct",
#            risk.table.y.text = FALSE,
#            risk.table.fontsize = 6,
#            tables.theme = theme_survminer(base_size = 5,
#                                           font.main = c(16, "bold", "black"),
#                                           font.x = c(16, "bold", "black"),
#                                           font.y = c(16, "bold", "transparent"),
#                                           font.tickslab = c(19, "bold", "black")
#            ),
#            # Censor
#            censor = TRUE
# )
# # dev.off()

################################################################################### IV ### PFS/OS drug date by Drug----
# drug yes/no----
# PFS
mysurv <- Surv(time = germline_patient_surv$month_at_progression_drug, event = germline_patient_surv$drug_progression_event)
myplot <- survfit(mysurv~pfs_drugs, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS Drugs from drug date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Drugs from drug date (pfs_drugs is when drugs <or= germline date)",
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
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$Progression_event)
myplot <- survfit(mysurv~Drugs, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS Drugs from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Drugs from drug date (Drugs is general yes/no, need to do on MM only)",
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

# OS
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_event)
myplot <- survfit(mysurv~pfs_drugs, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/OS Drugs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS Drugs (pfs_drugs is when drugs <or= germline date)",
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

# by regimen----
germline_patient_drug_surv <- germline_patient_surv %>% 
  filter(str_detect(first_regimen_name_MM, "No Drugs|VRd|Bor-Dex|^Rd|CyBorD or VCd|Dexamethasone|Lenalidomide|^Td|^KRd|Bortezomib|Melphalan|VAd|ABCD|D-RVd or dara-RVd|IRD"))


mysurv <- Surv(time = germline_patient_drug_surv$month_at_progression_drug, event = germline_patient_drug_surv$drug_progression_event)
myplot <- survfit(mysurv~first_regimen_name, data = germline_patient_drug_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/PFS Regimen from drugs date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_drug_surv,
           title = "PFS regimen from drug date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(8, "bold", "black"), # 20
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
mysurv <- Surv(time = germline_patient_drug_surv$month_at_os, event = germline_patient_drug_surv$os_event)
myplot <- survfit(mysurv~first_regimen_name, data = germline_patient_drug_surv)
# jpeg(paste0(path, "/Figures/Survivals/Treatment/OS Regimen.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_drug_surv,
           title = "OS regimen",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(8, "bold", "black"), # 20
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













