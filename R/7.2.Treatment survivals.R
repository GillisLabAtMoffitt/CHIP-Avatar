germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)
################################################################################### I ### PFS/OS hct date by HCT----
# From Dx
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$progression_surv)
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

myplot <- survfit(mysurv~HCT, data = germline_patient_surv)
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
mysurv <- Surv(time = germline_patient_surv$month_at_progression_hct, event = germline_patient_surv$progression_hct_surv)
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

myplot <- survfit(mysurv~CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH HCT from hct date.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS HCT from  HCT date",
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

# OS
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
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


################################################################################### II ### PFS/OS rad date by Radiation----
# From Dx
mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$progression_surv)
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
mysurv <- Surv(time = germline_patient_surv$month_at_progression_rad, event = germline_patient_surv$progression_rad_surv)
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
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
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

######################################################################################################### By CH----
# Dx
# mysurv <- Surv(time = germline_patient_surv$month_at_progression_Dx, event = germline_patient_surv$progression_surv)
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

################################################################################### I ### PFS/OS hct date by HCT----

# drug
mysurv <- Surv(time = germline_patient_surv$month_at_progression_drug, event = germline_patient_surv$progression_drug_surv)
myplot <- survfit(mysurv~Radiation+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH Radiation from drugs date.jpeg"), width = 1500, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "PFS Radiation (need to do from overall treatment drug or rad, rad only)",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(15, "bold", "black"),
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

# OS
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
myplot <- survfit(mysurv~Radiation+CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS by CH Radiation.jpeg"), width = 1500, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS Radiation from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(15, "bold", "black"),
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
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()

# tbl <- 
  germline_patient_data %>%
  distinct(avatar_id, .keep_all = TRUE) %>% 
  select(CH_status, Radiation, Radiation_event) %>% 
  tbl_summary(by = CH_status, 
              sort = list(everything() ~ "frequency")) %>% add_p() %>% 
  as_gt()
# gt::gtsave(tbl, zoom = 1, paste0(path, "/Figures/CHIP/Radiation by CH.pdf"))
