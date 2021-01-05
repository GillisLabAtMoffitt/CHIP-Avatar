
# Overall Survival from date of diagnosis----
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_os, event = germline_patient_data_seqsimp$os_surv_cor)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Output Survivals/OS sequenced patients from date of diagnosis.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_seqsimp,
           title = "OS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # legend.labs = c("Early Relapse MM", "Late Relapse MM ",
           # "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           # "SM"),
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
                                          font.tickslab = c(19, "bold", "black")),
           # Censor
           censor = TRUE
)
# dev.off()

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Output Survivals/OS sequenced patients MM combined from date of diagnosis.jpeg"), width = 1200, height = 900)
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

myplot <- survfit(mysurv~CH_status, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Output Survivals/OS CHIP status sequenced patients MM combined from date of diagnosis.jpeg"), width = 1200, height = 900)
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


################################################################################### VI ### For sequenced<drugs patients G<drugs----
patient <- readxl::read_xlsx(paste0(path, "/Nancy's working files/MM Avatar_Sequenced subset.xlsx"),
                             sheet = "Sequenced_before.drug.start") %>% 
  select(avatar_id) %>% distinct()
id <- paste(patient$avatar_id, collapse = "|")
germline_patient_data_Dseqsimp <- germline_patient_data_simp[ grepl(id, germline_patient_data_simp$avatar_id) , ]

# PFS Survivals per disease status from Dx ----
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_progression_Dx, event = germline_patient_data_Dseqsimp$progression_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Output Survivals/PFS sequenced<drugs patients by disease status germline.jpeg"), width = 1200, height = 900)
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
           legend.title = "Disease Status",
           # legend.labs = c("Pre Treatment Newly Diagnosed MM",
           # "SM"),
           palette = c("#33FF33", "blue"),
           # surv.median.line = c("hv"),
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
# jpeg(paste0(path, "/Output Survivals/PFS sequenced<drugs patients MM combined.jpeg"), width = 1200, height = 900)
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
                                          font.tickslab = c(19, "bold", "black")
           )
           # risk.table.pos = "in",
           # Censor
           # censor = TRUE,
           # ncensor.plot = TRUE
)
# dev.off()

myplot <- survfit(mysurv~CH_status, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Output Survivals/PFS CHIP status sequenced<drugs patients MM combined.jpeg"), width = 1200, height = 900)
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

# PFS Survivals from first date of drugs ----
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_progression_drug, event = germline_patient_data_Dseqsimp$progression_drug_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Output Survivals/PFS sequenced<drugs patients from drugs date.jpeg"), width = 1200, height = 900)
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
           # legend.labs = c("Pre Treatment Newly Diagnosed MM",
           # "SM"),
           palette = c("#33FF33", "blue"),
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
# jpeg(paste0(path, "/Output Survivals/PFS sequenced<drugs patients MM combined from drugs date.jpeg"), width = 1200, height = 900)
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

myplot <- survfit(mysurv~CH_status, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Output Survivals/PFS CHIP status sequenced<drugs patients MM combined from drugs date.jpeg"), width = 1200, height = 900)
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


# Overall Survival from date of diagnosis----
mysurv <- Surv(time = germline_patient_data_Dseqsimp$month_at_os, event = germline_patient_data_Dseqsimp$os_surv_cor)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Output Survivals/OS sequenced<drugs patients from date of diagnosis.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_data_Dseqsimp,
           title = "OS from date of diagnosis",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "Disease Status",
           # legend.labs = c("Pre Treatment Newly Diagnosed MM",
           # "SM"),
           palette = c("#33FF33", "blue"),
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
# jpeg(paste0(path, "/Output Survivals/OS sequenced<drugs patients MM combined from date of diagnosis.jpeg"), width = 1200, height = 900)
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

myplot <- survfit(mysurv~CH_status, data = germline_patient_data_Dseqsimp)
# jpeg(paste0(path, "/Output Survivals/OS CHIP status sequenced<drugs patients MM combined from date of diagnosis.jpeg"), width = 1200, height = 900)
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


################################################################################### VI ### PFS IMIDS ----
# In germline 651 patients
IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "IMIDs")
# PFS Dx
mysurv <- Surv(time = IMIDs$month_at_progression_Dx, event = IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status germline patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status germline patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
           title = "PFS from drug",
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
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status germline patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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


No_IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "No IMIDs")
# PFS Dx
mysurv <- Surv(time = No_IMIDs$month_at_progression_Dx, event = No_IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status germline patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status germline patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
           title = "PFS from drug",
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
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status germline patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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

# In MM
IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "IMIDs") %>% 
  filter(Disease_Status_facet == "MM")

mysurv <- Surv(time = IMIDs$month_at_progression_Dx, event = IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status MM patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status MM patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
           title = "PFS from drug",
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
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status MM patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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


No_IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "No IMIDs") %>% 
  filter(Disease_Status_facet == "MM")
# PFS Dx
mysurv <- Surv(time = No_IMIDs$month_at_progression_Dx, event = No_IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status MM patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status MM patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
           title = "PFS from drug",
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
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status MM patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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
IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "IMIDs") %>% 
  filter(Disease_Status_facet == "Smoldering")
# PFS Dx
mysurv <- Surv(time = IMIDs$month_at_progression_Dx, event = IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status SM patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status SM patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
           title = "PFS from drug",
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
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status SM patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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


No_IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "No IMIDs") %>% 
  filter(Disease_Status_facet == "Smoldering")
# PFS Dx
mysurv <- Surv(time = No_IMIDs$month_at_progression_Dx, event = No_IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status SM patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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
           ))
# dev.off()
# PFS drug
mysurv <- Surv(time = No_IMIDs$month_at_progression_drug, event = No_IMIDs$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status SM patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
           title = "PFS from drug",
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
mysurv <- Surv(time = No_IMIDs$month_at_os, event = No_IMIDs$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status SM patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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


# In MGUS
IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "IMIDs") %>% 
  filter(Disease_Status_facet == "MGUS")
# PFS Dx
mysurv <- Surv(time = IMIDs$month_at_progression_Dx, event = IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status Mgus patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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
           ))
# dev.off()
# PFS drug
mysurv <- Surv(time = IMIDs$month_at_progression_drug, event = IMIDs$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status Mgus patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
           title = "PFS from drug",
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
mysurv <- Surv(time = IMIDs$month_at_os, event = IMIDs$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = IMIDs)
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status Mgus patients with IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = IMIDs,
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


No_IMIDs <- germline_patient_data %>% 
  filter(received_IMIDs == "No IMIDs") %>% 
  filter(Disease_Status_facet == "MGUS")
# PFS Dx
mysurv <- Surv(time = No_IMIDs$month_at_progression_Dx, event = No_IMIDs$progression_surv)
myplot <- survfit(mysurv~CH_status, data = No_IMIDs)
# jpeg(paste0(path, "/Output Survivals/PFS Dx CH status Mgus patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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
# jpeg(paste0(path, "/Output Survivals/PFS Drug CH status Mgus patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
           title = "PFS from drug",
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
# jpeg(paste0(path, "/Output Survivals/OS Dx CH status Mgus patients with No IMIDs.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = No_IMIDs,
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

###### Combined----
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
myplot <- survfit(mysurv~CH_status+received_IMIDs, data = germline_patient_data)

surv_pvalue(myplot)
coxfit <- coxph(
  Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor) ~ CH_status+received_IMIDs,
  data = germline_patient_surv,
  ties = 'exact')
summary(coxfit)

# jpeg(paste0(path, "/Output Survivals/CH status vs IMIDs OS_Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
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
           linetype = "received_IMIDs",
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


mysurv <- Surv(time = germline_patient_surv$month_at_progression_drug, event = germline_patient_surv$progression_drug_surv)
myplot <- survfit(mysurv~CH_status+received_IMIDs, data = germline_patient_data)
# jpeg(paste0(path, "/Output Survivals/CH status vs IMIDs PFS_Drug.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
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
           linetype = "received_IMIDs",
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

# LPreT
PreT <- germline_patient_data %>% filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma")
mysurv <- Surv(time = PreT$month_at_os, event = PreT$os_surv_cor)
myplot <- survfit(mysurv~CH_status+received_IMIDs, data = PreT)


# jpeg(paste0(path, "/Output Survivals/CH status vs IMIDs in PreT OS_Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT,
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
           linetype = "received_IMIDs",
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


mysurv <- Surv(time = PreT$month_at_progression_drug, event = PreT$progression_drug_surv)
myplot <- survfit(mysurv~CH_status+received_IMIDs, data = PreT)
# jpeg(paste0(path, "/Output Survivals/CH status vs IMIDs in PreT PFS_Drug.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = PreT,
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
           linetype = "received_IMIDs",
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