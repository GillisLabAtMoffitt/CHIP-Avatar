germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)




################################################################################### VII ### For sequenced patients----
patient <- readxl::read_xlsx(paste0(path, "/Nancy's working files/MM Avatar_Sequenced subset.xlsx"),
                                 sheet = "Sequenced") %>% 
  select(avatar_id) %>% distinct()
id <- paste(patient$avatar_id, collapse = "|")
germline_patient_data_seqsimp <- germline_patient_data_simp[ grepl(id, germline_patient_data_simp$avatar_id) , ]


# PFS Survivals by disease status from Dx ----
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_progression_Dx, event = germline_patient_data_seqsimp$progression_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS sequenced patients.jpeg"), width = 1200, height = 900)
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
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from Dx in sequenced patients.jpeg"), width = 1200, height = 900)
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

######################################################################################### By CH----
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from Dx in sequenced patients MM+MGUS.jpeg"), width = 1200, height = 900)
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


# PFS Survivals from first date of drugs ----
mysurv <- Surv(time = germline_patient_data_seqsimp$month_at_progression_drug, event = germline_patient_data_seqsimp$progression_drug_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/PFS by DS from drugs date sequenced patients.jpeg"), width = 1200, height = 900)
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
# jpeg(paste0(path, "/Figures/Survivals/PFS by simplify DS from drugs date sequenced patients.jpeg"), width = 1200, height = 900)
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

#################################################################################################### By CH----
myplot <- survfit(mysurv~CH_status, data = germline_patient_data_seqsimp)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS by CH from drugs date in sequenced patients MM+MGUS.jpeg"), width = 1200, height = 900)
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

########################################### For MM patients----
MM_surv <- germline_patient_drug_surv %>% filter(Disease_Status_facet == "MM")

mysurv <- Surv(time = MM_surv$month_at_progression_Dx, event = MM_surv$progression_surv)
myplot <- survfit(mysurv~1, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/PFS MM patients from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "PFS MM patients from diagnosis",
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

mysurv <- Surv(time = MM_surv$month_at_progression_drug, event = MM_surv$progression_drug_surv)
myplot <- survfit(mysurv~CH_status, data = MM_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/PFS MM patient by CH from Dx.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = MM_surv,
           title = "PFS MM patients from drug date",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(11, "bold", "black"), # 20 when legend has names
           font.tickslab = c(18, "bold", "black"),
           size = 1.5, # line thickness default = 1
           
           xlab = "Time in months", 
           legend = "top",
           legend.title = "CH Status",
           # # legend.labs = c("Early Relapse MM", "Late Relapse MM", "MGUS",
           #                 "Post Treatment Newly Diagnosed MM", "Pre Treatment Newly Diagnosed MM",
           #                 "SM"),
           # palette = c("#006600", "#009900", "red", "#00FF99", "#33FF33", "blue"),
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


