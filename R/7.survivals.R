germline_patient_surv <- germline_patient_data %>% distinct(avatar_id, .keep_all = TRUE)
################################################################################### I ### PFS Survivals from date of diagnosis ----
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

# 651 patient in germline_patient_data -1 for the one duplicated patient
table(germline_patient_surv$progression_surv)
# 285 who progressed



################################################################################### II ### PFS Survivals per disease status ----

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

# temp <- germline_patient_data[,c("Disease_Status_germline", "month_at_progression_Dx", "progressed_surv", "progression_date", 
#                          "date_death", "was_contact_lost", "date_of_diagnosis", "last_date_available", "last_event_available")] %>% 
#   filter(str_detect(Disease_Status_germline, "Amyloidosis|marrow|MYELOFIBROSIS|Solitary|WALDENSTROM|Refractory"))

# Summary of survival curves
res.sum <- surv_summary(myplot)
res.sum

summary(myplot)
a <- summary(myplot)$table
write.csv(a, paste0(path, "/Figures/Survivals/summary PFS from Dx by simplify DS.csv"))

# Log-Rank test comparing survival curves for Significant Difference
survdiff(Surv(time = germline_patient_data_simp$month_at_progression_Dx, event = germline_patient_data_simp$progression_surv) ~
           Disease_Status_germline, data = germline_patient_data_simp)


############################################################################################################################################# By CH----
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
write.csv(a, paste0(path, "/Figures/Survivals/CHIP/summary PFS by CH from Dx.csv"))


################################################################################### III ### PFS Survivals from first date of drugs ----
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
write.csv(a, paste0(path, "/Figures/Survivals/summary PFS by DS from drug date.csv"))

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
write.csv(a, paste0(path, "/Figures/Survivals/summary PFS by simplify DS from drugs date.csv"))

################################################################################################################################ By CH----
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


################################################################################### IV ### Overall Survival from date of diagnosis----
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
write.csv(a, paste0(path, "/Figures/Survivals/summary OS by DS.csv"))

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
write.csv(a, paste0(path, "/Figures/Survivals/summary OS by simplify DS.csv"))

######################################################################################################### By CH----
mysurv <- Surv(time = germline_patient_surv$month_at_os, event = germline_patient_surv$os_surv_cor)
myplot <- survfit(mysurv~CH_status, data = germline_patient_surv)
# jpeg(paste0(path, "/Figures/Survivals/CHIP/OS by CH.jpeg"), width = 1200, height = 900)
ggsurvplot(myplot, data = germline_patient_surv,
           title = "OS date of diagnosis",
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
write.csv(a, paste0(path, "/Figures/Survivals/CHIP/summary OS by CH.csv"))


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






