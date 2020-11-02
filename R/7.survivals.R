################################################################################### I ### PFS Survivals from date of diagnosis ----

mysurv <- Surv(time = germline_patient_data$month_at_progression_Dx, event = germline_patient_data$progressed_surv)

myplot <- survfit(mysurv~1)
plot(myplot)

ggsurvplot(myplot, data = germline_patient_data,
           title = "PFS",
           font.main = c(16, "bold", "black"),
           xlab = "Time (months)", 
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE,
           censor = TRUE
)

Surv(germline_patient_data$month_at_progression_Dx, germline_patient_data$progressed_surv)[1:10]
germline_patient_data$progressed_surv[1:10]

names(myplot)
myplot

summary(survfit(mysurv~1), times = 12) # probability of surviving (PFS) beyond 12 months 

# 651 patient in germline_patient_data -1 for the one duplicated patient
table(germline_patient_data$progressed_surv)
# 285 who progressed



################################################################################### II ### PFS Survivals per disease status ----

germline_patient_data_simp <- germline_patient_data %>% 
  filter(!str_detect(Disease_Status_germline, "Amyloidosis|MYELOFIBROSIS|Solitary|WALDENSTROM|Refractory|Normal"))

mysurv <- Surv(time = germline_patient_data_simp$month_at_progression_Dx, event = germline_patient_data_simp$progressed_surv)
myplot <- survfit(mysurv~Disease_Status_germline, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Output Survivals/PFS for disease status germline.jpeg"), width = 1600, height = 800)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "PFS",
           font.main = c(16, "bold", "black"),
           xlab = "Time (months)", 
           legend = "right",
           legend.title = "Disease Status at germline",
           legend.labs = c("Early Relapse Multiple Myeloma", "Late Relapse Multiple Myeloma ", "MGUS",
                           "Post Treatment Newly Diagnosed Multiple Myeloma", "Pre Treatment Newly Diagnosed Multiple Myeloma",
                           "SM"),
           surv.median.line = c("hv"),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.3,
           risk.table.title = "Risk table",
           tables.theme = theme_survminer(font.y = c(14, "plain", "transparent")),
           # Censor
           censor = TRUE
)
# dev.off()

myplot <- survfit(mysurv~Disease_Status_facet, data = germline_patient_data_simp)
# jpeg(paste0(path, "/Output Survivals/PFS for simplify disease status germline.jpeg"), width = 1400, height = 900)
ggsurvplot(myplot, data = germline_patient_data_simp,
           title = "PFS",
           font.main = c(16, "bold", "black"),
           font.x = c(14),
           # font.y = c(16),
           font.legend = c(14),
           
           xlab = "Time (months)", 
           legend = "right",
           legend.title = "Disease Status at germline",
           legend.labs = c("MGUS", "MM", "SM"),
           surv.median.line = "hv",
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           # risk.table = TRUE,
           tables.height = 0.14,
           risk.table.title = "Risk table",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.y = c(14, "plain", "transparent"))
           # risk.table.pos = "in",
           # Censor
           # censor = TRUE,
           # ncensor.plot = TRUE
           )
# dev.off()

temp <- germline_patient_data[,c("Disease_Status_germline", "month_at_progression_Dx", "progressed_surv", "progression_date", 
                         "date_death", "was_contact_lost", "date_of_diagnosis", "last_date_available", "last_event_available")] %>% 
  filter(str_detect(Disease_Status_germline, "Amyloidosis|marrow|MYELOFIBROSIS|Solitary|WALDENSTROM|Refractory"))

summary(myplot)
# Summary of survival curves
res.sum <- surv_summary(myplot)
res.sum

# Log-Rank test comparing survival curves for Significant Difference
survdiff(Surv(time = germline_patient_data_simp$month_at_progression_Dx, event = germline_patient_data_simp$progressed_surv) ~
           Disease_Status_germline, data = germline_patient_data_simp)





