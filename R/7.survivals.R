library(survival)
library(survminer)

mysurv <- Surv(time = germline_patient_data$month_at_progression, event = germline_patient_data$progressed_surv)

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

Surv(germline_patient_data$month_at_progression, germline_patient_data$progressed_surv)[1:10]
germline_patient_data$progressed_surv[1:10]

names(myplot)
myplot

summary(survfit(mysurv~1), times = 12) # probability of surviving (PFS) beyond 12 months 

# 651 patient in germline_patient_data -1 for the one duplicated patient
table(germline_patient_data$progressed_surv)
# 285 who progressed
a <- germline_patient_data[c("progressed_surv", "month_at_progression", "progression_date", "progression_surv", 
                             "progression_date_surv", "date_of_diagnosis")] %>%
  arrange(month_at_progression)

germline_patient_data[c("last_date_available", "date_last_follow_up")]
