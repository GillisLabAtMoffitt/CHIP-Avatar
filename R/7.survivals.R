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
           # tables.theme = theme_cleantable(),
           risk.table.title = "Risk table",
           conf.int = FALSE
)
