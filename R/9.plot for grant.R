# load data

path1 <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "Grant with Dr. Coghill")
# 1.1.Load Demographics data -------------------------------------------------------------------------------------
hiv_data <-
  readxl::read_xlsx(paste0(path1, "/Copy of HIV Dataset for Christelle.xlsx")) %>% 
  mutate(vital_status = ifelse(`Vital Status` == "alive", 0, 1))

mysurv <- Surv(time = hiv_data$`Survival Time (years)`, event = hiv_data$vital_status)
myplot <- survfit(mysurv~hiv_data$`HIV status`, data = hiv_data)
jpeg(paste0(path1, "/OS by HIV.jpeg"), height = 600, width = 600)
p <- ggsurvplot(myplot, data = hiv_data,
           title = "OS",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in years", 
           legend = "top",
           legend.title = "",
           legend.labs = c("HIV negative", "HIV positive"),
           palette = c("blue", "yellow"),
           xlim = c(-0.20, 10),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
jpeg(paste0(path1, "/OS by HIV.jpeg"), height = 600, width = 600)
p
dev.off()


mysurv <- Surv(time = hiv_data$`Survival Time (years)`, event = hiv_data$vital_status)
myplot <- survfit(mysurv~CH_status, data = hiv_data)
jpeg(paste0(path1, "/OS by CH.jpeg"), height = 600, width = 600)
p <- ggsurvplot(myplot, data = hiv_data,
           title = "OS",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in years", 
           legend = "top",
           legend.title = "",
           legend.labs = c("CH", "No CH"),
           # color = "Ethnicity",
           # palette = c("darkred", "darkgreen"),
           xlim = c(-0.20, 10),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
jpeg(paste0(path1, "/OS by CH.jpeg"), height = 600, width = 600)
p
dev.off()




hiv_pos <- hiv_data %>% filter(`HIV status` == "positive")

mysurv <- Surv(time = hiv_pos$`Survival Time (years)`, event = hiv_pos$vital_status)
myplot <- survfit(mysurv~CH_status, data = hiv_pos)
png(paste0(path1, "/PFS by CH in HIV positive.png"), height = 600, width = 600)
p <- ggsurvplot(myplot, data = hiv_pos,
           title = "OS in HIV positive patients",
           font.main = c(24, "bold", "black"),
           font.x = c(20, "bold", "black"),
           font.y = c(20, "bold", "black"),
           font.legend = c(20, "bold", "black"),
           font.tickslab = c(18, "bold", "black"),
           size = 1.5,
           
           xlab = "Time in years", 
           legend = "top",
           legend.title = "",
           # color = "Ethnicity",
           # palette = c("darkred", "darkgreen"),
           legend.labs = c("CH", "No CH"),
           xlim = c(-0.20, 10),
           pval = TRUE,
           conf.int = FALSE,
           # Add risk table
           tables.height = 0.3,
           risk.table.title = "Risk table (count(%))",
           risk.table = "abs_pct",
           risk.table.y.text = FALSE,
           tables.theme = theme_survminer(font.main = c(16, "bold", "black"),
                                          font.x = c(16, "bold", "black"),
                                          font.y = c(16, "bold", "transparent"),
                                          font.tickslab = c(19, "bold", "black")
           ),
           # Censor
           censor = TRUE
)
jpeg(paste0(path1, "/PFS by CH in HIV positive.jpeg"), height = 600, width = 600)
p
dev.off()
