################################################################################# Demo in all patients ####

pdf(paste0(path, "/Age at diagnosis repartition in MM Avatar.pdf"), height = 6, width = 9)
p <- qplot(x =Age_at_diagosis, data=subset(Age_data,!is.na(Age_at_diagosis)), fill=..count.., geom="histogram") 
p + scale_fill_viridis_c(
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1,
  option = "D",
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
) +
  theme_minimal() +
  labs(x="Age at Diagosis", y="Number of Patient", title="Age at Diagnisis Repartition in Avatar")
dev.off()

# Gender
pdf(paste0(path, "/Age repartition per gender in MM Avatar.pdf"), height = 6, width = 9)
p <- ggplot(Age_data %>% filter(!is.na(Age_at_diagosis), !is.na(Gender)),
            aes(x=Gender, y=Age_at_diagosis), fill=Gender) + 
  geom_boxplot(color= c("purple3", "royalblue2")) +
  theme_minimal() +
  labs(x="Gender", y="Age at Diagosis", title="Age repartition per gender in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2))
dev.off()
pdf(paste0(path, "/Age repartition by gender facet Disease status in MM Avatar.pdf"), height = 6, width = 9)
p <- ggplot(germline_patient_data %>%
              mutate(Disease_Status_facet = factor(Disease_Status_facet, levels=c("MGUS", "Smoldering", "MM"))) %>%
              filter(!is.na(Age_at_diagosis), !is.na(Gender), !is.na(Disease_Status_facet)),
            aes(x=Gender, y=Age_at_diagosis), fill=Gender) + 
  geom_boxplot(color= rep(c("purple3", "royalblue2"), 3)) +
  theme_minimal() +
  labs(x="Gender", y="Age at Diagosis", title="Age repartition per gender in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ Disease_Status_facet) +
  theme(strip.background = element_rect(colour="black", fill="white",
                                        size=1.5, linetype="solid"))
# +
  # theme(strip.text.x = element_text(size=12, color="red",
  #                                   face="bold.italic"),
  #       strip.text.y = element_text(size=12, color="red",
  #                                   face="bold.italic"))
dev.off()

# t <- as.data.table(layer_data(p, 1)) %>% 
#   select(c("ymin", "middle", "ymax")) %>% 
#   `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per gender.csv"))


# Ethnicity
pdf(paste0(path, "/Age repartition per ethnicity in MM Avatar.pdf"), height = 6, width = 9)
p <- ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)), # , (Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic") 
            aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot(color = c("darkred", "darkgrey", "black")) +
  theme_minimal() +
  labs(x="Ethnicity", y="Age at Diagosis", title="Age repartition per ethnicity in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2))
dev.off()
pdf(paste0(path, "/Age repartition by ethnicity facet Disease status in MM Avatar.pdf"), height = 6, width = 9)
p <- ggplot(germline_patient_data %>% 
              mutate(Disease_Status_facet = factor(Disease_Status_facet, levels=c("MGUS", "Smoldering", "MM"))) %>%
              filter(!is.na(Age_at_diagosis), (Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic"),
                     !is.na(Disease_Status_facet)),
            aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot() + # color = rep(c("darkred", "darkgrey"),3)
  theme_minimal() +
  labs(x="Ethnicity", y="Age at Diagosis", title="Age repartition per ethnicity in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ Disease_Status_facet)
dev.off()

# Race
pdf(paste0(path, "/Age repartition per race.pdf"), height = 6, width = 9)
p <- Age_data %>% filter(!is.na(Race)) %>% 
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
    )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "AM INDIAN", "Asian", "More than one race", "Others"))) %>% 
  ggplot(aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot() + # color= c("#60136EFF", "#A92E5EFF", "#E65D2FFF")
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per race in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2))
dev.off()
pdf(paste0(path, "/Age repartition by race facet Disease status in MM Avatar.pdf"), height = 6, width = 9)
p <- germline_patient_data %>% filter(!is.na(Race),!is.na(Disease_Status_facet)) %>% 
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "AM INDIAN", "Asian", "More than one race", "Others"))) %>% 
  mutate(Disease_Status_facet = factor(Disease_Status_facet, levels=c("MGUS", "Smoldering", "MM"))) %>% 
  ggplot(aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per race in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ Disease_Status_facet)
dev.off()

# Age_data <- Age_data %>% 
#   mutate(Race_Ethnicity = case_when(
#     Ethnicity == "Hispanic" ~ "Hispanic",
#     Race == "African American" ~ "African American",
#     Race == "White" &
#       Ethnicity == "Non- Hispanic" ~ "Caucasian",
#     Race == "Others" &
#       Ethnicity == "Non- Hispanic" ~ "Non-Hispanic"
#   ))
# # pdf(paste0(path, "/Age repartition per race_ethnicity.pdf"), height = 6, width = 9)
# p <- ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Race_Ethnicity, y=Age_at_diagosis), fill=Race_Ethnicity) + 
#   # geom_boxplot(color= viridis::plasma(n=4)) +
#   ggtitle("Age repartition per race/ethnicity")
# p + geom_jitter(shape=16, position=position_jitter(0.2))
# # dev.off()
# 
# t <- as.data.table(layer_data(p, 1)) %>% 
#   select(c("ymin", "middle", "ymax")) %>% 
#   `colnames<-`(c("min", "median", "max"))
# # write.csv(t,paste0(path, "/Age repartition per race_ethnicity"))

# Disease status
pdf(paste0(path, "/Age repartition per disease status in MM Avatar.pdf"), height = 6, width = 9)
p <-  ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)) %>% filter(!is.na(Disease_Status_germline)), 
             aes(x=Age_at_diagosis, y=Disease_Status_germline)) + 
  geom_boxplot(color= inferno(n=12)) +
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per disease status in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2))

#p <-  ggplot(data=subset(Age_data, !is.na(Age_at_diagosis)), aes(x=Age_at_diagosis, y=Disease_Status.germline)) + geom_point()
dev.off()

########################################################################## Stats
wilcox.test(Age_at_diagosis ~ Gender, Age_data)

wilcox.test(Age_at_diagosis ~ Ethnicity, Age_data,
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, Age_data)


################################################################################# Demo in germline patients ####

pdf(paste0(path, "/Germline patients Age at diagnosis repartition.pdf"), height = 6, width = 9)
p <- qplot(x =Age_at_diagosis, 
           data=subset(germline_patient_data,!is.na(Age_at_diagosis)), fill=..count.., geom="histogram") 
p + scale_fill_viridis_c(
  alpha = 1,
  begin = 0,
  end = 1,
  direction = 1,
  option = "magma",
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
) +
  theme_minimal()
dev.off()

# Gender
pdf(paste0(path, "/Germline patients Age repartition per gender.pdf"), height = 6, width = 9)
p <- ggplot(germline_patient_data %>% filter(!is.na(Age_at_diagosis), !is.na(Gender)),
            aes(x=Gender, y=Age_at_diagosis), fill=Gender) + 
  geom_boxplot(color= c("purple3", "royalblue2")) +
  theme_minimal() +
  labs(x="Gender", y="Age at Diagosis", title="Age repartition per gender in germline")
p + geom_jitter(shape=16, position=position_jitter(0.2))
dev.off()

# t <- as.data.table(layer_data(p, 1)) %>% 
#   select(c("ymin", "middle", "ymax")) %>% 
#   `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per gender.csv"))


# Ethnicity
pdf(paste0(path, "/Germline patients Age repartition per ethnicity.pdf"), height = 6, width = 9)
p <- ggplot(germline_patient_data %>% filter(!is.na(Age_at_diagosis)), # , (Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic"))
            aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot(color = c("darkred", "darkgrey", "black")) + 
  theme_minimal() +
  labs(x="Ethnicity", y="Age at Diagosis", title="Age repartition per ethnicity in germline")
p + geom_jitter(shape=16, position=position_jitter(0.2))
dev.off()

# Race
pdf(paste0(path, "/Germline patients Age repartition per race.pdf"), height = 6, width = 9)
p <- germline_patient_data %>% filter(!is.na(Race)) %>% 
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "AM INDIAN", "Asian", "More than one race", "Others"))) %>% 
  ggplot(aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per race in germline")
p + geom_jitter(shape=16, position=position_jitter(0.2))
dev.off()

########################################################################## Stats
# For germline patients
wilcox.test(Age_at_diagosis ~ Gender, germline_patient_data)

wilcox.test(Age_at_diagosis ~ Ethnicity, germline_patient_data,
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, germline_patient_data)

# For MM patients
wilcox.test(Age_at_diagosis ~ Gender, germline_patient_data %>% 
              filter(Disease_Status_facet == "MM"))

wilcox.test(Age_at_diagosis ~ Ethnicity, germline_patient_data %>% 
              filter(Disease_Status_facet == "MM"),
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, germline_patient_data %>% 
               filter(Disease_Status_facet == "MM"))

# For MGUS patients
wilcox.test(Age_at_diagosis ~ Gender, germline_patient_data %>% 
              filter(Disease_Status_facet == "MGUS"))

wilcox.test(Age_at_diagosis ~ Ethnicity, germline_patient_data %>% 
              filter(Disease_Status_facet == "MGUS"),
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, germline_patient_data %>% 
               filter(Disease_Status_facet == "MGUS"))

# For Smoldering patients
wilcox.test(Age_at_diagosis ~ Gender, germline_patient_data %>% 
              filter(Disease_Status_facet == "Smoldering"))

wilcox.test(Age_at_diagosis ~ Ethnicity, germline_patient_data %>% 
              filter(Disease_Status_facet == "Smoldering"),
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, germline_patient_data %>% 
               filter(Disease_Status_facet == "Smoldering"))
####################################################################### Demographics
Mul_Myeloma <- Age_data %>%
  filter(Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myeloma" |
           Disease_Status_germline == "Early Relapse Multiple Myeloma" |
           Disease_Status_germline == "Late Relapse Multiple Myeloma")

Smoldering <- Age_data %>%
  filter(Disease_Status_germline == "Smoldering Multiple Myeloma")
Mgus <- Age_data %>%
  filter(Disease_Status_germline == "Mgus")

demographics_of_MM <- matrix(c(
  "", "MM", "MGUS", "Smoldering",
  "total", NROW(Mul_Myeloma), NROW(Mgus), NROW(Smoldering),
  "Age at Diagnosis", 
  paste0(round((summary(Mul_Myeloma$Age)["Median"]), digits = 2), ", (range:", (round(summary(Mul_Myeloma$Age)["Min."], digits = 2)) , "-", (round(summary(Mul_Myeloma$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Mgus$Age)["Median"]), digits = 2), ", (range:", (round(summary(Mgus$Age)["Min."], digits = 2)) , "-", (round(summary(Mgus$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Smoldering$Age)["Median"]), digits = 2), ", (range:", (round(summary(Smoldering$Age)["Min."], digits = 2)) , "-", (round(summary(Smoldering$Age)["Max."], digits = 2)), ")")
  ,
  "Sex", "", "", "",
  "Male", sum(str_count(Mul_Myeloma$Gender, "Male"), na.rm = TRUE), sum(str_count(Mgus$Gender, "Male"), na.rm = TRUE), sum(str_count(Smoldering$Gender, "Male"), na.rm = TRUE),
  "Female", sum(str_count(Mul_Myeloma$Gender, "Female"), na.rm = TRUE), sum(str_count(Mgus$Gender, "Female"), na.rm = TRUE), sum(str_count(Smoldering$Gender, "Female"), na.rm = TRUE),
  "Race", "", "", "", 
  "White", sum(str_count(Mul_Myeloma$Race, "White"), na.rm = TRUE), sum(str_count(Mgus$Race, "White"), na.rm = TRUE), sum(str_count(Smoldering$Race, "White"), na.rm = TRUE),
  "Age", 
  paste0(round((summary(Mul_Myeloma$Age)["Median"]), digits = 2), ", (range:", (round(summary(Mul_Myeloma$Age)["Min."], digits = 2)) , "-", (round(summary(Mul_Myeloma$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Mgus$Age)["Median"]), digits = 2), ", (range:", (round(summary(Mgus$Age)["Min."], digits = 2)) , "-", (round(summary(Mgus$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Smoldering$Age)["Median"]), digits = 2), ", (range:", (round(summary(Smoldering$Age)["Min."], digits = 2)) , "-", (round(summary(Smoldering$Age)["Max."], digits = 2)), ")")
  ,
  "Black", sum(str_count(Mul_Myeloma$Race, "African"), na.rm = TRUE), sum(str_count(Mgus$Race, "African"), na.rm = TRUE), sum(str_count(Smoldering$Race, "African"), na.rm = TRUE),
  "Other", sum(str_count(Mul_Myeloma$Race, "Other"), na.rm = TRUE), sum(str_count(Mgus$Race, "Other"), na.rm = TRUE), sum(str_count(Smoldering$Race, "Other"), na.rm = TRUE),
  "Ethnicity", "", "", "",
  "Hispanic", sum(str_count(Mul_Myeloma$Ethnicity, "^Hispanic"), na.rm = TRUE), sum(str_count(Mgus$Ethnicity, "^Hispanic"), na.rm = TRUE), sum(str_count(Smoldering$Ethnicity, "^Hispanic"), na.rm = TRUE),
  "Non-Hispanic", sum(str_count(Mul_Myeloma$Ethnicity, "Non- Hispanic"), na.rm = TRUE), sum(str_count(Mgus$Ethnicity, "Non- Hispanic"), na.rm = TRUE), sum(str_count(Smoldering$Ethnicity, "Non- Hispanic"), na.rm = TRUE)
  #"Unknown"
  ), 
  ncol = 4, byrow = TRUE
)
write.csv(demographics_of_MM, paste0(path, "/Demographics of MM patients with WES.csv"))

table1 <- germline_patient_data %>%
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>%
  select(Age_at_diagosis, Race) %>% 
  tbl_summary(by = Race , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Race) ~ 2)) %>% 
  add_p()
table2 <- germline_patient_data %>%
  select(Age_at_diagosis, Ethnicity) %>% 
  tbl_summary(by = Ethnicity , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Ethnicity) ~ 2)) %>% 
  add_p()
tbl_merge(list(table1, table2),
          tab_spanner = c("**Race**", "**Ethnicity**")) %>%
  bold_labels() %>%
  italicize_levels()


table1 <- germline_patient_data %>%
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black"))) %>% 
  filter(Race == "Black" | Race == "White") %>% 
  select(Age_at_diagosis, Race) %>% 
  tbl_summary(by = Race , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Race) ~ 2)) %>% 
  add_p()

table2 <- germline_patient_data %>%
  select(Age_at_diagosis, Ethnicity) %>% 
  filter(Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic")  %>% 
  tbl_summary(by = Ethnicity , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Ethnicity) ~ 2)) %>% 
  add_p()
tbl_merge(list(table1, table2),
          tab_spanner = c("**Race**", "**Ethnicity**")) %>%
  bold_labels() %>%
  italicize_levels()

rm(demographics_of_MM)
