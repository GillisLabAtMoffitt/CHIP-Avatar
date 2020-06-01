
Age_data <- Global_data

enddate <- today()
Age_data$Age <- interval(start= Global_data$Date_of_Birth, end= enddate)/                      
  duration(n=1, unit="years")
Age_data$Age <- round(Age_data$Age, 3)
# summary(Age_data$Age)

Age_data$Age_at_diagosis <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_diagnosis_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_diagosis <- round(Age_data$Age_at_diagosis, 3)
# summary(Age_data$Age_at_diagosis, na.rm = TRUE)

Age_data$Age_at_death <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_death)/                      
  duration(n=1, unit="years")
Age_data$Age_at_death <- round(Age_data$Age_at_death, 3)
# summary(Age_data$Age_at_death, na.rm = TRUE)

Age_data$Age_at_lastfollowup <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_last_follow_up)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastfollowup <- round(Age_data$Age_at_lastfollowup, 3)
# summary(Age_data$Age_at_lastfollowup, na.rm = TRUE)

Age_data$Age_at_lastdate <- interval(start= Global_data$Date_of_Birth, end= Global_data$last_date_available)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastdate <- round(Age_data$Age_at_lastdate, 3)
# summary(Age_data$Age_at_lastdate, na.rm = TRUE)

Age_data$Age_at_firstdrug <- interval(start= Global_data$Date_of_Birth, end= Global_data$drug_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstdrug <- round(Age_data$Age_at_firstdrug, 3)
# summary(Age_data$Age_at_firstdrug, na.rm = TRUE)

Age_data$Age_at_firstbmt <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_first_bmt)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstbmt <- round(Age_data$Age_at_firstbmt, 3)
# summary(Age_data$Age_at_firstbmt, na.rm = TRUE)

Age_data$Age_at_firstrad <- interval(start= Global_data$Date_of_Birth, end= Global_data$rad_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstrad <- round(Age_data$Age_at_firstrad, 3)
# summary(Age_data$Age_at_firstrad, na.rm = TRUE)

Age_data$Age_at_germcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt_germline)/                      
  duration(n=1, unit="years")
Age_data$Age_at_germcollect <- round(Age_data$Age_at_germcollect, 3)
# summary(Age_data$Age_at_germcollect, na.rm = TRUE)

Age_data$Age_at_tumorcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt_tumor_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_tumorcollect <- round(Age_data$Age_at_tumorcollect, 3)
# summary(Age_data$Age_at_tumorcollect, na.rm = TRUE)


# Create variable for plotting
age_germline_patient_data <- Age_data[!is.na(Age_data$moffitt_sample_id_germline),]

age_germline_patient_data <- age_germline_patient_data %>% 
  mutate(Disease_Status_facet = case_when(
    Disease_Status_germline == "Pre Treatment Newly Diagnosed Multiple Myeloma" |
      Disease_Status_germline == "Post Treatment Newly Diagnosed Multiple Myelom" |
      Disease_Status_germline == "Early Relapse Multiple Myeloma" |
      Disease_Status_germline == "Late Relapse Multiple Myeloma"                      ~ "MM",
    Disease_Status_germline == "Mgus"                                                 ~ "MGUS",
    Disease_Status_germline == "Smoldering Multiple Myeloma"                          ~ "Smoldering"
  ))


################################################################################# Demo in all patients ####

# pdf(paste0(path, "/Age at diagnosis repartition.pdf"), height = 6, width = 9)
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
# dev.off()

# Gender
# pdf(paste0(path, "/Age repartition per gender.pdf"), height = 6, width = 9)
p <- ggplot(age_germline_patient_data %>% filter(!is.na(Age_at_diagosis), !is.na(Gender)),
            aes(x=Gender, y=Age_at_diagosis), fill=Gender) + 
  geom_boxplot(color= c("purple3", "royalblue2")) +
  theme_minimal() +
  labs(x="Gender", y="Age at Diagosis", title="Age repartition per gender in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()
# pdf(paste0(path, "/Age repartition by gender facet Disease status.pdf"), height = 6, width = 9)
p <- ggplot(age_germline_patient_data %>%
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
# dev.off()

# t <- as.data.table(layer_data(p, 1)) %>% 
#   select(c("ymin", "middle", "ymax")) %>% 
#   `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per gender.csv"))


# Ethnicity
# pdf(paste0(path, "/Age repartition per ethnicity.pdf"), height = 6, width = 9)
p <- ggplot(age_germline_patient_data %>% filter(!is.na(Age_at_diagosis), (Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic")), 
            aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot(color = c("darkred", "darkgrey")) + 
  theme_minimal() +
  labs(x="Ethnicity", y="Age at Diagosis", title="Age repartition per ethnicity in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()
# pdf(paste0(path, "/Age repartition by ethnicity facet Disease status.pdf"), height = 6, width = 9)
p <- ggplot(age_germline_patient_data %>% 
              mutate(Disease_Status_facet = factor(Disease_Status_facet, levels=c("MGUS", "Smoldering", "MM"))) %>%
              filter(!is.na(Age_at_diagosis), (Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic"),
                     !is.na(Disease_Status_facet)),
            aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot(color = rep(c("darkred", "darkgrey"),3)) + 
  theme_minimal() +
  labs(x="Ethnicity", y="Age at Diagosis", title="Age repartition per ethnicity in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ Disease_Status_facet)
# dev.off()

# Race
pdf(paste0(path, "/Age repartition per race.pdf"), height = 6, width = 9)
p <- age_germline_patient_data %>% filter(!is.na(Race)) %>% 
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
    )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>% 
  ggplot(aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot(color= c("#60136EFF", "#A92E5EFF", "#E65D2FFF")) +
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per race in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2))
dev.off()
# pdf(paste0(path, "/Age repartition by race facet Disease status.pdf"), height = 6, width = 9)
p <- age_germline_patient_data %>% filter(!is.na(Race),!is.na(Disease_Status_facet)) %>% 
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>% 
  mutate(Disease_Status_facet = factor(Disease_Status_facet, levels=c("MGUS", "Smoldering", "MM"))) %>% 
  ggplot(aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot(color= c("#60136EFF", "#A92E5EFF", "#E65D2FFF", "#60136EFF", "#A92E5EFF", "#60136EFF", "#A92E5EFF", "#E65D2FFF")) +
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per race in Avatar")
p + geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ Disease_Status_facet)
# dev.off()

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
# pdf(paste0(path, "/Age repartition per disease status.pdf"), height = 6, width = 9)
p <-  ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)) %>% filter(!is.na(Disease_Status_germline)), 
             aes(x=Age_at_diagosis, y=Disease_Status_germline)) + 
  geom_boxplot(color= inferno(n=12)) +
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per disease status")
p + geom_jitter(shape=16, position=position_jitter(0.2))

#p <-  ggplot(data=subset(Age_data, !is.na(Age_at_diagosis)), aes(x=Age_at_diagosis, y=Disease_Status.germline)) + geom_point()
p
dev.off()

########################################################################## Stats
wilcox.test(Age_at_diagosis ~ Gender, Age_data)

wilcox.test(Age_at_diagosis ~ Ethnicity, Age_data,
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, Age_data)


################################################################################# Demo in germline patients ####

# pdf(paste0(path, "/Germline patients Age at diagnosis repartition.pdf"), height = 6, width = 9)
p <- qplot(x =Age_at_diagosis, 
           data=subset(age_germline_patient_data,!is.na(Age_at_diagosis)), fill=..count.., geom="histogram") 
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
# dev.off()

# Gender
# pdf(paste0(path, "/Germline patients Age repartition per gender.pdf"), height = 6, width = 9)
p <- ggplot(age_germline_patient_data %>% filter(!is.na(Age_at_diagosis), !is.na(Gender)),
            aes(x=Gender, y=Age_at_diagosis), fill=Gender) + 
  geom_boxplot(color= c("purple3", "royalblue2")) +
  theme_minimal() +
  labs(x="Gender", y="Age at Diagosis", title="Age repartition per gender")
p + geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()

# t <- as.data.table(layer_data(p, 1)) %>% 
#   select(c("ymin", "middle", "ymax")) %>% 
#   `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per gender.csv"))


# Ethnicity
# pdf(paste0(path, "/Germline patients Age repartition per ethnicity.pdf"), height = 6, width = 9)
p <- ggplot(age_germline_patient_data %>% filter(!is.na(Age_at_diagosis), (Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic")), 
            aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot(color = c("darkred", "darkgrey")) + 
  theme_minimal() +
  labs(x="Ethnicity", y="Age at Diagosis", title="Age repartition per ethnicity")
p + geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()

# Race
# pdf(paste0(path, "/Germline patients Age repartition per race.pdf"), height = 6, width = 9)
p <- age_germline_patient_data %>% filter(!is.na(Race)) %>% 
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>% 
  ggplot(aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot(color= c("#60136EFF", "#A92E5EFF", "#E65D2FFF")) +
  theme_minimal() +
  labs(x="Race", y="Age at Diagosis", title="Age repartition per race")
p + geom_jitter(shape=16, position=position_jitter(0.2))
# dev.off()

########################################################################## Stats
# For germline patients
wilcox.test(Age_at_diagosis ~ Gender, age_germline_patient_data)

wilcox.test(Age_at_diagosis ~ Ethnicity, age_germline_patient_data,
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, age_germline_patient_data)

# For MM patients
wilcox.test(Age_at_diagosis ~ Gender, age_germline_patient_data %>% 
              filter(Disease_Status_facet == "MM"))

wilcox.test(Age_at_diagosis ~ Ethnicity, age_germline_patient_data %>% 
              filter(Disease_Status_facet == "MM"),
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, age_germline_patient_data %>% 
               filter(Disease_Status_facet == "MM"))

# For MGUS patients
wilcox.test(Age_at_diagosis ~ Gender, age_germline_patient_data %>% 
              filter(Disease_Status_facet == "MGUS"))

wilcox.test(Age_at_diagosis ~ Ethnicity, age_germline_patient_data %>% 
              filter(Disease_Status_facet == "MGUS"),
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, age_germline_patient_data %>% 
               filter(Disease_Status_facet == "MGUS"))

# For Smoldering patients
wilcox.test(Age_at_diagosis ~ Gender, age_germline_patient_data %>% 
              filter(Disease_Status_facet == "Smoldering"))

wilcox.test(Age_at_diagosis ~ Ethnicity, age_germline_patient_data %>% 
              filter(Disease_Status_facet == "Smoldering"),
            subset = Ethnicity %in% c("Hispanic", "Non- Hispanic"))

kruskal.test(Age_at_diagosis ~ Race, age_germline_patient_data %>% 
               filter(Disease_Status_facet == "Smoldering"))
####################################################################### Demographics
mul_myeloma <- Age_data %>% 
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
  "total", NROW(mul_myeloma), NROW(Mgus), NROW(Smoldering),
  "Age at Diagnosis", 
  paste0(round((summary(mul_myeloma$Age)["Median"]), digits = 2), ", (range:", (round(summary(mul_myeloma$Age)["Min."], digits = 2)) , "-", (round(summary(mul_myeloma$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Mgus$Age)["Median"]), digits = 2), ", (range:", (round(summary(Mgus$Age)["Min."], digits = 2)) , "-", (round(summary(Mgus$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Smoldering$Age)["Median"]), digits = 2), ", (range:", (round(summary(Smoldering$Age)["Min."], digits = 2)) , "-", (round(summary(Smoldering$Age)["Max."], digits = 2)), ")")
  ,
  "Sex", "", "", "",
  "Male", sum(str_count(mul_myeloma$Gender, "Male"), na.rm = TRUE), sum(str_count(Mgus$Gender, "Male"), na.rm = TRUE), sum(str_count(Smoldering$Gender, "Male"), na.rm = TRUE),
  "Female", sum(str_count(mul_myeloma$Gender, "Female"), na.rm = TRUE), sum(str_count(Mgus$Gender, "Female"), na.rm = TRUE), sum(str_count(Smoldering$Gender, "Female"), na.rm = TRUE),
  "Race", "", "", "", 
  "White", sum(str_count(mul_myeloma$Race, "White"), na.rm = TRUE), sum(str_count(Mgus$Race, "White"), na.rm = TRUE), sum(str_count(Smoldering$Race, "White"), na.rm = TRUE),
  "Age", 
  paste0(round((summary(mul_myeloma$Age)["Median"]), digits = 2), ", (range:", (round(summary(mul_myeloma$Age)["Min."], digits = 2)) , "-", (round(summary(mul_myeloma$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Mgus$Age)["Median"]), digits = 2), ", (range:", (round(summary(Mgus$Age)["Min."], digits = 2)) , "-", (round(summary(Mgus$Age)["Max."], digits = 2)), ")")
  , 
  paste0(round((summary(Smoldering$Age)["Median"]), digits = 2), ", (range:", (round(summary(Smoldering$Age)["Min."], digits = 2)) , "-", (round(summary(Smoldering$Age)["Max."], digits = 2)), ")")
  ,
  "Black", sum(str_count(mul_myeloma$Race, "African"), na.rm = TRUE), sum(str_count(Mgus$Race, "African"), na.rm = TRUE), sum(str_count(Smoldering$Race, "African"), na.rm = TRUE),
  "Other", sum(str_count(mul_myeloma$Race, "Other"), na.rm = TRUE), sum(str_count(Mgus$Race, "Other"), na.rm = TRUE), sum(str_count(Smoldering$Race, "Other"), na.rm = TRUE),
  "Ethnicity", "", "", "",
  "Hispanic", sum(str_count(mul_myeloma$Ethnicity, "^Hispanic"), na.rm = TRUE), sum(str_count(Mgus$Ethnicity, "^Hispanic"), na.rm = TRUE), sum(str_count(Smoldering$Ethnicity, "^Hispanic"), na.rm = TRUE),
  "Non-Hispanic", sum(str_count(mul_myeloma$Ethnicity, "Non- Hispanic"), na.rm = TRUE), sum(str_count(Mgus$Ethnicity, "Non- Hispanic"), na.rm = TRUE), sum(str_count(Smoldering$Ethnicity, "Non- Hispanic"), na.rm = TRUE)
  #"Unknown"
  ), 
  ncol = 4, byrow = TRUE
)
write.csv(demographics_of_MM, paste0(path, "/Demographics of MM patients with WES.csv"))

table1 <- age_germline_patient_data %>%
  mutate_at(("Race"), ~ case_when(
    . == "African American" ~ "Black",
    TRUE ~ .
  )) %>% 
  mutate(Race = factor(Race, levels=c("White", "Black", "Others"))) %>%
  select(Age_at_diagosis, Race) %>% 
  tbl_summary(by = Race , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Race) ~ 2)) %>% 
  add_p()
table2 <- age_germline_patient_data %>%
  select(Age_at_diagosis, Ethnicity) %>% 
  tbl_summary(by = Ethnicity , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Ethnicity) ~ 2)) %>% 
  add_p()
tbl_merge(list(table1, table2),
          tab_spanner = c("**Race**", "**Ethnicity**")) %>%
  bold_labels() %>%
  italicize_levels()


table1 <- age_germline_patient_data %>%
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

table2 <- age_germline_patient_data %>%
  select(Age_at_diagosis, Ethnicity) %>% 
  filter(Ethnicity == "Hispanic" | Ethnicity == "Non- Hispanic")  %>% 
  tbl_summary(by = Ethnicity , statistic = all_continuous() ~ "{median} ({sd})", 
              digits = list(c(Age_at_diagosis, Ethnicity) ~ 2)) %>% 
  add_p()
tbl_merge(list(table1, table2),
          tab_spanner = c("**Race**", "**Ethnicity**")) %>%
  bold_labels() %>%
  italicize_levels()

