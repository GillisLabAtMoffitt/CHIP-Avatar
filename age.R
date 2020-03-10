library(lubridate)


enddate <- today()
Global_data$Age <- interval(start= Global_data$Date_of_Birth, end= enddate)/                      
  duration(n=1, unit="years")
Global_data$Age <- round(Global_data$Age, 2)
summary(Global_data$Age)

Global_data$Age_at_diagosis <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_diagnosis_1)/                      
  duration(n=1, unit="years")
Global_data$Age_at_diagosis <- round(Global_data$Age_at_diagosis, 2)
summary(Global_data$Age_at_diagosis, na.rm = TRUE)

#pdf(paste0(path, "/Age at diagnosis repartition.pdf"), height = 6, width = 9)
p <- qplot(x =Age_at_diagosis, data=subset(Global_data,!is.na(Age_at_diagosis)), fill=..count.., geom="histogram") 
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
)
layer_data(p, 1)
#dev.off()

# Gender
#pdf(paste0(path, "/Age repartition per gender.pdf"), height = 6, width = 9)
p <- ggplot(Global_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Gender, y=Age_at_diagosis), fill=Gender) + 
  geom_boxplot(color= c("purple3", "royalblue2"))
p + geom_jitter(shape=16, position=position_jitter(0.2))
#dev.off()
t <- as.data.table(layer_data(p, 1)) %>% 
  select(c("ymin", "middle", "ymax")) %>% 
  `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per gender.csv"))


# Ethnicity
#pdf(paste0(path, "/Age repartition per ethnicity.pdf"), height = 6, width = 9)
p <- ggplot(Global_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot(color= c("darkred", "darkgreen"))
p + geom_jitter(shape=16, position=position_jitter(0.2))
p
#dev.off()

# Race
#pdf(paste0(path, "/Age repartition per race.pdf"), height = 6, width = 9)
p <- ggplot(Global_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot(color= viridis::plasma(n=3))
p + geom_jitter(shape=16, position=position_jitter(0.2))
p
#dev.off()

Global_data <- Global_data %>% 
  mutate(Race_Ethnicity = case_when(
    Ethnicity == "Hispanic" ~ "Hispanic",
    Race == "African American" ~ "African American",
    Race == "White" &
      Ethnicity == "Non- Hispanic" ~ "Caucasian",
    Race == "Others" &
      Ethnicity == "Non- Hispanic" ~ "Non-Hispanic"
  ))
#pdf(paste0(path, "/Age repartition per race_ethnicity.pdf"), height = 6, width = 9)
p <- ggplot(Global_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Race_Ethnicity, y=Age_at_diagosis), fill=Race_Ethnicity) + 
  geom_boxplot(color= viridis::plasma(n=4))
p + geom_jitter(shape=16, position=position_jitter(0.2))
#dev.off()
t <- as.data.table(layer_data(p, 1)) %>% 
  select(c("ymin", "middle", "ymax")) %>% 
  `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per race_ethnicity"))

# Disease status
#pdf(paste0(path, "/Age repartition per disease status.pdf"), height = 6, width = 9)
p <-  ggplot(Global_data %>% filter(!is.na(Age_at_diagosis)) %>% filter(!is.na(Disease_Status.germline)), 
             aes(x=Age_at_diagosis, y=Disease_Status.germline)) + geom_point()

#p <-  ggplot(data=subset(Global_data, !is.na(Age_at_diagosis)), aes(x=Age_at_diagosis, y=Disease_Status.germline)) + geom_point()
p
#dev.off()

