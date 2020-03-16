library(lubridate)
Age_data <- Global_data[, c("avatar_id", "TCC_ID", "Gender", "Ethnicity", "Race", 
                            "Disease_Status.germline", "Disease_Status_1", "smoking_status_1",
                            "current_smoker_1", "alcohol_use_1", "bmi_at_dx_v2_1")]

enddate <- today()
Age_data$Age <- interval(start= Global_data$Date_of_Birth, end= enddate)/                      
  duration(n=1, unit="years")
Age_data$Age <- round(Age_data$Age, 2)
summary(Age_data$Age)

Age_data$Age_at_diagosis <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_diagnosis_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_diagosis <- round(Age_data$Age_at_diagosis, 2)
summary(Age_data$Age_at_diagosis, na.rm = TRUE)

Age_data$Age_at_death <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_death_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_death <- round(Age_data$Age_at_death, 2)
summary(Age_data$Age_at_death, na.rm = TRUE)

Age_data$Age_at_lastfollowup <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_last_follow_up_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastfollowup <- round(Age_data$Age_at_lastfollowup, 2)
summary(Age_data$Age_at_lastfollowup, na.rm = TRUE)

Age_data$Age_at_lastdate <- interval(start= Global_data$Date_of_Birth, end= f$last_date_deathorfollowup)/                      
  duration(n=1, unit="years")
Age_data$Age_at_lastdate <- round(Age_data$Age_at_lastdate, 2)
summary(Age_data$Age_at_lastdate, na.rm = TRUE)

Age_data$Age_at_firstdrug <- interval(start= Global_data$Date_of_Birth, end= Global_data$drug_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstdrug <- round(Age_data$Age_at_firstdrug, 2)
summary(Age_data$Age_at_firstdrug, na.rm = TRUE)

Age_data$Age_at_firstbmt <- interval(start= Global_data$Date_of_Birth, end= Global_data$date_of_first_bmt_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstbmt <- round(Age_data$Age_at_firstbmt, 2)
summary(Age_data$Age_at_firstbmt, na.rm = TRUE)

Age_data$Age_at_firstrad <- interval(start= Global_data$Date_of_Birth, end= Global_data$rad_start_date_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_firstrad <- round(Age_data$Age_at_firstrad, 2)
summary(Age_data$Age_at_firstrad, na.rm = TRUE)

Age_data$Age_at_germcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt.germline)/                      
  duration(n=1, unit="years")
Age_data$Age_at_germcollect <- round(Age_data$Age_at_germcollect, 2)
summary(Age_data$Age_at_germcollect, na.rm = TRUE)

Age_data$Age_at_tumorcollect <- interval(start= Global_data$Date_of_Birth, end= Global_data$collectiondt_1)/                      
  duration(n=1, unit="years")
Age_data$Age_at_tumorcollect <- round(Age_data$Age_at_tumorcollect, 2)
summary(Age_data$Age_at_tumorcollect, na.rm = TRUE)

#pdf(paste0(path, "/Age at diagnosis repartition.pdf"), height = 6, width = 9)
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
)
layer_data(p, 1)
#dev.off()

# Gender
#pdf(paste0(path, "/Age repartition per gender.pdf"), height = 6, width = 9)
p <- ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Gender, y=Age_at_diagosis), fill=Gender) + 
  geom_boxplot(color= c("purple3", "royalblue2")) +
  ggtitle("Age repartition per gender")
p + geom_jitter(shape=16, position=position_jitter(0.2))
#dev.off()
t <- as.data.table(layer_data(p, 1)) %>% 
  select(c("ymin", "middle", "ymax")) %>% 
  `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per gender.csv"))


# Ethnicity
#pdf(paste0(path, "/Age repartition per ethnicity.pdf"), height = 6, width = 9)
p <- ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Ethnicity, y=Age_at_diagosis), fill=Ethnicity) + 
  geom_boxplot(color= c("darkred", "darkgreen")) +
  ggtitle("Age repartition per ethnicity")
p + geom_jitter(shape=16, position=position_jitter(0.2))
p
#dev.off()

# Race
#pdf(paste0(path, "/Age repartition per race.pdf"), height = 6, width = 9)
p <- ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Race, y=Age_at_diagosis), fill=Race) + 
  geom_boxplot(color= viridis::plasma(n=3)) +
  ggtitle("Age repartition per race")
p + geom_jitter(shape=16, position=position_jitter(0.2))
p
#dev.off()

Age_data <- Age_data %>% 
  mutate(Race_Ethnicity = case_when(
    Ethnicity == "Hispanic" ~ "Hispanic",
    Race == "African American" ~ "African American",
    Race == "White" &
      Ethnicity == "Non- Hispanic" ~ "Caucasian",
    Race == "Others" &
      Ethnicity == "Non- Hispanic" ~ "Non-Hispanic"
  ))
#pdf(paste0(path, "/Age repartition per race_ethnicity.pdf"), height = 6, width = 9)
p <- ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)), aes(x=Race_Ethnicity, y=Age_at_diagosis), fill=Race_Ethnicity) + 
  geom_boxplot(color= viridis::plasma(n=4)) +
  ggtitle("Age repartition per race/ethnicity")
p + geom_jitter(shape=16, position=position_jitter(0.2))
#dev.off()
t <- as.data.table(layer_data(p, 1)) %>% 
  select(c("ymin", "middle", "ymax")) %>% 
  `colnames<-`(c("min", "median", "max"))
# write.csv(t,paste0(path, "/Age repartition per race_ethnicity"))

# Disease status
#pdf(paste0(path, "/Age repartition per disease status.pdf"), height = 6, width = 9)
p <-  ggplot(Age_data %>% filter(!is.na(Age_at_diagosis)) %>% filter(!is.na(Disease_Status.germline)), 
             aes(x=Age_at_diagosis, y=Disease_Status.germline)) + geom_point() +
  ggtitle("Age repartition per disease status")

#p <-  ggplot(data=subset(Age_data, !is.na(Age_at_diagosis)), aes(x=Age_at_diagosis, y=Disease_Status.germline)) + geom_point()
p
#dev.off()



#######################################################################

colnames(Age_data)

library(timevis)

data <- data.frame(
  id = Age_data$avatar_id,
  content = c("Age_at_diagosis", "Age_at_death", "Age_at_firstdrug", "Age_at_firstbmt", "Age_at_firstrad",
              "Age_at_germcollect", "Age_at_tumorcollect"),
  start = c(Age_data$Age_at_diagosis, Age_data$Age_at_death, Age_data$Age_at_firstdrug, Age_data$Age_at_firstbmt, Age_data$Age_at_firstrad,
            Age_data$Age_at_germcollect, Age_data$Age_at_tumorcollect),
  end = c(NA, NA, "2020-03-31", NA, "2020-03-31", NA, NA)
)
timevis::timevis(data)
##
bah <- Global_data[(!is.na(Global_data$date_of_diagnosis_1)),]
data <- data.frame(
  id = bah$avatar_id,
  content = "Age_at_diagosis",
  start = bah$date_of_diagnosis_1,
  end = NA
)
timevis::timevis(data)
#############
a <- list(names = Age_data$avatar_id, level = 1, start = Age_data$Age_at_diagosis, end = Age_data$Age_at_firstdrug)

library(candela)
candela("GanttChart",
        data = a, label = names, 
        start = "start", end = "end", level = "level", width = "auto", height = "auto")


##############
library(DiagrammeR)
df <- data.frame(task = c("task1", "task2", "task3"),
                 #status = c("done", "active", "crit"),
                 #pos = c("first_1", "first_2", "first_3"),
                 start = Age_data$Age_at_diagosis,
                 end = Age_data$Age_at_firstdrug)
mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  YYYY-MM-DD", "\n", 
    "title A Very Nice Gantt Diagram", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
    paste(df %>%
            unite(i, task, sep = ":") %>%
            unite(j, i, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)
# m$x$config = list(ganttConfig = list(
#   axisFormatter = list(list(
#     "%b %d, %Y" 
#     ,htmlwidgets::JS(
#       'function(d){ return d.getDay() == 1 }' 
#     )
#   ))
# ))

################
library("vistime")  

dat <- data.frame(Position=c(rep("President", 3), rep("Vice", 3)),
                  Name = c("Washington", "Adams", "Jefferson", "Adams", "Jefferson", "Burr"),
                  start = rep(c("1789-03-29", "1797-02-03", "1801-02-03"), 2),
                  end = rep(c("1797-02-03", "1801-02-03", "1809-02-03"), 2),
                  color = c('#cbb69d', '#603913', '#c69c6e'),
                  fontcolor = rep("white", 3))

vistime(dat, events="Position", groups="Name", title="Presidents of the USA")


# as.Date
diag <- Global_data[,c("avatar_id", "date_of_diagnosis_1", "date_of_diagnosis_1")]
diag$diag <- "diagnosis"
drug1 <- Global_data[,c("avatar_id", "drug_start_date_1", "drug_start_date_1")]
drug1$drug <- "drug"




 data <- read.csv(text="event,group,start,end,color
                        Phase 1,ID1,2016-12-22,2016-12-23,#c8e6c9
                        Phase 3,ID1,2016-12-29,2017-01-06,#fb8c00
                        Phase 4,ID1,2017-01-06,2017-02-02,#DD4B39
                        Room 334,ID2,2016-12-22,2016-12-28,#DEEBF7
                        Room 335,ID2,2016-12-28,2017-01-05,#C6DBEF
                        Room 335,ID2,2017-01-05,2017-01-23,#9ECAE1
                        Phase 2,ID1,2016-12-23,2016-12-29,#a5d6a7
                        Group 1,ID2,2016-12-22,2016-12-28,#E5F5E0
                        Group 2,Team 2,2016-12-28,2017-01-23,#C7E9C0
                        1-217.0,category 2,2016-12-27,2016-12-27,#90caf9
                        3-200,category 1,2016-12-25,2016-12-25,#1565c0
                        3-330,category 1,2016-12-25,2016-12-25,#1565c0
                        3-223,category 1,2016-12-28,2016-12-28,#1565c0
                        3-225,category 1,2016-12-28,2016-12-28,#1565c0
                        3-226,category 1,2016-12-28,2016-12-28,#1565c0
                        3-226,category 1,2017-01-19,2017-01-19,#1565c0
                        3-330,category 1,2017-01-19,2017-01-19,#1565c0
                        4-399.7,moon rising,2017-01-13,2017-01-13,#f44336
                        8-831.0,sundowner drink,2017-01-17,2017-01-17,#8d6e63
                        9-984.1,birthday party,2016-12-22,2016-12-22,#90a4ae
                        F01.9,Meetings,2016-12-26,2016-12-26,#e8a735
                        Z71,Meetings,2017-01-12,2017-01-12,#e8a735
                        B95.7,Meetings,2017-01-15,2017-01-15,#e8a735
                        T82.7,Meetings,2017-01-15,2017-01-15,#e8a735")

 vistime(data)

############# plotty
 
 library(plotly)
 
 df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/GanttChart-updated.csv", 
                stringsAsFactors = F)
 
 df$Start  <- as.Date(df$Start, format = "%m/%d/%Y")
 client    <- "Sample Client"
 cols      <- RColorBrewer::brewer.pal(length(unique(df$Resource)), name = "Set3")
 df$color  <- factor(df$Resource, labels = cols)
 
 p <- plot_ly()
 for(i in Global_data$avatar_id){
   p <- add_trace(p,
                  x = c(Global_data$date_of_diagnosis_1[i], (Global_data$date_of_diagnosis_1[i] + Global_data$Age[i])), 
                  y = c(i, i), 
                  mode = "lines",
                  # line = list(color = df$color[i], width = 20),
                  # showlegend = F,
                  # hoverinfo = "text",
                  # text = paste("Task: ", df$Task[i], "<br>",
                  #              "Duration: ", df$Duration[i], "days<br>",
                  #              "Resource: ", df$Resource[i]),
                  # evaluate = T
   )
 }
 
 p


