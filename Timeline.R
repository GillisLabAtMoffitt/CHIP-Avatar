
dat1 <- Demo_RedCap_V4ish[, c("avatar_id", "Date_of_Birth")]
ggplot(dat1, aes(Date_of_Birth, avatar_id)) +
geom_point(size = 1) +
labs(x="Birth", y=NULL, title="Birth timeline")

dat2 <- mm_history[, c("avatar_id", "date_of_diagnosis")]
ggplot(dat2, aes(date_of_diagnosis, avatar_id)) +
  geom_point(size = 1, colour = "red") +
  labs(x="diagnosis", y=NULL, title="diagnosis timeline")

dat3 <- sct
dat3 <- dat3 %>% pivot_longer(c("date_of_first_bmt", "date_of_second_bmt", "date_of_third_bmt"), 
                              names_to = "type", values_to = "bmt_date")
ggplot(dat3, aes(bmt_date, avatar_id)) +
  geom_point(size = 1, colour = "green") +
  labs(x="BMT", y=NULL, title="BMT timeline")


dat4 <- radiation[, c("avatar_id", "rad_start_date")]
# dat4 <- dat4 %>% pivot_longer(c("rad_start_date", "rad_stop_date"), 
#                       names_to = "type", values_to = "rad_date") %>% 
#  arrange(rad_date)
ggplot(dat4, aes(rad_start_date, avatar_id)) +
geom_point(size = 1, colour = "blue") +
labs(x="Radiation", y=NULL, title="Radiation timeline")

dat5 <- treatment[, c("avatar_id", "drug_start_date")]
# dat5 <- dat5 %>% pivot_longer(c("drug_start_date", "drug_stop_date"), 
#                               names_to = "type", values_to = "drug_date") %>% 
#   select(c("avatar_id", "type", "drug_date")) %>% 
#   arrange(drug_date)
ggplot(dat5, aes(drug_start_date, avatar_id)) +
  geom_point(size = 1, colour = "orange") +
  labs(x="Treatment", y=NULL, title="Treatment timeline")

dat6 <- Vitals[, c("avatar_id", "date_death")]

all <- bind_rows(dat1, dat2, dat3,dat4,dat5) #%>%
  # pivot_longer(c("rad_date", "drug_date"),
  #              names_to = "Type", values_to = "Dates")
ggpp <- ggplot() 
ggpp +
  geom_point(data=dat6, aes(date_death, avatar_id), colour = "black", alpha=1) +
  geom_point(data=dat5, aes(drug_start_date, avatar_id), colour = "yellow", alpha=0.8) +
  geom_point(data=dat2, aes(date_of_diagnosis, avatar_id), colour = "red", alpha=0.3) +
  geom_point(data=dat3, aes(bmt_date, avatar_id), colour = "green", alpha=0.3) +
  geom_point(data=dat4, aes(rad_start_date, avatar_id), colour = "blue", alpha=0.3) +
  labs(x="all", y=NULL, title="all timeline") +
  scale_fill_manual(values=c("black", "yellow", "red", "green", "blue"), 
                    name="Experimental\nCondition",
                    labels=c("death","drugs", "a", "b", "c"))
#+legend(x = "right", c("death","drugs", "a", "b", "c"), col= col)
  
  #theme(legend.position = c("death" = "black"))
col <- c("black", "yellow", "red", "green", "blue")
ggp <- ggplot()
ggp +
  geom_point(data=dat1, aes(Date_of_Birth, avatar_id, colour = "birth", alpha=0.3)) +
  geom_point(data=dat5, aes(drug_start_date, avatar_id, colour = "orange", alpha=0.3)) +
  geom_point(data=dat2, aes(date_of_diagnosis, avatar_id, colour = "red", alpha=0.3)) +
  geom_point(data=dat3, aes(bmt_date, avatar_id, colour = "green", alpha=0.3)) +
  geom_point(data=dat4, aes(rad_start_date, avatar_id, colour = "blue", alpha=0.3)) +
  labs(x="all", y=NULL, title="all timeline") +
  scale_fill_manual(values=c("black", "yellow", "red", "green", "blue"), 
                    name="Experimental\nCondition",
                    labels=c("death","drugs", "a", "b", "c"))
ggp +
  geom_point(data=dat1, aes(Date_of_Birth, avatar_id, color="col", alpha ="col")) +
  geom_point(data=dat5, aes(drug_start_date, avatar_id)) +
  geom_point(data=dat2, aes(date_of_diagnosis, avatar_id)) +
  geom_point(data=dat3, aes(bmt_date, avatar_id)) +
  geom_point(data=dat4, aes(rad_start_date, avatar_id)) +
  scale_color_manual(values = c("black", "yellow", "red", "green", "blue")) +
  scale_alpha_manual(values=c(0.1,1, 0.3, 0.3, 0.3)) +
  labs(x="all", y=NULL, title="all timeline")


ggplot(all, aes(Dates, avatar_id, color=Type, alpha = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("black", "yellow", "red", "green", "blue")) +
  scale_alpha_manual(values=c(0.1,1, 0.3, 0.3, 0.3)) +
  labs(x="all", y=NULL, title="all timeline")

rm()



















library(timevis)

data <- data.frame(
  id = "dat1$avatar_id",
  content = c("Date_of_Birth", "Date_of_Birth"),
  start = c("2013-08-12"	, "2015-01-15"),
  end = c("2013-08-30"	, "2015-01-30")
)
timevis::timevis(data)

data <- data.frame(
  id      = 1:5,
  content = c("Item one"  , "Item two"  ,"Ranged item", "Item four","fith"),
  start   = c("2016-01-10", "2016-01-11", "2016-01-20", "2016-02-14 15:00:00", NA),
  end     = c(NA          ,           NA, "2016-02-04", NA, NA)
)

timevis(data)

#####################################


library("vistime")  
# will have to do list...
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

##########################################


############# plotty

library(plotly)

df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/GanttChart-updated.csv", 
               stringsAsFactors = F)

df$Start  <- as.Date(df$Start, format = "%m/%d/%Y")
client    <- "Sample Client"
cols      <- RColorBrewer::brewer.pal(length(unique(df$Resource)), name = "Set3")
df$color  <- factor(df$Resource, labels = cols)

p <- plot_ly()
for(i in 1:(nrow(df) - 1)){
  p <- add_trace(p,
                 x = c(df$Start[i], df$Start[i] + df$Duration[i]), 
                 y = c(i, i), 
                 mode = "lines",
                 line = list(color = df$color[i], width = 20),
                 showlegend = F,
                 hoverinfo = "text",
                 text = paste("Task: ", df$Task[i], "<br>",
                              "Duration: ", df$Duration[i], "days<br>",
                              "Resource: ", df$Resource[i]),
                 evaluate = T
  )
}

p












##
bah <- Global_data[(!is.na(Global_data$date_of_diagnosis_1)),]
data <- data.frame(
  id = bah$avatar_id,
  content = "Age_at_diagosis",
  start = bah$date_of_diagnosis_1,
  end = NA
)
timevis::timevis(data)
a <- list(names = Tml$avatar_id, level = 1, start = Tml$Age_at_diagosis, end = Tml$Age_at_firstdrug)
