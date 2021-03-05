Tml0 <- Demo_RedCap_V4ish[, 
                         c("avatar_id", "Date_of_Birth")] %>% 
  `colnames<-`(c("avatar_id", "date"))
Tml0$type <- "Date_of_Birth"

Tml <- Global_data[ ,c("avatar_id", "collectiondt_germline")]
Tml <- gather(Tml,"type", "date", "collectiondt_germline")

Tml1 <- Vitals[, c("avatar_id", "date_last_follow_up", "date_death")]
Tml1 <- gather(Tml1,"type", "date", "date_last_follow_up", "date_death")
                              
# LiDate <- list(birthdate = Demo_RedCap_V4ish[, c("avatar_id", "Date_of_Birth")],
#                deathdate = Vitals[, c("avatar_id", "date_death")],
#                lastfollow = Vitals[, c("avatar_id", "date_last_follow_up")])
# df <- as.data.frame.list(LiDate)
Tml2 <- Radiation[, ]
Tml2 <- gather(Tml2,"state", "date", 2:5) %>% select(c(avatar_id, type, date))
Tml2$type <- "Radiation"

Tml3 <- SCT[, c(1:2)] %>% rename(date = date_of_bmt_1)
Tml3$type <- "SCT"

Tml4 <- Treatment[, c(1:4, 38, 56, 72)]
Tml4 <- gather(Tml4,"state", "date", 2:ncol(Tml4)) %>% 
mutate_all(funs(str_replace_all(., "drug_start_date_..", "start"))) %>%
mutate_all(funs(str_replace_all(., "drug_start_date_.", "start"))) %>%
mutate_all(funs(str_replace_all(., "drug_stop_date_..", "stop"))) %>%
mutate_all(funs(str_replace_all(., "drug_stop_date_.", "stop"))) %>% 
  arrange(date)
Tml4$type <- "Drugs"
Tml4$date <- as.Date(Tml4$date)
g <- ggplot(Tml4, aes(date, avatar_id), group=avatar_id)
gg <- g +
  geom_line(aes(color = type), size = 2, position = position_dodge(width = 0.5))
#geom_line(aes(color = state), size = 1) 
gg
class(Tml4$date)
Tml5 <- MM_history[, c(1,2,4,6)]
Tml5 <- gather(Tml5,"type", "date", 2:4)

Tml5 <- bind_rows(Tml, Tml1, Tml2, Tml3, Tml4, Tml5) #%>% 
  # mutate_all(funs(str_replace_all(., "drug_start_date_..", "drug_start_date"))) %>% 
  # mutate_all(funs(str_replace_all(., "drug_start_date_.", "drug_start_date"))) %>% 
  # mutate_all(funs(str_replace_all(., "drug_stop_date_..", "drug_stop_date"))) %>% 
  # mutate_all(funs(str_replace_all(., "drug_stop_date_.", "drug_stop_date"))) %>% 
  # mutate_all(funs(str_replace_all(., "rad_start_date_.", "rad_start_date"))) %>% 
  # mutate_all(funs(str_replace_all(., "rad_stop_date_.", "rad_stop_date"))) 

table(Tml5$type)

ggplot(Tml5, aes(date, avatar_id)) +
  geom_line(size = 1, colour = "blue") +
  geom_point(size = 1, colour = "red") +
  labs(x="Radiation", y=NULL, title="Radiation timeline")
ggplot(Tml5, aes(date, avatar_id)) +
  geom_line(size = 1, colour = "blue") +
  labs(x="Radiation", y=NULL, title="Radiation timeline")

ggplot(Tml5, aes(date, avatar_id)) +
  # theme(legend.position = "none") +
  geom_line(aes(color = state), size = .5) +
  geom_point(aes(color = type), size = 1) #+
  # labs(x="date", y=NULL, title=NULL)

g <- ggplot(Tml5, aes(date, avatar_id))

gg <- g +
  geom_line(aes(color = state), size = 4)
  #geom_line(aes(color = state), size = 1) 
gg
summary(g)
gg +
  geom_point(aes(color = type), size = 1)

#############################################################################################################
# events_dates <- all_dates %>% 
#   filter(grepl("drug|rad", event))

events_dates <- all_dates %>% 
  filter(!str_starts(event, "drug|rad"))

events_period <- all_dates %>% 
  filter(str_starts(event, "drug|rad")) %>% 
  mutate(status = case_when(
    str_detect(event, "start") ~ "start",
    str_detect(event, "stop") ~ "stop"
  )) %>% 
  mutate(event = 
           str_replace(event,
                       "_1_1|_1_2|_1_3|_1_4|_1_5|_1_6|_1_7|_1_8|_1_9|_1_10|_1_11|_1_12|_1_13|_1_14|_1_15|_1_16|_1_17|_1_18",
                       "_1")) %>% 
  mutate(event = 
           str_replace(event,
                       "_2_1|_2_2|_2_3|_2_4|_2_5|_2_6|_2_7|_2_8|_2_9",
                       "_1")) %>% 
  mutate(event = 
           str_replace(event,
                       "_3_1|_3_2|_3_3|_3_4|_3_5|_3_6|_3_7",
                       "_1")) %>% 
  group_by(avatar_id, event) %>% 
  distinct(event, .keep_all = TRUE)

events <-  bind_rows(events_dates, events_period)

events_dates %>% 
  ggplot() +
  geom_point(aes(x=date, y=avatar_id, color=event))

events_period %>% 
  ggplot(aes(x=date, y=avatar_id, color=event, group = avatar_id)) +
  geom_line()

events %>% 
  ggplot(aes(x=date, y=avatar_id, color=event, group = avatar_id)) +
  geom_line()+
  geom_point(aes(x=date, y=avatar_id, color=event))

gg <- ggplot()
ggg <- gg + geom_line(data = events_period, aes(x=date, y=avatar_id, color=event, group = avatar_id))
gggg <- ggg + geom_point(data = events_dates, aes(x=date, y=avatar_id, color=event))

#### Plot with age

events_dates1 <- events_dates %>% 
  group_by(avatar_id) %>% 
  mutate(age = date - lag(date)) %>% 
  mutate(int =  interval(start = diagnosisdate, end = followupdate)/ duration(n=1, units = "years")) %>% 
  mutate(aage = as.POSIXct(age), )
  # mutate(lag1_within.consecutive = lag(date, 1,time.step = "within"))

  
  
gg_ev <- ggg + geom_point(data = events_dates1, aes(x=date, y=avatar_id, color=event), size=1, alpha=0.3) +
  theme(legend.position='top')






