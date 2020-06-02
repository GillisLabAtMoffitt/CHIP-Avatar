Tml0 <- Demo_RedCap_V4ish[, 
                         c("avatar_id", "Date_of_Birth")] %>% 
  `colnames<-`(c("avatar_id", "date"))
Tml0$type <- "Date_of_Birth"

Tml <- Global_data[ ,c("avatar_id", "collectiondt.germline")]
Tml <- gather(Tml,"type", "date", "collectiondt.germline")

Tml1 <- Vitals[, c("avatar_id", "date_last_follow_up", "date_death")]
Tml1 <- gather(Tml1,"type", "date", "date_last_follow_up", "date_death")
                              
# LiDate <- list(birthdate = Demo_RedCap_V4ish[, c("avatar_id", "Date_of_Birth")],
#                deathdate = Vitals[, c("avatar_id", "date_death")],
#                lastfollow = Vitals[, c("avatar_id", "date_last_follow_up")])
# df <- as.data.frame.list(LiDate)
Tml2 <- Radiation[, ]
Tml2 <- gather(Tml2,"state", "date", 2:5)
Tml2$type <- "Radiation"

Tml3 <- SCT[, c(1, 6:11)]
Tml3 <- gather(Tml3,"type", "date", 2:7)

Tml4 <- Treatment[, c(1, 35:100)]
Tml4 <- gather(Tml4,"state", "date", 2:67) %>% 
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



