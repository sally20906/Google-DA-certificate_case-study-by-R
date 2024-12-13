
install.packages("readr")
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(data.table)
library(ggplot2)
library(readr)

#Check format
trip_2019_Q1 <- Divvy_Trips_2019_Q1
trip_2020_Q1 <- Divvy_Trips_2020_Q1
str(trip_2019_Q1)
sum(is.na(trip_2019_Q1))
summary(trip_2019_Q1)

##計算ride_duration
trip_2019_Q1 <- trip_2019_Q1 %>% mutate(ride_duration = as.numeric(difftime(end_time, start_time, units = "mins")))
sum(is.na(trip_2019_Q1$ride_duration)) ##確認沒有NA值
sum(is.na(trip_2019_Q1$gender)) #有NA
sum(is.na(trip_2019_Q1$usertype)) #沒有NA
trip_2019_Q1_2 <- subset(trip_2019_Q1, !is.na(gender))

##依照usertype分兩組
trip_2019_Q1_subscriber <- trip_2019_Q1_2 %>% filter(usertype=="Subscriber")
trip_2019_Q1_customer <- trip_2019_Q1_2 %>% filter(usertype=="Customer")

#scale_y...讓縱軸顯示正確數字非科學符號
ggplot(trip_2019_Q1_subscriber) +
  geom_bar(mapping = aes(x = gender, fill=gender), na.rm=TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total of Subscribers by Gender",subtitle = "Data in 2019 Q1", y = "Count") +
  theme_minimal()

ggplot(trip_2019_Q1_customer) +
  geom_bar(mapping = aes(x = gender, fill=gender)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total of Customer by Gender",subtitle = "Data in 2019 Q1", y = "Count") +
  theme_minimal()

# usertype 的總數
total_count <- nrow(trip_2019_Q1)
usertype_counts <- table(trip_2019_Q1$usertype) ##可以直接把兩種type分開
prop.table(usertype_counts) * 100
print(usertype_percentages) ##customer：6%，subscriber：94%

ggplot(trip_2019_Q1) + geom_bar(mapping = aes(x = usertype, fill=usertype))+
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total trip of usertype",subtitle = "Data in 2019 Q1", y = "Count") +
  theme_minimal()

####平均ride_duration####
trip_2019_Q1 <- data.table(trip_2019_Q1)
trip_2019_Q1[ride_duration == 177200 , ] ##要先移除這個極端值

avg_duration <- trip_2019_Q1 %>% filter(ride_duration != 177200) %>%
  group_by(usertype) %>% 
  summarise(mean_user = mean(ride_duration))
ggplot(avg_duration) +
  geom_col(mapping = aes(x = usertype, y= mean_user, fill=usertype)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average ride duration",subtitle = "Data in 2019 Q1", y = "AVG ride duration") +
  theme_minimal()



####以性別分析：只就男生跟女生來說，女生的平均ride duration 比男性多一些####
trip_2019_Q1_2 <- data.table(trip_2019_Q1_2)
trip_2019_Q1_2[ , .N, gender]
avg_gender_ride <- trip_2019_Q1_2 %>% group_by(gender) %>%
  summarise(avg_ride= mean(ride_duration))
ggplot(avg_gender_ride) + geom_col(mapping = aes(x=gender, y=avg_ride, fill=gender))+
  labs(title = "Average ride duration by gender", subtitle="Data in 2019 Q1", y = "AVG ride duration")

####By date####
trip_2019_Q1 <- data.table(trip_2019_Q1)

trip_2019_Q1[, .N, by=start_time]#含時間
#trip_2019_Q1[, start_date := NULL]

trip_2019_Q1 <- trip_2019_Q1 %>%
  mutate(start_date = substr(start_time, 1, 8))
trip_2019_Q1$start_date <- as.Date(trip_2019_Q1$start_date)
trip_2019_Q1[, .N, by=start_date]

##創2019年每日數量
Q1_2019 <- data.table(seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by =
                             "day"))
Q1_2019 <- trip_2019_Q1[ ,.N, by=Date]
setnames(Q1_2019, "Date")
setnames(trip_2019_Q1, old = "start_date", new = "Date")##因為是data table，不能用colnames
trip_by_date <- merge(Q1_2019, trip_2019_Q1[, .N, by = Date], 
                      by = "Date", all.x = TRUE)

avg_duration_Q1_gender <- trip_2019_Q1_2 %>%
  group_by(usertype, gender) %>%
  summarise(mean = mean(ride_duration))

ggplot(avg_duration_Q1_gender) + geom_col(mapping=aes(x=gender, y=mean, fill=gender)) + 
  facet_wrap(~usertype)+
  labs(title = "Average Ride Duration of gender by User Type", subtitle = "Data in 2019 Q1", y="AVG Ride Duration")


ggplot(avg_duration_Q1_gender) + geom_col(mapping=aes(x=gender, y=mean, fill=gender)) + 
  facet_wrap(~usertype)+
  labs(title = "Average Ride Duration of gender by User Type", subtitle = "Data in 2019 Q1", y="AVG Ride Duration")

##可看每一季共享單車使用數趨勢及最低點
min_date <- Q1_2019 %>% filter(N == min(N)) %>% pull(Date) 
ggplot(Q1_2019, aes(x = Date, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of useage", title = "Ride over time") +
  scale_x_date(breaks = "15 day") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1))+
  geom_vline(xintercept = as.numeric(min_date), color = "blue", linetype = "dashed")


#依照相同規則處理Q1~Q4資料
##合併2019年四季資料
trip_2019_Q4 <- trip_2019_Q4 %>%
  mutate(start_date = substr(start_time, 1, 10))

str(trip_2019_Q4$start_date)
trip_2019_Q4$start_date <- as.Date(trip_2019_Q4$start_date)

Q4_2019 <- trip_2019_Q4[ ,.N, by=start_date]
setnames(Q4_2019, "start_date", "Date")
trip_by_date <- merge(trip_by_date, Q4_2019, by="Date" , all.x = TRUE)


#2019年每日共享單車使用數
trip_by_date <- trip_by_date %>%
  mutate(N.x = ifelse(is.na(N.x), 0, N.x),
         N.y = ifelse(is.na(N.y), 0, N.y)) %>%
  mutate(N = N.x+N.y)%>%
  select(-N.x) %>%
  select(-N.y)

##將usertype total count按每個月分開
Q1_month <- trip_2019_Q1[ , c("Date", "usertype")]
Q1_month <- Q1_month %>% mutate(month = substr(Date, 6, 7)) %>% group_by(month, usertype) %>%
  select(-Date) %>%summarize(count = n()) %>%
  pivot_wider(names_from = usertype, values_from = count)


Q2_month <- trip_2019_Q2[ , c("start_date", "usertype")]
Q2_month <- Q2_month %>% mutate(month = substr(start_date, 6, 7)) %>% group_by(month, usertype) %>%
  select(-start_date) %>%summarize(count = n()) %>%
  pivot_wider(names_from = usertype, values_from = count)

Q3_month <- trip_2019_Q3[ , c("start_date", "usertype")]
Q3_month <- Q3_month %>% mutate(month = substr(start_date, 6, 7)) %>% group_by(month, usertype) %>%
  select(-start_date) %>%summarize(count = n()) %>%
  pivot_wider(names_from = usertype, values_from = count)

Q4_month <- trip_2019_Q4[ , c("start_date", "usertype")]
Q4_month <- Q4_month %>% mutate(month = substr(start_date, 6, 7)) %>% group_by(month, usertype) %>%
  select(-start_date) %>%summarize(count = n()) %>%
  pivot_wider(names_from = usertype, values_from = count)

month_by_user <- bind_rows(Q1_month, Q2_month, Q3_month, Q4_month)
month_by_user <- month_by_user %>%
  pivot_longer(cols = c(Subscriber, Customer), 
               names_to = "user_type", 
               values_to = "count")

ggplot(month_by_user, aes(x = as.factor(month), fill = user_type)) +
  geom_bar(aes(y = count), stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Month", 
    y = "Count",
    fill = "user_type"
  ) +
  scale_fill_manual(values = c("Subscriber" = "skyblue", "Customer" = "orange")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Total Rides by Month")

