library(dplyr)
library(spatstat) 
library(forecast) 
library(lubridate)
library(MASS)

table <- read.csv2('vk_posts.csv', header=TRUE, sep=',')
table2<-read.csv2('applications.csv',header=TRUE, sep=';')
table3<-read.csv2('vk_statistics.csv',header=TRUE, sep=',')

# Converting columns

names(table2) <- c("Date", "Номер", "Статус","Комментарий","id","Имя пользователя", "Фамилия пользователя","Дата рождения","Пол","Город", "Страна","Имя","Фамилия","Телефон","Почта","Пользователь","Ссылка","День")
names(table3) <- c("Date", "Criterion","1","2","Value")

table$Date <- as.POSIXct(table$Date)
table2$Дата <- as.Date(table2$Дата, '%d.%m.%Y')
table3$Date <- as.Date(table3$Date, '%d.%m.%Y')

table2 %>%
  group_by(Дата=floor_date(Date,"day"))%>%
  mutate(Количество=n()) ->table2.filtered

table %>%
  group_by(Date=floor_date(as.Date(Date),"day"))%>%
  summarise(Likes=sum(Likes)) ->table.filtered

table3[table3$`1`=='Likes',] ->Likes
table3[table3$`1`=='Comments',] ->Comments
table3[table3$`1`=='Shares',] ->Shares
table3[table3$`1`=='Votes in polls',] ->Votes
table3[table3$`1`=='Additions to Bookmarks',] ->Bookmarks
table3[table3$`1`=='New members',] ->Followers
table3[table3$`1`=='Members lost',] ->Unfollowers
table3[table3$Criterion=='views',] ->Views
table3[table3$Criterion=='visitors',] ->Visitors

summary(data)
summary(table)
summary(table2)
summary(table3)

#Likes

merge(Likes, table2.filtered, by=c('Date')) -> data

linear.model.raw.1 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.1)
plot(linear.model.raw.1)

#Comments

merge(Comments, table2.filtered, by=c('Date')) -> data

linear.model.raw.2 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.2)
plot(linear.model.raw.2)

#Shares

merge(Shares, table2.filtered, by=c('Date')) -> data

linear.model.raw.3 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.3)
plot(linear.model.raw.3)

#Votes

merge(Votes, table2.filtered, by=c('Date')) -> data

linear.model.raw.4 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.4)
plot(linear.model.raw.4)

#Followers

merge(Followers, table2.filtered, by=c('Date')) -> data

linear.model.raw.5 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.5)
plot(linear.model.raw.5)

#Unfollowers

merge(Followers, table2.filtered, by=c('Date')) -> data

linear.model.raw.6 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.6)
plot(linear.model.raw.6)

#Views

merge(Views, table2.filtered, by=c('Date')) -> data

linear.model.raw.7 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.7)
plot(linear.model.raw.7)

#Visitors

merge(Visitors, table2.filtered, by=c('Date')) -> data

linear.model.raw.8 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.8)
plot(linear.model.raw.8)

#Filter

##Likes

table3[table3$`1`=='Likes',] ->Likes

plot(Likes$Value ~ Likes$Date)

Likes <- Likes %>%
  filter(Value <= 60) 
  
merge(Likes, table2.filtered, by=c('Date')) -> data

linear.model.raw.1 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.1)
plot(linear.model.raw.1)

data.length <- length(data$Date)
middle <- round(data.length * 0.5) 
data.train <- data[1:middle+1 , ] 
data.test <- data[(middle+1) :data.length , ]

summary(middle)

linear.model.raw.11 <- lm(data.train$Value ~ data.train$Количество)
summary(linear.model.raw.11)
plot(linear.model.raw.11)

predict(linear.model.raw.11, data.frame(Количество=data.test$Количество)) -> predictions

line(data$Date ~ data$Количество)
plot(data.test$Количество ~ data.test$Date)

plot(predictions)
plot(data.test)


plot(predictions, type='l', col='red') 
lines(data.test$Количество, col='black')

data.test <- data.frame(Количество=data.test$Количество, Date=data.test$Date)

accuracy(predictions)

show(predictions)

##Comments

plot(Comments$Value ~ Comments$Date)

Comments <- Comments %>%
  filter(Value < 4) 

merge(Comments, table2.filtered, by=c('Date')) -> data

linear.model.raw.2 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.2)
plot(linear.model.raw.2)

##Shares

plot(Shares$Value ~ Shares$Date)

Shares <- Shares %>%
  filter(Value <= 15) 

merge(Shares, table2.filtered, by=c('Date')) -> data

linear.model.raw.3 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.3)
plot(linear.model.raw.3)

##Votes

plot(Votes$Value ~ Votes$Date)

Votes <- Votes %>%
  filter(Value <= 20) 

merge(Votes, table2.filtered, by=c('Date')) -> data

linear.model.raw.4 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.4)
plot(linear.model.raw.4)

##Folowers

plot(Followers$Value ~ Followers$Date)

Followers <- Followers %>%
  filter(Value <= 17) 

merge(Followers, table2.filtered, by=c('Date')) -> data

linear.model.raw.5 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.5)
plot(linear.model.raw.5)

##Unfollowers

plot(Unfollowers$Value ~ Unfollowers$Date)

Unfollowers <- Unfollowers %>%
  filter(Value <= 5) 

merge(Followers, table2.filtered, by=c('Date')) -> data

linear.model.raw.6 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.6)
plot(linear.model.raw.6)

##Views

plot(Views$Value ~ Views$Date)

Views <- Views %>%
  filter(Value <= 200) 

merge(Views, table2.filtered, by=c('Date')) -> data

linear.model.raw.7 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.7)
plot(linear.model.raw.7)

##Visitors

plot(Visitors$Value ~ Visitors$Date)

Visitors <- Visitors %>%
  filter(Value <= 85) 

merge(Visitors, table2.filtered, by=c('Date')) -> data

linear.model.raw.8 <- lm(data$Value ~ data$Количество)
summary(linear.model.raw.8)
plot(linear.model.raw.8)


#____________________________




