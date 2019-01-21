library(caret)
library(spatstat)
library(lubridate)
library(dplyr)

vk.posts <- read.csv2('vk_posts.csv', header=TRUE, sep=',')
applications <- read.csv2('applications.csv', header=TRUE, sep=';')
vk.statistics <- read.csv2('vk_stat.csv' ,header=TRUE, sep=',')

summary(vk.statistics)

# Converting columns

names(applications) <- c(
  "Date",
  "Number",
  "Status",
  "Comment",
  "id",
  "User.first.name",
  "User.last.name",
  "Birthday",
  "Sex",
  "City",
  "Country",
  "First.name",
  "Last.name",
  "Phone",
  "Mail",
  "User",
  "Link")


vk.posts$Date <- as.Date(vk.posts$Date, '%Y-%m-%d')
applications$Date <- as.Date(applications$Date, '%d.%m.%Y')
vk.statistics$Date <- as.Date(vk.statistics$Date, '%d.%m.%Y')

applications %>%
  group_by(Date=floor_date(Date, "day")) %>%
  mutate(Applications=n()) %>%
  dplyr::select(Date, Applications) -> applications.filtered

vk.statistics %>%
  filter(Parameter.1 == 'Likes') %>%
  mutate(Likes=Value) %>%
  dplyr::select(Date, Likes) -> Likes

vk.statistics %>%
  filter(Parameter.1 == 'Comments') %>%
  mutate(Comments=Value) %>%
  dplyr::select(Date, Comments) -> Comments

vk.statistics %>%
  filter(Parameter.1 == 'Shares') %>%
  mutate(Shares=Value) %>%
  dplyr::select(Date, Shares) -> Shares


merge(applications.filtered, Likes, by=c('Date')) -> data
merge(data, Comments, by=c('Date')) -> data
merge(data, Shares, by=c('Date')) -> data

par(mfrow=c(1,1))
plot(data$Applications ~ data$Date, main='Applications')
plot(data$Comments ~ data$Date, main='Comments')
plot(data$Likes ~ data$Date, main='Likes')
plot(data$Shares ~ data$Date, main='Shares')

# Scatter
par(mfrow=c(1, 1))
scatter.smooth(x=data$Likes, y=data$Applications, main='Applications vs Likes')
scatter.smooth(x=data$Comments, y=data$Applications, main='Applications vs Comments')
scatter.smooth(x=data$Shares, y=data$Applications, main='Applications vs Shares')

# Boxplot
par(mfrow=c(1, 2))
boxplot(data$Applications, main='Applications')
boxplot(data$Likes, main='Likes')
boxplot(data$Comments, main='Comments')
boxplot(data$Shares, main='Shares')

# bad model
linear.model.1 <- lm(Applications ~ Likes, data=data)
summary(linear.model.1)

# ok but not nearly good
linear.model.2 <- lm(Applications ~ Comments, data=data)
summary(linear.model.2)

# bad model but better than first
linear.model.3 <- lm(Applications ~ Shares, data=data)
summary(linear.model.3)

# Comments are slighty ok
linear.model.4 <- lm(Applications ~ ., data=data)
summary(linear.model.4)

# again, worst than Comments model
linear.model.5 <- lm(Applications ~ Comments + Shares, data=data)
summary(linear.model.5)

# model quality
summary(linear.model.2)
AIC(linear.model.2)
BIC(linear.model.2)

# let's test model
train <- createDataPartition(data, p=0.7, list=F)
data.training <- data[train,]
data.test <- data[-train,]

model <- lm(Applications ~ Likes, data=data.training)
model.prediction <- predict.lm(model, data.test)

summary(model)

MAE(model.prediction, data.test$Applications)

par(mfrow=c(1, 1))

plot(data.test$Applications, type='l', col='black')
lines(model.prediction, type='l', col='red')

# let's filter model
merge(Comments, applications.filtered, by=c('Date')) -> data
data %>% filter(Applications < 8) -> data

Likes %>%
  filter(Likes < 70) -> Likes

plot(Likes$Likes ~ Likes$Date, main='Likes')

applications.filtered %>%
  filter(Applications < 15) -> applications.filtered

plot(applications.filtered$Applications ~ applications.filtered$Date, main='Applications')

save(data, file='dataset.Rda')
