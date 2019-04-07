airbnb <- read.table("airbnb_session_data.txt", sep="|", header=TRUE)

str(airbnb)

airbnb$day <- weekdays(as.Date(airbnb$ds))

airbnb$date1 = as.POSIXlt(airbnb$ts_min,format="%Y-%m-%d %H:%M:%S")
airbnb$date2 = as.POSIXlt(airbnb$ts_max,format="%Y-%m-%d %H:%M:%S")

airbnb$sessiontime <- difftime(airbnb$date2,  airbnb$date1,units="mins")

airbnb$sessionstart_hour <- hour(airbnb$date1)

#install.packages("lubridate")
library(lubridate)

library(dplyr)

airbnb %>% summarise_all(n_distinct)

# we will look at the following normalized metrics to see how they are influences - searches per visitor, messages per visitor, booking requests per visitor

# summarizing by device

summary_device <- airbnb %>% group_by(dim_device_app_combo) %>% summarize(total_searches=sum(did_search), 
                                                        total_messages =sum(sent_message) , 
                                                        total_booking_requests= sum(sent_booking_request), 
                                                        total_visitors= n_distinct(id_visitor)
                                                        ,searches_per_visitor = sum(did_search)/n_distinct(id_visitor)
                                                        ,messages_per_visitor = sum(sent_message)/n_distinct(id_visitor)
                                                        ,booking_requests_per_visitor = sum(sent_booking_request)/n_distinct(id_visitor))

library(ggplot2)

library(reshape2)

data.m <- melt(summary_device, id.vars='dim_device_app_combo')

data.m <- data.m %>% filter(variable %in% c("searches_per_visitor","messages_per_visitor","booking_requests_per_visitor"))

#visualizing summary metrics by device

ggplot(data.m, aes(dim_device_app_combo, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#summarizing by day of week

summary_dayofweek <- airbnb %>% group_by(day) %>% summarize(total_searches=sum(did_search), 
                                                                          total_messages =sum(sent_message) , 
                                                                          total_booking_requests= sum(sent_booking_request), 
                                                                          total_visitors= n_distinct(id_visitor)
                                                                          ,searches_per_visitor = sum(did_search)/n_distinct(id_visitor)
                                                                          ,messages_per_visitor = sum(sent_message)/n_distinct(id_visitor)
                                                                          ,booking_requests_per_visitor = sum(sent_booking_request)/n_distinct(id_visitor))


data.d <- melt(summary_dayofweek, id.vars='day')

data.d <- data.d %>% filter(variable %in% c("searches_per_visitor","messages_per_visitor","booking_requests_per_visitor"))

#visulazing summary metrics by day of week

ggplot(data.d, aes(day, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))




#summarizing by hour of session start

summary_hour <- airbnb %>% group_by(sessionstart_hour) %>% summarize(total_searches=sum(did_search), 
                                                            total_messages =sum(sent_message) , 
                                                            total_booking_requests= sum(sent_booking_request), 
                                                            total_visitors= n_distinct(id_visitor)
                                                            ,searches_per_visitor = sum(did_search)/n_distinct(id_visitor)
                                                            ,messages_per_visitor = sum(sent_message)/n_distinct(id_visitor)
                                                            ,booking_requests_per_visitor = sum(sent_booking_request)/n_distinct(id_visitor))


data.d <- melt(summary_hour, id.vars='sessionstart_hour')

data.d <- data.d %>% filter(variable %in% c("searches_per_visitor","messages_per_visitor","booking_requests_per_visitor"))

#visulazing summary metrics by summarizing by hour of session start

ggplot(data.d, aes(sessionstart_hour, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))




#visualzing session time effect on metrics

summary(airbnb)

#searches

ggplot(data = airbnb, aes(x= sessiontime ))  + facet_wrap(~did_search)  +   scale_x_continuous(limits = c(0,100))  + geom_histogram()

#messages

ggplot(data = airbnb, aes(x= sessiontime ))  + facet_wrap(~sent_message)  +   scale_x_continuous(limits = c(0,100))  + geom_histogram()

bookings <- subset(airbnb , sent_booking_request == 1)

#bookings

ggplot(data = airbnb, aes(x= sessiontime ))  + facet_wrap(~sent_booking_request)  +   scale_x_continuous(limits = c(0,100))    + geom_histogram()

# there are very few bookings obviously, so let's see the bookings in isolation

ggplot(data = bookings, aes(x= sessiontime ))  + facet_wrap(~sent_booking_request)  +   scale_x_continuous(limits = c(0,100))    + geom_histogram()

# let's try to do some predictive modelling on messages sent

# we can start with a simple logistic regression 

str(airbnb)

#divide into train and test


smp_size <- floor(0.8 * nrow(airbnb))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(airbnb)), size = smp_size)

train <- airbnb[train_ind, ]
test <- airbnb[-train_ind, ]

lr_reg <- glm(sent_message  ~ sessiontime + sessionstart_hour + day + dim_device_app_combo    ,data =train , family= 'binomial' )

summary(lr_reg)


predict <- predict(lr_reg, type = 'response')

roc.curve(train$sent_message, predict)


predict[0:100]

summary(predict)

pred_test <- predict(lr_reg, newdata = test , type = "response")

roc.curve(test$sent_message, pred_test)

# we are dealing with class imbalance here, so let's try oversampling

#install.packages("ROSE")

library(ROSE)

table(train$sent_message)

data_balanced_over <- ovun.sample(sent_message ~ sessiontime + sessionstart_hour + day + dim_device_app_combo, data = train, method = "over",N = 12954)$data

lr <- glm(sent_message  ~ sessiontime + sessionstart_hour + day + dim_device_app_combo    ,data =data_balanced_over , family= 'binomial' )

summary(lr)

table(data_balanced_over$sent_message)


predict <- predict(lr, type = 'response')


predict[0:100]

summary(predict)

data_balanced_over$sent_message_prob <- predict

table(data_balanced_over$sent_message, predict > 0.5)

roc.curve(data_balanced_over$sent_message, predict)

pred_test <- predict(lr, newdata = test , type = "response")
 
summary(pred_test)


table(test$sent_message, pred_test > 0.8)

roc.curve(test$sent_message, pred_test)


# we get better auc values here when using oversampling
