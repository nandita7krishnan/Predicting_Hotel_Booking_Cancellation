# Title:Hotel booking demand
# Group 2
# Member:
# Source: https://www.sciencedirect.com/science/article/pii/S2352340918315191

# ----------------Install Required Packages ----------------

install.packages("caTools")
install.packages("ROCR")
install.packages('caret')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('readxl')


# ----------------Part 1 : Data ----------------

## --1. import data
library(readxl)
hotel_booking_data <- read.csv("hotel_bookings.csv",stringsAsFactors = TRUE) # wanna see the data category

## --2. basic information of dataset
dim(hotel_booking_data) # 119390 rows, 32 cols
head(hotel_booking_data) 
summary(hotel_booking_data)

## -- 3. data type

hotel_booking_data$is_canceled <- factor(hotel_booking_data$is_canceled)
hotel_booking_data$agent <- as.character(hotel_booking_data$agent) #ID of the travel agency that made the booking
hotel_booking_data$company <- as.character(hotel_booking_data$company) #ID of the company/entity that made the booking or responsible for paying the booking
hotel_booking_data$is_repeated_guest <- factor(hotel_booking_data$is_repeated_guest)
hotel_booking_data$reservation_status_date <- as.Date(hotel_booking_data$reservation_status_date)
hotel_booking_data$arrival_date_year <- factor(hotel_booking_data$arrival_date_year)

## --4. find the missing value
sum(is.na(hotel_booking_data))
hotel_booking <- na.omit(hotel_booking_data)
sum(is.na(hotel_booking))
summary(hotel_booking)


## -- data type
str(hotel_booking) 

# ----------------Part 2 : Basic stats & Data Exploration & Data Visualization----------------

# --- * Target variable: is_canceled * ---
# *Description: Value indicating if the booking was canceled (1) or not (0)

# -- single variable plot
library(scales)
library(ggplot2)
cancel_plot <- ggplot(data = hotel_booking,mapping = aes( x = is_canceled)) 
cancel_plot +
  geom_bar(stat="count",width=0.5,fill='coral2')+
  geom_text(stat='count',
            aes(label=(scales::percent((..count..)/sum(..count..)))), 
            vjust=1.6, color="white", size=3.5)

# --- hotel ---
# *Description:  One of the hotels is a resort hotel and the other is a city hotel

## --Single variable Plot
hotel_plot <- ggplot(data = hotel_booking,mapping = aes( x = hotel)) 
hotel_plot +
  geom_bar(stat="count",width=0.5,fill='coral2')+
  geom_text(stat='count',
            aes(label=(scales::percent((..count..)/sum(..count..)))), 
            vjust=1.6, color="white", size=3.5)
# with target variable plot
hotel_canceled_plot <- ggplot(data= hotel_booking, mapping = aes(x = is_canceled,group = hotel))
hotel_canceled_plot +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of different hotels") + 
  scale_fill_discrete(labels=c("not canceled","canceled"))+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5,size=3) +
  facet_grid(~hotel) +
  scale_y_continuous(labels = scales::percent)


library(dplyr)
## *****show the percentage of hotel group by is_canceled table.[cw][**]
cancel_hotel_tb <-hotel_booking %>%
  group_by(is_canceled,hotel)%>%
  summarise(n = n()) %>%
  mutate(rel.freq =  scales::percent(n/sum(n), accuracy = 0.1))
cancel_hotel_tb


# Checking variable importance
plot(hotel_booking$is_canceled~hotel_booking$hotel)
chisq.test(table(hotel_booking$is_canceled, hotel_booking$hotel), correct = FALSE) 

# ---lead_time--- [*]
# *Description: Number of days that elapsed between the entering date of the booking into the PMS and the arrival date

boxplot(hotel_booking$lead_time)
hist(hotel_booking$lead_time) #consider using log()
hist(log(hotel_booking$lead_time))



# --- arrival_date_year --- [*]
# *Description: Year of arrival date

## Single variable
arrival_date_year_plot <- ggplot(data = hotel_booking,mapping = aes( x = arrival_date_year)) 
arrival_date_year_plot +
  geom_bar(position = "dodge")

## target variable
arryear_canceled_plot <- ggplot(data= hotel_booking, mapping = aes(x = arrival_date_year,fill = is_canceled))
arryear_canceled_plot +
  geom_bar(position = "dodge", stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of different year") + 
  scale_fill_discrete(labels=c("not canceled","canceled"))

# Checking variable importance
plot(hotel_booking$is_canceled~hotel_booking$arrival_date_year)
chisq.test(table(hotel_booking$is_canceled, hotel_booking$arrival_date_year), correct = FALSE)

# --- arrival_date_month ---[*]
# *Description: Month of arrival date with 12 categories: ??January?? to ??December??

## Single variable
arrival_date_month_plot <- ggplot(data = hotel_booking,mapping = aes( x = arrival_date_month)) 
arrival_date_month_plot +
  geom_bar(position = "dodge")

## target variable [*]
arrmon_canceled_plot <- ggplot(data= hotel_booking, mapping = aes(x = arrival_date_month,group = is_canceled,fill= is_canceled))
arrmon_canceled_plot +
  geom_bar(position = "dodge", stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of different year") + 
  scale_fill_discrete(labels=c("not canceled","canceled"))+
  facet_grid(~is_canceled)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Checking variable importance
plot(hotel_booking$is_canceled~hotel_booking$arrival_date_month,las=2)
chisq.test(table(hotel_booking$is_canceled, hotel_booking$arrival_date_month), correct = FALSE)


# --- arrival_date_week_number --- [*]
## *Description: Week number of the arrival

# Finding the outlier
arrival_date_week_number_plot_h <- ggplot(data = hotel_booking,mapping = aes( x = arrival_date_week_number)) 
arrival_date_week_number_plot_h +
  geom_histogram(bins = 30L)
arrival_date_week_number_plot_b <- ggplot(data = hotel_booking,mapping = aes( x = "  ", y = arrival_date_week_number))
arrival_date_week_number_plot_b +
  geom_boxplot()

# Checking variable importance
plot(hotel_booking$is_canceled~hotel_booking$arrival_date_week_number)
chisq.test(table(hotel_booking$is_canceled, hotel_booking$arrival_date_week_number), correct = FALSE)

# --- arrival_date_day_of_month ---
## *Description:   Day of the month of the arrival_date

### Outlier
arrival_date_day_of_month_plot_h <- ggplot(data = hotel_booking,mapping = aes( x = arrival_date_day_of_month)) 
arrival_date_day_of_month_plot_h +
  geom_histogram(bins = 30L) 
#[*]15 count more than other days, consider the system problems,so not use this in the prediction.
arrival_date_day_of_month_plot_b <- ggplot(data = hotel_booking,mapping = aes( x = " ", y = arrival_date_day_of_month))
arrival_date_day_of_month_plot_b +
  geom_boxplot()

# Checking variable importance
plot(hotel_booking$is_canceled~hotel_booking$arrival_date_day_of_month)
chisq.test(table(hotel_booking$is_canceled, hotel_booking$arrival_date_day_of_month), correct = FALSE)

# --- stays_in_weekend_nights ---
## *Description:Number of weekend nights (Saturday or Sunday) the guest stayed or booked to stay at the hotel
## Outlier
boxplot(hotel_booking$stays_in_weekend_nights)
hist(hotel_booking$stays_in_weekend_nights)
outlier.stays_in_weekend_nights <- boxplot.stats(hotel_booking$stays_in_weekend_nights)$out
length(outlier.stays_in_weekend_nights)
hotel_new <- hotel_booking %>%
  filter(!(hotel_booking$stays_in_weekend_nights %in% outlier.stays_in_weekend_nights))
dim(hotel_new)

# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$stays_in_weekend_nights)
chisq.test(table(hotel_new$is_canceled, hotel_new$stays_in_weekend_nights), correct = FALSE) #[*keep*]

# --- stays_in_week_nights ---
## *Description:Number of week nights (Monday to Friday) the guest stayed or booked to stay at the hotel
## Outlier
boxplot(hotel_new$stays_in_week_nights)
outlier.stays_in_week_nights <- boxplot.stats(hotel_new$stays_in_week_nights)$out
length(outlier.stays_in_week_nights)
hotel_new <- hotel_new %>%
  filter(!(hotel_new$stays_in_week_nights %in% outlier.stays_in_week_nights))
dim(hotel_new)
# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$stays_in_week_nights)
chisq.test(table(hotel_new$is_canceled, hotel_new$stays_in_week_nights), correct = FALSE) #[keep]

# --- adults & children &babies ---
## *Description:Number of adults, children and babies
# remove [*] many outliers and not practical value for the model.
adults_plot <- ggplot(data = hotel_new,mapping = aes( x = adults)) 
adults_plot +
  geom_histogram(bins = 30L)
adults_plot_b <- ggplot(data = hotel_new,mapping = aes( x = " ", y = adults))
adults_plot_b +
  geom_boxplot()
hotel_new <- hotel_new %>%
  filter(!(hotel_new$adults >= 20))
dim(hotel_new)
# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$adults)

children_plot_h <- ggplot(data = hotel_new,mapping = aes( x = children)) 
children_plot_h +
  geom_histogram(bins = 30L)
children_plot_b <- ggplot(data = hotel_new,mapping = aes( x = " ", y = children))
children_plot_b +
  geom_boxplot() 

# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$children)
chisq.test(table(hotel_new$is_canceled, hotel_new$children), correct = FALSE)

babies_plot_h <- ggplot(data = hotel_new,mapping = aes( x = babies)) 
babies_plot_h +
  geom_histogram(bins = 30L)
babies_plot_b <- ggplot(data = hotel_new,mapping = aes( x = " ", y = babies))
babies_plot_b +
  geom_boxplot() 
hotel_new <- hotel_new %>%
  filter(!(hotel_new$babies >= 10))
dim(hotel_new)

# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$babies)


# --- meal --- 
# *Description:Type of meal booked. Categories are presented in standard hospitality meal packages
#   Undefined/SC ?C no meal package;
#   BB ?C Bed & Breakfast;
#   HB ?C Half board (breakfast and one other meal ?C usually dinner);
#   FB ?C Full board (breakfast, lunch and dinner)

#  Undefined is the same as SC,merge the data
levels(hotel_new$meal)
meal_c <- list(
  SC = c("Undefined","SC"),
  HB  = c("HB"),
  FB =  c("FB"),
  BB = c("BB")
)
for (i in 1:length(meal_c)) 
  levels(hotel_new$meal)[levels(hotel_new$meal)%in%meal_c[[i]]] <- names(meal_c)[i]

## single variables
levels(hotel_new$meal)
meal_plot_ba <- ggplot(data = hotel_new,mapping = aes( x = meal)) 
meal_plot_ba +
  geom_bar(position = "dodge")

## included the target variables
Meal_canceled_plot <- ggplot(data= hotel_new, mapping = aes(x = meal,group = is_canceled))
Meal_canceled_plot +
  geom_bar(position = "dodge", stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of different meal package") + 
  scale_fill_discrete(labels=c("not canceled","canceled"))+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -33.5,size=2)+
  facet_grid(~is_canceled) 

# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$meal)
chisq.test(table(hotel_new$is_canceled, hotel_new$meal), correct = FALSE)

# --- Country --- [*remove*]
country_plot_t <- table(hotel_new$country, hotel_new$is_canceled)
country_plot_t
#Reason: too many value so just see the table, consider remove, we don't need this


# --- market_segment ---
## *Description: Market segment designation. In categories, the term ??TA?? means ??Travel Agents?? and ??TO?? means ??Tour Operators??
market_plot_t <- table(hotel_new$market_segment, hotel_new$is_canceled)
market_plot_t
# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$market_segment)

market_canceled_plot <- ggplot(data= hotel_new, mapping = aes(x = market_segment,group = is_canceled))
market_canceled_plot +
  geom_bar(aes(y = ..prop.., fill = factor(..group..)), stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of different market") +
  geom_text(aes( label = scales::percent(..prop..,accuracy = 0.2),
                 y= ..prop..), stat= "count", vjust = -.5,size=3) + 
  scale_fill_discrete(labels=c("not canceled","canceled"))+
  facet_grid(~is_canceled) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# --- distribution_channel ---  
## *Description: Booking distribution channel. 
dischan_plot_t <- table(hotel_new$distribution_channel, hotel_new$is_canceled)
dischan_plot_t
hotel_new <- hotel_new %>%
  filter(hotel_new$distribution_channel != 'Undefined')  #remove variables has undefined
dim(hotel_new)

# --- is_repeated_guest --- [*]            
## *Description:Value indicating if the booking name was from a repeated guest (1) or not (0)
## Single variable Plot
repeated_guest_plot <- ggplot(data = hotel_new,mapping = aes( x = is_repeated_guest)) 
repeated_guest_plot +
  geom_bar(stat="count",width=0.5,fill='coral2')+
  geom_text(stat='count',
            aes(label=(scales::percent((..count..)/sum(..count..)))), 
            vjust=1.6, color="white", size=3.5)
# with target variable plot
repeated_canceled_plot <- ggplot(data= hotel_booking, mapping = aes(x = is_repeated_guest,group = is_canceled))
repeated_canceled_plot +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of repeated guest") + 
  scale_fill_discrete(labels=c("not repeated guest ","a repeated guest"))+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5,size=3) +
  facet_grid(~is_canceled) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# --- previous_cancellations ---   [*]     
## *Description: Number of previous bookings that were cancelled by the customer prior to the current booking
## Outlier
summary(hotel_new$previous_cancellations)
pre_cancel_plot_h <- ggplot(data = hotel_new,mapping = aes( x = previous_cancellations)) 
pre_cancel_plot_h +
  geom_histogram(bins = 30L)
hist(log(hotel_new$previous_cancellations))
pre_cancel_plot_b <- ggplot(data = hotel_new,mapping = aes( x = " ", y = previous_cancellations))
pre_cancel_plot_b +
  geom_boxplot()
plot(hotel_new$is_canceled~hotel_new$previous_cancellations)
#[*] large number of zero ,remove.


# --- previous_bookings_not_canceled --- [*]
## *Description: Number of previous bookings not cancelled by the customer prior to the current booking

summary(hotel_new$previous_bookings_not_canceled)
pre_not_cancel_plot_b <- ggplot(data = hotel_new,mapping = aes( x = " ", y = previous_bookings_not_canceled))
pre_not_cancel_plot_b +
  geom_boxplot()
pre_not_cancel_plot_h <- ggplot(data = hotel_new,mapping = aes( x = previous_bookings_not_canceled)) 
pre_not_cancel_plot_h +
  geom_histogram(bins = 30L)
hist(log(hotel_new$previous_bookings_not_canceled))
hist(log(hotel_new$previous_bookings_not_canceled+1))
## consider log this one in the model.
## however it do has 0 in the database which will lead to "-inf", 
## so when we try using log(x+1) still not good perform, consider just use original data or don't use.
plot(hotel_new$is_canceled~hotel_new$previous_bookings_not_canceled)
chisq.test(table(hotel_new$is_canceled, hotel_new$meal), correct = FALSE)#[keep]

# --- reserved_room_type & assigned_room_type ---
## *Description: reserved_room_type :Code of room type reserved. Code is presented instead of designation for anonymity reasons.
## *Description: assigned_room_type :Code for the type of room assigned to the booking. Sometimes the assigned room type differs from the reserved room type due
hoteltype_canceled <- ggplot(data = hotel_new,mapping = aes(x = hotel, fill = is_canceled))
hoteltype_canceled + 
  geom_bar(position = "dodge") + 
  labs(title = "The count of canceled in different type of hotel") + #set title
  scale_fill_discrete(labels=c("not canceled","canceled")) #set label as 0 is not canceled, 1 is canceled
hotel_room <- ggplot(data = hotel_new,mapping = aes(x = assigned_room_type, fill = hotel))                                                     
hotel_room +
  geom_bar(position = "dodge") +
  labs(title = "room type in different hotel") # notice maybe L and P is null

#check the reserved room type is the same as the assigned room type
table(hotel_new$reserved_room_type)
table(hotel_new$assigned_room_type) #they have different levels
char_reserved <- as.character(hotel_new$reserved_room_type)
char_assigned <- as.character(hotel_new$assigned_room_type)
hotel_new$check_room_type <- ifelse(char_reserved == char_assigned, "Same","Different")
hotel_new$check_room_type <- factor(hotel_new$check_room_type)
table(hotel_new$check_room_type)

# check the different room type and canceled
roomtype_canceled <- ggplot(data = hotel_new,mapping = aes(x = check_room_type, fill = is_canceled))
roomtype_canceled + 
  geom_bar(position="dodge") + 
  labs(title = "The count of canceled in unmatch room type") + 
  scale_fill_discrete(labels=c("not canceled","canceled"))

# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$reserved_room_type)
plot(hotel_new$is_canceled~hotel_new$assigned_room_type)
plot(hotel_new$is_canceled~hotel_new$check_room_type)


# --- booking_changes ---   [*]         
## *Description:Number of changes/amendments made to the booking from the moment the booking was entered on the PMS

summary(hotel_new$booking_changes)
book_changes_plot_b <- ggplot(data = hotel_new,mapping = aes( x = " ", y = booking_changes))
book_changes_plot_b +
  geom_boxplot()
book_changes_plot_h <- ggplot(data = hotel_new,mapping = aes( x = booking_changes)) 
book_changes_plot_h +
  geom_histogram(bins = 30L)
hist(log(hotel_new$booking_changes))

hotel_new <- hotel_new %>%
  filter(!(hotel_new$booking_changes >= 15))
dim(hotel_new)



# --- deposit_type ---    [*]   
## *Description:Indication on if the customer made a deposit to guarantee the booking. This variable can assume three categories: No
deposit_plot_t <- table(hotel_new$deposit_type, hotel_new$is_canceled)
deposit_plot_t
# Checking variable importance
hotel_canceled_plot <- ggplot(data= hotel_booking, mapping = aes(x = is_canceled,group = deposit_type))
hotel_canceled_plot +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of different deposit_type") + 
  scale_fill_discrete(labels=c("not canceled","canceled"))+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5,size=3) +
  facet_grid(~deposit_type) +
  scale_y_continuous(labels = scales::percent)
## interesting part is for non refund, it has higher cancel rate.
## If the payment was equal or exceeded the total cost of stay, the value is set as ??Non Refund??.


# --- agent & company --- [*remove*]
## *Description:agent :ID of the travel agency that made the booking
## *Description:compnay:ID of the company/entity that made the booking or responsible for paying the booking.
# Contain large number of null value, consider remove

# --- days_in_waiting_list ---[*]
## *Description:Number of days the booking was in the waiting list before it was confirmed to the customer
summary(hotel_new$days_in_waiting_list)
waiting_list_plot_b <- ggplot(data = hotel_new,mapping = aes( x = " ", y = days_in_waiting_list))
pre_not_cancel_plot_b +
  geom_boxplot()
waiting_plot_h <- ggplot(data = hotel_new,mapping = aes( x = days_in_waiting_list)) 
waiting_plot_h +
  geom_histogram(bins = 30L)
hist(log(hotel_new$days_in_waiting_list))
hist(log(hotel_new$days_in_waiting_list+1))
## consider log in the model?? but it do has 0 in the database which will lead to -inf, 
## so when we try using log(x+1) still not good perform, consider just use original data or don't use.

# --- customer_type --- [*]
## *Description:Type of booking, assuming one of four categories: Contract - when the booking has an allotment or other type of contract associated to it; Group – when the booking is associated to a group; Transient – when the booking is not part of a group or contract, and is not associated to other transient booking; Transient-party – when the booking is transient, but is associated to at least other transient booking
customer_plot_t <- table(hotel_new$customer_type, hotel_new$is_canceled)
customer_plot_t
customer_canceled_plot <- ggplot(data= hotel_new, mapping = aes(x = customer_type,group = is_canceled))
customer_canceled_plot +
  geom_bar(aes(y = ..prop.., fill = factor(..group..)), stat="count",width = 0.5) + 
  labs(title = "Cancellation rate of different customer") +
  geom_text(aes( label = scales::percent(..prop..,accuracy = 0.2),
                 y= ..prop..), stat= "count", vjust = -.5,size=3) + 
  scale_fill_discrete(labels=c("not canceled","canceled"))+
  facet_grid(~is_canceled) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# --- adr ---                          
## *Description: Average Daily Rate as defined by dividing the sum of all lodging transactions by the total number of staying nights
boxplot(hotel_new$adr)
hist(hotel_new$adr)
outlier.adr <- boxplot.stats(hotel_new$adr)$out
length(outlier.adr)
hotel_new <- hotel_new %>%
  filter(!(hotel_new$adr %in% outlier.adr))
dim(hotel_new)

# --- required_car_parking_spaces ---  
## *Description: Number of car parking spaces required by the customer
car_park_plot_h <- ggplot(data = hotel_new,mapping = aes( x = required_car_parking_spaces)) 
car_park_plot_h +
  geom_histogram(bins = 30L)
boxplot(hotel_new$required_car_parking_spaces)
outlier.required_car_parking_spaces <- boxplot.stats(hotel_new$required_car_parking_spaces)$out
length(outlier.required_car_parking_spaces)
hotel_new <- hotel_new %>%
  filter(!(hotel_new$required_car_parking_spaces >= 8))
dim(hotel_new)

# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$total_of_special_requests)


# --- total_of_special_requests ---    
## *Description: Number of special requests made by the customer (e.g. twin bed or high floor)
# Outlier
boxplot(hotel_new$total_of_special_requests)
hist(hotel_new$total_of_special_requests) #0-5 is in the reasonable ranges.
dim(hotel_new)
summary(hotel_new$total_of_special_requests)

# Checking variable importance
plot(hotel_new$is_canceled~hotel_new$total_of_special_requests)
chisq.test(table(hotel_new$is_canceled, hotel_new$total_of_special_requests), correct = FALSE)

# --- reservation_status --- [*remove*]            
## *Description: Reservation last status, assuming one of three categories: Canceled – booking was canceled by the customer; Check-Out – customer has checked in but already departed; No-Show – customer did not check-in and did inform the hotel of the reason why
reser_status_plot_t <- table(hotel_new$reservation_status, hotel_new$is_canceled)
reser_status_plot_t
#remove, no-show and canceled are all canceled, and check-out is no canceled.

# --- reservation_status_date ---  [*remove*]    
## *Description: Date at which the last status was set. This variable can be used in conjunction with the ReservationStatus to understand when was the booking canceled or when did the customer checked-out of the hotel
# not using the date data in our model.


# --- final variable selection ---
# Removing agent & company & country &reservation_status_date
pred_cancel <- hotel_new %>% select(-agent & -company & -country & -reservation_status_date)
dim(pred_cancel) #112276 rows     29 cols
summary(pred_cancel)
pred_model <- pred_cancel 
pred_model$logleadtime <- log(pred_model$lead_time+1) # contain 0 so add a constant.
pred_model <- pred_model %>% select(-adults & -babies & -children  & -arrival_date_day_of_month &
                                      -reserved_room_type &-assigned_room_type &-reservation_status)
dim(pred_model) # 112276 rows, 21 cols.
head(pred_model)


# ----------------Part 3 :    Modeling  --------------------

library(caTools)
set.seed(123)
cancel.split <- sample.split(pred_model$is_canceled,SplitRatio = 0.6)
trainset <- pred_model[cancel.split,]
validationset <- pred_model[-cancel.split,]

### build logistic.mod1:
logistic.mod1 <- glm(is_canceled ~ hotel + logleadtime + 
                       stays_in_week_nights + stays_in_weekend_nights + 
                     arrival_date_month +arrival_date_year+meal + market_segment + 
                       distribution_channel + is_repeated_guest + booking_changes +
                       deposit_type + days_in_waiting_list + customer_type +adr + 
                       total_of_special_requests + check_room_type, 
                     family=binomial("logit"), data=trainset)
logistic.mod1
summary(logistic.mod1)
## Logistic.mod1: McFadden R square 1-(58440/88832)= 0.3421 =34.21%
## AIC:58520

### Build logistic.mod2:
#Remove the hotel and stays_in_weekend_nights and is_repeated_guest and days_in waiting_list 
logistic.mod2 <- glm(is_canceled ~ logleadtime  + 
                       arrival_date_month +arrival_date_year + meal + market_segment + 
                       distribution_channel + booking_changes + 
                       deposit_type  + customer_type + adr + 
                       total_of_special_requests + check_room_type, 
                     family=binomial("logit"), data=trainset)
logistic.mod2
summary(logistic.mod2)
# Logistic.mod2: McFadden R square = 1-(58466/88832)=0.3418  = 34.18%
# Logistic.mod2: AIC:  58538
step(logistic.mod2,direction = "backward")## not need to change


### -- Performance of logistic.mod1:
# Make predication
logistic.mod1.prob <- predict(logistic.mod1, newdata = validationset, type = "response")

# ROC Curve of logistic.mod1
library(ROCR)
ROCpred1 <- prediction(logistic.mod1.prob, validationset$is_canceled)
ROCperf1 <- performance(ROCpred1,"tpr","fpr")
plot(ROCperf1, colorize=TRUE, print.cutoffs.at = seq(0,1,by=0.1), text.adj = c(-0.5,0.5))
#take time to run

# Confusion Matrix of Validation set of logistic.mod1
library(caret)
if_logistic.mod1.prob <- ifelse(logistic.mod1.prob > 0.5, 1, 0)
logistic.mod1.pred <- factor(if_logistic.mod1.prob)
caret::confusionMatrix(logistic.mod1.pred, validationset$is_canceled, positive="1")
# Accuracy of logistic.mod1: 0.8034  

### -- Performance of logistic.mod2:
# Make predication
logistic.mod2.prob <- predict(logistic.mod2, newdata = validationset,type = "response")


# ROC Curve of logistic.mod2
library(ROCR)
ROCpred2 <- prediction(logistic.mod2.prob, validationset$is_canceled)
ROCperf2 <- performance(ROCpred2,"tpr","fpr")
plot(ROCperf2,colorize=TRUE, print.cutoffs.at = seq(0,1,by=0.1), text.adj=c(-0.5,0.5))

# Confusion Matrix of Validation set of logistic.mod2
library(caret)
if_logistic.mod2.prob <- ifelse(logistic.mod2.prob > 0.5, 1, 0)
logistic.mod2.pred <- factor(if_logistic.mod2.prob)
caret::confusionMatrix(logistic.mod2.pred, validationset$is_canceled, positive="1")
# Accuracy of logistic.mod2: 0.8033

## For the model accuracy, even Accuracy of model 2 is slightly less than 1, but it is more simpler than model 1. 

## variable importance
overall_important <- varImp(logistic.mod2,scale=FALSE)
overall_important

ggplot2::ggplot(overall_important, aes(x=reorder(rownames(overall_important),Overall), y=Overall)) +
  geom_point( color="coral2", size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(overall_important), xend=rownames(overall_important), y=0, yend=Overall), 
                color='coral2') +
  xlab('Variable')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()

# ----------------Part 4 :    Reference  --------------------
## ggplot:
### Line 183 https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/
### https://github.com/rstudio/cheatsheets/blob/main/data-visualization-2.1.pdf
### #Line 233 https://www.statology.org/ggplot-legend-labels
## dplyr:
### https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
## confusion matrix
### https://www.statology.org/confusion-matrix-in-r/
## stepwise
### http://www.sthda.com/english/articles/36-classification-methods-essentials/150-stepwise-logistic-regression-essentials-in-r/