library(readr)
hotel_bookings <- read_csv("hotel_bookings.csv",
                           col_types = cols(reservation_status_date = col_date(format = "%Y-%m-%d")))
summary(hotel_bookings)

str(hotel_bookings)
library(tidyverse)
library(lubridate)
# Clean data set
myHotel_bookings<-hotel_bookings%>%
  mutate(arrival_date=paste0(arrival_date_day_of_month,
                             arrival_date_month,
                             arrival_date_year)%>%
           dmy(),
         arrival_status_diff=arrival_date-reservation_status_date,
         reservation_status_date_month=month(reservation_status_date),
         reservation_status_date_year=year(reservation_status_date),
         arrival_date_month_num=month(arrival_date))%>% # clean dates
  mutate(hotel=as.factor(hotel),
         meal = as.factor(meal),
         country = as.factor(country),
         market_segment = as.factor(market_segment),
         distribution_channel = as.factor(distribution_channel),
         reserved_room_type = as.factor(reserved_room_type),
         assigned_room_type = as.factor(assigned_room_type),
         deposit_type = as.factor(deposit_type),
         agent = as.factor(agent),
         company = as.factor(company),
         reservation_status = as.factor(reservation_status),
         customer_type =as.factor(customer_type))%>% # convert character variables to factors
  mutate(reservation_date = arrival_date -ddays(lead_time),
         is_LMC = as.numeric(is_canceled==1 & arrival_status_diff<=30)) # engineer new variables
# Find LMCs by month
myHotel_LMCs<-myHotel_bookings%>%
  filter(is_LMC==1)%>% # select only LMCs
  group_by(hotel, reservation_status_date_year,reservation_status_date_month)%>%
  summarise(cancelations=sum(is_LMC))%>% # get total LMCs by month, year, and hotel
  arrange(hotel, reservation_status_date_year,reservation_status_date_month) # sort the data

print(myHotel_LMCs)

myHotel_bookings_completed_Jul17 <- myHotel_bookings%>%
  filter(reservation_status_date<ymd("20170701") | arrival_date<ymd("20170701"))
myHotel_bookings_outstanding_Jul17 <- myHotel_bookings%>%
  filter(reservation_status_date>=ymd("20170701") &
           arrival_date>=ymd("20170701") &
           reservation_date<ymd("20170701"))%>%
  select(-is_canceled,
         -reservation_status,
         -reservation_status_date,
         -reservation_status_date_month,
         -reservation_status_date_year,
         -arrival_status_diff,
         -assigned_room_type)
myHotel_bookings_completed_Aug17 <- myHotel_bookings%>%
  filter(reservation_status_date<ymd("20170801") | arrival_date<ymd("20170801"))
myHotel_bookings_outstanding_Aug17 <- myHotel_bookings%>%
  filter(reservation_status_date>=ymd("20170801") &
           arrival_date>=ymd("20170801") &
           reservation_date<ymd("20170801"))%>%
  select(-is_canceled,
         -is_LMC,
         -reservation_status,
         -reservation_status_date,
         -reservation_status_date_month,
         -reservation_status_date_year,
         -arrival_status_diff,
         -assigned_room_type)

print(myHotel_LMCs)

modelTester <- function(for1707, for1708){
  return(sqrt((((for1707-193)^2 + (for1708-135)^2)) / 2))
}
