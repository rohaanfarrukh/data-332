library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)

rm(list = ls())

setwd("C:/Users/Rooooohan/Documents/rprojects/trucking")

df_truck_0001<- read_excel('truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369<- read_excel('truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226<- read_excel('truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442<- read_excel('truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478<- read_excel('truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539<- read_excel('truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769<- read_excel('truck data 1769.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_pay<- read_excel('Driver Pay Sheet.xlsx', .name_repair = 'universal')



df<-rbind(df_truck_0001,df_truck_0369,df_truck_1226,df_truck_1442,df_truck_1478,df_truck_1539,df_truck_1769)

df_startin_pivot <- df %>% 
  group_by(Truck.ID) %>%
  summarize(count = n())

df <- left_join(df,df_pay,by= c('Truck.ID'))


df = subset(df, select = -c(...10))

date1 <- min(df$Date)
date2 <- max(df$Date)

number_days_on_road <- date2 - date1

days <- as.numeric(difftime(max(df$Date),min(df$Date)))

print(number_days_on_road)

num_hrs_driving <- sum(df$Hours)
print(round(num_hrs_driving/days,3))

toll_expens <- df$Tolls
misc_expens <- df$Misc
df$other_expnes <- df$Tolls + df$Misc
print(sum(df$other_expnes))

df[c('warehouse', 'starting_city_state')]<- str_split_fixed(df$Starting.Location, ',',2)

df$starting_city_state <- gsub(',',"",df$starting_city_state)

df[c('col1','col2')] <- str_split_fixed(df$starting_city_state, ' ',2)

df[c('col1','col2','col3')] <- str_split_fixed(df$col2, ' ',3)

df_starting_pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean (Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE))

ggplot(df_starting_pivot, aes(x = starting_city_state, y= count))+
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))



df$total_fuel_expens <- df$Gallons * df$Price.per.Gallon
print(sum(df$total_fuel_expens))

df$total_expens <- df$total_fuel_expens + df$other_expnes
print(sum(df$total_expens))

total_gallons <- sum(df$Gallons)
print(total_gallons)
df$total_mile <-df$Odometer.Ending - df$Odometer.Beginning
total_miles <- sum(df$total_mile)
print(total_miles)

miles_per_gallon <- total_miles/total_gallons
print(miles_per_gallon)

total_cost_per_mile <- sum(df$total_expens)/total_miles
print(total_cost_per_mile)

df_pay_pivot <- df %>% 
  group_by(Truck.ID) %>%
  summarize(pay = sum(labor_per_mil * total_mile), .groups = "drop")

ggplot(df_pay_pivot, aes(x = Truck.ID, y= pay, fill = Truck.ID))+
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()
