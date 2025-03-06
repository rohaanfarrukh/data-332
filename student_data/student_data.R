library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(scales)


rm(list = ls())

setwd("C:/Users/Rooooohan/Documents/rprojects/student_data")

course <- read_excel('Course.xlsx', .name_repair = 'universal')
registration <- read_excel('Registration.xlsx', .name_repair = 'universal')
student <- read_excel('Student.xlsx', .name_repair = 'universal')



df <- left_join(student, registration, by = c('Student.ID'))

df <- left_join(df, course, by = c('Instance.ID'))

df_major_pivot <- df %>%
  group_by(Title)%>%
  summarize(count = n())

ggplot(df_major_pivot, aes(x =Title, y= count, fill = Title))+
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

df <- df %>%
  mutate(birth_year = as.numeric(format(Birth.Date, "%Y")))

ggplot(df, aes(x = birth_year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  geom_text(stat = "bin", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Birth Years", x = "Birth Year", y = "Count") +
  theme_minimal()

df <- df %>%
  mutate(PaymentPlanCategory = ifelse(Payment.Plan == TRUE, "Installments", "Full Payment"))

cost_summary <- df %>%
  group_by(Title, PaymentPlanCategory) %>%
  summarize(Total_Cost = sum(Cost, na.rm = TRUE))

ggplot(cost_summary, aes(x = Title, y = Total_Cost, fill = PaymentPlanCategory)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(title = "Total Cost per Major, Segmented by Payment Plan",
       x = "Major",
       y = "Total Cost",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

t_balance <- df %>%
  group_by(Title, PaymentPlanCategory)%>%
  summarise(Total_Balance_due = sum(Balance.Due, na.rm = TRUE))%>%
  ungroup()

ggplot(t_balance, aes(x = reorder(Title, -Total_Balance_due), y = Total_Balance_due, fill = PaymentPlanCategory)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(title = "Total Balance Due by Major and Payment Plan",
       x = "Major",
       y = "Total Balance Due",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
