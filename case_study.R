#First, I will install and load the necessary R packages.
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(skimr)
library(lubridate)
library(janitor)

#After that, I will import the necessary CSV documents using the read.csv command.
#Although there are 18 CSV documents in the dataset, after analysis in spreadsheets, 
#I found that we need only three documents: dailyActivity_merged, sleepDay_merged, 
#and weightLogInfo_merged. Namely, these three tables contain all 
#the data found in the other tables.

setwd("C:/Users/marko/Desktop/Case Study & Portfolio/Data")
daily_activity <- read.csv("dailyActivity_merged.csv")
daily_sleep <- read.csv("sleepDay_merged.csv")
weight_log <- read.csv("weightLogInfo_merged.csv")

#Next, I will use the glimpse function which returns a summary of the data frame, 
#including the number of columns and rows. 

glimpse(daily_activity)
glimpse(daily_sleep)
glimpse(weight_log)

#The Head function will give us a brief insight into each of these tables.
head(daily_activity)
head(daily_sleep)
head(weight_log)

#Using these functions, I determined the number of columns and rows and the data 
#type for each column. The only problem is that the data type for date columns is 
#a character, which I have to change to the DateTime data type.
daily_activity$Rec_Date <- as.Date(daily_activity$ActivityDate,"%m/%d/%y")
head(daily_activity)
daily_activity$month <- format(daily_activity$Rec_Date,"%B")
daily_activity$day_of_week <- format(daily_activity$Rec_Date,"%A")

#The next step is to see how many unique IDs we have, that is, 
#how many respondents participated in the survey.
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(weight_log$Id)

#I found that there are 33 unique IDs in the daily_activity table, in the daily-sleep
#24, and in weight_log only 8, which automatically calls into question the reliability of the adoption
#conclusions on such a small sample.

#The following is to determine if there are duplicates in our tables and if there are, to remove them.
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(weight_log))
#There are three duplicates in the daily_sleep table that we will remove using distinct function.
daily_sleep <- daily_sleep %>%
  distinct()

###4.ANALYZE and SHARE 
#I will start by summarizing the data.
daily_activity %>%  
  select(TotalSteps,TotalDistance,SedentaryMinutes,VeryActiveMinutes, Calories) %>% 
  summary()

weight_log %>%  
  select(WeightKg,BMI) %>% 
  summary()


(sum(daily_sleep$TotalMinutesAsleep)/sum(daily_sleep$TotalSleepRecords))/60




###PIE CHART
daily_average <- daily_activity %>%
  group_by(Id) %>%
  summarise (average_daily_steps = mean(TotalSteps))

head(daily_average)

user_type <- daily_average %>%
  mutate(user_type = case_when(
    average_daily_steps < 5000 ~ "low active",
    average_daily_steps >= 5000 & average_daily_steps < 7500 ~ "somewhat active",
    average_daily_steps >= 7500 & average_daily_steps <10000 ~ "active",
    average_daily_steps >= 10000 ~ "very active"
  ))

head(user_type)

user_type_percent <- user_type %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type , levels = c("very active", "active", "somewhat active", "low active"))

head(user_type_percent)

user_type_percent %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#85e085","#e6e600", "#ffd480", "#ff8080")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="User Activity",
       subtitle = "Categories according to the number of steps taken per day")+
  theme(plot.subtitle = element_text(hjust = 0.55))

###### BARCHART

daily_activity$day_of_week <- 
  ordered(daily_activity$day_of_week,levels=c("Ponedjeljak","Utorak","Srijeda","Četvrtak","Petak","Subota","Nedjelja"))

ggplot(data=daily_activity) + 
  geom_bar(mapping = aes(x=day_of_week),fill="blue") +
  labs(x="Day of week",y="Count",title="No. of times users used tracker across week")


head(daily_activity)

###SCATTERPLOT

ggplot(data=daily_activity,aes(x = VeryActiveMinutes, y = Calories, color = Calories)) + 
  geom_point() + 
  geom_smooth(method = "loess",color="orange") +
  labs(x="Very Active Minutes",y="Calories",title = "Very Active Minutes vs Calories Burned")


######
daily_activity %>%
  ggplot() +
  aes(x = TotalSteps, y = Calories) +
  geom_point(shape = "circle", size = 0.5, colour = "#ffa600") +
  geom_smooth(span = 0.75) +
  labs(
    title = "Relation between Total Steps vs Calories"
  )


########
ggplot(data=daily_sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point() + 
  stat_smooth(method = lm) +
  labs(x="Total Minutes a sleep", y="Total Time in Bed", title = "Sleep Time vs Time in Bed")

#####
ggplot(data=weight_log) + 
  geom_point(mapping=aes(x=Id, y=BMI, size = BMI))

