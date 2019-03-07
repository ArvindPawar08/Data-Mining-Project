#Olympics Games Analysis

#------- Installing required Packages --------

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggmap")
install.packages("tseries")
install.packages("rpart")
install.packages("rpart.plot")

library(stats)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(forecast)
library(tseries)

#Read the data
athlete_events <- read_csv("~/Downloads/120-years-of-olympic-history-athletes-and-results/athlete_events.csv")
noc_regions <- read_csv("~/Downloads/120-years-of-olympic-history-athletes-and-results/noc_regions.csv")

#Updating the Attributes to remove NA values

athlete_events$Weight <- ifelse(is.na(athlete_events$Weight),
                                ave(athlete_events$Weight,FUN = function(x) mean(x,na.rm = TRUE)),
                                athlete_events$Weight)

athlete_events$Height <- ifelse(is.na(athlete_events$Height),
                                ave(athlete_events$Height,FUN = function(x) mean(x,na.rm = TRUE)),
                                athlete_events$Height)

athlete_events$Age <- ifelse(is.na(athlete_events$Age),
                             ave(athlete_events$Age,FUN = function(x) mean(x,na.rm = TRUE)),
                             athlete_events$Age)

#Dimensions of data
dim(athlete_events)

#Name of the columns
names(athlete_events)

#Checking for null values
is.null(athlete_events)

#Data Frame
athlete_events = as.data.frame(athlete_events)
str(athlete_events)

#Summary of data
summary(athlete_events)

#Data Exploration-(EDA) with plots

#Getting the count of Athletes, Nations and events

counts <- athlete_events  %>%
  group_by(Year, Season) %>%
  summarize(
    Athletes = length(unique(ID)),
    Nations = length(unique(NOC)),
    Events = length(unique(Event))
  )

counts

#Plotting the participation statistics

#Athletes
athletes_plot <- ggplot(counts, aes(x=Year, y=Athletes, group=Season, color=Season)) +
  geom_point(size=2) + ggtitle("Number of Athletes participated over the Years") +
  geom_line() 

athletes_plot

#Nations
nations_plot <- ggplot(counts, aes(x=Year, y=Nations, group=Season, color=Season)) +
  geom_point(size=2) + ggtitle("Number of Countries participated over the Years") +
  geom_line() 

nations_plot

#Events
events_plot <- ggplot(counts, aes(x=Year, y=Events, group=Season, color=Season)) +
  geom_point(size=2) + ggtitle("Number of Events occurred over the Years") +
  geom_line() 

events_plot

#Male and Female over the years 

mf_counts <- athlete_events %>% filter(Sport != "Art Competitions")
original <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(original)) {
  mf_counts$Year <- gsub(original[i], new[i], mf_counts$Year)
}
mf_counts$Year <- as.integer(mf_counts$Year)

counts_sex <- mf_counts %>% group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))
counts_sex$Year <- as.integer(counts_sex$Year)

gender_plot <- ggplot(counts_sex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  labs(title = "Number of Male and Female participation over the Years") +
  theme(plot.title = element_text(hjust = 0.5))

gender_plot

#Winning Trends

#Total number of medals for top 10 nations
medal_counts <- athlete_events %>%
  filter(Medal != "<NA>") %>%
  group_by(Team) %>%
  summarise(Total = length(Medal))%>%
  arrange(desc(Total)) %>%
  ungroup() %>%
  mutate(country = reorder(Team,Total)) %>%
  top_n(10) 
  
total_medals_plot <- ggplot(medal_counts, aes(x = country,y = Total)) +
  geom_bar(stat='identity',colour="white", fill = "lightgreen") +
  geom_text(aes(x = country, y = .1, label = paste0("(",round(Total,2),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(title = "Top 10 Nations - Total Number of Medals") +
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10)) +
  labs(x = 'Country', 
       y = 'Number of Medals'
  ) +
  coord_flip() + 
  theme_bw()

total_medals_plot

# Count number of medals awarded to females at Olympics
counts_female <- athlete_events %>% filter(Medal != "<NA>") %>% filter(Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

# Order NOC by total medal count
l_female <- counts_female %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(desc(Total)) %>%
  top_n(10) %>%
  select(NOC)

l_female

counts_female$NOC <- factor(counts_female$NOC, levels=l_female$NOC)

# Plot female medals
female_medal_plot <- ggplot(counts_female, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold4","gold1", "gray70")) +
  ggtitle("Medal counts for women at the Olympics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=6))

female_medal_plot

# Count number of medals awarded to males at Olympics
counts_male <- athlete_events %>% filter(Medal != "<NA>")  %>% filter(Sex=="M") %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

# Order NOC by total medal count
l_male <- counts_male %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(desc(Total)) %>%
  top_n(10) %>%
  select(NOC)

counts_male$NOC<- factor(counts_male$NOC, levels=l_male$NOC)

# Plot male medals
male_medal_plot <- ggplot(counts_male, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold4","gold1", "gray70")) +
  ggtitle("Medal counts for men at Olympics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=6))

male_medal_plot

#Height and Weight Characteristics of Medal Winners 








#Analysis for Basketball

sport_bb<- athlete_events %>% 
  filter(Sport == "Basketball")

# count number of medals awarded to each Team(with respect to country)
medal_counts <- sport_bb %>% filter(Medal != "<NA>")  %>% 
  group_by(Team, Medal) %>%
  summarize(Count=length(Medal)) 

medal_counts

# order Team by total medal count
arrange_levs <- medal_counts %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(Team) %>%
  top_n(10)

arrange_levs

medal_counts$Team <- factor(medal_counts$Team, levels=arrange_levs$Team)

bb_plot <- ggplot(medal_counts, aes(x=Team, y=Count, fill=Medal)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values=c("gold4","gold1","gray70")) +
  ggtitle("Historical medal counts from Basketball")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))

bb_plot

ggplot(athlete_events, aes(x = Medal, y = Height)) + geom_boxplot(fill="cyan") + facet_grid(.~Sex)
#Those who did not receive any medals had lower height in case of both genders. Also, Those males who won gold medals have higher median height.

ggplot(athlete_events, aes(x = Medal, y = Weight)) + geom_boxplot(fill="cyan") + facet_grid(.~Sex) 
#Similarly, Those who did not receive any medals had lower weight in case of both genders

#------ Analysis - Decision Tree ---------

#Decision Tree

attach(athlete_events)

athlete_events$ID<-NULL
athlete_events$Name <- NULL
athlete_events$Games <- NULL
athlete_events$Event <- NULL
athlete_events$NOC <- NULL
athlete_events$City <- NULL
athlete_events$Sport <- NULL
athlete_events$Team <- NULL

athlete_events$Age <- as.numeric(athlete_events$Age)
athlete_events$Height <- as.numeric(athlete_events$Height)
athlete_events$Year <- as.numeric(athlete_events$Year)


str(athlete_events)

library(rpart)
library(rpart.plot)

str(athlete_events)
dt_model<-rpart(Season~.,data = athlete_events)

rpart.plot(dt_model,type = 1) #plotting tree for whole model

#asRules(dt_model)

library(caret)
#Splitting the data into test and train (75-25%)
index_dt_1 <- createDataPartition(athlete_events$Season, p = 0.75, list = FALSE)
train_dt_1 <- athlete_events[index_dt_1,]
test_dt_1 <- athlete_events[-index_dt_1,]


dt_model_train <- rpart(Season~., data = train_dt_1)#train Model 
summary(dt_model_train)

#predicting Values using test data
pred_class <- predict(dt_model_train, test_dt_1, type = "class")
pred_class[1:10]

confusionMatrix(pred_class, test_dt_1$Season, positive = "Summer")

