library(tidyverse)
library(broom)
library(dplyr)
library(lubridate)
library(stringr)
library(tidytext)
library(textdata)

data(stop_words)
afinn <- get_sentiments("afinn")

data <- read_csv("data.csv") %>%
  tidy()

data <- data %>%
  mutate(dateTime = ymd_hms(CreatedDate),
         Year = year(dateTime),
         Month = month(dateTime),
         Day = day(dateTime),
         Date = paste(Year, Month, Day, sep = "-") %>% ymd() %>% as.Date()) %>%
  select(-dateTime, -Day, -Month, -Year) %>%
  select(Date, everything())

data_noNAs <- data %>%
  drop_na()

dataUSA <- subset(data, Country_USA == 1)
dataGBR <- subset(data, Country_GBR == 1)
dataCAN <- subset(data, Country_CAN == 1)

dataUSA %>%
  select(Title, Hits)

names(data)

allTags <- gather(data, "tags", "hasTag", 13:35) %>%
  select(StoryID, tags, hasTag)
allTags <- allTags[order(allTags$StoryID),]

allTags %>%
  filter(StoryID == 146316 & hasTag == 1)


