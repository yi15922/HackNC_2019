library(tidyverse)
library(broom)
library(dplyr)
library(lubridate)

data <- read_csv("data.csv") %>%
  tidy()

data <- data %>%
  mutate(dateTime = ymd_hms(CreatedDate),
         Year = year(dateTime),
         Month = month(dateTime),
         Day = day(dateTime),
         Date = paste(Year, Month, Day, sep = "-") %>% ymd() %>% as.Date() %>%
           select(data$Date, everything()))