# 
# Title:    Makeover Monday 2020w31
# Purpose:  (Knowledge Development) Development of data for 2020w31 dashboard
# Author:   Billy Caughey
# Date:     2020.08.02 - Initial Build 

##### Libraries #####

library(tidyverse)
library(httr)
library(readxl)
library(lubridate)

##### Bring in data #####

url <- "https://query.data.world/s/igxj3pmxgtrtfum2lorsoedczkkcao"
invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xls")))))
df <- read_excel(tf) %>%
    filter(grepl(pattern = "[0-9][0-9][0-9][0-9]\\s[A-Z][A-Z][A-Z]", 
                 x = Title)) %>%
    rowwise() %>%
    mutate(year = str_split(string = Title, pattern = " ")[[1]][1],
           month = str_split(string = Title, pattern = " ")[[1]][2],
           qtr = ifelse(month %in% c("JAN", "FEB", "MAR"), "Qtr1",
                 ifelse(month %in% c("APR", "MAY", "JUN"), "Qtr2",
                 ifelse(month %in% c("JUL", "AUG", "SEP"), "Qtr3", "Qtr4"))))
          
          