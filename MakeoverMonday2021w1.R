# 
# Title:    Makeover Monday 2021w01
# Purpose:  (Personal Development) Developing my own analysis for week 1
# Author:   Billy Caughey
# Date:     2021.01.10 - Initial Build 
# 

##### Libraries #####
library(httr)
library(readxl)
library(tidyverse)
library(lubridate)

##### Pull data in #####
url <- "https://query.data.world/s/nedy2zrrvisyaev42qe4hputjvgyou" 
invisible(
    capture.output(
        GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
    )
)

df <- read_excel(tf) %>%
    mutate(Week = as_date(Week),
           Week_Num = as.numeric(gsub(pattern = "Week ", 
                                      replacement = "",
                                      x = Timeframe)))

##### Descriptive Statistics #####

str(df)

plot1 <- df %>%
    ggplot(aes(x = Week_Num, 
               y = `Counts (31 counters)`,
               color = factor(Year))) +
    geom_line(size = 2, alpha = 0.65) +
    theme_bw() +
    labs(y = "Trail Counters",
         x = "Week #",
         title = "Trail Counters vs Weeks, 2019-2020") +
    theme(axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 24),
          legend.title = element_blank(),
          legend.text = element_text(size = 16))

plot1

plot2 <- df %>%
    ggplot(aes(x = Week_Num, 
               y = `Bikes (14 counters)`,
               color = factor(Year))) +
    geom_line(size = 2, alpha = 0.65) +
    theme_bw() +
    labs(y = "Bikes",
         x = "Week Number",
         title = "Bikes vs Weeks, 2019-2020") +
    theme(axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 24),
          legend.title = element_blank(),
          legend.text = element_text(size = 16))

plot2

plot3 <- df %>%
    ggplot(aes(x = Week_Num, 
               y = `Pedestrians (14 counters)`,
               color = factor(Year))) +
    geom_line(size = 2, alpha = 0.65) +
    theme_bw() +
    labs(y = "Pedestrians",
         x = "Week Number",
         title = "Pedestrians vs Weeks, 2019-2020") +
    theme(axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 24),
          legend.title = element_blank(),
          legend.text = element_text(size = 16))

plot3




















