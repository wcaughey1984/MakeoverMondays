#
# Title:    Makeover Monday 20200119
# Purpose:  (Knowledge Development) Review and develop a new visual and analysis of data
# Author:   Billy Caughey
# Date:     2020.01.19 - Initial build
#           2020.01.20 - Still building
#

##### Libraries #####

library(tidyverse)
library(ggplot2)
library(httr)
library(readxl)
library(fmsb)

##### Data Import #####

GET("https://query.data.world/s/qo3rtujqofbixo3psvnls4g6c52hkg",
    write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf,
                 col_names = c("groups","y1_1","y1_2","y1_3","y1_4"),
                 skip = 1)
# y1 : 2008/09 - 2009/10
# y2 : 2010/11 - 2011/12
# y3 : 2012/13 - 2013/14
# y4 : 2014/15 - 2015/16

head(df)

str(df)

df1 <- df %>%
    gather(Year,Percent,y1_1:y1_4) %>%
    mutate(Year = ifelse(Year == "y1_1","2010",
                  ifelse(Year == "y1_2","2012",
                  ifelse(Year == "y1_3","2014","2016"))),
           Year = as.numeric(Year)) %>%
    arrange(groups)

head(df1, 200)

##### Bar Graphs #####

df1 %>%
    filter(grepl("*[Mm]en",groups),
           !grepl("65 years and over",groups)) %>%
    ggplot(aes(x = groups, y = Percent, fill = groups)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ Year) +
    coord_flip() +
    theme(axis.text.y = element_blank())

df1 %>%
    filter(!grepl("*[Mm]en",groups),
           groups != "Adults 65 years and over") %>%
    mutate(groups = factor(groups, levels = c("Children 1.5-3 years","Children 4-10 years","Children 11-18 years",
                                              "Adults 19-64 years","Adults 65-74 years","Adults 75 years and over"))) %>%
    ggplot(aes(x = groups, y = Percent, fill = groups)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ Year) +
    coord_flip() +
    theme(axis.text.y = element_blank())


##### Line Graphs #####

df1 %>%
    filter(grepl("*[Mm]en",groups),
           !grepl("65 years and over",groups)) %>%
    ggplot(aes(x = Year, y = Percent, color = groups)) +
    geom_line(size = 2)

df1 %>%
    filter(!grepl("*[Mm]en",groups),
           groups != "Adults 65 years and over") %>%
    mutate(groups = factor(groups, levels = c("Children 1.5-3 years","Children 4-10 years","Children 11-18 years",
                                              "Adults 19-64 years","Adults 65-74 years","Adults 75 years and over"))) %>%
    ggplot(aes(x = Year, y = Percent, color = groups)) +
    geom_line(size = 2)
