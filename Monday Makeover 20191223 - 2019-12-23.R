#
#   Title:      Monday Makeover 20191223
#   Purpose:    (Knowledge Development) Scrap paper to build Monday Makeover
#   Author:     Billy Caughey
#   Date:       2019.12.23 - Initial Build
#   



library(tidyverse)
library(ggplot2)
library(scales)
library(httr)
library(readxl)

GET("https://query.data.world/s/yuycdlchnfcjcp35gc4zxzqsfhxduf", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

df %>%
    ggplot(aes(x = `Amount in £`, y = Category, color = Year)) +
    geom_point() +
    facet_wrap(~ Region, scales = "free")

bar1 <- df %>%
    ggplot(aes(x = Category, y = `Amount in £`, fill = as.character(Year))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Year") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 16),
          strip.text = element_text(size = 24)) +
    facet_wrap(~ Region) 

bar1

bar2 <- df %>%
    group_by(Region, Category) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(`Pct Change` = (`Amount in £`/lag(`Amount in £`)) - 1) %>%
    select(Region, Category, `Pct Change`) %>%
    filter(is.na(`Pct Change`) == FALSE) %>%
    ggplot(aes(x = Category, y = `Pct Change`, fill = Region)) + 
    geom_bar(stat = "identity", position = "dodge") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          strip.text = element_text(size = 24)) +
    scale_y_continuous(labels = percent) 

bar2
