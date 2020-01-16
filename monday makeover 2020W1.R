# Title:        Makeover Monday 2020W1
# Purpose:      (Knowledge Development) Working on my R programming
# Author:       Billy Caughey
# Date:         2020.01.09 - Initial build

##### Libraries #####

library(httr)
library(readxl)
library(tidyverse)
library(ggplot2)

##### Collect Data #####

GET("https://query.data.world/s/neghbubgrzjnmunhdmjdosakmeyobb", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

head(df, n = 100)

df1 <- df %>%
    gather(Year, Percent, `2017`:`2004`) %>%
    mutate(Sport = ifelse(Sport %in% c("Football","Basketball","Baseball", "None"), Sport, "Other")) %>%
    group_by(Sport, Year) %>%
    summarize(Percent = sum(Percent)) %>%
    ungroup() %>%
    mutate(Year = as.numeric(Year))

df1 %>%
    ggplot(aes(x = Year, y = Percent, color = Sport)) +
    geom_line()
