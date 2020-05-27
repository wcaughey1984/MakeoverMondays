# 
# Title:    Makeover Monday w20
# Purpose:  (Personal Development) My Monday Makeover for w20 submission
# Author:   Billy Caughey
# Date:     2020.05.22 - Initial Build
# 

##### Library #####

library(tidyverse)
library("httr")
library("readxl")

##### Removing Scientific Notation #####
options(scipen = -999)

##### Bringing in data #####

url <- "https://query.data.world/s/x54cfmvrgkxzl2tjd5iez2i2q6zhlm"

invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xlsx"))))) 
df <- read_excel(tf, skip = 1) 

##### Sketch a few visuals #####

df %>%
    ggplot(aes(x = `Minimum coverage`, y = `Full coverage`, col = `Difference`)) +
    geom_point(size = 3, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, col = "red")

mod1 <- lm(`Full coverage` ~ `Minimum coverage`,
           data = df)
summary(mod1)

df %>%
    filter(State != "Michigan") %>%
    ggplot(aes(x = `Minimum coverage`, y = `Full coverage`, col = `Difference`)) +
    geom_point(size = 3, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, col = "green")

mod2 <- lm(`Full coverage` ~ `Minimum coverage`,
           data = df %>% filter(State != "Michigan"))
summary(mod2)

