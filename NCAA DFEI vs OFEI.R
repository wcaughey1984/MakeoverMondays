# 
# Title:    NCAA DFEI vs OFEI
# Purpose:  (Personal Growth) Compare defensive and offensive efficiency in NCAA football
# Author:   Billy Caughey
# Date:     2019.12.27 - Initial Build
# 

##### Libraries #####

library(tidyverse)
library(ggplot2)
library(xml2)
library(rvest)
library(tibble)

##### Pull in Data #####

# Urls

defense_url <- "https://www.footballoutsiders.com/stats/feidef/2019"
offense_url <- "https://www.footballoutsiders.com/stats/feioff/2019"

# set up for html 

defense_wp <- read_html(defense_url)
offense_wp <- read_html(offense_url)

# Put the data into a tibble 

defense <- html_table(defense_wp)[[1]] %>%
    as_tibble(.name_repair = "unique") %>%
    rename(Rk = X1,
           Team = X2,
           W_L = X3,
           DFEI = X4,
           DEP = X5,
           Rk_DEP = X6,
           DOA = X7,
           Rk_DOA = X8,
           DTD = X9,
           Rk_DTD = X10,
           DFD = X11,
           Rk_DFD = X12,
           DAY = X13,
           Rk_DAY = X14,
           DED = X15,
           Rk_DED = X16,
           DBD = X17,
           Rk_DBD = X18,
           DTO = X19,
           Rk_DTO = X20) %>%
    filter(Rk != "Rk")

offense <- html_table(offense_wp)[[1]] %>%
    as_tibble(.name_repair = "unique") %>%
    rename(Rk = X1,
           Team = X2,
           W_L = X3,
           OFEI = X4,
           OEP = X5,
           Rk_OEP = X6,
           OOA = X7,
           Rk_OOA = X8,
           OTD = X9,
           Rk_OTD = X10,
           OFD = X11,
           Rk_OFD = X12,
           OAY = X13,
           Rk_OAY = X14,
           OED = X15,
           Rk_OED = X16,
           OBD = X17,
           Rk_OBD = X18,
           OTO = X19,
           Rk_OTO = X20) %>%
    filter(Rk != "Rk")

