#
# Title:    Makeover Monday 2020w44
# Purpose:  (Personal Development) Determining the digital gender gap
# Author:   Billy Caughey
# Date:     2020.11.01 - Initial Build
#

##### Libraries #####

library(tidyverse)
library(httr)
library(readxl)
library(visdat)
library()

##### Bring in data #####

url <- "https://query.data.world/s/3d3imd75a3lx44m7oafe5v7ivjjj7t"
#invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))))
#df <- read_excel(tf)

mm_get <- function(data_url){

    invisible(capture.output(GET(data_url, write_disk(tf <- tempfile(fileext = ".xlsx")))))
    df <- read_excel(tf)

}

mm_w44 <- mm_get(data_url = url) %>%
    rename(`Internet Users` = `Internet users; % of households`,
           `Gender Gap in Internet Access` = `Gender gap in internet access; % difference`,
           `Gender Gap in Mobile Phone Access` = `Gender gap in mobile phone access; % difference`)

mm_w44 <- mm_w44 %>%
    rename(`Internet users; % of households` = `Internet Users`,
           `Gender gap in internet access; % difference` = `Gender Gap in Internet Access`,
           `Gender gap in mobile phone access; % difference` = `Gender Gap in Mobile Phone Access`) %>%
    mutate()
