# 
# Title:    Makeover Monday 2020w31
# Purpose:  (Knowledge Development) Development of data for 2020w31 dashboard
# Author:   Billy Caughey
# Date:     2020.08.02 - Initial Build 

##### Libraries #####

library(tidyverse)
library(httr)
library(readxl)

##### Bring in data #####

url <- "https://query.data.world/s/igxj3pmxgtrtfum2lorsoedczkkcao"
invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xls"))))
df <- read_excel(tf)

