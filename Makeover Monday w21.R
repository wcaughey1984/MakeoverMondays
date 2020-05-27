#
# Title:    Makeover Monday w21
# Purpose:  (Personal Development) Makeover Monday w21 submission
# Author:   Billy Caughey
# Date:     2020.05.24 - Initial Build 
# 

##### Libraries #####

library(tidyverse)
library("httr")
library("readxl")

GET("https://query.data.world/s/bgum7ipk4c45xtm2ddnvmavvwfmshg", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)