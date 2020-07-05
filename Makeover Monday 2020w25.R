# 
# Title:    Makeover Monday 2020w25
# Purpose:  (R Programming Practice) Code for week 25 of Makeover Monday
# Author:   Billy Caughey 
# Date:     2020.07.03 - Initial Build 
# 

##### Libraries #####
library(tidyverse)
library(readr)
library(httr)
library(readxl)
library(grid)
library(gridExtra)
library(scales)

##### Bring in Data #####
url <- "https://query.data.world/s/mtrdnivw6h25tdhlgraglcoreg7wj7"
invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))))
df <- read_excel(tf)

##### Preprocessing #####

mm25 <- df %>%
  mutate(`Revenue (US $M)` = gsub(",","",`Revenue (US $M)`),
         `Revenue (US $M)` = gsub("\\$","",`Revenue (US $M)`),
         `Revenue (US $M)` = as.numeric(`Revenue (US $M)`),
         `Net Income (US $M)` = gsub(",","",`Net Income (US $M)`),
         `Net Income (US $M)` = gsub("\\$","",`Net Income (US $M)`),
         `Net Income (US $M)` = as.numeric(`Net Income (US $M)`),
         `Income Ratio` = `Net Income (US $M)` / `Revenue (US $M)`) %>%
  gather(key = Revenue, value = Amount, `Revenue (US $M)`:`Income Ratio`) 

rev_plot <- mm25 %>%
  filter(Revenue == "Revenue (US $M)") %>%
  ggplot(aes(x = Quarter, y = Amount)) +
  geom_line() +
  theme_bw() +
  ylab("Revenue\n(US $M)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14))

inc_plot <- mm25 %>%
  filter(Revenue == "Net Income (US $M)") %>%
  ggplot(aes(x = Quarter, y = Amount)) +
  geom_line() +
  theme_bw() +
  ylab("Net Income\n(US $M)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14))

ratio_plot <- mm25 %>%
  filter(Revenue == "Income Ratio") %>%
  ggplot(aes(x = Quarter, y = Amount)) +
  geom_line() +
  theme_bw() +
  ylab("Income\nRatio") +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14)) +
  scale_y_continuous(labels = percent)

grid.arrange(rev_plot, inc_plot, ratio_plot, nrow = 3)
