# 
# Title:    Makeover Monday 2020w32
# Purpose:  (Practicing) Code for week 32 of Makeover Monday
# Author:   Billy Caughey 
# Date:     2020.08.14 - Initial Build 
# 

##### Libraries #####

library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(ggpubr)
library(scales)

##### Bring in the data #####

url <- "https://query.data.world/s/otvxdqu7wcrxpzwz6vikc3vxxc2lld"
invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))))
df <- read_excel(tf)

##### Preprocessing #####

df_bene <- df %>%
  mutate(`Benefit of remote work` = factor(`Benefit of remote work`,
                                           levels = c("Other",
                                                      "Ability to work from home",
                                                      "Ability to spend time with family",
                                                      "Not having to commute",
                                                      "Flexibility to work from any location",
                                                      "Ability to have a flexible schedule")))

## Add the struggles 

struggle <- c("Difficulties with collaboration and communication",
              "Loneliness",
              "Not being able to unplug",
              "Distractions at home",
              "Being in a different timezone than teammates",
              "Staying motivated",
              "Taking vacation time",
              "Finding reliable wifi",
              "Other")

percent <- c(20, 20, 18, 12, 10, 7, 5, 3, 5)

df1 <- tibble(`Percentage` = percent,
              `Struggles of remote work` = struggle) %>%
  mutate(`Struggles of remote work` = factor(`Struggles of remote work`,
                                             levels = c("Other",
                                                        "Finding reliable wifi",
                                                        "Taking vacation time",
                                                        "Staying motivated",
                                                        "Being in a different timezone than teammates",
                                                        "Distractions at home",
                                                        "Not being able to unplug",
                                                        "Loneliness",
                                                        "Difficulties with collaboration and communication")))

##### Visual #####

# benefits

bene_viz <- ggplot(df_bene, 
                   aes(x = `Benefit of remote work`, 
                       y = `Percentage`)) +
  geom_bar(stat = "identity") +
  ggtitle("Benefits of Working at Home") +
  theme_bw() +
  theme(legend.key = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x = element_text(size = 12),
           axis.title.y = element_blank(),
           axis.text.y = element_text(size = 12)) +
  coord_flip() 

bene_viz

# Struggles

strg_viz <- ggplot(df1, 
                   aes(x = `Struggles of remote work`, 
                       y = `Percentage`)) +
  geom_bar(stat = "identity") +
  ggtitle("Struggles of Working at Home") +
  theme_bw() +
  theme(legend.key = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x = element_text(size = 12),
           axis.title.y = element_blank(),
           axis.text.y = element_text(size = 12)) +
  coord_flip() 

strg_viz


## Bring together 

ggarrange(bene_viz, 
          strg_viz, 
          ncol = 1, 
          align = "hv")