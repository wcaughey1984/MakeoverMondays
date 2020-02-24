#
#   Title:      Makeover Monday 2020w8
#   Purpose:    (Personal Knowledge) Code to support the R dashboard for weekly Makeover
#   Author:     Billy Caughey
#   Date:       2020.02.23 - Initial Build
#   

##### Libraries #####

library(flexdashboard)
library(httr)
library(readxl)
library(tidyverse)
library(scales)
library(ggpubr)
library(kableExtra)

##### Pulling in the data #####

invisible(capture.output(GET("https://query.data.world/s/btmtcqnqumda6yhsxzqwtzbyb5uar7", 
                             write_disk(tf <- tempfile(fileext = ".xlsx")))))
df <- read_excel(tf, sheet = 2)

##### Explorative Data Analytsis #####

df1 <- df %>%
    mutate(completion = ifelse(`Client situation at end of support` == "Unknown",
                               "Dropped Out","Completed")) %>%
    group_by(Year, completion) %>%
    summarise(grp_obs = sum(`Size (no. of clients)`)) %>%
    mutate(pct_complete = grp_obs / sum(grp_obs)) %>%
    ungroup()

comp1415 <- df1 %>%
    filter(Year == names(table(df1$Year))[1]) %>%
    ggplot(aes(x = completion, y = pct_complete)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2014-15")

comp1516 <- df1 %>%
    filter(Year == names(table(df1$Year))[2]) %>%
    ggplot(aes(x = completion, y = pct_complete)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2015-16")

comp1617 <- df1 %>%
    filter(Year == names(table(df1$Year))[3]) %>%
    ggplot(aes(x = completion, y = pct_complete)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2016-17")

comp1718 <- df1 %>%
    filter(Year == names(table(df1$Year))[3]) %>%
    ggplot(aes(x = completion, y = pct_complete)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2017-18")


##### Drop out #####

ggarrange(comp1415, comp1516, comp1617, comp1718,
          ncol = 2, nrow = 2)

##### homeless situation #####


df2 <- df %>%
    mutate(`Housing situation at end of support` = ifelse(
        `Client situation at end of support` %in% c("Couch surfer","Rough sleeper","Short term temporary accommodation"),"Homeless","At Risk")) %>%
    select(Year, `Housing situation at start of support`, `Housing situation at end of support`, `Size (no. of clients)`) %>%
    mutate(situation = paste0(`Housing situation at start of support`," - ", `Housing situation at end of support`)) %>%
    group_by(Year, situation) %>%
    summarize(Size = sum(`Size (no. of clients)`)) %>%
    mutate(pct_situation = Size / sum(Size))

home1415 <- df2 %>%
    filter(Year == names(table(df1$Year))[1]) %>%
    ggplot(aes(x = situation, y = pct_situation)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2014-15")

comp1516 <- df2 %>%
    filter(Year == names(table(df1$Year))[2]) %>%
    ggplot(aes(x = situation, y = pct_situation)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2015-16")

comp1617 <- df2 %>%
    filter(Year == names(table(df1$Year))[3]) %>%
    ggplot(aes(x = situation, y = pct_situation)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2016-17")

comp1718 <- df2 %>%
    filter(Year == names(table(df1$Year))[3]) %>%
    ggplot(aes(x = situation, y = pct_situation)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    scale_y_continuous(labels = percent) +
    ggtitle("2017-18")

ggarrange(home1415, home1516, home1617, home1718,
          ncol = 2, nrow = 2)
