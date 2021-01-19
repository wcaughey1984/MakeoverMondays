# 
# Title:    Monday Makeover 2021w02
# Purpose:  (Personal Dev) Analysis for the MM2020w02
# Author:   Billy Caughey 
# Date:     2021.01.16 - Initial Build 
#

##### Libraries #####

library(tidyverse) 
library(Amelia)

##### Pre-Processing #####

file_url <- "https://query.data.world/s/2bnbe3zaxn2w2w72xisuwcenvjx5r5"
df <- read.csv(file_url, 
               header=TRUE, 
               stringsAsFactors=FALSE) 

## Review structure ##

str(df)

## Put it together ##

df1 <- df %>%
    rename(region = UNICEF.Region,
           new_hiv_infection_per_1000 = Estimated.incidence.rate.of.new.HIV.infection.per.1.000.uninfected.population,
           gender = Sex,
           year = Year,
           aids_related_deaths_per_100000 = Estimated.rate.of.annual.AIDS.related.deaths..per.100.000.population) %>%
    select(region, gender, year, 
           new_hiv_infection_per_1000, aids_related_deaths_per_100000) %>%
    group_by(region, gender, year) %>%
    summarize(new_hiv_infection_per_1000_avg = mean(new_hiv_infection_per_1000, na.rm = T),
              aids_related_deaths_per_100000_avg = mean(aids_related_deaths_per_100000, na.rm = T)) %>%
    ungroup() %>%
    mutate(region = ifelse(region == "Eastern and Southern Africa",
                           "Eastern and \nSouthern Africa",
                           "West and \nCentral Africa"))

##### Data Viz #####

## Region vs HIV ##

df1 %>%
    ggplot(aes(x = year, y = new_hiv_infection_per_1000_avg)) +
    geom_line(size = 2) +
    theme_bw() +
    labs(y = "New Infections Per 1,000 (Avg)",
         title = "New HIV Infections Per 1,000 (Avg); 1990 - 2020") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14, angle = 45, vjust = 0.5),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20)) +
    facet_grid(region ~ gender)

## Region vs Deaths ##

df1 %>%
    ggplot(aes(x = year, y = aids_related_deaths_per_100000_avg)) +
    geom_line(size = 2) +
    theme_bw() +
    labs(y = "Deaths per 100,000 (Avg)",
         title = "Aids related deaths per 100,000 (Avg); 1990 - 2020") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14, angle = 45, vjust = 0.5),
          strip.text = element_text(size = 18),
          plot.title = element_text(size = 20)) +
    facet_grid(region ~ gender)


























