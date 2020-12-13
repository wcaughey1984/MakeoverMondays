# 
# Title:    MM2020w50
# Purpose:  (Makeover Monday) 2020 week 50 - Bob Ross!
# Author:   Billy Caughey 
# Date:     2020.12.13 - Initial build 
# 

##### Libraries #####

library(tidyverse)

##### Set working directory #####

setwd("C:/Users/Owner/Documents/GitHub/MakeoverMondays/makeover_monday_2020w50")

##### Functions #####

## cat elements
cat_elements <- function(x){
    
    y <- NULL
    
    if(x %in% c("Lakes", "Dock", "Boat", "Ocean", "Waterfall", "River", "Lake", "Beach", 
                "Waves")){
        y <- "Oceans, Lakes, and Rivers"
    } else if(x %in% c("Building", "Farm", "Lighthouse", "Windmill", "Mill", "Bridge", 
                       "Barn", "Fence", "Cabin", "Structure")){
        y <- "Farms and Structures"
    } else if(x %in% c("Diane Andre", "Person", "Steve Ross", "Guest")){
        y <- "Guest"
    } else if(x %in% c("Apple Frame", "Double Oval Frame", "Florida Frame", 
                       "Half Circle Frame", "Half Oval Frame", "Rectangular 3D Frame", 
                       "Rectangular frame", "Seashell Frame", "Split Frame", "Tomb Frame", 
                       "Triple Frame", "Window Frame", "Wood Frame", "Circle Frame", 
                       "Portrait", "Oval Frame", "Framed", "Rectangle 3D Frame", 
                       "Rectangular Frame", "Wood Framed")){
        y <- "Painting Frame"
    } else if(x %in% c("Aurora Borealis", "Moon", "Night", "FOG", "SUN", "Winter", "Snow", 
                       "Cumulus", "Clouds", "Fire", "Cirrus")){
        y <- "Weather and Clouds"
    } else if(x %in% c("Cactus", "Flowers", "Bushes", "Grass")){
        y <- "Flowers and Bushes"
    } else if(x %in% c("Cliff", "Hills", "Path", "Rocks", "Mountains", "Snowy Mountain", 
                       "Mountain")){
        y <- "Mountains and Hills"
    } else if(x %in% c("Palm Trees", "Conifer", "Deciduous", "Trees", "Tree")){
        y <- "Trees"
    } else {
        y <- "MISSING"
    }
    
    y
    
}


##### Bring in the data #####

df <- read.csv("elements-by-episode.csv", 
               header=TRUE, 
               stringsAsFactors=FALSE) %>%
    mutate(season = as.numeric(substr(Episode, 2, 3)),
           episode = as.numeric(substr(Episode, 5, 6)),
           cat_elements = sapply(X = Element, FUN = cat_elements),
           cat_incident = (Included > 0))

##### Explore #####

# Basic structure of data
str(df)

# Totally number of seasons and episodes: 31 seasons, 13 episodes per season
df %>%
    select(season, episode) %>%
    unique() %>%
    group_by(season) %>%
    summarize(n()) %>%
    ungroup() %>%
    View()

# What is in 'element'?
df %>%
    select(Element) %>%
    unique() %>%
    View()

# How many elements are seen across all seasons? There are 67 unique elements... condense?
df %>%
    select(Element, Included) %>%
    group_by(Element) %>%
    summarize(sum(Included)) %>%
    View()

##### Visualize #####

df %>%
    group_by(season, episode, cat_elements) %>%
    summarize(incident = max(cat_incident)) %>%
    ungroup() %>%
    group_by(season, cat_elements) %>%
    summarize(paintings = sum(incident)) %>%
    ungroup() %>%
    arrange(season, paintings) %>%
    filter(!(cat_elements %in% c("Guest", "Painting Frame"))) %>%
    ggplot(aes(x = season, y = paintings)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    ylim(c(0,13)) +
    ylab("Paintings") +
    xlab("Seasons") +
    theme_bw() +
    facet_wrap(~ cat_elements) +
    theme(axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          strip.text = element_text(size = 18)) 


    