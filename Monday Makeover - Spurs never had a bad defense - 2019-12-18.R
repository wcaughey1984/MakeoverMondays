# 
# Title:    Monday Makeover - Spurs never had a bad defense
# Purpose:  (Knowledge Development) Determine an analysis to show Spurs never had a bad defense.
# Author:   Billy Caughey
# Date:     2019.12.18 - Initial Build
# 

##### Libraries #####
library(tidyverse)
library(ggplot2)

##### Pre-Processing #####

nba <- read.csv("/Users/wcaughey/Downloads/NBA Defensive Ratings.csv")

##### Feature Creation #####

nba <- nba %>%
    mutate(season_champ = 
               ifelse(SEASON == "1996-97" & TEAM == "Chicago Bulls", 1,
               ifelse(SEASON == "1997-98" & TEAM == "Chicago Bulls", 1,
               ifelse(SEASON == "1998-99" & TEAM == "San Antonio Spurs", 1,
               ifelse(SEASON == "1999-00" & TEAM == "Los Angeles Lakers", 1,
               ifelse(SEASON == "2000-01" & TEAM == "Los Angeles Lakers", 1,
               ifelse(SEASON == "2001-02" & TEAM == "Los Angeles Lakers", 1,
               ifelse(SEASON == "2002-03" & TEAM == "Detroit Pistons", 1,
               ifelse(SEASON == "2003-04" & TEAM == "San Antonio Spurs", 1,
               ifelse(SEASON == "2004-05" & TEAM == "San Antonio Spurs", 1,
               ifelse(SEASON == "2005-06" & TEAM == "Miami Heat", 1,
               ifelse(SEASON == "2006-07" & TEAM == "San Antonio Spurs", 1,
               ifelse(SEASON == "2007-08" & TEAM == "Boston Celtics", 1,
               ifelse(SEASON == "2008-09" & TEAM == "Los Angeles Lakers", 1, 
               ifelse(SEASON == "2009-10" & TEAM == "Los Angeles Lakers", 1,
               ifelse(SEASON == "2010-11" & TEAM == "Dallas Mavericks", 1,
               ifelse(SEASON == "2011-12" & TEAM == "Miami Heat", 1,
               ifelse(SEASON == "2012-13" & TEAM == "Miami Heat", 1,
               ifelse(SEASON == "2013-14" & TEAM == "San Antonio Spurs", 1,
               ifelse(SEASON == "2014-15" & TEAM == "Golden State", 1,
               ifelse(SEASON == "2015-16" & TEAM == "Cleveland Cavaliers", 1,
               ifelse(SEASON == "2016-17" & TEAM == "Golden State Warriors", 1,
               ifelse(SEASON == "2017-18" & TEAM == "Golden State Warriors", 1, 
               ifelse(SEASON == "2018-19" & TEAM == "Toronto Raptors", 1,0
                      )))))))))))))))))))))))) 

##### Build sets for champs, spurs, and league average #####

champs <- nba %>%
    filter(season_champ == 1) %>%
    mutate(WINNING_PERC = round((W / G), 2)) %>%
    select(SEASON, focus_group, WINNING_PERC, DEF.RTG)

spurs <- nba %>%
    filter(TEAM == "San Antonio Spurs") %>%
    mutate(WINNING_PERC = round((W / G), 2)) %>%
    select(SEASON, focus_group, WINNING_PERC, DEF.RTG)

league_avg <- nba %>%
    filter(TEAM != "League Avg") %>%
    select(SEASON, G, W, DEF.RTG) %>%
    group_by(SEASON) %>%
    summarize(G = max(G),
              W = mean(W),
              DEF.RTG = mean(DEF.RTG)) %>%
    ungroup() %>%
    mutate(WINNING_PERC = round((W / G), 2),
           focus_group = "League Avg") %>%
    select(SEASON, focus_group, WINNING_PERC, DEF.RTG)

plot_nba <- bind_rows(champs, spurs, league_avg)

plot_nba %>%
    ggplot(aes(x = DEF.RTG, y = WINNING_PERC, color = focus_group)) +
    geom_point()