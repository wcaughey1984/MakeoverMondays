#
# Title:    Makeover Monday 2020w26
# Purpose:  (Getting better at R) Code for Makover Monday
# Author:   Billy Caughey 
# Date:     2020.07.02 - Initial Build 
# 

##### Libraries #####
library(tidyverse)
library(readr)

##### Bring in data #####

df <- read.csv("https://query.data.world/s/5d5xqmzikfdreafkl7wcaimvvtr4jy", 
               header = TRUE, 
               stringsAsFactors = FALSE) %>%
  mutate(Region = ifelse(Country == "Afghanistan", 
                         "Europe & Central Asia", Region)) 

##### Preprocessing #####

# AVG({FIXED [Country], [WBL Report Year], [Question Category]:
#   SUM([Index Score]})

mm1 <- df %>%
  group_by(Country, Region, WBL.Report.Year, Question.Category) %>%
  summarize(score = sum(Index.Score, na.rm = T)) %>%
  ungroup() %>%
  group_by(Country, Region, WBL.Report.Year) %>%
  summarize(avg_score = mean(score, na.rm = T)) %>%
  ungroup() %>%
  group_by(Region, WBL.Report.Year) %>%
  summarize(region_avg_score = round(mean(avg_score, na.rm = T), 2))

mm1 %>%
  ggplot(aes(x = WBL.Report.Year, y = region_avg_score, color = Region)) +
  geom_line(size = 2) +
  ylab("Avg. Regional Score") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14))

mm2 <- df %>%
  filter(Country %in% c("United States", "Denmark", "Sweden", "Hungary",
                        "Canada", "Iceland", "Finland", "Japan")) %>%
  group_by(Country, WBL.Report.Year, Question.Category) %>%
  summarize(score = sum(Index.Score, na.rm = T)) %>%
  ungroup() %>%
  group_by(Country, WBL.Report.Year) %>%
  summarize(avg_score = mean(score, na.rm = T)) %>%
  ungroup()

mm2 %>%
  ggplot(aes(x = WBL.Report.Year, y = avg_score, color = Country)) +
  geom_line(size = 2) +
  ylab("Avg. Regional Score") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14))

mm3 <- df %>%
  filter(Country %in% c("United States", "Albania", "Cyprus", "Taiwan, China",
                        "Bulgaria", "Romania", "Ecuador", 
                        "Hong Kong SAR, China")) %>%
  group_by(Country, WBL.Report.Year, Question.Category) %>%
  summarize(score = sum(Index.Score, na.rm = T)) %>%
  ungroup() %>%
  group_by(Country, WBL.Report.Year) %>%
  summarize(avg_score = mean(score, na.rm = T)) %>%
  ungroup() %>%
  arrange(Country, WBL.Report.Year)

mm3 %>%
  ggplot(aes(x = WBL.Report.Year, y = avg_score, color = Country)) +
  geom_line(size = 2) +
  ylab("Avg. Regional Score") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14))














