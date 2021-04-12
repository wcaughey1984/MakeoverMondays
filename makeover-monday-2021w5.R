# 
# Title:	Makeover Monday 2021W5
# Purpose:	(Knowledge Development) Building Week 5 Dashboard 
# Author:	Billy Caughey 
# Date:		2021.03.21 - Initial Build 
#

##### Libraries #####
library(tidyverse)
library(httr)
library(readxl)
library(scales)

##### Import Data #####

url <- "https://query.data.world/s/elmc5b27izmkphj6fi3py3hpcpqnra"
invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))))
df <- read_excel(tf)

##### Data Exploration #####

# Open the data
View(df)

# Structure of the data 
str(df)

table(df$Year)
table(df$Area)
table(df$Variable)

##### Pre-Processing #####

# functions 

area_to_region <- function(x){
	
	# Function to return which region a European country belongs to
	if(x %in% c("Denmark", "Estonia", "Finland", "Ireland", "Latvia", 
				"Lithuania", "Sweden", "United Kindgom")){
		return("Northern Europe")
	} else if(x %in% c("Austria", "Belgium", "Germany", "Luxembourg", 
					   "Netherlands")){
		return("Western Europe")
	} else if(x %in% c("Bulgaria", "Czech Republic", "Hungary", "Poland",
					   "Romania", "Slovakia")){
		return("Eastern Europe")
	} else if(x %in% c("EU-27", "EU27+1")){
		return("drop")
	} else {
		return("Southern Europe")
	}
	
}

# Group countries into regions 

mm5_data <- df %>%
	# Convert Countries to Regions
	mutate(Region = sapply(X = Area, FUN = area_to_region)) %>%
	select(Region, Variable, Year, `Generation (TWh)`) %>%
	# Aggregate Total and Average TWh per Region
	group_by(Region, Variable, Year) %>%
	summarize(total_twh = sum(`Generation (TWh)`, na.rm = T),
			  avg_twh = mean(`Generation (TWh)`, na.rm = T),
			  .groups = "drop") %>%
	# Only consider 4 regions and 4 fuel methods
	filter(Variable %in% c("Renewables","Fossil", "Wind and solar", "Coal"),
		   Region != "drop") %>%
	# Compute Pct Change 
	arrange(Region, Variable, Year) %>%
	group_by(Region, Variable) %>%
	mutate(total_twh_pct = (total_twh / lag(total_twh) - 1),
		   avg_twh_pct = (avg_twh / lag(avg_twh) - 1)) %>%
	filter(Year >= 2010)

##### Plot 1: Total Pct Change #####

mm5_data %>%
	ggplot(aes(x = Year, y = total_twh_pct, color = Region)) +
	geom_line(size = 2) +
	geom_point(size = 4) +
	labs(y = "Total TWh (Pct Change)") +
	scale_x_continuous(breaks = pretty_breaks()) +
	scale_y_continuous(label = percent) +
	theme_bw() +
	theme(axis.title.x = element_blank(),
		  axis.text.x = element_text(size = 16),
		  axis.title.y = element_text(size = 20),
		  axis.text.y = element_text(size = 16),
		  strip.text = element_text(size = 24),
		  legend.title = element_blank(),
		  legend.text = element_text(size = 16)) +
	facet_wrap(. ~ Variable, 
			   scales = "free_y") 

##### Table 1: Total TWh by Region by Source Since 2010 ####

mm5_data %>%
	select(Region, total_twh, Variable) %>%
	group_by(Region, Variable) %>%
	summarize(total_twh_all_years = sum(total_twh, na.rm = T),
			  .groups = "drop") %>%
	spread(Variable, total_twh_all_years)
















