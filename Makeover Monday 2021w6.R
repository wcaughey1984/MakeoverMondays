# 
# Title:	Makeover Monday 2021w6 
# Purpose:	(Knowledge Development) Work on code for week 6 
# Author:	Billy Caughey 
# Date:		2021.03.21 - Initial Build 
#

##### Libraries #####

library(httr)
library(readxl)
library(tidyverse)

##### Bring in the Data ####

url <- "https://query.data.world/s/4xb7inm3smwlgnovvtg6hkkeknezwf"
invisible(capture.output(GET(url, 
							 write_disk(tf <- tempfile(fileext = ".xlsx")))))
df <- read_excel(tf)

##### Pre-Processing #####

simplify_response <- function(x){
	
	# Simplify survey responses 
	if(grep("treated", x) == TRUE){
		return("Treated Differently")
	} else if(grep("crises", x) == TRUE)

}

mm6_data <- df %>%
	filter(Gender != "Total") 

##### Visualize #####

mm6_data %>%
	filter(Country == "New Zealand") %>% 
	ggplot(aes(x = Question, y = `% of respondents`,
			   fill = Gender)) +
	geom_bar() +
	coord_flip()
	facet_grid(. ~ Country)