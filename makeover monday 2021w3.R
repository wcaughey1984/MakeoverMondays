# 
# Title:    Makeover Monday 2021w3 
# Purpose:  (Personal Dev) topic is global warming 
# Author:   Billy Caughey 
# Date:     2021.01.17 - Initial Build 
# 

##### Libraries #####

library(tidyverse) 

##### Pre-Processing #####

months_to_seasons <- function(x){
    
    if(x %in% c("Mar", "Apr", "May")){
        return("Spring")
    } else if(x %in% c("Jun", "Jul", "Aug")){
        return("Summer")
    } else if(x %in% c("Sep", "Oct", "Nov")){
        return("Fall")
    } else if(x %in% c("Dec", "Jan", "Feb")){
        return("Winter")
    }
    
}

file_loc <- "https://query.data.world/s/fsmi4inacindztp6iybli5kbysm6lx"
df <- read.csv(file_loc, 
               header = TRUE, 
               stringsAsFactors = FALSE) %>%
    select(Hemisphere:Dec) %>%
    gather(key = "months", value = "temps", Jan:Dec) %>%
    mutate(months = factor(months,
                           levels = c("Jan", "Feb", "Mar", "Apr", 
                                      "May", "Jun", "Jul", "Aug",
                                      "Sep", "Oct", "Nov", "Dec")),
           seasons = factor(sapply(X = months, FUN = months_to_seasons),
                            levels = c("Spring", "Summer", 
                                       "Fall", "Winter")))

##### Visuals #####

df %>%
    group_by(Hemisphere, Year, seasons) %>%
    summarise(temps = mean(temps, na.rm = T)) %>%
    ungroup() %>%
    ggplot(aes(x = Year, y = temps, color = seasons)) +
    geom_line(alpha = 0.6) +
    stat_smooth() +
    facet_grid(Hemisphere ~ seasons)

