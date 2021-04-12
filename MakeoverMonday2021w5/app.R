#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Libraries #####

library(shiny)
library(flexdashboard)
library(tidyverse)
library(httr)
library(readxl)
library(scales)

##### Import Data #####

url <- "https://query.data.world/s/elmc5b27izmkphj6fi3py3hpcpqnra"
invisible(capture.output(GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))))
df <- read_excel(tf)

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
	filter(Year >= 2001)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Years",
                        "Start From:",
                        min = 2001,
                        max = 2019,
                        value = 2010)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        mm5_data %>%
            filter(Year >= input$Years)
	        ggplot(aes(x = Year, y = total_twh_pct, color = Region)) +
	        geom_line() +
	        geom_point() +
	        labs(y = "Total TWh (Pct Change)") +
	        scale_x_continuous(breaks = pretty_breaks()) +
	        scale_y_continuous(label = percent) +
	        theme_bw() +
	        theme(axis.title.x = element_blank(),
	        	  axis.text.x = element_text(angle = 45)) +
	        facet_wrap(. ~ Variable, 
	        		   scales = "free_y") 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
