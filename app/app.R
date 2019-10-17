##Load required libraries
library(shiny)
library(leaflet)
library(sp)
library(sf)
library(rgdal)

##Data prep
blotter <- read.csv("Blotter_Data_Archive.csv") #Read blotter data
blotter$INCIDENTTIME <- strptime(x = as.character(blotter$INCIDENTTIME), #Convert INCIDENTTIME to datetime
                                 format = "%Y-%m-%dT%H:%M:%S")
blotter$type <- cut(blotter$HIERARCHY, c(-Inf, 9, 98, Inf), labels = c("Type 1 - Major Crime", "Type 2 - Minor Crime", "No Data or None")) #Convert hierarchy to bucketed factors
blotter_subset <- blotter[blotter$INCIDENTTIME >= "2018-01-01" & blotter$INCIDENTTIME <= "2018-12-31" & blotter$X <= -78 & blotter$Y >= 39 & !is.na(blotter$type),]
blotter_subset <- blotter_subset[sample(1:nrow(blotter_subset), 25),]
historic <- readOGR("City_Designated_Historic_Districts.geojson.json") #read historic district dat
cc_districts <- readOGR("City_Council_Districts.geojson") #read city council district data

# Define UI for application that draws a histogram
ui <- fluidPage(

)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
