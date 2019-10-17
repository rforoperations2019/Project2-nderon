##Load required libraries
library(shiny)
library(leaflet)
library(sp)
library(sf)
library(rgdal)
library(DT)

##Data prep
blotter <- read.csv("Blotter_Data_Archive.csv") #Read blotter data
blotter$INCIDENTTIME <- strptime(x = as.character(blotter$INCIDENTTIME), #Convert INCIDENTTIME to datetime
                                 format = "%Y-%m-%dT%H:%M:%S")
blotter$type <- cut(blotter$HIERARCHY, c(-Inf, 9, 98, Inf), labels = c("Type 1 - Major Crime", "Type 2 - Minor Crime", "No Data or None")) #Convert hierarchy to bucketed factors
blotter_data <- blotter[blotter$INCIDENTTIME >= "2018-01-01" & blotter$INCIDENTTIME <= "2018-12-31" & blotter$X <= -78 & blotter$Y >= 39 & !is.na(blotter$type),]
blotter_data <- blotter_data[sample(1:nrow(blotter_subset), 200),]
historic <- readOGR("City_Designated_Historic_Districts.geojson.json") #read historic district dat
cc_districts <- readOGR("City_Council_Districts.geojson") #read city council district data

# Define UI
ui <- fluidPage(
    sidebarLayout(
        #Sidebar panel for criteria selection
        sidebarPanel(
            
            #City council selection input
            checkboxGroupInput(inputId = "selected_districts",
                               label = "Select Council District(s)",
                               choices = c(1:9),
                               selected = c(1:9)),
            
            #date range input
            dateRangeInput(inputId = "date_range",
                           label = "Date Range: yyyy-mm-dd",
                           start = "2009-01-01",
                           end = "2019-01-01",
                           startview = "year"),
            
            #Hierarchy level input
            checkboxGroupInput(inputId = "selected_hierarchy",
                               label = "Select Crime Severity",
                               choices = levels(blotter_data$type),
                               selected = levels(blotter_data$type))
            
        ),
        
        mainPanel(
            
            tabsetPanel(
                tabPanel("Map"),
                tabPanel("Time Trends"),
                tabPanel("Data", dataTableOutput(outputId = "DT"))
                
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    blotter_subset <- reactive({    #Reactive subset based on inputs
        req(input$selected_districts, input$date_range, input$selected_hierarchy)
        filter(blotter_data, COUNCIL_DISTRICT %in% input$selected_districts & 
                   between(INCIDENTTIME, as.POSIXct(input$date_range[[1]]), as.POSIXct(input$date_range[[2]])) &
                   type %in% input$selected_hierarchy)
    })
    
    output$DT <- renderDataTable({    #Datatable code
        DT::datatable(blotter_subset(), options = list(scrollY = "300px", scrollX = T))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
