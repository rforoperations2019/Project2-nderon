##Load required libraries
library(shiny)
library(leaflet)
library(sp)
library(sf)
library(rgdal)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)

##Data prep
blotter <- read.csv("Blotter_Data_Archive.csv") #Read blotter data
blotter$INCIDENTTIME <- strptime(x = as.character(blotter$INCIDENTTIME), #Convert INCIDENTTIME to datetime
                                 format = "%Y-%m-%dT%H:%M:%S")
blotter$type <- cut(blotter$HIERARCHY, c(-Inf, 9, 98, Inf), labels = c("Type 1 - Major Crime", "Type 2 - Minor Crime", "No Data or None")) #Convert hierarchy to bucketed factors
blotter_data <- blotter[blotter$INCIDENTTIME >= "2009-01-01" & blotter$INCIDENTTIME <= "2018-12-31" & blotter$X <= -78 & blotter$Y >= 39 & !is.na(blotter$type),]
blotter_data <- blotter_data[sample(1:nrow(blotter_data), 200),]
blotter_data$INCIDENTTIME <- as.POSIXct(blotter_data$INCIDENTTIME) #to avoid POSIXlt error
blotter_data$time <- format(blotter_data$INCIDENTTIME, "%H:%M:%S") #From datetime to character
blotter_data$time <- as.POSIXct(x = blotter_data$time, format = "%H:%M:%S") #From character to continuous time var
blotter_data$date <- format(blotter_data$INCIDENTTIME, "%m-%d")
blotter_data$date <- as.POSIXct(x = blotter_data$date, format = "%m-%d")
historic <- readOGR("City_Designated_Historic_Districts.geojson.json") #read historic district data
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
                tabPanel("Map", leafletOutput(outputId = "leaflet")),
                tabPanel("Time Trends", plotlyOutput(outputId = "time"), plotlyOutput(outputId = "date")),
                tabPanel("Data", dataTableOutput(outputId = "DT"), downloadButton(outputId = "downloadData", "Download"))
                
            )
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    #Reactive subset based on inputs
    blotter_subset <- reactive({    
        req(input$selected_districts, input$date_range, input$selected_hierarchy)
        filter(blotter_data, blotter_data$COUNCIL_DISTRICT %in% input$selected_districts & 
                   between(INCIDENTTIME, as.POSIXct(input$date_range[[1]]), as.POSIXct(input$date_range[[2]])) &
                   type %in% input$selected_hierarchy)
    })
    
    #Rendering main leaflet map
    output$leaflet <- renderLeaflet({
        leaflet() %>%
        addTiles(group = "OSM (default)") %>% #default basemap
        addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>% #extra basemaps
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        setView(lng = -79.9959, lat = 40.4406, zoom = 12) %>% #default view
        addLegend(values = blotter_subset()$type, colors = c("red", "orange", "grey"), labels = levels(blotter_subset()$type)) %>% #lengend
        addPolygons(data = historic, color = "red", fillColor = "#495D4E", opacity = 1, weight = 1, fillOpacity = 0.5, group = "Historic Districts") %>% #historic district polygons
        addLayersControl(
            baseGroups = c("OSM (default)", "Positron", "Toner"),
            overlayGroups = c("Historic Districts", "City Council"),
            options = layersControlOptions(collapsed = FALSE)
        )
    })    
    
    #observer to add markers
    observe({
        blot <- blotter_subset()
        
        #get color for markers
        getColor <- function(data) {
            sapply(data$type, function(type) {
                if(type == "Type 1 - Major Crime") {
                    "red"
                } else if(type == "Type 2 - Minor Crime") {
                    "orange"
                } else {
                    "grey"
                } })
        }
        
        #generate and add markers to leaflet map
        leafletProxy("leaflet", data = blot) %>%
            clearMarkers() %>%
            addAwesomeMarkers(
                lng = blot$X, 
                lat = blot$Y,
                icon = awesomeIcons(
                    icon = 'ios-close',
                    iconColor = 'black',
                    library = 'ion',
                    markerColor = getColor(blot)
                )
            )
    })
    
    #Reactive function returning city council district polygons based on inputs selected
    districts <- reactive({
        return(cc_districts[cc_districts@data$council_district %in% input$selected_districts,])
    })
    
    #LeafletProxy observer to add city council district polylines
    observe({
        dist <- districts()
        leafletProxy("leaflet", data = dist) %>%
        clearGroup("City Council") %>%
        addPolylines(data = dist, opacity = 1, weight = 1, group = "City Council") #city council district polylines
    })
    
    #Time of day plot
    output$time <- renderPlotly({   #Count of incidents by time of day plot
        blot <- blotter_subset()
        (ggplot(blot, aes(x = blot$time)) + 
             geom_freqpoly(stat = "bin", binwidth = 3600) +
             geom_freqpoly(stat = "bin", binwidth = 3600, aes(color = type)) +
             scale_x_datetime(date_label = "%H:%M") +
             labs(x = "Time of Day", y = "Count", title = "Count of Police Blotter Incidents by Time of Day")) %>%
        ggplotly(tooltip = c("y", "type"))
    })
    
    #Time of year plot
    output$date <- renderPlotly({   #Count of incidents by time of year plot
        blot <- blotter_subset()
        (ggplot(blotter_subset(), aes(x = blotter_subset()$date, fill = type)) + 
             geom_histogram(stat = "bin", bins = 12) +
             scale_x_datetime(date_label = "%m") +
             labs(x = "Time of Year", y = "Count", title = "Count of Police Blotter Incidents by Time of Year and Type")) %>%
            ggplotly(tooltip = c("y", "type"))
    })
    
    #Datatable code
    output$DT <- renderDataTable({    
        DT::datatable(blotter_subset(), options = list(scrollY = "300px", scrollX = T))
    })
    
    
    #Download Data button
    output$downloadData <- downloadHandler(
        filename = function(){
            "Blotter-data.csv"
        },
        content = function(file){
            write.csv(blotter_subset(), file, row.names = F)
        },
        contentType = "text/csv"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
