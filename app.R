library(shiny)
library(readxl)
library(maps)
library(ggmap)
library(mapdata)
library(dplyr)
library(leaflet)

states = map_data("state")

mn = subset(states, region == "minnesota")

counties = map_data("county")

mn_county = subset(counties, region == "minnesota")

data1 = as.data.frame(read_xlsx('MUDAC_data_Problem1.xlsx',sheet = 'data',skip = 2)[1:50,])

data2 = data1 %>% 
    group_by(Long, Lat) %>% 
    arrange(Long)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Change in TSS based on Watershed Characteristics"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("var", "Characteristic:", choices = names(data1)[-c(1,17:20)],
                        selected = "Cropland"),
            sliderInput("threshold",
                        "Threshold:",
                        min = 0,
                        max = 1,
                        value = 0.2)
        ),
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mymap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({

        getColor <- function(threshold) {
            sapply(data1$`Total Suspended Solid (TSS)`, function(`Total Suspended Solid (TSS)`) {
                if(`Total Suspended Solid (TSS)` <= 75) {
                    "green"
                } else if(`Total Suspended Solid (TSS)` > 75 & `Total Suspended Solid (TSS)` <= 175) {
                    "orange"
                } else {
                    "red"
                } })
        }
        
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(data1)
        )
        
        pal <- colorNumeric(c("green",  "orange", "red"), 0:255)
        
        leaflet(data1) %>% addTiles() %>%
            addAwesomeMarkers(~ Long, ~ Lat , icon=icons, label = ~ `Watershed/Monitoring Site Location`) %>% 
            addPolylines(data = data2[data2[,input$var] > input$threshold[1],], lng = ~ Long, lat = ~ Lat) %>% 
            addLegend(position = "bottomright", pal = pal, values = c(0,75,175,250)) 
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
