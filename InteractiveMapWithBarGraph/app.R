# Map with navigation bar and bar graph

## Load Packages

library(rgdal)     # R wrapper around GDAL/OGR
library(dplyr)     # Data manipulation  
library(shiny)     # Make the app     
library(leaflet)   # Interactive maps with pop-ups 
library(ggplot2)   # Generate plots
library(ggthemes)  # Plot themes
require(sp)        # Used to merge shape file and data frame
library(plotly)

## The Shape File

# Path to use when playing
#Indonesia <- readOGR(dsn = "/Users/User/Desktop/R Prac/UNICEF Project/exploring_shiny/InteractiveMapWithBarGraph/IDN_adm1", "IDN_adm1") # Shapefile 
# Path to use when publishing
Indonesia <- readOGR(".", "IDN_adm1") # Shapefile

## What are the map boundaries?

bnds <- Indonesia@bbox # The boundaries of the shapefile

long <- (bnds[1,1]-bnds[1,2]/2) + bnds[1,1] # The central longitude of the map

lat <- (bnds[2,1]-bnds[2,2]/2) - bnds[2,1] # The central latitude of the map

## The Province Data

# Path to use when playing
 df = read.csv(file = "/Users/User/Desktop/R Prac/UNICEF Project/exploring_shiny/InteractiveMapWithBarGraph/SDG.csv", sep=",") # SDG Data

# Path to use when publishing
#df = read.csv(file = "SDG.csv", sep=",")

SDG = df[,colSums(is.na(df)) != nrow(df)] # Remove empty columns

# The App


ui <- navbarPage(theme = "bootstrap.css",
                # Title       
                titlePanel(""),
                # Tab panels for inputs ----
                tabPanel(
                         # SDG Input
                         selectInput("select1", "", 
                                     choices = c("Goal 1: No Poverty" = 1, 
                                                 "Goal 2: Zero Hunger" = 2, 
                                                 "Goal 3: Good Health and Well-Being" = 3, 
                                                 "Goal 4 : Quality Education" = 4, 
                                                 "Goal 5: Gender Equality" = 5, 
                                                 "Goal 6: Clean Water and Sanitation" = 6, 
                                                 "Goal 8: Decent Work and Economic Growth" = 8, 
                                                 "Goal 13: Climate Action" = 13, 
                                                 "Goal 16: Peace Justice and Strong Institutions" = 16), 
                                     selected = "Goal 1: No Poverty")
                 ),
                 tabPanel(
                         # Indicator Innput 
                         uiOutput("select2")
                 ),
                 
                
                
                # Map output
                 column(8, leafletOutput("mymap")),
                # Bar Graph
                column(4, plotOutput("plot1"))        
)



server <- function(input, output){
        
        # Generate reactive list of Indicators for the selected SDG               
        output$select2 <- renderUI({
                choice <-  unique(SDG[SDG$SDG.Goal %in% input$select1, "Indicator"]) # Subset Indicator for selected SDG
                selectInput("select2", # Reactive input name
                            "", # No label for the tab panel
                            choices = choice, # Indicator options for the selected SDG
                            selected = choice[1]) # Select the first Indicator for the selected SDG
        })
     
        # Generate reactive data frame with selected SDG and Indicator
        SDG1 <-reactive({
                SDG1 <- SDG %>% 
                        filter(SDG$SDG.Goal %in% input$select1, #Subset selected SDG from data frame
                               SDG$Indicator %in% input$select2 # Subset selected Indicators from the resulting data frame
                        )
                
                SDG1
        })
        
        # Merge the shape file and the dataframe
        #oo@data <- Indonesia@data %>%
        # left_join(Indonesia@data, SDG1(), by=c("ID_1"))
        
        #oo <- merge(Indonesia, SDG1(), by="ID_1") # Merge the shape file and the dataframe
        
        UNICEF <- c('#DDA63A','#DB8E3E', '#991D2E', '#00689D') #Gold, Orange, Red, Blue
        
        no_classes <- 4 # Number of data ranges
        
        observe({
                
                SDGoo <- SDG1()
                
                SDGp <- SDGoo %>% # Select the variables that apply to the provinces 
                        select("SDG" = "SDG.Goal", "Indicator" = "Indicator", "Value" = "Value", "Province" = "Province") # Rename the variables to make them easier to work with and select the ones that apply to our needs
                
                SDGdf <- SDGp %>%
                        filter(Province != "All Indonesia") # Remove all national level data rows
                
                idName <- SDGdf %>% #Bind province names from the dataset to their numeric equivalents in the shapefile- ID_1
                        mutate(id = ifelse(Province == "Aceh", 0, ifelse(Province == "Bali", 1, ifelse(Province == "Banten", 3, ifelse(Province == "Bengkulu", 4, ifelse(Province == "DI Yogyakarta", 33, ifelse(Province == "DKI Jakarta", 7, ifelse(Province == "Gorontalo", 5, ifelse(Province == "Jambi", 8, ifelse(Province == "Jawa Barat", 9, ifelse(Province == "Jawa Tengah", 10, ifelse(Province == "Jawa Timur", 11, ifelse(Province == "Kalimantan Barat", 12, ifelse(Province == "Kalimantan Selatan", 13, ifelse(Province == "Kalimantan Tengah", 14, ifelse(Province == "Kalimantan Timur", 15, ifelse(Province == "Kalimantan Utara", 16, ifelse(Province == "Kepulauan Bangka Belitung", 2,  ifelse(Province == "Kepulauan Riau", 17, ifelse(Province == "Lampung", 18, ifelse(Province == "Maluku", 20, ifelse(Province == "Maluku Utara", 19, ifelse(Province == "Nusa Tenggara Barat", 21,  ifelse(Province == "Nusa Tenggara Timur", 22, ifelse(Province == "Papua", 23, ifelse(Province == "Papua Barat", 6, ifelse(Province == "Riau", 24, ifelse(Province == "Sulawesi Barat", 25,  ifelse(Province == "Sulawesi Selatan", 26, ifelse(Province == "Sulawesi Tengah", 27, ifelse(Province == "Sulawesi Tenggara", 28, ifelse(Province == "Sulawesi Utara", 29, ifelse(Province == "Sumatera Barat", 30, ifelse(Province == "Sumatera Selatan", 31, 32)))))))))))))))))))))))))))))))))) # "Sumatera Utara" left out of ifelse statement!
                
                idName1 <- idName%>% # In previous iteration, numbers were from 0-33, now they are 1-34 so add 1 to each of the numbers to match
                        mutate(ID_1 = id + 1)
                
                oo <- merge(Indonesia, idName1, by="ID_1") # Merge the shape file and the dataframe
                
                oo$Value <- as.numeric(as.character(oo$Value)) # Coerce Value into numeric data type
                
                oo2 <- cbind.data.frame(Province = oo$Province, Value = oo$Value)
                
                output$plot1 <- renderPlot({
                        p <- ggplot(data = oo2, 
                                    aes(x = reorder(Province, Value), y = Value)) +
                                geom_col(fill = c("#1CABE2"))+ 
                                labs(x = "Province", y= "Value") +
                                coord_flip() + 
                                theme_hc() +
                                geom_text(aes(label = Value), size = 3, fontface = 2, hjust = 1, col = "white")
                        
                        p + theme(legend.title=element_blank(),
                                  axis.ticks.y = element_blank(),
                                  plot.background = element_rect(fill = "grey88"),
                                  panel.background = element_rect(fill = "grey88"),
                                  legend.background = element_rect(fill = "grey88"))                
                        })
                
                labels <- sprintf( # Define what should come up in the pop-up
                        "<strong>%s</strong><br/>%g &#37", # Province name in bold, enter, value with a % sign after
                        oo$Province, oo$Value # Calling the province and the value
                ) %>% lapply(htmltools::HTML) # Applying HTML
                
                bins <- quantile(oo$Value, 
                                 probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE) # Generate quantile ranges to map
                
                pal <- colorBin(UNICEF, domain = oo$Value, bins = bins, na.color = "#F1F1F1") # Define palette
                
                output$mymap <- renderLeaflet({
                        leaflet(data = oo) %>% 
                                addPolygons( # From the shape file, what to map
                                        fillColor = ~pal(Value), # choropleth colours
                                        weight = .5, # Thickness of province border lines
                                        opacity = 1, # Opacity of border lines
                                        color = "white", # Colour of province border lines
                                        dashArray = "", # Border line type, solid/dashed/etc.
                                        fillOpacity = 0.7, # Opacity of choropleth colors
                                        highlight = highlightOptions( # Pop-up options
                                                weight = 2, # Thickness of pop-up boundaries
                                                color = "#666", # Colour of pop-up boundaries
                                                dashArray = "", # Line type for pop-up boundaries
                                                fillOpacity = 0.7, # Opacity of pop-up boundaries 
                                                bringToFront = TRUE), # Bring the pop-up boundaries in front of the border lines?
                                        label = labels, # Labels defined above
                                        labelOptions = labelOptions( # Options for the text inside the pop-up
                                                style = list(
                                                        "font-weight" = "normal", 
                                                        padding = "3px 8px"), # Text style options
                                                textsize = "15px", # Text size
                                                direction = "auto"))%>% # Left aligned, starting at upper left
                                
                                addLegend( # Legend options
                                        pal = pal, # Previously defined palette
                                        values = ~Value, # Values from data frame 
                                        opacity = 0.7, # Opacity of legend
                                        title = NULL, # Title
                                        position = "bottomleft")%>% # Where the legend is positioned
                                
                                setView( # Define map boundaries
                                        lng = long, # Center on defined longitude
                                        lat = lat, # Center on defined latitude 
                                        zoom = 4) # Zoom in how close
                })
        })         
                
}


shinyApp(ui, server)


