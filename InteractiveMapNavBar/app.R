# Map with navigation bar

## Load Packages

library(rgdal)     # R wrapper around GDAL/OGR
library(dplyr)     # Data manipulation  
library(shiny)     # Make the app     
library(leaflet)   # Interactive maps with pop-ups          
require(sp)        # Used to merge shape file and data frame

## The Shape File
Indonesia <- readOGR(".", "IDN_adm1") # Shapefile 

## The Province Data

df = read.csv(file = "SDG_Millie_vb_edit_provinces_v2_SuCor.csv", sep=",") # Original table

SDG = df[,colSums(is.na(df)) != nrow(df)] # Remove empty columns

# Generate some new columns 
SDGdf <- SDG[1:4112,] %>%
        mutate(Level = ifelse(Province == "All Indonesia", # Generate a column to distinguish national and provincial data
                              "National", 
                              "Provincial"))%>%
        mutate(Quintile = ifelse(# Generate a column with wealth categories to match those found in the report on page 25
                Wealth.Quintile..All.Poorest.Second.Middle.Fourth.Richest. == "Q5 (wealthiest)", "Richest 20%", 
                ifelse(Wealth.Quintile..All.Poorest.Second.Middle.Fourth.Richest. == "Q1 (poorest)", "Poorest 20%", 
                       ifelse(Wealth.Quintile..All.Poorest.Second.Middle.Fourth.Richest. == "Q2", "Second 20%", 
                              ifelse(Wealth.Quintile..All.Poorest.Second.Middle.Fourth.Richest. == "Q3", "Middle 20%", 
                                     ifelse(Wealth.Quintile..All.Poorest.Second.Middle.Fourth.Richest. == "Q4", "Fourth 20%", "All"))))))%>%
        mutate(Ed = ifelse(# Generate a column with education categories to match those found in the report on page 25
                Educational.attainment.of.household.head == "No school", "None", 
                ifelse(Educational.attainment.of.household.head == "Incomplete primary school (SD)/Islamic primary school (MI)", "Some Primary",
                       ifelse(Educational.attainment.of.household.head == "Incomplete primary school", "Some Primary",
                              ifelse(Educational.attainment.of.household.head == "Graduated primary school", "Primary",
                                     ifelse(Educational.attainment.of.household.head == "Graduated primary school (SD/MI)", "Primary", 
                                            ifelse(Educational.attainment.of.household.head == "Some secondary school", "Some Secondary",
                                                   ifelse(Educational.attainment.of.household.head == "Graduated junior secondary school (SMP/MTS)", "Junior Secondary",
                                                          ifelse(Educational.attainment.of.household.head == "Graduated from secondary school", "Secondary", 
                                                                 ifelse(Educational.attainment.of.household.head == "Graduated senior secondary school (SMA/MA)", "Senior Secondary",     
                                                                        ifelse(Educational.attainment.of.household.head == "More than secondary school", "Some Tertiary",
                                                                               ifelse(Educational.attainment.of.household.head == "Graduated university (D1-D3/PT)", "Tertiary", "All")))))))))))) 

# The App

ui <- fluidPage(
        ui <- navbarPage(theme = "bootstrap.css",
                         # Title       
                         titlePanel(""), # Blank title 
                         
                         
                         # Tab panels for inputs ----
                         tabPanel(
                                 # SDG Input
                                 selectInput("select1", # Reactive input name
                                             "Select a SDG:", # Input caption
                                             choices = c("Goal 1: No Poverty" = 1, # List of input values
                                                         "Goal 2: Zero Hunger" = 2, 
                                                         "Goal 3: Good Health and Well-Being" = 3, 
                                                         "Goal 4 : Quality Education" = 4, 
                                                         "Goal 5: Gender Equality" = 5, 
                                                         "Goal 6: Clean Water and Sanitation" = 6, 
                                                         "Goal 8: Decent Work and Economic Growth" = 8, 
                                                         "Goal 13: Climate Action" = 13, 
                                                         "Goal 16: Peace Justice and Strong Institutions" = 16), 
                                             selected = "Goal 1: No Poverty") # Pre-selected input for UI
                         ),
                         tabPanel(
                                 # Indicator Input 
                                 uiOutput("select2") # Reactive input name
                                 
                                 
                         ),
                         
                         # The rest of UI will fall below the tab panel 
                        
                         # Map output
                         column(12, # Map width 
                         leafletOutput("mymap") # Reactive map 
                        )
        )
        )


server <- function(input, output){
        
        # Generate reactive list of Indicators for the selected SDG               
        output$select2 <- renderUI({
                choice <-  unique(SDGdf[SDGdf$SDG.Goal %in% input$select1, "Indicator"]) # Subset Indicator for selected SDG
                selectInput("select2", # Reactive input name
                            "Select an Indicator:", # Title for dropdown 
                            choices = choice, # Indicator options for the selected SDG
                            selected = choice[1]) # Select the first Indicator for the selected SDG
        })
        
        # Generate reactive data frame with selected SDG and Indicator
        SDGselect <-reactive({
                SDGselect <- SDGdf %>% 
                        filter(SDGdf$SDG.Goal %in% input$select1, #Subset selected SDG from data frame
                               SDGdf$Indicator %in% input$select2 # Subset selected Indicators from the resulting data frame
                        )
                
                SDGselect
        })
        
        observe({
                
                ## Put reactive table into a static name for further adaptation into map and tree map
                SDGoo <- SDGselect()
                
                ## Provincial Map
                
                # Sorting out relevant provincial data 
                
                SDGp <- SDGoo %>% 
                        filter(Level == "Provincial") # Select only the provential level data rows
                
                SDGpCol <- SDGp %>% # Select the variables that apply to the provinces 
                        select("SDG" = "SDG.Goal", "Indicator", "Data", "Value" = "Value", "Province" = "Province") # Rename the variables to make them easier to work with and select the ones that apply to our needs
                
                SDGpColNam <- SDGpCol %>% #Bind province names from the dataset to their numeric equivalents in the shapefile- ID_1
                        mutate(id = ifelse(Province == "Aceh", 0, ifelse(Province == "Bali", 1, ifelse(Province == "Banten", 3, ifelse(Province == "Bengkulu", 4, ifelse(Province == "DI Yogyakarta", 33, ifelse(Province == "DKI Jakarta", 7, ifelse(Province == "Gorontalo", 5, ifelse(Province == "Jambi", 8, ifelse(Province == "Jawa Barat", 9, ifelse(Province == "Jawa Tengah", 10, ifelse(Province == "Jawa Timur", 11, ifelse(Province == "Kalimantan Barat", 12, ifelse(Province == "Kalimantan Selatan", 13, ifelse(Province == "Kalimantan Tengah", 14, ifelse(Province == "Kalimantan Timur", 15, ifelse(Province == "Kalimantan Utara", 16, ifelse(Province == "Kepulauan Bangka Belitung", 2,  ifelse(Province == "Kepulauan Riau", 17, ifelse(Province == "Lampung", 18, ifelse(Province == "Maluku", 20, ifelse(Province == "Maluku Utara", 19, ifelse(Province == "Nusa Tenggara Barat", 21,  ifelse(Province == "Nusa Tenggara Timur", 22, ifelse(Province == "Papua", 23, ifelse(Province == "Papua Barat", 6, ifelse(Province == "Riau", 24, ifelse(Province == "Sulawesi Barat", 25,  ifelse(Province == "Sulawesi Selatan", 26, ifelse(Province == "Sulawesi Tengah", 27, ifelse(Province == "Sulawesi Tenggara", 28, ifelse(Province == "Sulawesi Utara", 29, ifelse(Province == "Sumatera Barat", 30, ifelse(Province == "Sumatera Selatan", 31, 32)))))))))))))))))))))))))))))))))) # "Sumatera Utara" left out of ifelse statement!
                
                SDGpFin <- SDGpColNam%>% # In previous iteration, numbers were from 0-33, now they are 1-34 so add 1 to each of the numbers to match
                        mutate(ID_1 = id + 1)
                
                oo <- merge(Indonesia, SDGpFin, by="ID_1") # Merge the shape file and the dataframe
                
                oo$Value <- as.numeric(as.character(oo$Value)) # Coerce Value into numeric data type
                
                strVal <- paste0("<strong><a href='http://unicef.org.dedi642.your-server.de/indonesia-provinces/'/>%s</a></strong><br/>", prettyNum(oo$Value,big.mark=','), sep='') #Add commas to large numbers
                
                labels <- sprintf(strVal, oo$Province, strVal
                ) %>% lapply(htmltools::HTML)
                
                
                # Boundaries for map
                bnds <- Indonesia@bbox # The boundaries of the shapefile
                
                long <- (bnds[1,1]-bnds[1,2]/2) + bnds[1,1] - 1.5 # The central longitude of the map
                
                lat <- (bnds[2,1]-bnds[2,2]/2) - bnds[2,1] # The central latitude of the map
                
                # Colour scheme for map
                UNICEF <- c('#DDA63A','#DB8E3E', '#991D2E', '#00689D') # Gold, Orange, Red, Blue
                
                no_classes <- 4 # Number of data ranges for key
                
                bins <- quantile(oo$Value, 
                                 probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE) # Generate quantile ranges to map
                
                pal <- colorBin(UNICEF, domain = oo$Value, bins = bins, na.color = "#F1F1F1") # Define palette
                
                # Generating map 
                output$mymap <- renderLeaflet({
                        leaflet(data = oo) %>% 
                                addPolygons( # From the shape file, what to map
                                        fillColor = ~pal(Value), # choropleth colours
                                        weight = .5, # Thickness of province border lines
                                        opacity = 1, # Opacity of border lines
                                        color = "white", # Colour of province border lines
                                        dashArray = "", # Border line type, solid/dashed/etc.
                                        fillOpacity = 1, # Opacity of choropleth colors
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
                                        opacity = 1, # Opacity of legend colour markers
                                        title = "Data Quintiles", # Title
                                        position = "bottomleft")%>% # Where the legend is positioned
                                
                                setView( # Define map boundaries
                                        lng = long, # Center on defined longitude
                                        lat = lat, # Center on defined latitude 
                                        zoom = 4.25) # Zoom in how close
                })
                
        })
}

shinyApp(ui, server) # Generate app


