# National Data Table

# The packages
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Data Editing 
#SDG Data

# Path to use when editing 
#df = read.csv(file = "/Users/User/Desktop/R Prac/UNICEF Project/exploring_shiny/DynamicTableApp/SDG.csv", sep=",")

# Path to use when publishing
df = read.csv(file = "SDG.csv", sep=",")

# Remove empty columns from dataset 
SDG = df[,colSums(is.na(df)) != nrow(df)]

SDGdf1 <- SDG[1:4112,] %>%
        mutate(Level = ifelse(Province == "All Indonesia", "National", 
                              "Provincial"))
SDGdf2 <- SDGdf1 %>%
        filter(Level == "National")

SDGdf <- SDGdf2 %>%
        select("SDG" = "SDG.Goal", "Indicator" = "Indicator", "Value" = "Value","Gender" = "Gender..All.Male.Female.", "Geography" = "Geography..All.Urban.Rural.", "Disability" = "Disability..All.With.Without.", "WealthQuintile" = "Wealth.Quintile..All.Poorest.Second.Middle.Fourth.Richest.", "Education" = "Educational.attainment.of.household.head", "Age" = "Age.Range..i.e..0.24.M..15.60.Y.", "Year" = "Year", "Source" = "Source")

# The app
ui <- fluidPage(theme = "bootstrap.css",
        # App title ----
        titlePanel("Downloadable National Data"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        # SDG Input
                        selectInput("select1", 
                                    "Select Sustainable Development Goal:", 
                                    choices = c(
                                            "Goal 1: No Poverty" = 1, 
                                            "Goal 2: Zero Hunger" = 2, 
                                            "Goal 3: Good Health and Well-Being" = 3, 
                                            "Goal 4 : Quality Education" = 4, 
                                            "Goal 5: Gender Equality" = 5, 
                                            "Goal 6: Clean Water and Sanitation" = 6, 
                                            "Goal 8: Decent Work and Economic Growth" = 8, 
                                            "Goal 13: Climate Action" = 13, 
                                            "Goal 16: Peace Justice and Strong Institutions" = 16), 
                                    selected = "Goal 1: No Poverty"),
                        
                        #Checkbox output 
                        uiOutput("checkbox1"),
                        
                        # Button
                        downloadButton("downloadData", "Download")
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        # Table output
                        DT::dataTableOutput("SDGtable1")
                )
        )
)

# Server function
server <- function(input, output){
        
        # Generate the checkbox for Indicators
        output$checkbox1 <- renderUI({
                
                choice <-  unique(SDGdf[SDGdf$SDG %in% input$select1, "Indicator"]) # Make a list of unique indicators for the selected SDG
                
                checkboxGroupInput("checkbox1","Select Indicator:", choices = choice, # Make a checkbox list of all of the Indicators
                                   selected = choice[1]) # The UI should preselect the first indicator on the list
        })
        
        # Generate data frame
        df <-reactive({
                SDGdf %>% 
                        filter(SDGdf$SDG %in% input$select1, #Select SDG from data frame
                               SDGdf$Indicator %in% input$checkbox1 #Select resulting Indicators from data frame
                               )
                
        })
        
        observe({
                #Generate colour code and breaks
                UNICEF <- c('#DDA63A','#DB8E3E', '#991D2E', '#00689D', '#000000') #Gold, Orange, Red, Blue, other had to 
                
                no_classes <- 4 # Number of data ranges
                
                oo <- df() # Variable for reactive data
                
                oo$Value <- as.numeric(as.character(oo$Value)) # Coerce Value into numeric data type
                
                bins <- quantile(oo$Value, 
                                 probs = seq(0, 1, length.out = no_classes), na.rm = TRUE) # Generate quantile ranges for bars in table
                
        # Generate data table 
        output$SDGtable1 <- DT::renderDataTable({
                DT::datatable(oo, 
                              extensions = 'ColReorder','Buttons', 
                              options = list(
                                      dom = 'Bfrtip',
                                      colReorder = TRUE,
                                      buttons = c('copy', 'csv', 'excel', 'print'),
                                      searchHighlight = TRUE, # Cause results from search filter to highlight
                                      columnDefs = list(list(targets = c(1,2), visible = FALSE)))) %>% # Make SDG and Indicator columns invisible
                                      formatStyle("Value", # Format around the value column
                                                background = styleColorBar(range(oo$Value), '#1CABE2'), # Generate bars in value cells
                                                backgroundSize = '98% 88%', # Maximum size bars can take in cells
                                                backgroundRepeat = 'no-repeat', # Background unique to each cell in column
                                                backgroundPosition = 'center', # Bars at center hight in cells
                                                fontWeight = 'bold') #Make values bold so that they are easier to see with bars
        })
        })
        
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
                filename = function() {
                        paste(input$dataset, ".csv", sep = "")
                }, # Generate a .csv file
                content = function(file) {
                        write.csv(datasetInput(), file, row.names = FALSE)
                }) # Write .csv to a particular directory 
        
}
shinyApp(ui, server)
