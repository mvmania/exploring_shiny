## Province Data Table

# The packages
library(shiny)          # Generates shiny app
library(DT)             # Generates dynamic tables
library(dplyr)          # Data manipulation 

# Path to data table
df = read.csv(file = "SDG_Millie_vb_edit_provinces_v2_SuCor.csv", sep=",") # Original table

SDG = df[,colSums(is.na(df)) != nrow(df)] # Remove empty columns

# Generate some new columns 
SDGprov <- SDG[1:4112,] %>%
        mutate(Level = ifelse(Province == "All Indonesia", # Generate a column to distinguish national and provincial data
                              "National", 
                              "Provincial"))%>%
        filter(Level == "Provincial")%>% # Subset out rows with provincial data
        select("SDG" = "SDG.Goal", # Select columns with provincial data
               "Indicator", 
               "Value", 
               "Province", 
               "Age" = "Age.Range..i.e..0.24.M..15.60.Y.", 
               "Year", 
               "Source")

## The app
ui <- navbarPage(theme = "bootstrap.css",
                 # Title       
                 titlePanel(""), # Blank title 
                 
                 # Tab panels for inputs ----
                 tabPanel(
                         # SDG Input
                         selectInput("select1", # Reactive input name
                                     "Select One or More Sustainable Devlopment Goals:", # Input caption
                                     choices = c("Goal 1: No Poverty" = 1, # Make the SDG numbers appear with their titles in the UI
                                                 "Goal 2: Zero Hunger" = 2, 
                                                 "Goal 3: Good Health and Well-Being" = 3, 
                                                 "Goal 4 : Quality Education" = 4, 
                                                 "Goal 5: Gender Equality" = 5, 
                                                 "Goal 6: Clean Water and Sanitation" = 6, 
                                                 "Goal 13: Climate Action" = 13, 
                                                 "Goal 16: Peace Justice and Strong Institutions" = 16), 
                                     selected = "Goal 1: No Poverty", # Pre-selected input for UI- this does not work with multiple = TRUE
                                     multiple = TRUE) # Can the user select multiple options
                 ),
                 tabPanel(
                         # Indicator Input 
                         uiOutput("select2") # Reactive input name
                 ),
                 
                 # The rest of UI will fall below the tab panel 
                 
                 # Table output
                 DT::dataTableOutput("SDGtable1"), # Reactive table name

                 # Button
                 downloadButton("downloadData", "Download"),
                                  
                 # Message
                 wellPanel( # Box below the table with key information 
                         h5("Unfortunately, no provincial level data is available for Goal 8: Decent Work and Economic Growth.")
                 )

)        

server <- function(input, output){
        
        # Generate reactive list of Indicators for the selected SDG               
        output$select2 <- renderUI({
                choice <-  unique(SDGprov[SDGprov$SDG %in% input$select1, "Province"]) # Subset Province for selected SDG
                selectInput("select2", # Reactive input name
                            "Select a Province:", # Title for dropdown 
                            choices = choice, # Indicator options for the selected SDG
                            selected = choice[1] # Preselect the first option 
                )
        })
        
        # Generate reactive data frame with selected SDG and Indicator
        SDGselect <-reactive({
                SDGselect <- SDGprov %>% 
                        filter(SDGprov$SDG %in% input$select1, #Subset selected SDG from data frame
                               SDGprov$Province %in% input$select2 # Subset selected Indicators from the resulting data frame
                        )
                
                SDGselect
        })
        
        # Generate data table 
        output$SDGtable1 <- DT::renderDataTable({
                DT::datatable(SDGselect(), # Reactive table 
                              extensions = c( # Extensions allow for different things to occur in the UI, has to be matched with compatible 'options' in order to appear 
                                      'ColReorder', # Allows col to be reordered 
                                      'Buttons'), # Allows buttons
                              options = list(
                                      dom = 'Bfrtip', # Denotes where things appear in the UI, must have for buttons to appear! 
                                      colReorder = TRUE, # Give user ability to reorder the col 
                                      buttons = c('copy', # Copy the current table 
                                                  'print', # Print table
                                                  'colvis'), # Generates button that allows the columns to appear to be selected or unselected- all columns appear in download! 
                                      searchHighlight = TRUE)) # Cause results from search filter to highlight
        })
        
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
                filename = function() { # Generate a .csv file
                        paste(input$dataset, "SDGprov.csv", sep = ",")
                }, 
                content = function(file) { # Write .csv to a particular directory
                        write.csv(SDGselect(), file, na="")
                })  
               
}
shinyApp(ui, server) # Generate app  
