## Final National Table

# The packages
library(shiny)          # Generates shiny app
library(DT)             # Generates dynamic tables
library(ggplot2)        # Geberates plots
library(dplyr)          # Data manipulation 

# Data Editing 

# Path to data
df = read.csv(file = "SDG_Millie_vb_edit.csv", sep=",")

#Remove empty columns from dataset 
SDG = df[,colSums(is.na(df)) != nrow(df)]

SDGdf1 <- SDG[1:4112,] %>%
        mutate(Level = ifelse(Province == "All Indonesia", "National", 
                              "Provincial"))
SDGdf2 <- SDGdf1 %>%
        filter(Level == "National")

SDGdf <- SDGdf2 %>%
        select("SDG" = "SDG.Goal", 
               "Indicator" = "Indicator", 
               "Value" = "Value",
               "Gender" = "Gender..All.Male.Female.", 
               "Geography" = "Geography..All.Urban.Rural.", 
               "Disability" = "Disability..All.With.Without.", 
               "WealthQuintile" = "Wealth.Quintile..All.Poorest.Second.Middle.Fourth.Richest.", 
               "Education" = "Educational.Attainment.of.Household.Head", 
               "Age" = "Age.Range..i.e..0.24.M..15.60.Y.", 
               "Year" = "Year", 
               "Source" = "Source")

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
                                DT::dataTableOutput("SDGtable1"),
                                
                                wellPanel( # Box below the table with key information 
                                        h5(tags$strong("Table Key:")),
                                        h6("Gender: Gender (All/Male/Female)"),
                                        h6("Geography: Geography (All/Urban/Rural)"),
                                        h6("Disability: Disability (All/With/Without)"),
                                        h6("WealthQuintile: Wealth Quintile (All/Poorest/Second/Middle/Fourth/Richest)"),
                                        h6("Education: Educational Attainment of Household Head"),
                                        h6("Age: Age Range (i.e. 0-24 M, 15-60 Y)"),
                                        h6("SDG: Sustainable Development Goal")
                                )
                        )
                )
)

# Server function
server <- function(input, output){
        
        # Generate the checkbox for Indicators
        output$checkbox1 <- renderUI({
                
                choice <-  unique(SDGdf[SDGdf$SDG %in% input$select1, "Indicator"])
                
                checkboxGroupInput("checkbox1",
                                   "Select Indicator:", 
                                   choices = choice, 
                                   selected = choice[1])
        })
        
        # Generate data frame
        df <-reactive({
                SDGdf %>% 
                        filter(SDGdf$SDG %in% input$select1, #Select SDG from data frame
                               SDGdf$Indicator %in% input$checkbox1 #Select resulting Indicators from data frame
                        )
                
        })
        
        observe({
                
                oo <- df() # Variable for reactive data
                
                oo$Value <- as.numeric(as.character(oo$Value)) # Coerce Value into numeric data type
                
                # Generate data table 
                output$SDGtable1 <- DT::renderDataTable({
                        DT::datatable(oo)
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