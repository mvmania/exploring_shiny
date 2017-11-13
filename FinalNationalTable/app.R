## Final National Table

# The packages
library(shiny)          # Generates shiny app
library(DT)             # Generates dynamic tables
library(dplyr)          # Data manipulation 

# Data Editing 

# Path to data
df = read.csv(file = "SDG_Millie_vb_edit_provinces_v2_SuCor.csv", sep=",")

SDG = df[,colSums(is.na(df)) != nrow(df)] # Remove empty columns from dataset

SDGdf1 <- SDG[1:4112,] %>% 
        mutate(Level = ifelse(Province == "All Indonesia", # Create a new variable that describes if data is National or Provincial level data 
                              "National", 
                              "Provincial")) %>%
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

SDGdf <- SDGdf1 %>% # Subset the National data only
        filter(Level == "National")%>%
        select("SDG" = "SDG.Goal", # Select the variables that apply to the National data
               "Indicator", 
               "Value", 
               "Sex" = "Gender..All.Male.Female.", 
               "Residence" = "Geography..All.Urban.Rural.", 
               "Disability" = "Disability..All.With.Without.", 
               "WealthQuintile" = "Quintile", 
               "Education" = "Ed", 
               "Age" = "Age.Range..i.e..0.24.M..15.60.Y.", 
               "Year", 
               "Source")

# The app
ui <- fluidPage(theme = "bootstrap.css",
                # App title ----
                titlePanel("Downloadable National Data"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                                # SDG Input
                                selectInput("select1", # Input 
                                            "Select Sustainable Development Goal:", # Input Title 
                                            choices = c(
                                                    "Goal 1: No Poverty" = 1, # Make the SDG numbers appear with their titles in the UI
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
                
                choice <-  unique(SDGdf[SDGdf$SDG %in% input$select1, "Indicator"]) # For the selected SDG, subset the Indicators
                
                checkboxGroupInput("checkbox1", # Reactive input
                                   "Select Indicator:", # Input title 
                                   choices = choice, # What names go into the input list
                                   selected = choice[1]) # When the table comes up, the first option in the list should be selected
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
                
                oo$Value <- as.numeric(as.character(oo$Value)) # Coerce Value column into numeric data type
                
                # Generate data table 
                output$SDGtable1 <- DT::renderDataTable({
                        DT::datatable(oo, # Reactive table input
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
        })
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
                filename = function() { # Generate a .csv file
                        paste(input$dataset, "SDGdf.csv", sep = ",")
                }, 
                content = function(file) { # Write .csv to a particular directory
                        write.csv(df(), file, na="")
                })  
        
}
shinyApp(ui, server) # Run shiny app