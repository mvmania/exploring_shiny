# National Data Table

# The packages
library(shiny)          # Generates shiny app
library(DT)             # Generates dynamic tables
library(ggplot2)        # Geberates plots
library(dplyr)          # Data manipulation 

# Data Editing 
#SDG Data

# Path to use when editing 
#df = read.csv(file = "/Users/User/Desktop/R Prac/UNICEF Project/exploring_shiny/DynamicTableApp/SDG.csv", sep=",")

# Path to use when publishing
df = read.csv(file = "SDG.csv", sep=",")

# Remove empty columns from dataset 
SDG = df[,colSums(is.na(df)) != nrow(df)]

SDGdf1 <- SDG[1:4112,] %>% # Create a new variable that describes if data National or Provincial level data
        mutate(Level = ifelse(Province == "All Indonesia", 
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
ui <- fluidPage(theme = "bootstrap.css", #Styling CSS
        # App title ----
        titlePanel("Downloadable National Data"),
        
        # Sidebar layout with input and output definitions
        sidebarLayout(
                
                # Sidebar panel for inputs 
                sidebarPanel(
                        # SDG Input
                        selectInput("select1", # Name of input
                                    "Select Sustainable Development Goal:", # Label for the SDG dropdown 
                                    choices = c( # The options that appear in the dropdown 
                                            "Goal 1: No Poverty" = 1, 
                                            "Goal 2: Zero Hunger" = 2, 
                                            "Goal 3: Good Health and Well-Being" = 3, 
                                            "Goal 4 : Quality Education" = 4, 
                                            "Goal 5: Gender Equality" = 5, 
                                            "Goal 6: Clean Water and Sanitation" = 6, 
                                            "Goal 8: Decent Work and Economic Growth" = 8, 
                                            "Goal 13: Climate Action" = 13, 
                                            "Goal 16: Peace Justice and Strong Institutions" = 16), 
                                    selected = "Goal 1: No Poverty"), # The option initially selected in the UI
                        
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
                              extensions = c( # Extensions allow for different things to occur in the UI, has to be matched with compatible 'options' in order to appear 
                                      'ColReorder', # Allows col to be reordered 
                                      'Buttons', # Allows buttons
                                      'Scroller'), # Allows scroller 
                              options = list(
                                      dom = 'Bfrtip', # Denotes where things appear in the UI, must have for buttons to appear! 
                                      colReorder = TRUE, # Give user ability to reorder the col 
                                      buttons = c('copy', # Copy the current table 
                                                  'pdf', # Export pdf of table, does not work with bars in table
                                                  'csv', # Export a CSV of the total table, stopped working when bars were added to the table
                                                  'excel', # Export an excel spreadsheet of the total table, does not show up!
                                                  'print', # Print table
                                                  'colvis'), # Generates button that allows the columns to appear to be selected or unselected- all columns appear in download! 
                                      searchHighlight = TRUE, # Cause results from search filter to highlight
                                      deferRender = FALSE, # For use with large data sets to determine when an UI action is rendered 
                                      scrollY = 475, # Verticle scrolling, numeric value determines height of table- 475 displays 13 row with the current CSS  
                                      scroller = TRUE, # Should the scroller appear
                                      columnDefs = list(list(targets = c(1,2), visible = FALSE)))) #%>% # Make SDG and Indicator columns (cols 1 and 2) invisible, could not get to work with c("SDG", "Indicator") or oo$Indicator
                                      #formatStyle("Value", # Format around the value column
                                       #         background = styleColorBar(range(oo$Value), '#1CABE2'), # Generate bars in value cells
                                        #        backgroundSize = '98% 88%', # Maximum size bars can take in cells
                                         #       backgroundRepeat = 'no-repeat', # Background unique to each cell in column
                                          #      backgroundPosition = 'center', # Bars at center hight in cells
                                           #     fontWeight = 'bold') #Make values bold so that they are easier to see with bars
        })
        })
        
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
                filename = function() {
                        paste(input$dataset, "SDGdf.csv", sep = ",")
                        }, # Generate a .csv file
                content = function(file) {
                        write.csv(df(), file, na="")
                }) # Write .csv to a particular directory 
        
}
shinyApp(ui, server)
