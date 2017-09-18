# The packages
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Data Editing 
#SDG Data
df = read.csv(file = "/Users/User/Desktop/R Prac/UNICEF Project/DynamicTableAceh/SDG.csv", sep=",")

#Remove empty columns from dataset 
SDG = df[,colSums(is.na(df)) != nrow(df)]

SDGdf1 <- SDG[1:4112,] %>%
        mutate(Level = ifelse(Province == "All Indonesia", "National", 
                              "Provincial"))

SDGdf2 <- SDGdf1 %>%
        filter(Province == "Aceh")

SDGprov <- SDGdf2%>%
        select("SDG" = "SDG.Goal", "Indicator" = "Indicator", "Value" = "Value", "Province" = "Province", "Age" = "Age.Range..i.e..0.24.M..15.60.Y.", "Year" = "Year", "Source" = "Source")



ui <- fluidPage(theme = "bootstrap.css",
        # App title ----
        titlePanel("Downloadable Data for Aceh Province"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        # SDG Input
                        selectInput("select1", "Select Sustainable Development Goal:", choices = c(
                                "Goal 1: No Poverty" = 1, "Goal 2: Zero Hunger" = 2, "Goal 3: Good Health and Well-Being" = 3, "Goal 4 : Quality Education" = 4, "Goal 5: Gender Equality" = 5, "Goal 6: Clean Water and Sanitation" = 6, "Goal 8: Decent Work and Economic Growth" = 8, "Goal 13: Climate Action" = 13, "Goal 16: Peace Justice and Strong Institutions" = 16), selected = "Goal 1: No Poverty"),
                        
                        #Checkbox output 
                        uiOutput("checkbox1"),
                        
                        # Button
                        downloadButton("downloadData", "Download")
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        #Checkbox output 
                        #uiOutput("checkbox2"),
                        
                        # Table output
                        DT::dataTableOutput("SDGtable1")
                )
        )
)

# Server function
server <- function(input, output){
        
        # Generate the checkbox
        output$checkbox1 <- renderUI({
                choice <-  unique(SDGprov[SDGprov$SDG %in% input$select1, "Indicator"])
                checkboxGroupInput("checkbox1","Select Indicator:", choices = choice, 
                                   selected = choice[1])
        })
        
        # Generate the checkbox
        #output$checkbox2 <- renderUI({
        #       choiceLevel <-  unique(SDGprov[SDGprov$Indicator %in% input$checkbox1, "Level"])
        #      checkboxGroupInput("checkbox2","Select Level:", choices = choiceLevel,
        #                        selected = choiceLevel[1])
        #    })
        
        # Generate data frame
        df <-reactive({
                SDGprov %>% 
                        filter(SDGprov$SDG %in% input$select1, 
                               SDGprov$Indicator %in% input$checkbox1#,
                               #SDGprov$Level %in% input$checkbox2
                        )
                
        })
        
        # Generate data table 
        output$SDGtable1 <- DT::renderDataTable({
                DT::datatable(df())
        })
        
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
                filename = function() {
                        paste(input$dataset, ".csv", sep = "")
                },
                content = function(file) {
                        write.csv(datasetInput(), file, row.names = FALSE)
                })
        
}
shinyApp(ui, server)
