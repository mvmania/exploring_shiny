library(shiny)     # Make the app 
library(rgdal)     # R wrapper around GDAL/OGR
library(dplyr)     # Data manipulation 
library(ggplot2)
library(DT)
library(scales)
require(sp)        # Used to merge shape file and data frame


df = read.csv(file = "SDG_Millie_vb_edit_provinces_v2_SuCor.csv", sep=",")

SDG = df[,colSums(is.na(df)) != nrow(df)] # Remove empty columns

SDGg <- SDG %>% # Select the variables 
  select("SDG" = "SDG.Goal", "Indicator" = "Indicator", "Value" = "Value", 
         "Province" = "Province","Gender" = "Gender", 
         "Geography"="Geography","Wealth"="Wealth",
         "Education_of_Head_of_Household"="Education"
  ) # Rename the variables to make them easier to work with and select the ones that apply to our needs

SDGdf <- SDGg %>%
  filter(Province == "All Indonesia") # Remove all provincial level data rows
idName1 <- SDGdf

ui <- fluidPage(
  ui <- fluidPage(theme = "bootstrap.css",
                  # App title ----
                  titlePanel("Disaggregated Data "),
                  
                  # Sidebar layout with input and output definitions ----
                  sidebarLayout(
                    
                    # Sidebar panel for inputs ----
                    sidebarPanel(
                      # SDG Input
                      
                      selectInput("select1", "Select Sustainable Development Goal:", choices = c(
                        "Goal 1: No Poverty" = 1, "Goal 2: Zero Hunger" = 2, "Goal 3: Good Health and Well-Being" = 3, "Goal 4 : Quality Education" = 4, "Goal 5: Gender Equality" = 5, "Goal 6: Clean Water and Sanitation" = 6, "Goal 8: Decent Work and Economic Growth" = 8, "Goal 13: Climate Action" = 13, "Goal 16: Peace Justice and Strong Institutions" = 16), selected = "Goal 1: No Poverty"),
                      #Checkbox output 
                      uiOutput("select2"),
                      
                      selectInput("disaggregation","Disaggregated by",choices=colnames(SDGg)[5:8])
                    ),
                    # Main panel for displaying outputs ----
                    mainPanel(
                      # barplot output
                      plotOutput("disaggregationPlot"),
                      dataTableOutput("testtable")
                    )
                  )
  )
)

server <- function(input, output){
  
  
  # Generate reactive list of Indicators for the selected SDG               
  output$select2 <- renderUI({
    choice <-  unique(idName1[idName1$SDG %in% input$select1, "Indicator"]) # Subset Indicator for selected SDG
    selectInput("select2", # Reactive input name
                "Select Indicator:", # Title for dropdown 
                choices = choice, # Indicator options for the selected SDG
                selected = choice[1]) # Select the first Indicator for the selected SDG
  })
  # Generate reactive data frame with selected SDG and Indicator
  oo1 <-reactive({
      oo1<-idName1 %>% 
      filter(SDG %in% input$select1, #Subset selected SDG from data frame
             Indicator %in% input$select2 # Subset selected Indicators from the resulting data frame
      ) %>%
      select(Indicator,Value,input$disaggregation)
      oo1<-filter(oo1,oo1[3]!="All")
  })
  
  
  output$testtable<-renderDataTable({
    DT::datatable(oo1()) %>% formatCurrency(2,currency="",digit=2,interval=3)
  })
  output$disaggregationPlot<-renderPlot({
        ggplot(oo1(),aes_string(x=input$disaggregation, y="Value",color=input$disaggregation,fill=input$disaggregation)) + geom_col(stat="identity", width=0.3) +
        theme(legend.position="bottom", legend.title=element_blank(),legend.text=element_text(size=12), axis.text.y=element_text(size="14"), axis.title.x=element_text(size="14")) + scale_x_discrete(breaks=NULL) + 
        scale_y_continuous(labels= comma) + ylab("") + scale_colour_brewer(palette="Set2") + scale_fill_brewer(palette="Set2") + 
        ggtitle(paste("",input$select2)) + theme(plot.title=element_text(face="bold",size=15, hjust=0.5))
 
  })}
shinyApp(ui, server)