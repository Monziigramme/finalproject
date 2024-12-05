https://moniquepset2.shinyapps.io/my_app/
  
setwd("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")

#Question 2
library(shiny)
library(tidyverse)
library(plotly)
data <- read.csv("clean_chicago_data.csv")
ui <- fluidPage(
  
  titlePanel("Homelessness in Chicago: Affordable Housing vs. Vacant Lots"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "neigh",
                  label = "Choose a Neighbourhood",
                  choices = unique(data$community),
                  selected = "Auburn Gresham"), 
      selectInput(inputId = "neigh1",
                  label = "Choose another Neighbourhood",
                  choices = unique(data$community),
                  selected = "Rogers Park"), 
      selectInput(inputId = "property_type",
                  label = "Choose a Property Type",
                  choices = unique(data$property_type),
                  selected = "Multifamily")
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel(title = textOutput("combined"), plotlyOutput("ts")),
        tabPanel(title = textOutput("property_type_t"), plotlyOutput("ts1"))
      )
    )
  )
)

server <- function(input, output) {
  path <- "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject"
  data <- read_csv("clean_chicago_data.csv")
  
  output$combined <- renderText(paste({input$neigh}, "and", {input$neigh1}))
  output$property_type_t <- renderText({input$property_type})
  
  info <- reactive({
    filter(data, community == input$neigh | community == input$neigh1)
  })
  
  info1 <- reactive({
    filter(data, property_type == input$property_type & (community == input$neigh | community == input$neigh1))
  })
  
  
  output$ts <- renderPlotly({
    ggplotly(ggplot(data = info()) +
               geom_bar(aes(community, count_vacant, fill = community), stat = "identity") +
               labs(title = paste("Vacant Lots in", input$neigh, "and", input$neigh1), 
                    y = "Count",
                    fill = "Community Areas") +
               theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=1))
    )
  })
  
  output$ts1 <- renderPlotly({
    ggplotly(ggplot(data = info1()) +
               geom_bar(aes(community, affordable_units_no), stat = "identity") +
               labs(title = paste(input$property_type, "in", input$neigh, "and", input$neigh1), 
                    x = "Community Areas", 
                    y = "Total Number of Affordable Units")
    )
  })
  
}

shinyApp(ui = ui, server = server)


