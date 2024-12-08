setwd("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")
library(rsconnect)

library(shiny)
library(tidyverse)
library(plotly)
path <- "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject/data"
data <- read.csv(paste0(path, "/clean_chicago_data.csv"))
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
  data <- read.csv(paste0(path, "/clean_chicago_data.csv"))
  
  output$combined <- renderText(paste({input$neigh}, "and", {input$neigh1}))
  output$property_type_t <- renderText({input$property_type})
  
  info <- reactive({
    filter(data, community == input$neigh | community == input$neigh1)
  })
  
  info1 <- reactive({
    filter(data, property_type == input$property_type & (community == input$neigh | community == input$neigh1))
  })
  
  long_data <- reactive({
    info() %>%
      pivot_longer(cols = c(count_vacant, affordable_units_no),
                   names_to = "Metrics",
                   values_to = "Count") 
  })
  
  output$ts <- renderPlotly({
    ggplot(data = long_data()) +
      geom_bar(aes(x = community, y = Count, fill = Metrics), stat = "identity", position = "dodge") +
      scale_y_continuous(
        name = "Count",
        sec.axis = sec_axis(~ . / 10, name = "Affordable Units") 
      ) +
      scale_fill_manual(values = c("count_vacant" = "cyan", "affordable_units_no" = "pink"),
                        labels = c("Vacant Lots", "Affordable Housing")) +
      labs(title = paste("Vacant Lots and Affordable Units in", input$neigh, "and", input$neigh1), 
           x = "Community Areas",
           fill = "Metrics") +
      theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = 1))
  })
  
  output$ts1 <- renderPlotly({
    ggplotly(ggplot(data = info1()) +
               geom_bar(aes(x = community, y = affordable_units_no), stat = "identity") +
               labs(title = paste(input$property_type, "Affordable Housing Units in", input$neigh, "and", input$neigh1), 
                    x = "Community Areas", 
                    y = "Total Number of Affordable Units",
                    )
    )
  })
  
}

shinyApp(ui = ui, server = server)

#second shiny app

data_2 <- clean_chicago_data |>
  pivot_longer(
    cols = percent_of_housing_crowded:per_capita_income,
    values_to = "Values",
    names_to = "Hardship_Indicator"
  )

ui2 <- fluidPage(
  
  titlePanel("Hardship Indicators in Chicago"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "neigh",
                  label = "Choose a Neighbourhood",
                  choices = unique(data_2$community),
                  selected = "Auburn Gresham"), 
      selectInput(inputId = "neigh1",
                  label = "Choose another Neighbourhood",
                  choices = unique(data_2$community),
                  selected = "Rogers Park"), 
      selectInput(inputId = "harship_indi",
                  label = "Choose a Hardship Indicator",
                  choices = unique(data_2$Hardship_Indicator),
                  selected = "percent_of_housing_crowded")
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel(title = textOutput("hardship_t"), plotlyOutput("ts1"))
      )
    )
  )
)

server2 <- function(input, output) {
  
  output$combined <- renderText(paste({input$neigh}, "and", {input$neigh1}))
  output$hardship_t <- renderText({input$harship_indi})
  
  info <- reactive({
    filter(data_2, community == input$neigh | community == input$neigh1)
  })
  
  info1 <- reactive({
    filter(data_2, Hardship_Indicator == input$harship_indi & (community == input$neigh | community == input$neigh1))
  })
  
  output$ts1 <- renderPlotly({
    ggplotly(ggplot(data = info1()) +
               geom_bar(aes(x = community, y = Values), stat = "identity") +
               labs(title = paste(input$harship_indi, "in", input$neigh, "and", input$neigh1), 
                    x = "Community Areas", 
                    y = "Total")
    )
  })
}

shinyApp(ui = ui2, server = server2)
