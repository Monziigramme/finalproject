setwd("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject")

library(shiny)
library(tidyverse)
library(plotly)

data <- read.csv("C:/Users/momo_/OneDrive/Documents/GitHub/finalproject/clean_chicago_data.csv")
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
