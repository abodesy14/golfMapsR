{
library(shiny)
library(shinyWidgets)
library(shinybusy)
library(tidyverse)
library(dplyr)
}

ui <- fluidPage(
  add_busy_spinner(spin = "fading-circle"),
  theme = shinythemes::shinytheme("simplex"),
  navbarPage(
    "golf_map_makeR",
    tabPanel("Course Viewer", 
             selectInput("course", "Choose Course:", choices = unique(geojson_df$course_name), selected = "Erin Hills", multiple = F),
             plotOutput("map"))
    )
  )
  
