#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load libraries
library(shiny)
library(tidyverse)
library(ggplot2)

# Get list of teams
df <- read.csv("filtered_processed_data.csv")

fluidPage(
  titlePanel("Company Hiring Activity"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose Team", 
                  choices = sort(unique(df$defensiveTeam)))
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("change_o")),
        column(6, plotOutput("change_d"))
      ),
      fluidRow(
        column(6, plotOutput("playside_o")),
        column(6, plotOutput("playside_d"))
      )
    )
  )
)
