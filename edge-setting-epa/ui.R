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
team_choices <- sort(unique(df$defensiveTeam))

fluidPage(
  titlePanel("Company Hiring Activity"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose Defense Team", 
                  choices = team_choices),
      selectInput("opp", "Choose Offense Team", 
                  choices = team_choices)
    ),
    mainPanel(
      fluidRow(
        h1(textOutput("Defense_header")),
        column(6, plotOutput("def_change_plot")),
        column(6, plotOutput("def_playside_plot"))
      ),
      fluidRow(
        h1(textOutput("opp_header")),
        column(6, tableOutput("off_team_table")),
        column(6, tableOutput("off_player_table"))
      )
    )
  )
)
