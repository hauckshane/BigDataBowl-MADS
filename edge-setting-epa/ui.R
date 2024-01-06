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
opp_choices <- c("League", team_choices)
type_choices <- c("Defense", "Offense")

fluidPage(
  titlePanel("Company Hiring Activity"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose Your Team", 
                  choices = team_choices),
      selectInput("type", "View Defense or Offense", 
                  choices = type_choices),
      selectInput("opp", "Choose Your Opponent", 
                  choices = opp_choices)
    ),
    mainPanel(
      fluidRow(
        h1(textOutput("team_header")),
        column(6, plotOutput("team_change")),
        column(6, plotOutput("team_playside"))
      ),
      fluidRow(
        h1(textOutput("opp_header")),
        #column(6, plotOutput("opp_change")),
        #column(6, plotOutput("opp_playside"))
      )
    )
  )
)
