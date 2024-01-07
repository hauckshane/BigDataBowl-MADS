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
library(gt)

# Get list of teams
df <- read.csv("filtered_processed_data.csv")
team_choices <- sort(unique(df$defensiveTeam))

fluidPage(
  tags$head(
    tags$style(
      HTML("
        h2 {
          text-align: center;
        }
      "),
      HTML("
        h3 {
          text-align: center;
        }
      ")
    )
  ),
  titlePanel("Setting the Edge: Comparing team’s directional performances"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose Defense Team", 
                  choices = team_choices),
      uiOutput("team_logo"),
      selectInput("opp", "Choose Offense Team", 
                  choices = team_choices),
      uiOutput("opp_logo")
    ),
    mainPanel(
      fluidRow(
        h2(textOutput("def_header")),
        column(6, plotOutput("def_change_plot")),
        column(6, plotOutput("def_playside_plot"))
      ),
      fluidRow(
        h3("Top Edge Setters"),
        column(6, gt_output("eir_table_left")),
        column(6, gt_output("eir_table_right"))
      ),
      fluidRow(
        h2(textOutput("off_header")),
        column(6, gt_output("off_team_table_left")),
        column(6, gt_output("off_team_table_right"))
      ),
    )
  )
)
