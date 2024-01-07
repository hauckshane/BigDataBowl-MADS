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
  titlePanel("Setting the Edge: Comparing team directional performances"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose Defense Team", 
                  choices = team_choices),
      uiOutput("team_logo"),
      selectInput("opp", "Choose Offense Team", 
                  choices = team_choices),
      uiOutput("opp_logo"),
      #### Readme Text ####
      h4("Defense View"),
      h5("Directional Change Plots"),
      p("The key idea is that setting the edge forces ball carriers to change direction, making it easier for the defense to stop them with a tackle. The color of each slice in the plot represents average EPA for different ranges of total directional change, relative to the side of the play. We also have the directional change split by playside, where we can see the average EPA of ball carrier movements made against the team's defense. A darker purple color indicates a lower EPA, which is better for the defense. The Playside View has cutbacks, dives, and bounces labeled as C, D, B respectively."),
      h5("Top Edge Setters Tables"),
      p("The defense view tables show the top 5 players most frequently setting the edge during zone run plays for each playside. Here we have the Edge Intensity Rating (EIR) also displayed. EIR is a metric that takes into consideration how the defense reacts to the ball carrier's movement and vice versa. A higher EIR means a better edge is set. We have the average EIR for plays where the specific player is setting the edge, the average time in seconds until the player sets the edge, and the average EPA of plays where that player is setting the edge. A darker color indicates a lower EPA, which is better for the defense."),
      h4("Offense View"),
      h5("Run Type Tables"),
      p("Here we have a summary of the offensive team's performance on zone run plays. We have split the runs by playside and calculated the frequency and average EPA of plays on the left side and plays on the right side. We can see how often the team attempts cutbacks, dives, and bounces on each side and the average EPA for these plays. A darker color indicates a higher EPA, which is better for the offense.")
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
      tags$hr(style = "border-top: 2px solid #000;"),
      fluidRow(
        h2(textOutput("off_header")),
        column(6, gt_output("off_team_table_left")),
        column(6, gt_output("off_team_table_right"))
      ),
    )
  )
)
