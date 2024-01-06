#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load libraries
library(shiny)
library(tidyverse)
library(ggplot2)

# Read in the data
df <- read.csv("filtered_processed_data.csv")

# Set the change range for viewing graphs
max_range <- 30
n_buckets <- 5
df <- filter(df, total_change_at_max_change < max_range)
df <- filter(df, !is.na(expectedPointsAdded))
# Remove outliers
df <- filter(df, abs(expectedPointsAdded) <= 5)

# Create subsets of the data for plot purposes

## Dataframe for relative total change plots
change_df <- df %>% 
  filter(abs(relative_total_change) < max_range) %>%
  select(relative_total_change, expectedPointsAdded, 
         possessionTeam, defensiveTeam) %>%
  mutate(bucket = cut(relative_total_change, 
                      breaks = seq(-max_range, 
                                   max_range, 
                                   (max_range * 2) / n_buckets))) %>% 
  group_by(bucket) %>% 
  mutate(bucket_angle_min = as.numeric(sub("\\((.*),.*", "\\1", 
                                           as.character(bucket))),
         bucket_angle_max = as.numeric(gsub("\\[|\\]", "", 
                                            gsub(".*,(.*)\\]", "\\1", 
                                                 as.character(bucket)))),
         bucket_angle_mean = (bucket_angle_min + bucket_angle_max) / 2) %>%
  mutate(meanEPA = mean(expectedPointsAdded)) %>% 
  unique()

## Dataframe for playside plots
playside_df <- df %>% 
  filter(abs(relative_total_change) < max_range) %>%
  select(relative_total_change, expectedPointsAdded, playSide,
         possessionTeam, defensiveTeam) %>%
  group_by(playSide) %>%
  mutate(bucket = cut(relative_total_change, 
                      breaks = seq(-max_range, 
                                   max_range, 
                                   (max_range * 2) / n_buckets)))%>%
  group_by(playSide, bucket) %>%
  mutate(bucket_angle_min = as.numeric(sub("\\((.*),.*", "\\1", 
                                           as.character(bucket))),
         bucket_angle_max = as.numeric(gsub("\\[|\\]", "", 
                                            gsub(".*,(.*)\\]", "\\1", 
                                                 as.character(bucket)))),
         bucket_angle_mean = (bucket_angle_min + bucket_angle_max) / 2) %>%
  mutate(meanEPA = mean(expectedPointsAdded),
         meanEPA = ifelse(playSide == "left", meanEPA * -1, meanEPA)) %>%
  mutate(playSidedescr = ifelse(playSide == "left", 
                                "Playside Left", 
                                "Playside Right")) %>% 
  unique()

# We want the gradients to be all on the same scale
print(summary(change_df$meanEPA))
change_midpoint <- mean(change_df$meanEPA)
playside_midpoint <- mean(playside_df$meanEPA)
print(summary(playside_df$meanEPA))


# Define server logic required to draw reactive polar plots
function(input, output, session) {
  
  # Filter the dataframes based on user input choices
  team_change_df <- reactive({
    if(input$type == "Defense"){
      change_df$low_color <- "darkgreen"
      change_df$high_color <- "darkred"
      filter(change_df, defensiveTeam == input$team)
    } else{ # Offense
      change_df$low_color <- "darkred"
      change_df$high_color <- "darkgreen"
      filter(change_df, possessionTeam == input$team)
    }
  })
  
  team_playside_df <- reactive({
      if(input$type == "Defense"){
        playside_df$low_color <- "darkgreen"
        playside_df$high_color <- "darkred"
        filter(playside_df, defensiveTeam == input$team)
      } else{ # Offense
        playside_df$low_color <- "darkred"
        playside_df$high_color <- "darkgreen"
        filter(playside_df, possessionTeam == input$team)
      }
  })
  
  opp_change_df <- reactive({
    if(input$type == "Defense"){
      # Show the opposite type
      # Offense
      change_df$low_color <- "darkred"
      change_df$high_color <- "darkgreen"
      if(input$type != "League"){
        filter(change_df, possessionTeam == input$team)
      } else{
        change_df
      }
    } else{
      # Defense
      change_df$low_color <- "darkgreen"
      change_df$high_color <- "darkred"
      if(input$type != "League"){
        filter(change_df, possessionTeam == input$team)
      } else{
        change_df
      }
    }
  })
  
  opp_playside_df <- reactive({
    if(input$type == "Defense"){
      # Show the opposite type
      # Offense
      playside_df$low_color <- "darkred"
      playside_df$high_color <- "darkgreen"
      if(input$type != "League"){
        filter(playside_df, possessionTeam == input$team)
      } else{
        playside_df
      }
    } else{
      # Defense
      playside_df$low_color <- "darkgreen"
      playside_df$high_color <- "darkred"
      if(input$type != "League"){
        filter(playside_df, possessionTeam == input$team)
      } else{
        playside_df
      }
    }
  })
  
  team <- reactive({input$team})
  opp <- reactive({input$opp})
  team_type <- reactive({input$type})
  opp_type <- reactive({ifelse(input$type == "Defense", "Offense", "Defense")})
  
  # Team header
  output$team_header <- renderText({ 
    paste(team(), team_type())
  })
  
  # Opponent header
  output$opp_header <- renderText({ 
    paste(opp(), opp_type())
  })
  
  # Team change plot
    output$team_change <- renderPlot({
      df <- team_change_df()
      df <- na.omit(df)

      ggplot(data = df, aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-180,180)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = paste(team_type(), "Directional Change of Ball Carrier"), 
             x = "Directional Change", 
             y = " ", 
             subtitle = "Values < 0 are Cutbacks, Values > 0 are Bounces",
             fill = "EPA")  +
        scale_fill_gradient2(low = unique(df$low_color), 
                             high = unique(df$high_color), 
                             midpoint = change_midpoint,
                             limits = c(-0.25, 0.1)) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.size = unit(1, "cm"))

    })
    
    # Team playside plot
    output$team_playside <- renderPlot({
      df <- team_playside_df()
      df <- na.omit(df)
      
      ggplot(data = df, aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-50,50)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = paste(team_type(), "Directional Change of Ball Carrier"), 
             x = "Directional Change", y = "", 
             subtitle = "By Playside", 
             fill = "EPA")  +
        scale_fill_gradient2(low = unique(df$low_color), 
                             high = unique(df$high_color), 
                             midpoint = playside_midpoint,
                             limits = c(-0.55, 0.55)) +
        facet_wrap(~playSidedescr) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_blank(),
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.size = unit(1, "cm"),
              strip.text = element_text(size = 18))
      
    })

    
    # Opponent change plot
    output$opp_change <- renderPlot({
      df <- opp_change_df()
      df <- na.omit(df)
      
      ggplot(data = df, aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-180,180)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = paste(opp_type(), "Directional Change of Ball Carrier"), 
             x = "Directional Change", 
             y = "", 
             subtitle = "Values < 0 are Cutbacks, Values > 0 are Bounces",
             fill = "EPA")  +
        scale_fill_gradient2(low = unique(df$low_color), 
                             high = unique(df$high_color), 
                             midpoint = change_midpoint,
                             limits = c(-0.25, 0.1)) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.size = unit(1, "cm"))
      
    })
    
    # Opponent playside plot
    output$opp_playside <- renderPlot({
      df <- opp_playside_df()
      df <- na.omit(df)
      
      ggplot(data = df, aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-50,50)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = paste(opp_type(), "Directional Change of Ball Carrier"), 
             x = "Directional Change", y = " ", 
             subtitle = "By Playside", 
             fill = "EPA")  +
        scale_fill_gradient2(low = unique(df$low_color), 
                             high = unique(df$high_color), 
                             midpoint = playside_midpoint,
                             limits = c(-0.55, 0.55)) +
        facet_wrap(~playSidedescr) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_blank(),
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.size = unit(1, "cm"),
              strip.text = element_text(size = 18))
      
    })

}

