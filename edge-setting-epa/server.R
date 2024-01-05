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
df <- read.csv("all_processed_data.csv")

# Set the change range for viewing graphs
max_range <- 30
df <- filter(df, total_change_at_max_change < max_range)

# Create subsets of the data for plot purposes
change_df <- df %>% 
  filter(abs(relative_total_change) < max_range) %>%
  select(relative_total_change, expectedPointsAdded, 
         possessionTeam, defensiveTeam) %>%
  mutate(bucket = cut(relative_total_change, breaks = seq(0, max_range, 
                                                          max_range / 10))) %>% 
  group_by(bucket) %>% 
  mutate(bucket_angle_min = as.numeric(sub("\\((.*),.*", "\\1", 
                                           as.character(bucket))),
         bucket_angle_max = as.numeric(gsub("\\[|\\]", "", 
                                            gsub(".*,(.*)\\]", "\\1", 
                                                 as.character(bucket)))),
         bucket_angle_mean = (bucket_angle_min + bucket_angle_max) / 2) %>%
  mutate(meanEPA = mean(expectedPointsAdded)) %>% 
  unique()

playside_df <- df %>% 
  filter(abs(relative_total_change) < max_range) %>%
  select(relative_total_change, expectedPointsAdded, playSide,
         possessionTeam, defensiveTeam) %>%
  group_by(playSide) %>%
  mutate(bucket = cut(relative_total_change, breaks = seq(0, max_range, 
                                                          max_range / 10)))%>%
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
                                "Plays to the Left", 
                                "Plays to the Right")) %>% 
  unique()


# Define server logic required to draw polar plots
function(input, output, session) {
  
  change_o_df <- reactive({
    filter(change_df, possessionTeam == input$team)
  })
  
  change_d_df <- reactive({
    filter(change_df, defensiveTeam == input$team)
  })
  
  playside_o_df <- reactive({
    filter(playside_df, defensiveTeam == input$team)
  })
  
  playside_d_df <- reactive({
    filter(playside_df, defensiveTeam == input$team)
  })

    output$change_o <- renderPlot({

      ggplot(data = change_o_df(), aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-180,180)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = "Offense EPA by Relative Total Change", 
             x = "Relative Total Change", 
             y = " ", 
             subtitle = "Values < 0 are Cutbacks, Values > 0 are Bounces",
             fill = "EPA")  +
        scale_fill_gradient2(low = "darkblue", 
                             high = "firebrick4", 
                             midpoint = mean(change_o_df()$meanEPA, na.rm = TRUE)) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.size = unit(1, "cm"))

    })
    
    output$change_d <- renderPlot({
      
      ggplot(data = change_d_df(), aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-180,180)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = "Defense EPA by Relative Total Change", 
             x = "Relative Total Change", 
             y = " ", 
             subtitle = "Values < 0 are Cutbacks, Values > 0 are Bounces",
             fill = "EPA")  +
        scale_fill_gradient2(low = "darkblue", 
                             high = "firebrick4", 
                             midpoint = mean(change_d_df()$meanEPA, na.rm = TRUE)) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 12),
              legend.key.size = unit(1, "cm"))
      
    })
    
    output$playside_o <- renderPlot({
      
      ggplot(data = playside_o_df(), aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-50,50)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = "Offense EPA by Direction of Greatest Cut", 
             x = "Greatest Change of Direction", y = " ", 
             subtitle = "Considering the direction of the playside:", 
             fill = "EPA")  +
        scale_fill_gradient2(low = "darkblue", 
                             high = "firebrick4", 
                             midpoint = mean(playside_o_df()$meanEPA, na.rm = TRUE)) +
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
    
    output$playside_d <- renderPlot({
      
      ggplot(data = playside_d_df(), aes(x = bucket_angle_mean, y = 1)) +
        geom_col(aes(fill = meanEPA)) +
        coord_polar(start = 66) +
        scale_x_continuous(limits = c(-50,50)) +
        scale_y_continuous(limits = c(0,1)) +
        labs(title = "Defense EPA by Direction of Greatest Cut", 
             x = "Greatest Change of Direction", y = " ", 
             subtitle = "Considering the direction of the playside:", 
             fill = "EPA")  +
        scale_fill_gradient2(low = "darkblue", 
                             high = "firebrick4", 
                             midpoint = mean(playside_d_df()$meanEPA, na.rm = TRUE)) +
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
