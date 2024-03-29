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
library(gt)
library(ggplot2)
library(nflfastR)

# Read in the data for Defense plots and tables
df <- read.csv("filtered_processed_data.csv")
eir_df <- read.csv("eir_epa.csv") %>% select(-X)
# Round values to 2 decimal places
eir_df$setting_freq <- round(eir_df$setting_freq, 2)
eir_df$pretty_freq <- paste0(eir_df$setting_freq * 100, "%")
eir_df$meanEIR <- round(eir_df$meanEIR, 2)
eir_df$meanTime <- round(eir_df$meanTime, 2)
eir_df$freqEIR <- round(eir_df$setting_freq * eir_df$meanEIR, 4)
eir_df$percentile <- round(percent_rank(eir_df$freqEIR),3) * 100
# Make pretty column names
names(eir_df) <- c("Team", "Name", "Playside", "Num. Plays",
                       "Num. Plays for Player", "prop", "Avg Time",
                       "Avg EIR", "Avg EPA", "Setting Freq", "Weighted EIR", "Percentile")

# Set the relative_total_change range for viewing graphs
max_range <- 30
n_buckets <- 7
n_buckets_playside <- 3
df <- filter(df, total_change_at_max_change < max_range)
df <- filter(df, !is.na(expectedPointsAdded))
# Remove outliers
df <- filter(df, abs(expectedPointsAdded) <= 5)

# Dataframes for Defense plots
## Relative total change plots
change_df <- df %>% 
  filter(abs(relative_total_change) < max_range) %>%
  select(relative_total_change, expectedPointsAdded, 
         possessionTeam, defensiveTeam) %>%
  mutate(bucket = cut(relative_total_change, 
                      breaks = seq(-max_range, 
                                   max_range, 
                                   (max_range * 2) / n_buckets))) %>% 
  group_by(bucket, defensiveTeam) %>% 
  mutate(bucket_angle_min = as.numeric(sub("\\((.*),.*", "\\1", 
                                           as.character(bucket))),
         bucket_angle_max = as.numeric(gsub("\\[|\\]", "", 
                                            gsub(".*,(.*)\\]", "\\1", 
                                                 as.character(bucket)))),
         bucket_angle_mean = (bucket_angle_min + bucket_angle_max) / 2) %>%
  mutate(meanEPA = mean(expectedPointsAdded)) %>% 
  unique()

## Playside plots
playside_df <- df %>% 
  filter(abs(relative_total_change) < max_range) %>%
  select(relative_total_change, expectedPointsAdded, playSide,
         possessionTeam, defensiveTeam) %>%
  group_by(playSide) %>%
  mutate(bucket = cut(relative_total_change, 
                      breaks = seq(-max_range, 
                                   max_range, 
                                   (max_range * 2) / n_buckets_playside)))%>%
  group_by(playSide, bucket, defensiveTeam) %>%
  mutate(bucket_angle_min = as.numeric(sub("\\((.*),.*", "\\1", 
                                           as.character(bucket))),
         bucket_angle_max = as.numeric(gsub("\\[|\\]", "", 
                                            gsub(".*,(.*)\\]", "\\1", 
                                                 as.character(bucket)))),
         bucket_angle_mean = (bucket_angle_min + bucket_angle_max) / 2) %>%
  mutate(meanEPA = mean(expectedPointsAdded),
         meanEPA = ifelse(playSide == "left", meanEPA * -1, meanEPA)) %>%
  mutate(playSidedescr = ifelse(playSide == "left", 
                                "Left", 
                                "Right")) %>% 
  unique()

# To label cutbacks, dives, and bounces
play_types_R <- c("C", "D", "B")
play_types_L <- c("B", "D", "C")
ranges <- c(-45, -10, 10, 40)

playside_df_R <- playside_df %>%
  filter(playSide == "right") %>%
  # Make a column for play type: type
  mutate(type = cut(relative_total_change, breaks = ranges, 
                    labels = play_types_R, include.lowest = TRUE))

playside_df_L <- playside_df %>%
  filter(playSide == "left") %>%
  # Make a column for play type: type
  mutate(type = cut(relative_total_change, breaks = ranges, 
                    labels = play_types_L, include.lowest = TRUE))

playside_df <- rbind(playside_df_R, playside_df_L)


# Set color gradients for defensive plots
## We want the gradients to be all on the same scale
change_midpoint <- mean(change_df$meanEPA)
playside_midpoint <- mean(playside_df$meanEPA)


# Functions for drawing Defense plots
## Relative total change plot
defense_change_plot <- function(plot_df) {
  plot_df <- na.omit(plot_df)
  
  plot <- ggplot(data = plot_df, aes(x = bucket_angle_mean, y = 1)) +
    geom_col(aes(fill = meanEPA), color = "black") +
    coord_polar(start = 66) +
    scale_x_continuous(limits = c(-180,180)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(title = "Directional Change of Ball Carrier", 
         x = "Directional Change", 
         y = " ",
         fill = "EPA")  +
    scale_fill_gradient2(low = "darkcyan", 
                         high = "firebrick4", 
                         midpoint = change_midpoint,
                         limits = c(min(change_df$meanEPA), max(change_df$meanEPA))) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          legend.key.size = unit(1, "cm"))
  
  return(plot)
}

## Playside plot
defense_playside_plot <- function(plot_df) {
  plot_df <- na.omit(plot_df)
  
  # Calculate the mode type for each section
  mode_data <- plot_df %>%
    group_by(bucket_angle_mean, playSidedescr) %>%
    summarise(mode_type = names(which.max(table(type)))) %>%
    ungroup()
  
  # Merge the mode_data back to the plot_df
  plot_df <- left_join(plot_df, mode_data, 
                       by = c("bucket_angle_mean", "playSidedescr"))
  
  plot <- ggplot(data = plot_df, aes(x = bucket_angle_mean, y = 1)) +
    geom_col(aes(fill = meanEPA), color = "black") +
    coord_polar(start = 66) +
    scale_x_continuous(limits = c(-50,50)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(title = "Playside View of Directional Change", 
         x = "Directional Change", y = "",
         fill = "EPA",
         subtitle = "Showing Cutbacks, Dives, Bounces")  +
    geom_text(aes(label = mode_type), 
              position = position_stack(vjust = 0.5), 
              color = "black", size = 3) +
    scale_fill_gradient2(low = "darkcyan", 
                         high = "firebrick4", 
                         midpoint = playside_midpoint,
                         limits = c(min(change_df$meanEPA), max(change_df$meanEPA))) +
    facet_wrap(~playSidedescr) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          legend.key.size = unit(1, "cm"),
          strip.text = element_text(size = 18))
  
  return(plot)
}

## EIR left table
eir_table_left <- function(plot_df) {
  plot_df <- plot_df %>%
    filter(Playside == "left") %>%
    filter('Num. Plays for Player' >= 5)
  
  plot_df <- plot_df[order(-plot_df$Percentile), ]
  
  table <- plot_df %>%
    head(5) %>%
    select('Name', 'Setting Freq', 'Avg Time', 'Avg EIR', 'Avg EPA', 'Percentile') %>%
    gt() %>%
    fmt_number(
      columns = c("Avg EPA"), 
      decimals = 2
    ) %>%
    fmt_number(
      columns = c("Percentile"), 
      decimals = 2
    ) %>%
    data_color(
      columns = c("Percentile"),
      fn = scales::col_numeric(
        palette = c("darkred", "pink", "lightcyan", "darkcyan"),
        domain = c(25,100)
      )
    ) %>% 
    tab_header(
      title = "Left Edge Setters"
    )
  
  return(table)
}

## EIR right table
eir_table_right <- function(plot_df) {
  plot_df <- plot_df %>%
    filter(Playside == "right") %>%
    filter('Num. Plays for Player' >= 5)
  
  plot_df <- plot_df[order(-plot_df$Percentile), ]
  
  table <- plot_df %>%
    head(5) %>%
    select('Name', 'Setting Freq', 'Avg Time', 'Avg EIR', 'Avg EPA', 'Percentile') %>%
    gt() %>%
    fmt_number(
      columns = c("Avg EPA"), 
      decimals = 2
    ) %>%
    fmt_number(
      columns = c("Percentile"), 
      decimals = 2
    ) %>%
    data_color(
      columns = c("Percentile"),
      fn = scales::col_numeric(
        palette = c("darkred", "pink", "lightcyan", "darkcyan"),
        domain = c(25,100)
      )
    ) %>% 
    tab_header(
      title = "Right Edge Setters"
    )
  
  return(table)
}


# Dataframe for Offense tables
team_df <- read.csv("team_routes_epa.csv")
# Round values to 2 decimal places
team_df$freq <- round(team_df$freq, 2)
team_df$pretty_freq <- paste0(team_df$freq * 100, "%")
# Make pretty column names
names(team_df) <- c("X", "Team", "Playside", "Run Type", "Num. Plays", 
                    "Num. Plays for Type", "prop", "Avg EPA", "Frequency")


# Functions for drawing Offense tables
## Team table
team_table_left <- function(plot_df) {
  table <- plot_df %>%
    filter(Playside == "left") %>%
    arrange(desc('Run Type')) %>%
    select('Run Type', 'Frequency', 'Avg EPA') %>%
    gt() %>%
    fmt_number(
      columns = c("Avg EPA"), 
      decimals = 2
    ) %>%
    data_color(
      columns = c("Avg EPA"),
      fn = scales::col_numeric(
        palette = c("darkcyan", "lightcyan", "pink", "darkred"),
        domain = c(-1, 1)
      )
    ) %>% 
    tab_header(
      title = "Runs to the Left"
    )
  
  return(table)
}

team_table_right <- function(plot_df) {
  table <- plot_df %>%
    filter(Playside == "right") %>%
    arrange(desc('Run Type')) %>%
    select('Run Type', 'Frequency', 'Avg EPA') %>%
    gt() %>%
    fmt_number(
      columns = c("Avg EPA"), 
      decimals = 2
    ) %>%
    data_color(
      columns = c("Avg EPA"),
      fn = scales::col_numeric(
        palette = c("darkcyan", "lightcyan", "pink", "darkred"),
        domain = c(-1, 1)
      )
    ) %>% 
    tab_header(
      title = "Runs to the Right"
    )
  
  return(table)
}


##########################################################

# Define server logic required to draw reactive plots and views
function(input, output, session) {
  
  # Get teams from user input
  # Team is Defense, Opponent is Offense
  team <- reactive({input$team})
  opp <- reactive({input$opp})
  
  # Get the logos
  logo_df <- nflfastR::teams_colors_logos

  get_team_logo <- reactive({
    logo_df %>%
    filter(team_abbr == input$team) %>%
    pull(team_logo_espn)
  })
  
  output$team_logo <- renderUI({
    img(src = get_team_logo(), width = "150px", height = "150px")
  })
  
  get_opp_logo <- reactive({
    logo_df %>%
      filter(team_abbr == input$opp) %>%
      pull(team_logo_espn)
  })
  
  output$opp_logo <- renderUI({
    img(src = get_opp_logo(), width = "150px", height = "150px")
  })
  
  # Filter the dataframes based on user input choices
  ## Defense plot dataframes
  defense_change_df <- reactive({
    filter(change_df, defensiveTeam == input$team)
  })
  
  defense_playside_df <- reactive({
    filter(playside_df, defensiveTeam == input$team)
  })
  
  ## Defense EIR table dataframe
  eir_table_df <- reactive({
    filter(eir_df, Team == input$team)
  })
  
  ## Offense table dataframes
  offense_team_df <- reactive({
    filter(team_df, Team == input$opp)
  })
  
  offense_player_df <- reactive({
    filter(player_df, Team == input$opp)
  })
  
  # Team header
  output$def_header <- renderText({ 
    paste(team(), "Defense")
  })
  
  # Opponent header
  output$off_header <- renderText({ 
    paste(opp(), "Offense")
  })
  
  # Defense polar plots
  output$def_change_plot <- renderPlot({
      df <- defense_change_df()
      defense_change_plot(df)
    })
  
  output$def_playside_plot <- renderPlot({
      df <- defense_playside_df()
      defense_playside_plot(df)
    })
  
  # Defense EIR tables
  output$eir_table_left <- render_gt({
    df <- eir_table_df()
    eir_table_left(df)
  })
  
  output$eir_table_right <- render_gt({
    df <- eir_table_df()
    eir_table_right(df)
  })

  # Offense plots
  output$off_team_table_left <- render_gt({
      df <- offense_team_df()
      team_table_left(df)
    })
  
  output$off_team_table_right <- render_gt({
    df <- offense_team_df()
    team_table_right(df)
  })

}

