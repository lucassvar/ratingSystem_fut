# Load necessary libraries
library(shiny)
library(worldfootballR)
library(dplyr)
library(stringr)
source("functions.R")  # Assuming there's a file named "functions.R" with custom functions
load("rda/playersMatchLogs.rda")
load("rda/sh_logs.rda")

# Divide position (Pos) column by 4 (primary position to quaternary position)
playersMatchLogs[c("Pos_1", "Pos_2", "Pos_3", "Pos_4")] <- t(sapply(strsplit(playersMatchLogs$Pos, ","), function(x) c(x, rep(NA, 4 - length(x)))))
playersMatchLogs <- {playersMatchLogs %>%
    filter(!is.na(Pos_1)) %>%
    mutate(
      Role = case_when(
        Pos_1 == "LW" | Pos_1 == "RW" | Pos_1 == "LM" | Pos_1 == "RM" ~ "W",
        Pos_1 == "AM" | Pos_1 == "CM" ~ "CM",
        Pos_1 == "DM" ~ "DM",
        Pos_1 == "CB" ~ "CB",
        Pos_1 == "FW" ~ "FW",
        Pos_1 == "WB" | Pos_1 == "LB" | Pos_1 == "RB" ~ "FB",
        Pos_1 == "GK" ~ "GK",
        TRUE ~ NA_character_
      )
    )}

# Define Shiny app UI
ui <- fluidPage(
  titlePanel("Players Ratings"),
  sidebarLayout(
    # Sidebar panel with input controls
    sidebarPanel(
      tags$h3("Player", style = "color: #333;"),
      
      # Select player
      selectInput("player_selected",
                  "Player:",
                  choices = unique(playersMatchLogs$Player),
                  selected = "Enzo Fernández"),
      
      # Select teams
      selectInput("teams_selected",
                  "Team/s:",
                  choices = unique(c(playersMatchLogs$Team, sh_logs$Squad)),
                  multiple = TRUE),
      
      # Set minimum minutes per match
      numericInput("exclude_minutes_player",
                   "Min. Minutes per Match:",
                   value = 15,
                   min = 0,
                   max = 90),
      
      # Select date range of matches
      dateRangeInput("player_date_range",
                     "Date Range of Matches:",
                     start = Sys.Date()-365,
                     end = Sys.Date()),
      
      # Choose between Position and Role
      radioButtons("position_or_role",
                   "Position or Role:",
                   choices = c("Position" = "pos","Role" = "role"),
                   selected = "role",
                   inline = TRUE),
      
      # Select player positions
      selectInput("positions_selected",
                  "Player Positions:",
                  choices = NULL,
                  multiple = TRUE),
      
      # Select player leagues
      selectInput("player_leagues_selected",
                  "Player Leagues",
                  choices = NULL,
                  multiple = TRUE),
      
      # Horizontal line for separation
      tags$hr(style = "border-color: #535353; width: 50%;"),
      tags$h3("Comparison Pool", style = "color: #333;"),
      
      # Choose sex for comparison pool
      radioButtons("sex_selected",
                   "Sex:",
                   choices = c("M", "F"),
                   selected = "M",
                   inline = TRUE),
      
      # Set minimum minutes per match for comparison pool
      numericInput("exclude_minutes_comp_pool",
                   "Min. Minutes per Match:",
                   value = 15,
                   min = 0,
                   max = 90),
      
      # Select date range of matches for comparison pool
      dateRangeInput("comp_pool_date_range",
                     "Date Range of Matches:",
                     start = Sys.Date()-365,
                     end = Sys.Date()),
      
      # Select leagues for comparison pool
      selectInput("comp_pool_leagues",
                  "Leagues",
                  choices = NULL,
                  multiple = TRUE),
      
      # Horizontal line for separation
      tags$hr(style = "border-color: #535353; width: 50%;"),
      tags$h3("General", style = "color: #333;"),
      
      # Choose between Total and Auto-Filled p90s stats
      radioButtons("predicted_or_total",
                   "Total or Auto-Filled p90s stats:",
                   choices = c("Total" = "total", "Auto-Fill" = "predicted"),
                   selected = "total",
                   inline = TRUE),
      
      # Button to update the view
      actionButton("update_button", "Update View", icon("refresh"))
    ),
    # Main panel to display output table
    mainPanel(
      tableOutput("player_zscores_output")
    )
  )
)

# Define server function
server <- function(input, output, session) {
  # Update teams options based on player
  observe({
    teams_available <- unique(c(playersMatchLogs$Team[playersMatchLogs$Player == input$player_selected],
                                sh_logs$Squad[sh_logs$Player == input$player_selected]))
    updateSelectInput(session, "teams_selected", choices = teams_available)
  })
  
  # Update positions options based on player and pos_or_role
  observe({
    if (input$position_or_role == "pos") {
      positions_available <- unique(playersMatchLogs$Pos_1[playersMatchLogs$Player == input$player_selected & playersMatchLogs$Team %in% input$teams_selected])
    } else {
      positions_available <- unique(playersMatchLogs$Role[playersMatchLogs$Player == input$player_selected & playersMatchLogs$Team %in% input$teams_selected])
    }
    updateSelectInput(session, "positions_selected", choices = positions_available)
  })
  
  # Update league options based on player and team
  observe({
    leagues_available <- unique(playersMatchLogs$League[playersMatchLogs$Player == input$player_selected &
                                                          playersMatchLogs$Team %in% input$teams_selected])
    updateSelectInput(session, "player_leagues_selected", choices = leagues_available)
  })
  
  # Update comp. pool's league options based on sex
  observe({
    leagues_available <- unique(playersMatchLogs$League[playersMatchLogs$Sex == input$sex_selected])
    updateSelectInput(session, "comp_pool_leagues", choices = leagues_available)
  })
  
  # Define reactive expression for calculating Z-scores
  z_scores <- reactive({
    
    player_zscores <- plyML_zscores(ply_selected = input$player_selected, 
                                    ply_team = input$teams_selected, 
                                    ply_exclude_mins = input$exclude_minutes_player,
                                    ply_date_range = input$player_date_range,
                                    ply_positions = input$positions_selected,
                                    ply_leagues = input$player_leagues_selected,
                                    comp_pool_exclude_mins = input$exclude_minutes_comp_pool,
                                    comp_pool_sex = input$sex_selected,
                                    compPool_date_range = input$comp_pool_date_range,
                                    compPool_ply_leagues = input$comp_pool_leagues,
                                    predicted_or_total = input$predicted_or_total,
                                    pos_or_role = input$position_or_role)
    
    # Transform the data for rendering in a table
    transformed_dataframe <- data.frame(Position = {c("Team", "Progressive Passes", "Short Passing", "Medium Passing", "Long Passing",
                                                      "Progressive Carries", "xAG", "Passes into Pen. Box", "Tackles", "Tackles Against Challengers", "Blocks", "Int", "Dribbling",
                                                      "Carries into Pen. Box", "Passes Received", "Progressive Passes Received", "Ball Control", "Aerial Duels", "Shooting", "Heading",
                                                      "Shooting (Foot)", "Right Foot Shots", "Left Foot Shots")})
    
    # Fill the data frame with Z-scores
    for (row_num in 1:nrow(player_zscores)) {
      new_column <- c()
      for (col_num in 1:ncol(player_zscores)) {
        new_column <- c(new_column, as.character(player_zscores[row_num, col_num]))
      }
      transformed_dataframe[[new_column[3]]] <- new_column[c(2, 4:25)]
    }
    
    transformed_dataframe
  })
  
  # Update the output table when the "Update View" button is clicked
  observeEvent(input$update_button, {
    z_scores_filtered <- z_scores()
    output$player_zscores_output <- renderTable({
      z_scores_filtered
    })
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
