library(tidyverse)
library(ggplot2)
library(DT)
library(DBI)
library(shiny)
library(baseballr)
library(dplyr)
library(shinyWidgets)
library(shinydashboard)
library(ggrepel)



teams_df <- readRDS("../data/team_stats.rds")
team_results_df <- readRDS("../data/team_results.rds")
hitting_df <- readRDS("../data/hitting_stats.rds")
pitching_df <- readRDS("../data/pitching_stats.rds")
fielding_df <- readRDS("../data/fielding_stats.rds")
p_hit_df <- readRDS("../data/bref_daily_batter.rds")
p_pitch_df <- readRDS("../data/bref_daily_pitcher.rds")
team_standings_df <- readRDS("../data/MLB_Team_Standings.rds")
division_standings_df <- readRDS("../data/MLB_Division_Standings.rds")



  
get_team_data <- function(year) {
  tryCatch({
    teams_data <- teams_df |>
      filter(year == year) |>
      select(team_id,
             team_name,
             team_full_name,
             team_abbreviation,
             division_name) |>
      distinct()
    return(teams_data)
  }, error = function(e) {
    warning(paste("Error getting team data:", e$message))
    return(data.frame(
      team_id = integer(),
      team_name = character(),
      team_full_name = character(),
      team_abbreviation = character(),
      division_name = character()
    ))
  })
}


# Helper function for data filtering and summarizing
filter_and_summarize_data <- function(df, input) {
  cols_to_select <- unique(c("team_name", input[[paste0(input$table_type, "_column_select")]])) 
  
  df |>
    filter(year == input$year_input,
           team_id %in% input$team_select) |>
    select(all_of(cols_to_select)) |>
    group_by(team_name) |>
    summarise(across(everything(), 
                     ~if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)),
              .groups = 'drop')
}


format_datatable <- function(data) {
  dt <- DT::datatable(
    data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip'
    ),
    filter = 'top',
    rownames = FALSE
  ) |>
    formatStyle(columns = names(data),
                width = '150px',
                whiteSpace = 'nowrap')
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if(length(numeric_cols) > 0) {
    dt <- dt |> formatRound(columns = numeric_cols, digits = 3)
  }
  
  dt
}

all_stats <- bind_rows(
  pitching_stats %>% mutate(source = "Pitching"),
  fielding_stats %>% mutate(source = "Fielding"),
  hitting_stats %>% mutate(source = "Hitting")
)



ui <- dashboardPage(
  
  
  dashboardHeader(title = "MLB Analysis App"),
  
  # Sidebar with navigation menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("League Statistics", tabName = "league_stats", icon = icon("chart-bar")),
      menuItem("Team Statistics", tabName = "team_stats", icon = icon("users")),
      menuItem("Player Statistics", tabName = "player_stats", icon = icon("user")),
      menuItem("Statistical Modeling", tabName = "modeling", icon = icon("cogs"))
    )
  ),
  
  # Main body with tab contents
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(
        tabName = "home",
        fluidRow(
          column(12, 
                 h1("MLB Analysis App"),
                 p("Welcome to the MLB Analysis App! This app allows you to explore a variety of MLB statistics, analyze team and player performance, and build statistical models to predict outcomes. This app utlilizes the baseballr package to obtain all data."),
                 p("With the navigation bar, you can dive into team, player, and league statistics. You can also run regression models to better understand factors influencing team performance."),
                 
                 h3("League Statistics Tab"),
                 p("This tab provides an in-depth look at league-wide statistics. The bar graph displays team wins by division, offering insights into team success across different divisions. The line graph below shows attendance trends over time, categorized by team and division, allowing users to observe fluctuations and patterns in fan engagement. Beneath the graphs, an interactive data table provides detailed statistics, including wins, attendance, and more. Users can also view descriptive statistics for selected seasons and an ANOVA table to assess statistical differences in performance metrics across divisions."),
                 
                 h3("Team Statistics Tab"),
                 p("This tab shows a variety of team statistics. At the top of the page, users can select a year, data type, teams, and columns they want to display in a table. The bottom of the page allows users to select a variable from hitting, fielding, or pitching metrics and the output graphs will show the change in the selected variable from 1973 to now for the teams that were selected."),
                 
                 h3("Player Statistics Tab"),
                 p("This tab is for viewing player statistics. Users can select a year from 2019 to 2024, and then select to either view hitting statistics by player or pitching statistics by pitcher. This changes the player options in the dropdown menu. Players can be searched for in the menu and one or multiple can be selected. The user can then select which varibales they want to appear in a table for each selected player. Beneath the table, users can select a single metric to observe for each of the selected players. The bottom of the page includes a key for what each of the metric abbreviations stands for."),
                 
                 h3("Statistical Analysis Tab"),
                 p("This tab runs a regression analysis. You can select any team from the dropdown menu and then select to observe either hitting, pitching, or fielding statistics. One or multiple variables can be selected. This will then output a regression table that provides the overall fit of the variables selected and the coefficients for each selected variable. The dependent variable is set to wins for each team so this shows the effect of each selected variable on total team wins. At the bottom of the page, users can select 2 of any of the hitting, pitching, or fielding statistics to compare them visually in a graph.")
          )
        )
      ),
      
      # League Statistics Tab
      tabItem(
        tabName = "league_stats",
        h2("League Statistics"),
        
        # Bar Graph Section
        fluidRow(
          column(
            6,
            selectInput(
              "season_input",
              "Select Season(s):",
              choices = as.character(2015:2023), 
              selected = "2024",
              multiple = TRUE 
            )
          ),
          column(
            6,
            selectInput(
              "division_input_bar",
              "Select Division for Bar Graph:",
              choices = c(
                "All Divisions"
              ),
              selected = "All Divisions"
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            plotOutput("bar_graph")
          )
        ),
        
        # Line Graph Section
        fluidRow(
          column(
            6,
            selectInput(
              "division_input_line",
              "Select Division for Line Graph:",
              choices = c(
                "All Divisions"
              ),
              selected = "All Divisions"
            )
          ),
          column(
            6,
            selectInput("league_input", "Select League:", 
                        choices = c("Both", "American League", "National League"), 
                        selected = "Both"
            )
          )
        ),
        
        fluidRow(
          column(12, plotOutput("line_graph"))
        ),
        
        # Summary Table and Additional Outputs
        fluidRow(
          column(12, DT::dataTableOutput("league_summary_table"))
        ),
        
        fluidRow(
          column(
            6,
            selectInput(
              "season_input_stats",
              "Select Season(s) for Analysis:",
              choices = as.character(2015:2023), 
              selected = "2024",
              multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(12, h3("Descriptive Statistics"), tableOutput("descriptive_stats"))
        ),
        fluidRow(
          column(12, h3("ANOVA Table"), tableOutput("anova_table"))
        )
      ),
      
      # Team Statistics Tab
      # Team Statistics Tab
      tabItem(
        tabName = "team_stats",
        h2("Team Statistics"),
        
        # Year and Team Selection
        fluidRow(
          column(4,
                 numericInput("year_input", "Year:", value = 1973, min = 1973, max = 2023)
          ),
          column(8,
                 pickerInput(
                   inputId = "team_select",
                   label = "Select Teams:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE
                   )
                 )
          )
        ),
        
        # Data Type Selection and Column Picker
        fluidRow(
          column(4,
                 radioButtons("table_type", "Select Data Type:",
                              choices = c("Team Information" = "team",
                                          "Hitting Statistics" = "hitting",
                                          "Fielding Statistics" = "fielding",
                                          "Pitching Statistics" = "pitching"),
                              selected = "team")
          ),
          column(8,
                 conditionalPanel(
                   condition = "input.table_type == 'team'",
                   pickerInput(
                     inputId = "column_select",
                     label = "Select Columns to Display:",
                     choices = names(teams_df),
                     selected = c("team_name", "team_full_name", "division_name", "venue_name"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.table_type == 'hitting'",
                   pickerInput(
                     inputId = "hitting_column_select",
                     label = "Select Hitting Statistics to Display:",
                     choices = names(hitting_df),
                     selected = c("team_name"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.table_type == 'fielding'",
                   pickerInput(
                     inputId = "fielding_column_select",
                     label = "Select Fielding Statistics to Display:",
                     choices = names(fielding_df),
                     selected = c("team_name"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.table_type == 'pitching'",
                   pickerInput(
                     inputId = "pitching_column_select",
                     label = "Select Pitching Statistics to Display:",
                     choices = names(pitching_df),
                     selected = c("team_name"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                 )
                 
          )
        ),
        
        # Combined Table
        div(style = 'overflow-x: scroll; width: 100%',
            DT::DTOutput("combined_table")
        ),
        # Hitting Time Series Section
        br(),
        h3("Hitting Data"),
        fluidRow(
          column(4,
                 selectInput("hitting_metric", "Select Hitting Metric:",
                             choices = NULL  
                 )
          ),
          column(8,
                 plotOutput("hitting_time_plot", height = "400px")
          )
        ),
        
        # Fielding Time Series Section
        br(),
        h3("Fielding Data"),
        fluidRow(
          column(4,
                 selectInput("fielding_metric", "Select Fielding Metric:",
                             choices = NULL  
                 )
          ),
          column(8,
                 plotOutput("fielding_time_plot", height = "400px")
          )
        ),
        
        # Pitching Time Series Section
        br(),
        h3("Pitching Data"),
        fluidRow(
          column(4,
                 selectInput("pitching_metric", "Select Pitching Metric:",
                             choices = NULL  
                 )
          ),
          column(8,
                 plotOutput("pitching_time_plot", height = "400px")
          )
        )
      ),
      
      # Player Statistics Tab
      tabItem(
        tabName = "player_stats",
        h2("Player Statistics"),
        
        #Set up the picker rows for variable time/player/player type selection
        #(Code is similar across pitching and hitting tabs because I wanted the layout to be as consistent as possible)
        
        fluidRow(
          column(4,
                 checkboxGroupInput("p_year_input", "Time Range:", 
                                    choices=unique(p_hit_df$season),
                                    selected = '2024',
                                    inline=TRUE)
          ),
          column(4,
                 radioButtons("p_table_type", "Select Data Type:",
                              choices = c("Hitting Statistics by Player" = "p_hitting",
                                          "Pitching Statistics by Pitcher" = "p_pitching"))
          ),
          column(8,
                 conditionalPanel(
                   condition = "input.p_table_type == 'p_hitting'",
                   pickerInput(
                     inputId = "p_player_select_hit",
                     label = "Select Players:",
                     choices = p_hit_df$Name,
                     #selected a couple of my favorite players just to start to avoid it being empty
                     selected = c("Rowdy Tellez", "Andrew McCutchen", "Vladimir Guerrero Jr.", "Xander Bogaerts", "Rafael Devers"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                 ), 
                 conditionalPanel(
                   condition = "input.p_table_type == 'p_pitching'",
                   pickerInput(
                     inputId = "p_player_select_pitch",
                     label = "Select Players:",
                     choices = p_pitch_df$Name,
                     #again selected a few top pitchers so the dropdown menu wouldn't be empty
                     selected = c("Paul Skenes", "Gerrit Cole", "Chris Sale", "Shohei Ohtani", "Clayton Kershaw"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                 )
          )
        ),
        
        # Data Type Selection and Column Picker
        fluidRow(
          
          column(8,
                 conditionalPanel(
                   condition = "input.p_table_type == 'p_hitting'",
                   pickerInput(
                     inputId = "p_hitting_choices",
                     label = "Select Hitting Statistics to Display:",
                     choices = names(p_hit_df),
                     selected =  c("season", "Name", "Team", "BA"), 
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.p_table_type == 'p_pitching'",
                   pickerInput(
                     inputId = "p_pitching_choices",
                     label = "Select Pitching Statistics to Display:",
                     choices = names(p_pitch_df),
                     selected = c("season", "Name", "Team", "ERA", "SO", "IP"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   ), 
                 )
          )
        ),
        #Player table is the same call for both hitting and pitching (filtered later)
        DT::DTOutput("player_table"),
        
        #Setting up plots and summary analyses, also the key, which explains each statistic
        fluidRow(
          conditionalPanel(
            condition = "input.p_table_type == 'p_hitting'",
            br(),
            column(8,h3("Batters Visualized")),
            column(8,selectInput(
              "p_batter_plot_metric", "Select Batter Metric:",
              choices = names(p_hit_df),
              selected = "BA"
            )),
            column(10,
                   plotOutput("p_hit_plot", height = "400px")
            ),
            
            br(),
            br(),
            
            column(8,h3("Summary of Chosen Stat Among Your Batters")),
            column(8,tableOutput("chosen_hit_table")),
            
            br(),
            br(),
            
            column(8,h3("Summary of Chosen Stat Among All Batters")),
            column(8,tableOutput("all_hit_table")),
            
            br(),
            br(),
            column(8,h3("Batting Stats Key")),
            column(10,
                   DTOutput("hit_key"))
          ), conditionalPanel(
            condition = "input.p_table_type == 'p_pitching'",
            br(),
            column(8,h3("Pitchers Visualized")),
            column(8,selectInput(
              "p_pitcher_plot_metric", "Select Pitching Metric:",
              choices = names(p_pitch_df),
              selected = "ERA"
            )),
            column(10,
                   plotOutput("p_pitch_plot", height = "400px")
            ),
            
            br(),
            br(),
            
            column(8,h3("Summary of Chosen Stat Among Your Pitchers")),
            column(8,tableOutput("chosen_pitch_table")),
            
            br(),
            br(),
            
            column(8,h3("Summary of Chosen Stat Among All Pitchers")),
            column(8,tableOutput("all_pitch_table")),
            
            br(),
            br(),
            column(8,h3("Pitching Stats Key")),
            column(10,
                   DTOutput("pitch_key"))
          )
          
        ),
        
      ),
      
      tabItem(
        tabName = "modeling",
        h2("Statistical Modeling"),
        p("What Contributes Most to a Win?"),
        
        # Sidebar for inputs
        fluidRow(
          column(
            width = 3,
            
            # Team selection input
            selectizeInput(
              inputId = "regression_team_select",
              label = "Select Teams:",
              choices = unique(teams_df$team_name), # Assuming `team_name` is the column in `teams_df`
              selected = unique(teams_df$team_name)[1], 
              multiple = TRUE
            ),
            
            # Statistical table type selection
            radioButtons(
              inputId = "regression_table_type",
              label = "Select Stat Type:",
              choices = c("Hitting" = "hitting", 
                          "Fielding" = "fielding", 
                          "Pitching" = "pitching"),
              selected = "hitting"
            ),
            
            # Conditional inputs for columns based on stat type
            conditionalPanel(
              condition = "input.regression_table_type == 'hitting'",
              selectizeInput(
                inputId = "regression_hitting_column_select",
                label = "Select Hitting Stats:",
                choices = colnames(hitting_df), # Replace with actual column names
                multiple = TRUE
              )
            ),
            conditionalPanel(
              condition = "input.regression_table_type == 'fielding'",
              selectizeInput(
                inputId = "regression_fielding_column_select",
                label = "Select Fielding Stats:",
                choices = colnames(fielding_df), # Replace with actual column names
                multiple = TRUE
              )
            ),
            conditionalPanel(
              condition = "input.regression_table_type == 'pitching'",
              selectizeInput(
                inputId = "regression_pitching_column_select",
                label = "Select Pitching Stats:",
                choices = colnames(pitching_df), # Replace with actual column names
                multiple = TRUE
              )
            ),
            
            # Button to run regression
            actionButton(
              inputId = "run_regression",
              label = "Run Regression",
              icon = icon("chart-line")
            )
          ),
          
          column(
            width = 12,
            # Main panel for displaying results
              # Regression results table
              h4("Regression Results:"),
              verbatimTextOutput(outputId = "regression_results"),
            
            br(),
            
            # Section for selecting two stats and linear model toggle
            h4("Explore Relationships Between Two Statistics"),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "stat1_input",
                  label = "Select Stat 1:",
                  choices = colnames(all_stats),
                  selected = colnames(all_stats)[1]
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "stat2_input",
                  label = "Select Stat 2:",
                  choices = colnames(all_stats),
                  selected = colnames(all_stats)[2]
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                checkboxInput(
                  inputId = "add_lm",
                  label = "Run Linear Model?",
                  value = FALSE
                )
              )
            ),
            
            # Scatterplot output
            plotOutput(outputId = "stat_relationship_plot", height = "400px")
          )
        )
      )
      
  )
),
)



server <- function(input, output, session) {
  
  # Initialize hitting choices at server start
  hitting_choices <- setdiff(names(hitting_df), c("year", "team_id"))
  hitting_labels <- tools::toTitleCase(gsub("_", " ", hitting_choices))
  hitting_choices_named <- setNames(hitting_choices, hitting_labels)
  
  fielding_choices <- setdiff(names(fielding_df), c("year", "team_id"))
  fielding_labels <- tools::toTitleCase(gsub("_", " ", fielding_choices))
  fielding_choices_named <- setNames(fielding_choices, fielding_labels)
  
  pitching_choices <- setdiff(names(pitching_df), c("year", "team_id"))
  pitching_labels <- tools::toTitleCase(gsub("_", " ", pitching_choices))
  pitching_choices_named <- setNames(pitching_choices, pitching_labels)
 
  observe({
    updateSelectInput(session, "hitting_metric",
                      choices = hitting_choices_named)
  })
  
  # Update hitting column choices
  observe({
    updatePickerInput(
      session,
      "hitting_column_select",
      choices = hitting_choices_named,
      selected = "team_name"
    )
  })
  # Update fielding metric choices
  observe({
    updateSelectInput(session, "fielding_metric",
                      choices = fielding_choices_named)
  })
  
  # Update pitching metric choices
  observe({
    updateSelectInput(session, "pitching_metric",
                      choices = pitching_choices_named)
  })
  
  # Available teams reactive
  available_teams <- reactive({
    req(input$year_input)
    teams <- get_team_data(input$year_input)
    return(teams)
  })
  
  # Initialize team picker with named list
  # Initialize team picker
  observeEvent(available_teams(), {
    if (is.null(input$team_select)) {
      teams <- available_teams()
      
      if(nrow(teams) > 0) {
        team_choices <- setNames(teams$team_id, teams$team_name)
        updatePickerInput(
          session,
          "team_select",
          choices = team_choices,
          selected = team_choices
        )
      }
    }
  }, once = FALSE)
  # Create a reactive to map team IDs back to names
  team_id_to_name <- reactive({
    teams <- available_teams()
    setNames(teams$team_name, teams$team_id)
  })
  
  filtered_team_data <- reactive({
    req(input$team_select, input$column_select)
    
    cols_to_select <- unique(c("team_name", input$column_select))
    
    teams_df %>%
      filter(year == input$year_input,
             team_id %in% input$team_select) %>%
      select(all_of(cols_to_select))
  })
  
  
  
  # Filtered fielding data
  filtered_fielding_data <- reactive({
    req(input$team_select, input$year_input)
    
    # Initial filtering
    filtered_data <- fielding_df |>
      filter(year == input$year_input,
             team_id %in% input$team_select)
    
    # Summarize fielding statistics
    summarized_data <- filtered_data |>
      group_by(team_name) |>
      summarise(
        putouts = sum(putouts, na.rm = TRUE),
        assists = sum(assists, na.rm = TRUE),
        errors = sum(errors, na.rm = TRUE),
        double_plays = sum(double_plays, na.rm = TRUE),
        fielding_percentage = mean(fielding_percentage, na.rm = TRUE),
        total_chances = sum(total_chances, na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(summarized_data)
  })
  
  filtered_pitching_data <- reactive({
    # Ensure necessary inputs are provided
    req(input$team_select, input$year_input, input$pitching_column_select)
    
    # Define the columns to select
    cols_to_select <- c("team_name", input$pitching_column_select)
    
    # Filter and summarize the data
    #filtered_data <- pitching_df %>%
     # filter(
      #  year == input$year_input,
       # team_id %in% input$team_select
      #) %>%
      #select(all_of(cols_to_select)) %>%
      #group_by(team_name) %>%
      #summarise(
       # across(
        #  everything(), 
         # ~ if (is.numeric(.)) sum(., na.rm = TRUE) else first(.)
      #  ),
       # .groups = "drop"
    #  )
    #return(filtered_data)
  
  })
  
  
  # Filtered hitting data
  filtered_hitting_data <- reactive({
    req(input$team_select, input$year_input)
    validate(
      need(length(input$team_select) > 0, "Please select at least one team")
    )
    
    filtered_data <- hitting_df |>
      filter(year == input$year_input,
             team_id %in% input$team_select)
    
    summarized_data <- filtered_data |>
      group_by(team_name) |>
      summarise(
        batting_avg = mean(avg, na.rm = TRUE),  # Changed FALSE to TRUE
        home_runs = sum(home_runs, na.rm = TRUE),  # Changed FALSE to TRUE
        rbis = sum(rbi, na.rm = TRUE),
        ops = mean(ops, na.rm = TRUE),
        hits = sum(hits, na.rm = TRUE),
        doubles = sum(doubles, na.rm = TRUE),
        triples = sum(triples, na.rm = TRUE),
        walks = sum(base_on_balls, na.rm = TRUE),
        strikeouts = sum(strike_outs, na.rm = TRUE),
        .groups = 'drop'
      )
    
    summarized_data
  })
  # Fielding time series data preparation
  fielding_time_data <- reactive({
    req(input$team_select, input$fielding_metric)
    
    base_data <- fielding_df |>
      filter(team_id %in% input$team_select)
    
    if(length(input$team_select) > 5) {
      base_data |>
        group_by(year) |>
        summarise(
          value = mean(.data[[input$fielding_metric]], na.rm = TRUE),
          team_name = "Average",
          .groups = 'drop'
        ) |>
        filter(!is.na(value))
    } else {
      base_data |>
        group_by(year, team_name) |>
        summarise(
          value = mean(.data[[input$fielding_metric]], na.rm = TRUE),
          .groups = 'drop'
        ) |>
        filter(!is.na(value))
    }
  })
  
  # Pitching time series data preparation
  pitching_time_data <- reactive({
    req(input$team_select, input$pitching_metric)
    
    base_data <- pitching_df |>
      filter(team_id %in% input$team_select)
    
    if(length(input$team_select) > 5) {
      base_data |>
        group_by(year) |>
        summarise(
          value = mean(.data[[input$pitching_metric]], na.rm = TRUE),
          team_name = "Average",
          .groups = 'drop'
        ) |>
        filter(!is.na(value))
    } else {
      base_data |>
        group_by(year, team_name) |>
        summarise(
          value = mean(.data[[input$pitching_metric]], na.rm = TRUE),
          .groups = 'drop'
        ) |>
        filter(!is.na(value))
    }
  })
  
  
  #LEAGUE DATA 
  observe({
    updateSelectInput(session, "division_input", 
                      choices = c("All", unique(team_standings_df$Division)),
                      selected = "All")
  })
  
  league_filtered_data <- reactive({
    data <- team_standings_df
    
    if (!is.null(input$season_input)) {
      data <- data %>% filter(yearID %in% as.numeric(input$season_input))
    }
        if (input$division_input_bar != "All Divisions") {
      data <- data %>% filter(Division == input$division_input_bar)
    }
    
    return(data)
  })
  
  selected_metrics <- reactive({
    metrics <- c()
    if ("Wins" %in% input$metrics_input) metrics <- c(metrics, "W")
    if ("Attendance" %in% input$metrics_input) metrics <- c(metrics, "attendance")
    print(metrics)  # Debugging line
    return(metrics)
  })
  
  # League bar plot
  output$bar_graph <- renderPlot({
    data <- team_standings_df
    
    # Filter by season
    if (!is.null(input$season_input)) {
      data <- data %>% filter(yearID %in% as.numeric(input$season_input))
    }
    
    # Filter by division
    if (input$division_input_bar != "All Divisions") {
      data <- data %>% filter(Division == input$division_input_bar)
    }
    
    ggplot(data, aes(x = teamID, y = W, fill = Division)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Team Wins by Division",
        x = "Teams",
        y = "Wins",
        fill = "Division"
      ) +
      facet_wrap(~ Division, scales = "free") +
      theme_minimal()
  })
  
# Line Graph: Attendance Over Time
  output$line_graph <- renderPlot({
    data <- team_standings_df
    
    # Filter by league
    if (input$league_input != "Both") {
      league <- ifelse(input$league_input == "American League", "AL", "NL")
      data <- data %>% filter(lgID == league)
    }
    
    # Filter by division
    if (input$division_input_line != "All Divisions") {
      data <- data %>% filter(Division == input$division_input_line)
    }
    
    last_points <- data %>%
      group_by(teamID) %>%
      filter(yearID == max(yearID))
    
    ggplot(data, aes(x = yearID, y = attendance, color = teamID, group = teamID)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_text(data = last_points, 
                aes(label = teamID), 
                hjust = -0.2, 
                size = 3, 
                show.legend = FALSE) +
                
      labs(
        title = "Attendance Over Time by Team",
        x = "Year",
        y = "Attendance",
        color = "Team"
      ) +
      facet_wrap(~ Division, scales = "free_y") +
      theme_minimal()
  })
  
  
  # Data Table Output
  output$league_summary_table <- renderDT({
    data <- league_filtered_data()
    
    if (nrow(data) > 0) {
      datatable(
        data %>% select(yearID, teamID, lgID, G, W, L, attendance, Division),
        options = list(pageLength = 10)
      )
    } else {
      datatable(data.frame(), options = list(pageLength = 10))
    }
  })
  
  
  # Filter by season for descriptive stats and ANOVA
  filtered_data_stats <- reactive({
    data <- team_standings_df
    if (!is.null(input$season_input_stats)) {
      data <- data %>% filter(yearID %in% as.numeric(input$season_input_stats))
    }
    
    return(data)
  })
  
  # Descriptive Statistics Output
  output$descriptive_stats <- renderTable({
    data <-  filtered_data_stats()
    
    if (nrow(data) > 0) {
      summary_stats <- data.frame(
        Metric = c(
          "Mean_Wins", "Median_Wins", "SD_Wins",
          "Mean_Attendance", "Median_Attendance", "SD_Attendance"
        ),
        Value = c(
          mean(data$W, na.rm = TRUE), median(data$W, na.rm = TRUE), sd(data$W, na.rm = TRUE),
          mean(data$attendance, na.rm = TRUE), median(data$attendance, na.rm = TRUE), sd(data$attendance, na.rm = TRUE)
        )
      )
      return(summary_stats)
    } else {
      return(data.frame(Metric = "No data available", Value = ""))
    }
  })
  
  # ANOVA Table Output
  output$anova_table <- renderTable({
    data <-  filtered_data_stats()
    
    if (length(unique(data$Division)) > 1) {
      anova_result <- aov(attendance ~ Division, data = data)
      anova_summary <- summary(anova_result)[[1]]
      return(as.data.frame(anova_summary))
    } else {
      return(data.frame(
        Message = "ANOVA requires at least two levels for Division. Please select appropriate filters."
      ))
    }
  })
  
  # Render table
  output$combined_table <- DT::renderDT({
    req(input$team_select, input$table_type)
    
    data <- switch(input$table_type,
                   "team" = filtered_team_data(),
                   "hitting" = {
                     req(input$hitting_column_select)
                     filter_and_summarize_data(hitting_df, input)
                   },
                   "fielding" = {
                     req(input$fielding_column_select)
                     filter_and_summarize_data(fielding_df, input)
                   },
                   "pitching" = {
                     req(input$pitching_column_select)
                     filter_and_summarize_data(pitching_df, input)
                   }
    )
    
    format_datatable(data)
  })

  
  # Populate hitting metric choices
  observe({
    updateSelectInput(session, "hitting_metric",
                      choices = setNames(
                        hitting_choices,
                        tools::toTitleCase(gsub("_", " ", hitting_choices))
                      ))
  })
  # Update hitting column choices
  observe({
    hitting_labels <- gsub("_", " ", hitting_choices)
    hitting_labels <- tools::toTitleCase(hitting_labels)
    names(hitting_choices) <- hitting_labels
    
    updatePickerInput(
      session,
      "hitting_column_select",
      choices = hitting_choices,
      selected = "team_name"
    )
  })
  
  # Update fielding column choices
  observe({
    excluded_cols <- c("year", "team_id")
    fielding_choices <- setdiff(names(fielding_df), excluded_cols)
    
    fielding_labels <- gsub("_", " ", fielding_choices)
    fielding_labels <- tools::toTitleCase(fielding_labels)
    names(fielding_choices) <- fielding_labels
    
    updatePickerInput(
      session,
      "fielding_column_select",
      choices = fielding_choices,
      selected = c("team_name")
    )
  })
  
  # Update pitching column choices
  observe({
    excluded_cols <- c("year", "team_id")
    pitching_choices <- setdiff(names(pitching_df), excluded_cols)
    
    pitching_labels <- gsub("_", " ", pitching_choices)
    pitching_labels <- tools::toTitleCase(pitching_labels)
    names(pitching_choices) <- pitching_labels
    
    updatePickerInput(
      session,
      "pitching_column_select",
      choices = pitching_choices,
      selected = c("team_name")
    )
  })
  # Time series data preparation
  # Time series data preparation
  hitting_time_data <- reactive({
    req(input$team_select, input$hitting_metric)
    
    base_data <- hitting_df |>
      filter(team_id %in% input$team_select)
    
    if(length(input$team_select) > 5) {
      base_data |>
        group_by(year) |>
        summarise(
          value = mean(.data[[input$hitting_metric]], na.rm = TRUE),
          team_name = "Average",
          .groups = 'drop'
        ) |>
        filter(!is.na(value))
    } else {
      base_data |>
        group_by(year, team_name) |>
        summarise(
          value = mean(.data[[input$hitting_metric]], na.rm = TRUE),
          .groups = 'drop'
        ) |>
        filter(!is.na(value))
    }
  })
  
  
  
  
  # Hitting plot
  output$hitting_time_plot <- renderPlot({
    req(hitting_time_data())
    
    req(hitting_time_data())
    
    data <- hitting_time_data()
    metric_label <- names(hitting_choices_named)[hitting_choices_named == input$hitting_metric]
    if(length(metric_label) == 0) metric_label <- input$hitting_metric
    is_average_view <- length(input$team_select) > 5
    
    # Start Plot
    p <- ggplot(data, aes(x = year, y = value, group = team_name)) +
      theme_minimal() +
      theme(
        legend.position = if(is_average_view) "none" else "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        title = paste(metric_label, "Over Time"),
        subtitle = if(is_average_view) 
          "Showing average across selected teams" 
        else 
          "Individual team values",
        x = "Year",
        y = metric_label,
        color = "Team"
      ) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 2))
    
    if(is_average_view) {
      p + geom_line(color = "navy", linewidth = 1.5)
    } else {
      p +
        geom_line(aes(color = team_name), linewidth = 1) +
        geom_point(aes(color = team_name), size = 2) +
        guides(color = guide_legend(nrow = 2))
    }
  })
  
  # Fielding plot
  output$fielding_time_plot <- renderPlot({
    req(fielding_time_data())
    
    data <- fielding_time_data()
    metric_label <- names(fielding_choices_named)[fielding_choices_named == input$fielding_metric]
    if(length(metric_label) == 0) metric_label <- input$fielding_metric
    is_average_view <- length(input$team_select) > 5
    
    p <- ggplot(data, aes(x = year, y = value, group = team_name)) +
      theme_minimal() +
      theme(
        legend.position = if(is_average_view) "none" else "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        title = paste(metric_label, "Over Time"),
        subtitle = if(is_average_view) 
          "Showing average across selected teams" 
        else 
          "Individual team values",
        x = "Year",
        y = metric_label,
        color = "Team"
      ) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 2))
    
    if(is_average_view) {
      p + geom_line(color = "navy", linewidth = 1.5)
    } else {
      p +
        geom_line(aes(color = team_name), linewidth = 1) +
        geom_point(aes(color = team_name), size = 2) +
        guides(color = guide_legend(nrow = 2))
    }
  })
  
  # Pitching plot
  output$pitching_time_plot <- renderPlot({
    req(pitching_time_data())
    
    data <- pitching_time_data()
    metric_label <- names(pitching_choices_named)[pitching_choices_named == input$pitching_metric]
    if(length(metric_label) == 0) metric_label <- input$pitching_metric
    is_average_view <- length(input$team_select) > 5
    
    p <- ggplot(data, aes(x = year, y = value, group = team_name)) +
      theme_minimal() +
      theme(
        legend.position = if(is_average_view) "none" else "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        title = paste(metric_label, "Over Time"),
        subtitle = if(is_average_view) 
          "Showing average across selected teams" 
        else 
          "Individual team values",
        x = "Year",
        y = metric_label,
        color = "Team"
      ) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 2))
    
    if(is_average_view) {
      p + geom_line(color = "navy", linewidth = 1.5)
    } else {
      p +
        geom_line(aes(color = team_name), linewidth = 1) +
        geom_point(aes(color = team_name), size = 2) +
        guides(color = guide_legend(nrow = 2))
    }
  })


  
  #Players Tab
  
  #Not everyone might be as into baseball as we are! Here's a key to help understand the abbreviations in the stats here:
  
  output$hit_key <- DT::renderDataTable({
    statkey <- tibble("Batting Stat" = c("bbref_id", "season", "Name", "Age", "Level", "Team", "G", "PA", 
                                         "AB", "R", "H", "X1B", "X2B", "X3B", "HR", "RBI", "BB", 
                                         "IBB", "uBB", "SO", "HBP", "SH", "SF", "GDP", "SB", "CS", "BA", 
                                         "OBP", "SLG", "OPS"),
                      "What it Means" = c("ID given to player by database", "MLB Season", "Name of Player", "Age of Player", "League (minor, major)",
                                          "Player's Team(s)", "Games Played", "Plate Appearances", "At Bats", 
                                          "Runs", "Hits", "Singles", "Doubles", "Triples", "Home Runs", "Runs Batted In", 
                                          "Walks", "Intentional Walks", "Unintentional Walks", "Strikeouts", 
                                          "Hit by Pitch", "Sacrifice Bunt", "Sacrifice Fly", "Grounds into Double Play",
                                          "Stolen Bases", "Caught Stealing %", "Batting Average", "On Base Percentage",
                                          "Slugging Percentage", "On-Base + Slugging"))
    format_datatable(statkey)
  })
  
  output$pitch_key <- DT::renderDataTable({
    statkey2 <- tibble("Pitching Stat" = c("bbref_id", "season", "Name", "Age", "Level", "Team", 
                                           "G", "GS", "W", "L", "SV", "IP", "H", "R", "ER", 
                                           "uBB", "BB", "SO", "HR", "HBP", "ERA", "AB", "X1B", "X2B", 
                                           "X3B", "IBB", "GDP", "SF", "SB", "CS", "PO", "BF", "Pit", 
                                           "Str", "StL", "StS", "GB.FB", "LD", "PU", "WHIP", "BAbip",
                                           "SO9", "SO.W", "SO_perc", "uBB_perc", "SO_uBB"),
                       "What it Means" = c("ID given by pitcher to database", "MLB Season", "Name of Pitcher", 
                                           "Age of Pitcher", "League (minor, major)", "Pitcher's Team(s)",
                                           "Games", "Games Started", "Wins", "Losses", "Saves", "Innings Pitched",
                                           "Hits", "Runs", "Earned Runs",
                                           "Unintentional Walks", "Walks", "Strikeouts", "Home Runs",
                                           "Hit by Pitch", "Earned Run Average", "At Bats", "Singles",
                                           "Doubles", "Triples", "Intentional Walks", "Grounds into Double Play",
                                           "Sacrifice Fly", "Stolen Bases", "Caught Stealing %", "Outs Made", 
                                           "Batters Faced", "Pitch Count", "Strikes", "Stolen Bases", NA, 
                                           "Ground Ball-Fly Ball Ratio", "Line Drive Rate", NA, "Walks And Hits Per Inning Pitched",
                                           "Batting Average on Balls in Play", "Strikeouts per 9 Innings Pitched",
                                           NA, NA, NA, "Strikeouts per Unintentional Walk"))
    #some of these were really impossible to decipher, so I left NA! Didn't filter out in case there are some HUGE baseball fans who see this and find it useful/already know that meaning
    format_datatable(statkey2)
  })
  
  # Updating the player selection dropdown menus 
  
  observe({
    updatePickerInput(session, "p_player_select_hit", choices = p_hit_df$Name,
                      selected = c("Rowdy Tellez", "Andrew McCutchen", "Vladimir Guerrero Jr.", "Xander Bogaerts", "Rafael Devers")
    )
  })
  
  observe({
    updatePickerInput(session, "p_player_select_pitch",
                      choices = p_pitch_df$Name,
                      selected = c("Paul Skenes", "Gerrit Cole", "Chris Sale", "Shohei Ohtani", "Clayton Kershaw")
    )
  })
  
  #Creating the player table (including filtering to show only batters or only pitchers)
  
  output$player_table <- DT::renderDT({
    if(input$p_table_type == "p_hitting"){
      data <- p_hit_df %>%
        select(input$p_hitting_choices) %>%
        filter(Name %in% input$p_player_select_hit,
               season %in% input$p_year_input)
    } else if (input$p_table_type == "p_pitching"){
      data <- p_pitch_df %>%
        select(input$p_pitching_choices) %>%
        filter(Name %in% input$p_player_select_pitch,
               season %in% input$p_year_input) 
    }
    
    #Using the earlier defined format_datatable() for some consistency among the tabs
    format_datatable(data)
    data <- data %>%
      arrange(Name)
  })
  
  #This part makes sure that the dropdown menus to select the Y variable in the plots can be updated
  
  observe({
    updateSelectInput(session, "p_batter_plot_metric", choices = names(p_hit_df), selected = "BA")
  })
  
  observe({
    updateSelectInput(session, "p_pitcher_plot_metric", choices = names(p_pitch_df), selected = "ERA")
  })
  
  #Generate the batter plot
  
  output$p_hit_plot <- renderPlot({
    p_hit_edit <- p_hit_df %>%
      filter(Name %in% input$p_player_select_hit,
             season %in% input$p_year_input)
    
    p <- ggplot(p_hit_edit, aes(x = Name, y= get(input$p_batter_plot_metric)))  #COME BACK TO THIS
    theme_minimal()
    p <- p +
      geom_bar(fill="navy", stat="identity") + 
      ggtitle(paste(input$p_batter_plot_metric, "Per Player")) +
      xlab("Name") +
      ylab(input$p_batter_plot_metric)
    p
  })
  
  # Generate the pitcher plot
  
  output$p_pitch_plot <- renderPlot({
    p_pitch_edit <- p_pitch_df %>%
      filter(Name %in% input$p_player_select_pitch,
             season %in% input$p_year_input)
    
    p <- ggplot(p_pitch_edit, aes(x = Name, y = get(input$p_pitcher_plot_metric))) +
      theme_minimal() 
    p <- p + geom_bar(fill="navy", stat="identity") +
      ggtitle(paste(input$p_pitcher_plot_metric, "Per Pitcher")) +
      xlab("Name") + 
      ylab(input$p_pitcher_plot_metric)
    p
  })
  
  #This part generates all four statistical summaries
  output$chosen_hit_table <- renderTable({
    temp_hit <- p_hit_df %>%
      filter(Name %in% input$p_player_select_hit,
             season %in% input$p_year_input)
    tibble::enframe(summary(temp_hit[[input$p_batter_plot_metric]]))
  })
  
  output$chosen_pitch_table <- renderTable({
    temp_pitch <- p_pitch_df %>%
      filter(Name %in% input$p_player_select_pitch,
             season %in% input$p_year_input)
    tibble::enframe(summary(temp_pitch[[input$p_pitcher_plot_metric]]))
  })
  
  output$all_hit_table <- renderTable({ #COME BACK TO ALL THIS
    tibble::enframe(summary(p_hit_df[[input$p_batter_plot_metric]]))
  })
  
  output$all_pitch_table <- renderTable({
    tibble::enframe(summary(p_pitch_df[[input$p_pitcher_plot_metric]]))
  })

  
  #regression
  
  observe({
    all_teams <- unique(c(
      pitching_df$team_name, fielding_df$team_name, hitting_df$team_name
    ))
    updateSelectInput(session, "regression_team_select", choices = all_teams)
  })
  
  # Dynamic UI for selecting columns based on the chosen stat type
  output$stat_column_select <- renderUI({
    stat_type <- input$regression_table_type
    columns <- switch(
      stat_type,
      "hitting" = colnames(hitting_df)[!colnames(hitting_df) %in% c("team_name", "year")],
      "fielding" = colnames(fielding_df)[!colnames(fielding_df) %in% c("team_name", "year")],
      "pitching" = colnames(pitching_df)[!colnames(pitching_df) %in% c("team_name", "year")]
    )
    checkboxGroupInput(
      paste0("regression_", stat_type, "_column_select"),
      "Select Statistics",
      choices = columns
    )
  })
  
  # Observe the Run Regression button
  observeEvent(input$run_regression, {
    # Gather inputs
    selected_teams <- input$regression_team_select
    stat_type <- input$regression_table_type
    
    # Select the correct data frame
    selected_df <- switch(
      stat_type,
      "hitting" = hitting_df,
      "fielding" = fielding_df,
      "pitching" = pitching_df
    )
    
    # Filter the selected data frame
    data_for_regression <- selected_df %>%
      filter(
        team_name %in% selected_teams
      )
    
    validate(
      need(nrow(data_for_regression) > 0, "No data available for the selected year and teams.")
    )
    
    # Get user-selected columns for the regression
    selected_columns <- input[[paste0("regression_", stat_type, "_column_select")]]
    validate(
      need(length(selected_columns) > 0, "Please select at least one statistic for the regression.")
    )
    
    # Join with team results data frame
    data_for_regression <- team_results_df %>%
      left_join(data_for_regression, by = "team_name", relationship = "many-to-many") %>%
      select(team_name, total_wins, all_of(selected_columns))
    
    
    # Perform the regression
    regression_formula <- as.formula(paste("total_wins ~", paste(selected_columns, collapse = " + ")))
    # Perform regression
    tryCatch({
      model <- lm(regression_formula, data = data_for_regression)
      
      # Format regression results
      output$regression_results <- renderPrint({
        summary(model)
      })
      
    }, error = function(e) {
      showNotification("Regression failed. Check your inputs.", type = "error")
    })
    
  })
  
  output$stat_relationship_plot <- renderPlot({
    req(input$stat1_input, input$stat2_input) # Ensure both stats are selected
    
    plot_data <- all_stats %>%
      filter(year %in% input$regression_year_input,
             team_name %in% input$regression_team_select)
    
    gg <- ggplot(all_stats, aes_string(x = input$stat1_input, y = input$stat2_input)) +
      geom_point(color = "blue") +
      labs(
        x = input$stat1_input,
        y = input$stat2_input,
        title = paste("Relationship Between", input$stat1_input, "and", input$stat2_input)
      ) 
    
    # Add linear model line if checkbox is checked
    if (input$add_lm) {
      gg <- gg + geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE)
    }
    
    gg
  })
  
}

shinyApp(ui = ui, server = server)