library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(DT)

# ---------- DATA ----------
# Put cbb25.csv in the same folder as app.R
df <- read.csv("cbb25.csv")

stat_descriptions <- list(
  ADJOE  = "Adjusted Offensive Efficiency: Points scored per 100 possessions, adjusted for opponent strength.",
  ADJDE  = "Adjusted Defensive Efficiency: Points allowed per 100 possessions, adjusted for opponent strength (lower is better).",
  BARTHAG = "Power rating combining offensive and defensive efficiency to estimate win probability vs average team.",
  EFG_O  = "Effective Field Goal % (Offense): Shooting % that accounts for extra value of 3-pointers.",
  EFG_D  = "Effective Field Goal % Allowed (Defense): Opponent shooting efficiency.",
  TOR    = "Turnover Rate (Offense): % of possessions ending in a turnover.",
  TORD   = "Turnover Rate Forced (Defense): % of opponent possessions forced into turnover.",
  ORB    = "Offensive Rebound Rate: % of available offensive rebounds captured.",
  DRB    = "Defensive Rebound Rate: % of available defensive rebounds captured.",
  FTR    = "Free Throw Rate (Offense): Free throw attempts per field goal attempt.",
  FTRD   = "Free Throw Rate Allowed (Defense).",
  `2P_O` = "2-Point % (Offense).",
  `2P_D` = "2-Point % Allowed (Defense).",
  `3P_O` = "3-Point % (Offense).",
  `3P_D` = "3-Point % Allowed (Defense).",
  ADJ_T  = "Adjusted Tempo: Estimated possessions per 40 minutes.",
  WAB    = "Wins Above Bubble: How many more wins than a 'bubble' tournament team."
)

# Clean/standardize types
df <- df %>%
  mutate(
    CONF = as.character(CONF),
    Team = as.character(Team),
    SEED = as.numeric(SEED),
    WAB = as.numeric(WAB)
  )

# Choose numeric columns that we can graph as "metrics"
numeric_cols <- df %>%
  select(where(is.numeric)) %>%
  names()

# Remove ranking column from metrics if you want
numeric_cols <- setdiff(numeric_cols, c("RK"))

# ---------- UI ----------
ui <- page_sidebar(
  title = "College Basketball Statistics (cbb25)",
  sidebar = sidebar(
    width = 320,
    
    selectInput(
      "conf",
      "Conference",
      choices = c("All", sort(unique(df$CONF))),
    ),
    
    sliderInput(
      "seed_range",
      "Seed Range (only teams with a seed)",
      min = 1, max = 16, value = c(1, 16), step = 1
    ),
    
    checkboxInput("include_unseeded", "Include unseeded teams", value = TRUE),
    
    selectInput(
      "x_metric",
      "X-axis metric",
      choices = numeric_cols,
      selected = "BARTHAG"
    ),
    
    selectInput(
      "y_metric",
      "Y-axis metric",
      choices = numeric_cols,
      selected = "W"
    ),
    
    selectInput(
      "bar_metric",
      "Bar chart metric (Top teams)",
      choices = numeric_cols,
      selected = "BARTHAG"
    ),
    
    uiOutput("stat_tip"),
    
    sliderInput("top_n", "Top N teams", min = 5, max = 50, value = 25, step = 5),
    
  ),
  
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Scatter: Compare teams across two metrics"),
      plotlyOutput("scatter", height = "520px")
    ),
    card(
      full_screen = TRUE,
      card_header("Bar chart: Top teams by selected metric"),
      plotlyOutput("bar", height = "520px")
    ),
    col_widths = c(6, 6)
  ),
  
  card(
    full_screen = TRUE,
    card_header("Filtered table"),
    DTOutput("table")
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  filtered <- reactive({
    out <- df
    
    # Conference filter
    if (input$conf != "All") {
      out <- out %>% filter(CONF == input$conf)
    }
    
    # Seed filter logic
    if (!input$include_unseeded) {
      out <- out %>% filter(!is.na(SEED))
    }
    
    # If seeded teams exist, apply range to them; keep unseeded if include_unseeded = TRUE
    out <- out %>%
      filter(
        (is.na(SEED) & input$include_unseeded) |
          (!is.na(SEED) & SEED >= input$seed_range[1] & SEED <= input$seed_range[2])
      )
    
    out
  })
  
  
  output$stat_tip <- renderUI({
    x_desc  <- stat_descriptions[[input$x_metric]]
    y_desc  <- stat_descriptions[[input$y_metric]]
    bar_desc <- stat_descriptions[[input$bar_metric]]
    
    div(
      style = "background-color:#f8f9fa; padding:12px; border-radius:8px;",
      h5("Metric Explanations"),
      p(strong("X-axis: "), x_desc),
      p(strong("Y-axis: "), y_desc),
      p(strong("Bar Chart: "), bar_desc)
    )
  })
  
  
  output$scatter <- renderPlotly({
    d <- filtered()
    
    # Guard against empty filter results
    validate(
      need(nrow(d) > 0, "No teams match your filters. Try widening the filters.")
    )
    
    p <- ggplot(
      d,
      aes(
        x = .data[[input$x_metric]],
        y = .data[[input$y_metric]],
        text = paste0(
          "<b>", Team, "</b>",
          "<br>Conf: ", CONF,
          "<br>Seed: ", ifelse(is.na(SEED), "Unseeded", SEED),
          "<br>", input$x_metric, ": ", .data[[input$x_metric]],
          "<br>", input$y_metric, ": ", .data[[input$y_metric]],
          "<br>WAB: ", WAB
        )
      )
    ) +
      geom_point(alpha = 0.8) +
      labs(x = input$x_metric, y = input$y_metric)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$bar <- renderPlotly({
    d <- filtered()
    
    validate(
      need(nrow(d) > 0, "No teams match your filters. Try widening the filters.")
    )
    
    # Pick Top N by selected metric
    # Note: for metrics where "lower is better" (like ADJDE), you might want an option later.
    top <- d %>%
      arrange(desc(.data[[input$bar_metric]])) %>%
      slice_head(n = input$top_n)
    
    p <- ggplot(top, aes(x = reorder(Team, .data[[input$bar_metric]]), y = .data[[input$bar_metric]],
                         text = paste0("<b>", Team, "</b><br>", input$bar_metric, ": ", .data[[input$bar_metric]]))) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = input$bar_metric)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$table <- renderDT({
    datatable(
      filtered() %>% arrange(RK),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)