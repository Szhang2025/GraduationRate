
# ============================================================
# U.S. State Graduation Rate Explorer
# Shiny App
# ============================================================

# Install packages if needed:
# install.packages(c("shiny", "plotly", "DT", "dplyr"))

library(shiny)
library(plotly)
library(DT)
library(dplyr)

# ============================================================
# DATA
# 2023-24 Public High School Averaged Freshman Graduation Rate
# Source: U.S. Department of Education, NCES, Table 219.35
# ============================================================

graduation_data <- data.frame(
  State = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  
  Abbreviation = c(
    "AL", "AK", "AZ", "AR", "CA",
    "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA",
    "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO",
    "MT", "NE", "NV", "NH", "NJ",
    "NM", "NY", "NC", "ND", "OH",
    "OK", "OR", "PA", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT",
    "VA", "WA", "WV", "WI", "WY"
  ),
  
  Graduation_Rate = c(
    85.1, 85.7, 88.0, 88.7, 92.0,
    88.2, 90.8, 89.3, 89.7, 85.4,
    82.2, 88.6, 88.0, 91.3, 92.0,
    94.6, 89.9, 80.9, 90.7, 88.1,
    90.3, 86.4, 92.7, 82.3, 91.1,
    82.8, 92.9, 85.2, 85.6, 89.0,
    72.9, 86.0, 88.8, 84.9, 92.0,
    88.7, 87.7, 92.1, 85.2, 83.7,
    81.4, 83.3, 91.2, 89.3, 79.2,
    93.4, 81.9, 82.2, 96.2, 83.8
  )
)

# Rank states
graduation_data <- graduation_data %>%
  arrange(desc(Graduation_Rate)) %>%
  mutate(Rank = row_number())

# National rate used by NCES for this display
national_rate <- 89.0


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # Custom CSS
  # ----------------------------------------------------------
  
  tags$head(
    tags$title("U.S. State Graduation Rate Explorer"),
    
    tags$style(HTML("
      body {
        background-color: #f4f7fb;
        font-family: 'Segoe UI', Arial, sans-serif;
        color: #172033;
      }

      .app-header {
        background: linear-gradient(135deg, #0f172a, #1d4ed8);
        color: white;
        padding: 35px 30px;
        margin-bottom: 25px;
      }

      .app-header h1 {
        font-size: 40px;
        font-weight: 700;
        margin-bottom: 8px;
      }

      .app-header p {
        font-size: 16px;
        opacity: 0.92;
        max-width: 900px;
      }

      .stat-card {
        background: white;
        border: 1px solid #e2e8f0;
        border-radius: 16px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 8px 25px rgba(15,23,42,0.07);
        min-height: 115px;
      }

      .stat-label {
        color: #64748b;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 600;
      }

      .stat-value {
        color: #172033;
        font-size: 25px;
        font-weight: 700;
        margin-top: 8px;
      }

      .control-panel {
        background: white;
        border-radius: 16px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 8px 25px rgba(15,23,42,0.07);
        border: 1px solid #e2e8f0;
      }

      .content-panel {
        background: white;
        border-radius: 16px;
        padding: 20px;
        box-shadow: 0 8px 25px rgba(15,23,42,0.07);
        border: 1px solid #e2e8f0;
        margin-bottom: 20px;
      }

      .content-panel h3 {
        margin-top: 0;
        font-size: 19px;
        font-weight: 700;
      }

      .small-note {
        color: #64748b;
        font-size: 13px;
        margin-bottom: 15px;
      }

      .info-box {
        background: #eff6ff;
        border-radius: 12px;
        padding: 14px;
        margin-top: 15px;
        color: #334155;
        font-size: 13px;
      }

      .footer-note {
        color: #64748b;
        font-size: 12px;
        padding: 10px 0 30px 0;
      }

      .form-control {
        border-radius: 9px;
        border: 1px solid #cbd5e1;
      }

      .selectize-input {
        border-radius: 9px !important;
      }

      .btn {
        border-radius: 9px;
      }

      .dataTables_wrapper {
        font-size: 14px;
      }

      @media (max-width: 768px) {
        .app-header h1 {
          font-size: 28px;
        }
      }
    "))
  ),
  
  # ----------------------------------------------------------
  # Header
  # ----------------------------------------------------------
  
  div(
    class = "app-header",
    
    h1("U.S. State Graduation Rate Explorer"),
    
    p(
      "Explore the 2023–24 public high school averaged freshman ",
      "graduation rate (AFGR) for all 50 states. Search, filter, ",
      "compare, and visualize geographic patterns."
    )
  ),
  
  # ----------------------------------------------------------
  # Main container
  # ----------------------------------------------------------
  
  div(
    class = "container-fluid",
    style = "max-width: 1250px;",
    
    # --------------------------------------------------------
    # Summary Cards
    # --------------------------------------------------------
    
    fluidRow(
      
      column(
        width = 3,
        
        div(
          class = "stat-card",
          
          div(class = "stat-label", "U.S. Rate"),
          
          div(
            class = "stat-value",
            textOutput("national_rate", inline = TRUE)
          )
        )
      ),
      
      column(
        width = 3,
        
        div(
          class = "stat-card",
          
          div(class = "stat-label", "Highest State"),
          
          div(
            class = "stat-value",
            textOutput("highest_state", inline = TRUE)
          )
        )
      ),
      
      column(
        width = 3,
        
        div(
          class = "stat-card",
          
          div(class = "stat-label", "Lowest State"),
          
          div(
            class = "stat-value",
            textOutput("lowest_state", inline = TRUE)
          )
        )
      ),
      
      column(
        width = 3,
        
        div(
          class = "stat-card",
          
          div(class = "stat-label", "50-State Average"),
          
          div(
            class = "stat-value",
            textOutput("average_rate", inline = TRUE)
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Controls
    # --------------------------------------------------------
    
    div(
      class = "control-panel",
      
      fluidRow(
        
        column(
          width = 5,
          
          textInput(
            inputId = "search_state",
            label = "Search for a state",
            placeholder = "Example: Minnesota, Texas, MN..."
          )
        ),
        
        column(
          width = 4,
          
          selectInput(
            inputId = "rate_filter",
            label = "Graduation-rate range",
            
            choices = c(
              "All states" = "all",
              "90% or higher" = "90plus",
              "85%–89.9%" = "85to90",
              "80%–84.9%" = "80to85",
              "Below 80%" = "below80"
            ),
            
            selected = "all"
          )
        ),
        
        column(
          width = 3,
          
          br(),
          
          actionButton(
            inputId = "reset",
            label = "Reset Filters",
            icon = icon("rotate-left"),
            class = "btn-primary"
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # Map + Table
    # --------------------------------------------------------
    
    fluidRow(
      
      # Map
      column(
        width = 7,
        
        div(
          class = "content-panel",
          
          h3("Graduation Rate by State"),
          
          p(
            class = "small-note",
            "Darker shading indicates a higher graduation rate. ",
            "Hover over a state to see its rate."
          ),
          
          plotlyOutput(
            outputId = "graduation_map",
            height = "570px"
          )
        )
      ),
      
      # Table
      column(
        width = 5,
        
        div(
          class = "content-panel",
          
          h3("State Data"),
          
          p(
            class = "small-note",
            "Use the table controls to search and sort the states."
          ),
          
          DTOutput("state_table"),
          
          uiOutput("selection_message")
        )
      )
    ),
    
    # --------------------------------------------------------
    # About the data
    # --------------------------------------------------------
    
    div(
      class = "content-panel",
      
      h3("About the Data"),
      
      p(
        "The rates shown here are the ",
        strong(
          "public high school averaged freshman graduation rate (AFGR)"
        ),
        " for school year 2023–24."
      ),
      
      p(
        "AFGR estimates the percentage of students who receive a ",
        "regular diploma within four years of entering ninth grade."
      ),
      
      p(
        "It is different from the Adjusted Cohort Graduation Rate ",
        "(ACGR), another commonly used four-year graduation measure."
      ),
      
      div(
        class = "info-box",
        
        strong("Important: "),
        
        "This application displays the 50 states only. ",
        "The District of Columbia and other jurisdictions are not ",
        "included in the state ranking."
      )
    ),
    
    # --------------------------------------------------------
    # Footer
    # --------------------------------------------------------
    
    div(
      class = "footer-note",
      
      "Source: U.S. Department of Education, National Center for ",
      "Education Statistics (NCES), Common Core of Data, ",
      "Digest of Education Statistics, Table 219.35."
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # National rate
  # ----------------------------------------------------------
  
  output$national_rate <- renderText({
    paste0(format(national_rate, nsmall = 1), "%")
  })
  
  
  # ----------------------------------------------------------
  # Highest state
  # ----------------------------------------------------------
  
  output$highest_state <- renderText({
    
    x <- graduation_data %>%
      arrange(desc(Graduation_Rate)) %>%
      slice(1)
    
    paste0(
      x$State,
      " ",
      format(x$Graduation_Rate, nsmall = 1),
      "%"
    )
  })
  
  
  # ----------------------------------------------------------
  # Lowest state
  # ----------------------------------------------------------
  
  output$lowest_state <- renderText({
    
    x <- graduation_data %>%
      arrange(Graduation_Rate) %>%
      slice(1)
    
    paste0(
      x$State,
      " ",
      format(x$Graduation_Rate, nsmall = 1),
      "%"
    )
  })
  
  
  # ----------------------------------------------------------
  # 50-state average
  # ----------------------------------------------------------
  
  output$average_rate <- renderText({
    
    avg <- mean(graduation_data$Graduation_Rate)
    
    paste0(
      format(avg, nsmall = 1),
      "%"
    )
  })
  
  
  # ----------------------------------------------------------
  # Filtered data
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    df <- graduation_data
    
    # Search
    search <- trimws(tolower(input$search_state))
    
    if (search != "") {
      
      df <- df[
        grepl(
          search,
          tolower(df$State),
          fixed = TRUE
        ) |
          tolower(df$Abbreviation) == search,
      ]
    }
    
    
    # Rate filter
    filter_value <- input$rate_filter
    
    if (filter_value == "90plus") {
      
      df <- df %>%
        filter(Graduation_Rate >= 90)
      
    } else if (filter_value == "85to90") {
      
      df <- df %>%
        filter(
          Graduation_Rate >= 85,
          Graduation_Rate < 90
        )
      
    } else if (filter_value == "80to85") {
      
      df <- df %>%
        filter(
          Graduation_Rate >= 80,
          Graduation_Rate < 85
        )
      
    } else if (filter_value == "below80") {
      
      df <- df %>%
        filter(Graduation_Rate < 80)
    }
    
    df
  })
  
  
  # ----------------------------------------------------------
  # Interactive map
  # ----------------------------------------------------------
  
  output$graduation_map <- renderPlotly({
    
    df <- filtered_data()
    
    validate(
      need(
        nrow(df) > 0,
        "No states match the selected filters."
      )
    )
    
    plot_ly(
      data = df,
      
      type = "choropleth",
      
      locationmode = "USA-states",
      
      locations = ~Abbreviation,
      
      z = ~Graduation_Rate,
      
      text = ~State,
      
      customdata = ~Graduation_Rate,
      
      colorscale = list(
        c(0.00, "#fee2e2"),
        c(0.25, "#fed7aa"),
        c(0.50, "#bfdbfe"),
        c(0.75, "#60a5fa"),
        c(1.00, "#1d4ed8")
      ),
      
      zmin = 70,
      
      zmax = 100,
      
      marker = list(
        line = list(
          color = "white",
          width = 1
        )
      ),
      
      colorbar = list(
        title = "Rate (%)",
        ticksuffix = "%"
      ),
      
      hovertemplate =
        "<b>%{text}</b><br>" ~
        "Graduation rate: %{z:.1f}%<br>" ~
        "U.S. rate: 89.0%" ~
        "<extra></extra>"
    ) %>%
      
      layout(
        
        geo = list(
          scope = "usa",
          showlakes = TRUE,
          lakecolor = "#dbeafe",
          bgcolor = "rgba(0,0,0,0)"
        ),
        
        margin = list(
          l = 0,
          r = 0,
          t = 0,
          b = 0
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  # ----------------------------------------------------------
  # Data table
  # ----------------------------------------------------------
  
  output$state_table <- renderDT({
    
    df <- filtered_data()
    
    df <- df %>%
      arrange(desc(Graduation_Rate)) %>%
      mutate(
        Rank = row_number(),
        `Graduation Rate` =
          paste0(
            sprintf("%.1f", Graduation_Rate),
            "%"
          )
      ) %>%
      select(
        Rank,
        State,
        Abbreviation,
        `Graduation Rate`
      )
    
    datatable(
      
      df,
      
      rownames = FALSE,
      
      colnames = c(
        "Rank",
        "State",
        "State",
        "Graduation Rate"
      ),
      
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        searching = TRUE,
        ordering = TRUE,
        autoWidth = TRUE
      ),
      
      class = "stripe hover compact"
    )
  })
  
  
  # ----------------------------------------------------------
  # Selection message
  # ----------------------------------------------------------
  
  output$selection_message <- renderUI({
    
    n <- nrow(filtered_data())
    
    div(
      class = "info-box",
      
      paste0(
        "Showing ",
        n,
        " of 50 states."
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # Reset button
  # ----------------------------------------------------------
  
  observeEvent(input$reset, {
    
    updateTextInput(
      session,
      "search_state",
      value = ""
    )
    
    updateSelectInput(
      session,
      "rate_filter",
      selected = "all"
    )
  })
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
