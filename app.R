# Load required libraries
library(shiny)
library(plotly)
library(DT)
library(dplyr)

# Use the built-in state dataset
data(state)
state_df <- data.frame(
  state   = state.name,
  abbrev  = state.abb,
  hs_grad = state.x77[, "HS Grad"],
  region  = state.region,
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("U.S. High School Graduation Rates by State"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore high school graduation rates across U.S. states."),
      
      selectInput("plot_type", "Choose visualization:",
                  choices = c("Map", "Bar Chart", "Table"),
                  selected = "Map"),
      
      # Sort order control (applies to Bar Chart and Table)
      conditionalPanel(
        condition = "input.plot_type == 'Bar Chart' || input.plot_type == 'Table'",
        radioButtons("sort_order", "Sort by graduation rate:",
                     choices = c("Highest first (descending)" = "desc",
                                 "Lowest first (ascending)"  = "asc"),
                     selected = "desc")
      ),
      
      # Top N only for bar chart
      conditionalPanel(
        condition = "input.plot_type == 'Bar Chart'",
        sliderInput("top_n", "Show top N states:",
                    min = 5, max = 50, value = 15, step = 5)
      ),
      
      br(),
      h4("National Summary"),
      verbatimTextOutput("summary_text")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'Map'",
        plotlyOutput("map_plot", height = "600px")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Bar Chart'",
        plotlyOutput("bar_plot", height = "600px")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Table'",
        DTOutput("table_output")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  plot_data <- reactive({ state_df })
  
  # Summary text
  output$summary_text <- renderPrint({
    df <- plot_data()
    cat("National average:", round(mean(df$hs_grad), 1), "%\n")
    cat("Highest:", df$state[which.max(df$hs_grad)],
        "(", round(max(df$hs_grad), 1), "% )\n")
    cat("Lowest:",  df$state[which.min(df$hs_grad)],
        "(", round(min(df$hs_grad), 1), "% )\n")
    cat("Number of states:", nrow(df), "\n")
  })
  
  # Map (no sort — geography is fixed)
  output$map_plot <- renderPlotly({
    df <- plot_data()
    plot_geo(df, locationmode = "USA-states") %>%
      add_trace(
        z = ~hs_grad,
        locations = ~abbrev,
        text = ~paste(state, "<br>Graduation Rate:", round(hs_grad, 1), "%"),
        hoverinfo = "text",
        colorscale = "Blues",
        colorbar = list(title = "HS Grad Rate (%)")
      ) %>%
      layout(
        title = "High School Graduation Rate by State",
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = "white"
        )
      )
  })
  
  # Bar chart — respects the sort order toggle
  output$bar_plot <- renderPlotly({
    df      <- plot_data()
    top_n   <- input$top_n
    sortord <- input$sort_order
    
    if (sortord == "desc") {
      df_sorted <- df %>% arrange(desc(hs_grad)) %>% slice_head(n = top_n)
      title_txt <- paste("Top", top_n, "States — Highest Graduation Rates")
      # Reorder factor so tallest bar appears first (left-to-right)
      df_sorted$state <- factor(df_sorted$state, levels = df_sorted$state[order(df_sorted$hs_grad)])
    } else {
      df_sorted <- df %>% arrange(hs_grad) %>% slice_head(n = top_n)
      title_txt <- paste("Bottom", top_n, "States — Lowest Graduation Rates")
      df_sorted$state <- factor(df_sorted$state, levels = df_sorted$state[order(df_sorted$hs_grad)])
    }
    
    plot_ly(
      data = df_sorted,
      x = ~state,
      y = ~hs_grad,
      type = "bar",
      text = ~paste(round(hs_grad, 1), "%"),
      textposition = "outside",
      hoverinfo = "text",
      hovertext = ~paste(state, "<br>", round(hs_grad, 1), "%"),
      marker = list(color = ~hs_grad, colorscale = "Blues", showscale = FALSE)
    ) %>%
      layout(
        title = title_txt,
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Graduation Rate (%)", range = c(0, 100)),
        margin = list(b = 120)
      )
  })
  
  # Data table — respects the same sort order toggle
  output$table_output <- renderDT({
    df      <- plot_data()
    sortord <- input$sort_order
    
    df_sorted <- if (sortord == "desc") {
      df %>% arrange(desc(hs_grad))
    } else {
      df %>% arrange(hs_grad)
    }
    
    datatable(
      df_sorted %>%
        select(State = state,
               `Abbrev.` = abbrev,
               `HS Graduation Rate (%)` = hs_grad,
               Region = region),
      options = list(
        pageLength = 15,
        dom = 'ftip',
        order = list()   # keep the R-side sort as default
      ),
      rownames = FALSE,
      caption = paste("High School Graduation Rates by State —",
                      ifelse(sortord == "desc", "Highest first", "Lowest first"))
    ) %>%
      formatRound("HS Graduation Rate (%)", 1)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
