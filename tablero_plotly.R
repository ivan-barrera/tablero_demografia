library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# 1. GENERATE DUMMY DEMOGRAPHIC DATA ---------------------------------------
set.seed(42)
years <- seq(1950, 2020, by = 10)
age_groups <- c("0-14", "15-29", "30-44", "45-59", "60-74", "75+")

# Transition rates table (Birth/Death rates)
transition_data <- data.frame(
  Year = years,
  BirthRate = c(45, 42, 35, 28, 20, 15, 12, 11),
  DeathRate = c(30, 22, 15, 12, 10,  9,  8,  8)
)

# Pyramids table (Breakdown by Age and Sex)
pyramid_data <- expand.grid(Year = years, Age = age_groups, Sex = c("Male", "Female")) %>%
  mutate(
    # Simulate a flattening/aging population structure over time
    BasePop = case_when(
      Age == "0-14"  ~ 50 - (Year - 1950)/2,
      Age == "15-29" ~ 40 - (Year - 1950)/4,
      Age == "30-44" ~ 30 + (Year - 1950)/6,
      Age == "45-59" ~ 20 + (Year - 1950)/5,
      Age == "60-74" ~ 10 + (Year - 1950)/4,
      Age == "75+"   ~ 5  + (Year - 1950)/5
    ),
    Population = round(pmax(BasePop + rnorm(n(), 0, 2), 1))
  )

# 2. USER INTERFACE --------------------------------------------------------
ui <- fluidPage(
  titlePanel("Dynamic Demographic Linkage: Transition vs Pyramid"),
  p("Instructions: Click on any data point along the timeline in the left plot to dynamically update the population pyramid on the right."),
  br(),
  fluidRow(
    column(width = 6,
           wellPanel(
             h4("Demographic Transition Timeline"),
             plotlyOutput("linePlot")
           )
    ),
    column(width = 6,
           wellPanel(
             h4(textOutput("pyramidTitle")),
             plotlyOutput("pyramidPlot")
           )
    )
  )
)

# 3. SERVER LOGIC ----------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value to capture clicked year (defaults to the first year available)
  selected_year <- reactive({
    click_evt <- event_data("plotly_click", source = "timeline")
    if (is.null(click_evt)) {
      return(1950) # Fallback / Initial state
    } else {
      return(click_evt$x)
    }
  })
  
  # Dynamic Right Plot Title
  output$pyramidTitle <- renderText({
    paste("Population Pyramid for Year:", selected_year())
  })
  
  # Plot 1: The Transition Line Graph
  output$linePlot <- renderPlotly({
    plot_ly(transition_data, source = "timeline") %>%
      add_trace(x = ~Year, y = ~BirthRate, name = 'Birth Rate', type = 'scatter', mode = 'lines+markers',
                line = list(color = '#1f77b4', width = 3)) %>%
      add_trace(x = ~Year, y = ~DeathRate, name = 'Death Rate', type = 'scatter', mode = 'lines+markers',
                line = list(color = '#d62728', width = 3)) %>%
      layout(
        xaxis = list(title = "Year", tickmode = "array", tickvals = years),
        yaxis = list(title = "Rates per 1,000 people"),
        hovermode = "x unified",
        clickmode = "event+select"
      )
  })
  
  # Plot 2: The Interactive Population Pyramid 
  output$pyramidPlot <- renderPlotly({
    req(selected_year())
    
    # Filter the snapshot data reactively based on client click
    year_df <- pyramid_data %>% 
      filter(Year == selected_year())
    
    # Format Males as negative on the scale for back-to-back bars
    males_df <- year_df %>% filter(Sex == "Male") %>% mutate(PopPlot = -Population)
    females_df <- year_df %>% filter(Sex == "Female") %>% mutate(PopPlot = Population)
    
    # Determine symmetric x-axis limit dynamically
    max_val <- max(year_df$Population, na.rm = TRUE) * 1.15
    
    plot_ly() %>%
      add_bars(data = males_df, x = ~PopPlot, y = ~Age, orientation = 'h', name = 'Male',
               marker = list(color = '#3498db'),
               hoverinfo = 'text', text = ~paste("Age:", Age, "<br>Male Pop:", Population)) %>%
      add_bars(data = females_df, x = ~PopPlot, y = ~Age, orientation = 'h', name = 'Female',
               marker = list(color = '#e74c3c'),
               hoverinfo = 'text', text = ~paste("Age:", Age, "<br>Female Pop:", Population)) %>%
      layout(
        barmode = 'overlay',
        xaxis = list(
          title = "Population (Millions)",
          range = c(-max_val, max_val),
          # Transform negative labels back to positive integers for display
          tickvals = seq(-round(max_val), round(max_val), length.out = 5),
          ticktext = abs(seq(-round(max_val), round(max_val), length.out = 5))
        ),
        yaxis = list(title = "Age Cohorts", categoryorder = "array", categoryarray = age_groups),
        legend = list(orientation = "h", x = 0.4, y = -0.2)
      )
  })
}

shinyApp(ui, server)
