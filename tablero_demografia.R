library(shiny)
library(bslib)
library(tidyverse)

load("./Datos/pob_mit.RData")

ui <- page_fluid(
  selectInput("ent", "Entidad", choices = unique(pob_mit$entidad)),
  selectInput("ano", "Año", choices = unique(pob_mit$ano)),
  plotOutput("piramide")
  
)

server <- function(input, output, session) {

  seleccion <- reactive(pob_mit |> filter(entidad == input$ent, ano == input$ano))


  output$piramide <- renderPlot({
    seleccion() |>
      ggplot(aes(x = poblacion, y = as.factor(edad), fill = sexo)) +
      geom_col(width = 1) + 
      scale_x_continuous(labels = function(x) paste0(abs(x / 1000000), "m")) + 
      scale_y_discrete(breaks = scales::pretty_breaks(n = 10)) + 
      scale_fill_manual(values = c("#4575b4", "#d7301f")) +
      labs(title = "Pirámide de población",
      x = "Población",
      y = "Edad",
      fill = "") +
      theme_minimal(base_size = 16) + 
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)