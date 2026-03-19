library(shiny)
library(bslib)
library(tidyverse)

load("./Datos/pob_mit.RData")
load("./Datos/ind_dem.RData")

ui <- page_fluid(
  selectInput("ent", "Entidad", choices = unique(pob_mit$entidad)),
  selectInput("ano", "Año", choices = unique(pob_mit$ano)),
  plotOutput("piramide"),
  plotOutput("transicion")
  
)

server <- function(input, output, session) {

  pob_sel <- reactive(pob_mit |> filter(entidad == input$ent, ano == input$ano))

  tasas_sel <- reactive(ind_dem |> filter(entidad == input$ent))

  output$piramide <- renderPlot({
    pob_sel() |>
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

  output$transicion <- renderPlot({
    tasas_sel() |> 
      ggplot(aes(x = ano)) +
      geom_line(aes(y = t_bru_nat, color = "Tasa Bruta de Natalidad")) +
      geom_line(aes(y = t_bru_mor, color = "Tasa Bruta de Mortalidad")) +
      scale_color_manual(name = "", values = c("Tasa Bruta de Natalidad" = "blue", "Tasa Bruta de Mortalidad" = "red")) +
      labs(title = "Transición demográfica", x = "Año", y = "Tasa") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
  })
  
}

shinyApp(ui, server)