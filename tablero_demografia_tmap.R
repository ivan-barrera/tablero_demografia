library(shiny)
library(bslib)
library(tidyverse)
library(patchwork)
library(tmap)
library(sf)


load("./Datos/anios.RData")
load("./Datos/entidades.RData")
load("./Datos/indicadores.RData")
load("./Datos/pob_mit.RData")
load("./Datos/ind_dem.RData")
mexico <- read_sf("./mexico_shp/mexico_ent.shp")

estilos <- data.frame(
  cve_estilo = c("cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails","log10_pretty"),
  nom_estilo = c("Categorías", "Intervalos fijos", "Desviación estándar", "Intervalos iguales", "Intervalos 'pretty'", "Cuantiles", "K-means", "Jerárquico", "Binarización de clases", "Fisher/Jenks", "Jenks cortes naturales", "DPIH", "Head/tails breaks","Intervalos 'pretty' en escala logarítmica")
)

cve_ind <- setNames(indicadores$cve_ind, indicadores$nom_ind)

cves_estilos <- setNames(estilos$cve_estilo, estilos$nom_estilo)


cards <- list(
  card(
    full_screen = TRUE,
    card_header("Pirámide de población"),
    plotOutput("piramide")
  ),
  card(
    full_screen = TRUE,
    card_header("Transición demográfica"),
    plotOutput("transicion")
  ),
  card(
    full_screen = TRUE,
    card_header("Tabla"),
    tableOutput("tabla")
  ),
  card(
    full_screen = TRUE,
    card_header("Histograma"),
    plotOutput("histograma")
  ),
  card(
    full_screen = TRUE,
    card_header("Mapa"),
    plotOutput("mapa")
  )
)

ui <- page_navbar(
  title = "Explorador de datos demográficos",
  sidebar = sidebar(
    title = "Controles",
    selectInput("ent", "Entidad", choices = unique(pob_mit$entidad)),
    selectInput("ano", "Año", choices = anios),
    selectInput("ind", "Indicador", choices = cve_ind),
    selectInput("estilo", "Estilo de clasificación", choices = cves_estilos)
),
  nav_spacer(),
  nav_panel(
    "Piramides",
    layout_columns(
      col_widths = c(6,6),
      cards[[1]],
      cards[[2]]
    )
  ),
  nav_panel(
    "Indicadores",
    layout_columns(
      col_widths = c(6,6),
      cards[[4]],
      cards[[5]]
    )
  )
)

server <- function(input, output, server) {

  pob_sel <- reactive(pob_mit |> filter(entidad == input$ent, ano == input$ano))

  tasas_sel <- reactive(ind_dem |> filter(entidad == input$ent))

  ind_sel <- reactive(ind_dem |> filter(ano == input$ano) |> select(entidad, ano, input$ind))
  
  mapa_sel <- reactive(mexico |> filter(ano == input$ano))

  output$piramide <- renderPlot({
    pob_sel() |>
      ggplot(aes(x = poblacion, y = as.factor(edad), fill = sexo)) +
      geom_col(width = 1) + 
      scale_x_continuous(labels = function(x) paste0(abs(x / 1000000), "m")) + 
      scale_y_discrete(breaks = scales::pretty_breaks(n = 10)) + 
      scale_fill_manual(values = c("#4575b4", "#d7301f")) +
      labs(title = "", x = "Población", y = "Edad", fill = "") +
      theme_minimal(base_size = 16) + 
      theme(legend.position = "bottom")
  })

  output$transicion <- renderPlot({
    tasas_sel() |> 
      ggplot(aes(x = ano)) +
      geom_line(aes(y = t_bru_nat, color = "Tasa Bruta de Natalidad")) +
      geom_line(aes(y = t_bru_mor, color = "Tasa Bruta de Mortalidad")) +
      scale_color_manual(name = "", values = c("Tasa Bruta de Natalidad" = "blue", "Tasa Bruta de Mortalidad" = "red")) +
      labs(title = "", x = "Año", y = "Tasa") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
  })

  output$tabla <- renderTable(ind_sel() |> filter(entidad != "República Mexicana") |> select(entidad, input$ind))

  output$histograma <- renderPlot({
    h <- ind_sel() |> 
      filter(entidad != "República Mexicana") |> 
      ggplot(aes(x = .data[[input$ind]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 6, fill = "grey", color = "black") +
      geom_density(color = "red", linewidth = 1) +
      labs(title = "", x = "", y = "Densidad") +
      theme_minimal() +
      theme(axis.text.x = element_blank())

    b <- ind_sel() |> 
      ggplot(aes(y = .data[[input$ind]], x = "")) +
      geom_boxplot() +
      coord_flip() +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank())
    
    h + b +
  plot_layout(heights = c(2,1))

  })

  output$mapa <- renderPlot({
    mexico |>
      filter(ano == input$ano) |>
      select(entidad, ano, input$ind) |> 
      rename(indicador = 3) |> 
      tm_shape() +
      tm_polygons(fill = "indicador",
              fill.scale = tm_scale_intervals(style = input$estilo, values = "oranges"),
              fill.legend = tm_legend(position = c("left", "bottom"), na.show = FALSE)
            ) +
      tm_compass() +
      tm_layout(frame = FALSE)
}) 
  
}

shinyApp(ui, server)