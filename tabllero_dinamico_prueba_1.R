# Este SÍ FUNCIONA
# Aunque tiene un erro en los estados Chiapas aparece como Coahuila y Chihuahua como Colima y viceversa 

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

load("./Datos/anios.RData")
load("./Datos/indicadores.RData")

# Seleccionar solo años a partir de 2020
anios <- anios[anios >= 2020]
mexico <- read_sf("./mexico_dest.geojson")

cve_ind <- setNames(indicadores$cve_ind, indicadores$nom_ind)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("ano", "Año", choices = anios),
      selectInput("ind", "Indicador", choices = cve_ind)
    ),
    mainPanel(
      leafletOutput("mapa")
    )
  )
)

server <- function(input, output, session) {
  
  # Filtrar datos según año e indicador seleccionado
  mapa_sel <- reactive({
    cols_keep <- c("cve_geo", "entidad", "ano", colnames(mexico)[colnames(mexico) == input$ind])
    mexico |> 
      filter(ano == input$ano) |>
      select(all_of(intersect(cols_keep, colnames(mexico)))) |>
      as_tibble() |>
      st_as_sf() |>
      rename(valor = !! input$ind)
  })
  
  # Generar paleta de colores
  pal <- reactive({
    valores <- mapa_sel()$valor
    colorBin(
      "YlOrRd", 
      domain = valores[!is.na(valores)], 
      bins = 5,
      na.color = "#BDBDBD"
    )
  })

  output$mapa <- renderLeaflet({
    datos <- mapa_sel()
    nom_ind <- names(cve_ind)[cve_ind == input$ind]
    
    leaflet(data = datos) |>
      addTiles() |> 
      addPolygons(
        fillColor = ~pal()(valor),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~sprintf("%s", entidad),
        popup = ~sprintf(
          "<b>%s</b><br/><strong>%s:</strong> %.2f",
          entidad,
          nom_ind,
          valor
        ),
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal(),
        values = ~valor,
        title = nom_ind,
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)