library(sf)
library(leaflet)


mexico_estados <- read_sf("./mexico_shp/mexico_ent.shp")

mexico_estados <- mexico_estados |> 
  filter(ano == 2020)

leaflet(mexico_estados) |>
  addTiles() |> 
  addPolygons()

cuantiles <- quantile(mexico_estados$tgf)
pal <- colorBin("YlOrRd", domain = mexico_estados$tgf, bins = cuantiles)

leaflet(mexico_estados) |>
  addTiles() |> 
  addPolygons(
    fillColor = ~pal(tgf),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = ~paste0(entidad, ": ", round(tgf, digits = 2)),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) |> 
  addLegend(
    pal = pal,
    values = ~tgf,
    title = "Tasa Global de Fecundidad")


##### Prueba con geojson
library(sf)
library(leaflet)
library(rjson)

mexico_geojson <- st_read("./mexico_dest.geojson")



mexico_geojson <- mexico_geojson |> 
  filter(ano == 2020)

leaflet(mexico_geojson) |>
  addTiles() |> 
  addPolygons()

cuantiles <- quantile(mexico_geojson$tgf)
pal <- colorBin("YlOrRd", domain = mexico_geojson$tgf, bins = cuantiles)

leaflet(mexico_geojson) |>
  addTiles() |> 
  addPolygons(
    fillColor = ~pal(tgf),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = ~paste0(entidad, ": ", round(tgf, digits = 2)),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) |> 
  addLegend(
    pal = pal,
    values = ~tgf,
    title = "Tasa Global de Fecundidad")


## Mapa kml

library(sf)
library(leaflet)
library(rjson)

mexico_kml <- st_read("./mexico_dest.kml")



mexico_geojson <- mexico_geojson |> 
  filter(ano == 2020)

leaflet(mexico_geojson) |>
  addTiles() |> 
  addPolygons()

cuantiles <- quantile(mexico_geojson$tgf)
pal <- colorBin("YlOrRd", domain = mexico_geojson$tgf, bins = cuantiles)

leaflet(mexico_geojson) |>
  addTiles() |> 
  addPolygons(
    fillColor = ~pal(tgf),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = ~paste0(entidad, ": ", round(tgf, digits = 2)),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) |> 
  addLegend(
    pal = pal,
    values = ~tgf,
    title = "Tasa Global de Fecundidad")
