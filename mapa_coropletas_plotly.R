library(plotly)
library(rjson)

mexico_estados <- fromJSON(file = "./mexico_dest.geojson")
load("./Datos/ind_dem.RData")

ind_dem <- ind_dem |> 
  filter(ano == 2025, cve_geo != 0) |> 
  select(cve_geo, tgf)

fig <- plot_ly()

fig <- fig %>% add_trace(
  type = "choropleth",
  geojson = mexico_estados,
  locations = ind_dem$cve_geo,
  z = ind_dem$tgf,
  colorscale = "Viridis",
  marker = list(line = list(width = 0.5, color = "white"))
)

fig <- fig |> colorbar(title = "Tasa Global de Fecundidad")

fig <- fig %>% layout(
  title = "Tasa Global de Fecundidad por Estado en México (2025)",
  geo = list(
    scope = "mexico",
    projection = list(type = "transverse mercator"),
    showlakes = TRUE,
    lakecolor = "rgb(255, 255, 255)"
  )
)

fig