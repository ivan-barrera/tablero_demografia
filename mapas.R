library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(rjson)

# Cargar shapefile original de CONABIO
mexico <- read_sf("./mexico_shp/Mexico_Estados.shp")

# Limpiar y preparar el shapefile
mexico <- mexico |>
  clean_names() |> 
  mutate(cve_geo = as.integer(str_remove(codigo, "^MX0?")),
                   .keep = "unused",
                  .before = 1) |> 
  arrange(cve_geo)

# Cargar datos demográficos
load("./Datos/ind_dem.RData")

# Unir con datos demográficos
mexico <- mexico |>
  left_join(ind_dem, by = "cve_geo")

# Exportar a shapefile
st_write(mexico, "./mexico_dest.shp", driver = "ESRI Shapefile", delete_layer = TRUE)

# Exportar a kml
st_write(mexico, "./mexico_dest.kml", driver = "KML")

# Exportar a GeoJSON
st_write(mexico, "./mexico_dest.geojson", driver = "GeoJSON")

# Probar mapa con con tmap
mexico |> 
  filter(ano == 2020) |>
  tm_shape() +
  tm_polygons(fill = "tmi",
              fill.scale = tm_scale_intervals(style = "quantile", values = "oranges"),
              fill.legend = tm_legend(position = c("left", "bottom"), na.show = FALSE)
            ) +
  tm_compass()
