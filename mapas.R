library(tidyverse)
library(tmap)
library(sf)

mexico <- read_sf("./mexico_shp/Mexico_Estados.shp")

mexico <- mexico |> 
  mutate(CODIGO = str_remove(CODIGO, "^MX")) |> 
  mutate(CODIGO = str_remove(CODIGO, "^0+"))

mexico <- mexico |> 
  mutate(CODIGO = as.integer(CODIGO))

tmi_2025 <- tmi_2025 |> 
  mutate(cve_geo = as.integer(cve_geo))

mexico <- mexico |>
  left_join(tmi_2025, by = c("CODIGO" = "cve_geo"))

tm_shape(mexico) +
  tm_polygons(col = "tmi",
              palette= "brewer.spectral",
              fill.scale = tm_scale_intervals(),
              n = 5) +
  tm_legend(position = c("left", "bottom"), na.show = FALSE) +
  tm_layout(inner.margins=c(0, 0.05, 0.025, 0.01),
legend.position=tm_pos_in("left", "bottom"),
component.position=c("right", "bottom"), scale=.8, title.size = 1.3) +
  tm_compass()

