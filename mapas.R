library(tidyverse)
library(tmap)
library(sf)
library(janitor)


mexico <- read_sf("./mexico_shp/Mexico_Estados.shp")

mexico <- mexico |>
  clean_names() |> 
  mutate(cve_geo = as.integer(str_remove(codigo, "^MX0?")),
                   .keep = "unused",
                  .before = 1) |> 
  arrange(cve_geo)

mexico <- mexico |>
  left_join(ind_dem, by = "cve_geo")

st_write(obj = mexico, dsn = "./mexico_shp/mexico_ent.shp", driver = "ESRI Shapefile")

mexico |> 
  filter(ano == 2020) |>
  tm_shape() +
  tm_polygons(fill = "tmi",
              fill.scale = tm_scale_intervals(style = "quantile", values = "oranges"),
              fill.legend = tm_legend(position = c("left", "bottom"), na.show = FALSE)
            ) +
  tm_compass()
