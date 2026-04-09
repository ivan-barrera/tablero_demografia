library(tidyverse)
library(readxl)
library(plotly)

load("./Datos/ind_dem.RData")
piber <- read_xlsx("./Datos/PIBER_1980_2024.xlsx")

piber <- piber |>
  pivot_longer(!c(cve_num, cve_alfa, ent_nom, abr_ent, abrev_ent, region_eco, region_ine),
                names_to = "ano", values_to = "pib")

piber <- piber |> 
  mutate(cve_geo = as.integer(cve_num),
         ano = as.integer(ano),
         pib = as.double(pib)) |> 
  select(cve_geo, region_eco, region_ine, ano, pib)
piber <- piber |> 
  mutate(pib = pib * 1000000)

ind_dem <- ind_dem |> left_join(piber, by = c("cve_geo", "ano"))

ind_dem <- ind_dem |> mutate(pib_pc = pib/pob_mit_ano)

ind_dem_sn_camp <- ind_dem |> 
  filter(entidad != "Campeche")


p <- ind_dem_sn_camp |>
  plot_ly(
     x = ~pib_pc, 
     y = ~ev, 
     size = ~pob_mit_ano,
     sizes = c(60, 600), 
     color = ~region_ine, 
     frame = ~ano, 
     text = ~entidad, 
     hoverinfo = "text",
     type = 'scatter',
     mode = 'markers') |> 
   layout(
     xaxis = list(type = "log")
   ) |> 
    animation_slider(
      currentvalue = list(prefix = "Año ", font = list(color="red"))
  )

p
