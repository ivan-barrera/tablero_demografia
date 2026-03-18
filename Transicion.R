library(tidyverse)
library(readxl)
library(janitor)

ind_dem <- read_xlsx("./Datos/5_Indicadores_demográficos_proyecciones.xlsx")

ind_dem <- ind_dem |> clean_names() |> select(-renglon)

trans <- ind_dem |>
  select(ano, cve_geo, t_bru_nat, t_bru_mor) |> 
  filter(cve_geo == 0)

trans |> 
  ggplot(aes(x = ano)) +
  geom_line(aes(y = t_bru_nat, color = "Tasa Bruta de Natalidad")) +
  geom_line(aes(y = t_bru_mor, color = "Tasa Bruta de Mortalidad")) +
  scale_color_manual(name = "", values = c("Tasa Bruta de Natalidad" = "blue", "Tasa Bruta de Mortalidad" = "red")) +
  labs(title = "Transición demográfica", x = "Año", y = "Tasa") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")
