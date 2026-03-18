library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)

ind_dem <- read_xlsx("./Datos/5_Indicadores_demográficos_proyecciones.xlsx")

ind_dem <- ind_dem |> clean_names() |> select(-renglon)

tmi_2025 <- ind_dem |>
  filter(ano == 2025, cve_geo >= 0) |>
  select(entidad, cve_geo, tmi)

hist <- tmi_2025 |>
  ggplot(aes(x = tmi)) +
  geom_histogram(aes(y = after_stat(density)), bins = 6, fill = "grey", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma", x = "", y = "Densidad")
  theme_minimal() +
  theme(axis.text.x = element_blank())

box <- tmi_2025 |> 
  ggplot(aes(y = tmi, x = "")) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()
      )

hist / box +
  plot_layout(heights = c(3,1))

