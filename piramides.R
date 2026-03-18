library(tidyverse)
library(readxl)
library(janitor)

pob_mit <- read_xlsx("./Datos/0_Pob_Mitad_1950_2070.xlsx")

pob_mit <- pob_mit |> 
  clean_names() |> 
  select(-renglon) |> 
  mutate(
    gpo_etario = case_when(
    edad %in% 0:4 ~ "0-4",
    edad %in% 5:9 ~ "5-9",
    edad %in% 10:14 ~ "10-14",
    edad %in% 15:19 ~ "15-19",
    edad %in% 20:24 ~ "20-24",
    edad %in% 25:29 ~ "25-29",
    edad %in% 30:34 ~ "30-34",
    edad %in% 35:39 ~ "35-39",
    edad %in% 40:44 ~ "40-44",
    edad %in% 45:49 ~ "45-49",
    edad %in% 50:54 ~ "50-54",
    edad %in% 55:59 ~ "55-59",
    edad %in% 60:64 ~ "60-64",
    edad %in% 65:69 ~ "65-69",
    edad %in% 70:74 ~ "70-74",
    edad %in% 75:79 ~ "75-79",
    edad %in% 80:84 ~ "80-84",
    edad %in% 85:89 ~ "85-89",
    edad >= 90 ~ "90 +"),
    gpo_etario = as_factor(gpo_etario),
    poblacion = if_else(sexo == "Hombres", -poblacion, poblacion)
  ) |> 
  group_by(sexo, gpo_etario)

save(pob_mit, file = "./Datos/pob_mit.RData")

datos_piramide_2025 <- pob_mit |> 
  filter(ano == 2025, cve_geo == 0)


# Piramide
datos_piramide_2025 |>
  ggplot(aes(x = poblacion, y = as.factor(edad), fill = sexo)) +
  geom_col(width = 1) + 
  scale_x_continuous(labels = function(x) paste0(abs(x / 1000000), "m")) + 
  scale_y_discrete(breaks = scales::pretty_breaks(n = 10)) + 
  scale_fill_manual(values = c("#4575b4", "#d7301f")) +
  labs(title = "Pirámide de población",
       x = "Población",
       y = "Edad",
       fill = "") +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "bottom")

