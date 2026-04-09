library(tidyverse)
library(readxl)
library(janitor)

ind_dem <- read_xlsx("./Datos/5_Indicadores_demográficos_proyecciones.xlsx")

ind_dem <- ind_dem |>
  clean_names() |>
  select(-renglon)

ind_dem <- ind_dem |> 
  mutate(
    across(
      c(ano, cve_geo, cre_nat, cre_soc, cre_tot, def, edad_med, emi_est, emi_int,
        hom_mit_ano, inm_est, inm_int, mig_net_est, mig_net_int, muj_mit_ano, nac,
        pob_mit_ano, muj_12_14, muj_15_17, muj_15_19, muj_15_29, muj_15_49, muj_18_24,
        muj_20_24, muj_3_5, muj_30_64, muj_6_11, muj_65_mas, hom_12_14, hom_15_17,
        hom_15_19, hom_15_29, hom_15_49, hom_18_24, hom_20_24, hom_3_5, hom_30_64,
        hom_6_11, hom_65_mas, pob_12_14, pob_15_17, pob_15_19, pob_15_29, pob_15_49,
        pob_18_24, pob_20_24, pob_3_5, pob_30_64, pob_6_11, pob_65_mas),
        as.integer
    ),
    across(
      c(evh, evm, ev, ind_env, raz_dep_adu, raz_dep_inf, raz_dep, t_bru_mor, t_bru_nat,
        t_cre_nat, t_cre_soc, t_cre_tot, t_emi_est, t_inm_est, t_mig_net_est, t_mig_net_int,
        tmi, tef_ado, tgf),
      as.numeric
    )
  )

save(ind_dem, file = "./Datos/ind_dem.RData")

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



ind_dem <- ind_dem |> mutate(ano = as.integer(ano))

ind_dem <- ind_dem |> mutate(across(c(ano, cve_geo), as.integer))

ind_dem <- ind_dem |> mutate(across(c(cre_nat, cre_soc), as.integer, ~na_if(., "ND")))

ind_dem <- ind_dem |> 
  mutate(
    across(
      c(ano, cve_geo, cre_nat, cre_soc),
      ~na_if(., "ND"), as.integer
          )
        )




