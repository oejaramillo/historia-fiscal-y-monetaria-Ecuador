rm(list = ls())

# Librerias ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)



# Datos -------------------------------------------------------------------
base_vf <- read_excel("C:/Users/pauls/Desktop/Investigacion/politicas/data_vf.xlsx")


# Tablas ------------------------------------------------------------------

base_vf <- base_vf %>%
  mutate(periodo = case_when(
    year >= 1948 & year <= 1971 ~ "1948-1971",
    year >= 1972 & year <= 1981 ~ "1972-1981",
    year >= 1982 & year <= 1999 ~ "1982-1999",
    year >= 2000 & year <= 2006 ~ "2000-2006",
    TRUE ~ NA_character_
  ))


vars <- c(
  "crec_pib_real_per_capita",
  "real exchange rate",
  "ingresos_gdp",
  "gasto_gdp",
  "deficit_gdp",
  "primary defict",
  "deuda_externa_pib",
  "deuda_domestica_pib",
  "cambio_deuda",
  "balanza_comercial_pib",
  "Wti_2010",
  "fbkf_pib",
  "fbkf_Publica_pib",
  "fbkf_Privada_pib"
)


tabla_promedios <- base_vf %>%
  filter(!is.na(periodo)) %>%
  group_by(periodo) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)))



tabla_larga <- tabla_promedios %>%
  pivot_longer(-periodo, names_to = "variable", values_to = "promedio") %>%
  mutate(grupo = case_when(
    variable %in% c("crec_pib_real_per_capita", "real exchange rate") ~ "Macro",
    variable %in% c("ingresos_gdp", "gasto_gdp", "deficit_gdp", "primary defict") ~ "Fiscal",
    variable %in% c("deuda_externa_pib", "deuda_domestica_pib", "cambio_deuda") ~ "Deuda",
    variable %in% c("balanza_comercial_pib", "Wti_2010") ~ "Externo",
    variable %in% c("fbkf_pib", "fbkf_Publica_pib", "fbkf_Privada_pib") ~ "Inversión"
  ))


tabla_final <- tabla_larga %>%
  arrange(grupo, variable, periodo)



tabla_wide <- tabla_larga %>%
  select(grupo, variable, periodo, promedio) %>%
  pivot_wider(names_from = periodo, values_from = promedio) %>%
  arrange(grupo)




write.xlsx(tabla_wide, 
           file = "C:/Users/pauls/Desktop/Investigacion/politicas/tabla_promedios.xlsx")