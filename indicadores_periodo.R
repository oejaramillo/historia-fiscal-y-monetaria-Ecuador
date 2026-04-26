rm(list = ls())

# Librerias ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)



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
           file = "C:/Users/pauls/Desktop/Investigacion/politicas/local/tabla_promedios.xlsx")



# Gráfico Inversión pública vs privada %PIB -------------------------------
# Convertir a formato largo (más limpio para ggplot)
df_long <- base_vf %>%
  select(year, fbkf_Privada_pib, fbkf_Publica_pib) %>%
  tidyr::pivot_longer(
    cols = c(fbkf_Privada_pib, fbkf_Publica_pib),
    names_to = "tipo",
    values_to = "valor"
  )

# Renombrar para etiquetas más limpias
df_long$tipo <- recode(df_long$tipo,
                       "fbkf_Privada_pib" = "FBKF privado (% PIB)",
                       "fbkf_Publica_pib" = "FBKF pública (% PIB")

p <- ggplot(df_long, aes(x = year, y = valor, color = tipo)) +
  
  geom_line(size = 1.2) +
  
  # Líneas verticales más sutiles
  geom_vline(xintercept = c(1971, 1980, 2000, 2006, 2017),
             linetype = "dashed",
             color = "grey60",
             size = 0.4) +
  annotate("rect", xmin = 2006, xmax = 2017, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "grey70")+
  
  # Colores modernos (sobrios)
  scale_color_manual(values = c("#1b1b1b", "#4a90e2")) +
  
  scale_x_continuous(breaks = seq(min(df_long$year), max(df_long$year), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(df_long$valor, na.rm = TRUE), by = 2)) +
  
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "",
    color = NULL,
    caption = ""
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "bottom",   # 👈 aquí el cambio
    legend.text = element_text(size = 11),
    axis.text = element_text(color = "grey20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85")
  )

ggsave(
  filename = "grafico_fbkf.png",
  plot = p,
  path = "C:/Users/pauls/Desktop/Investigacion/politicas/local/graficos",
  width = 8,
  height = 5,
  dpi = 300
)
