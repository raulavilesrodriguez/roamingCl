library(readxl)
library(tidyverse)
library(highcharter) #Interactive Plot
library(DT)
library(stringr) # to regex

#___Usuarios___
usuarios.claro <- read_excel("./data/usuarios_claro.xlsx")

avClaro.outbound <- mean(usuarios.claro$usuarios_outbound)
avClaro.intbound <- mean(usuarios.claro$usuarios_inbound)

n <- nrow(usuarios.claro)

variation.claro.outbound <- ((usuarios.claro[['usuarios_outbound']][n] / usuarios.claro[['usuarios_outbound']][1]) - 1)*100
variation.claro.inbound <- ((usuarios.claro[['usuarios_inbound']][n] / usuarios.claro[['usuarios_inbound']][1]) - 1)*100

tail(usuarios.claro)

usuarios.claro2 <- pivot_longer(usuarios.claro, cols=2:3, names_to = 'tipo', values_to = 'usuarios')

usuarios.claro2$fecha <- as.Date(usuarios.claro2$fecha)

hchart(usuarios.claro2, "line", hcaes(x = fecha, y = usuarios, group = tipo)) |>
  hc_title(text="<b>Usuarios Claro Roaming Chile-Ecuador</b>") |>
  hc_subtitle(text = 'Country: Ecuador') |> 
  hc_credits(enable = TRUE, text = 'https://github.com/raulavilesrodriguez') |>
  hc_tooltip(sort = TRUE, table = TRUE) |>
  hc_caption(text = 'Elaborated: Byron Raúl Avilés Rodríguez') |>
  hc_add_theme(hc_theme_darkunica())


usuarios.movi <- read_excel("./data/usuarios_movi.xlsx")

avMovi.outbound <- mean(usuarios.movi$usuarios)

variation.movi.outbound <- ((usuarios.movi[['usuarios']][n] / usuarios.movi[['usuarios']][1]) - 1)*100

usuarios.movi$Fecha <- as.Date(usuarios.movi$Fecha)

hchart(usuarios.movi, "line", hcaes(x = Fecha, y = usuarios)) |>
  hc_title(text="<b>Usuarios Movistar Roaming Chile-Ecuador</b>") |>
  hc_subtitle(text = 'Country: Ecuador') |> 
  hc_credits(enable = TRUE, text = 'https://github.com/raulavilesrodriguez') |>
  hc_tooltip(sort = TRUE, table = TRUE) |>
  hc_caption(text = 'Elaborated: Byron Raúl Avilés Rodríguez') |>
  hc_add_theme(hc_theme_darkunica())


iot.claro.inbound <- read_excel("./data/iot_claro_inbound.xlsx")

iot.claro.inbound


#---TRÁFICO---
trafico.claro <- read_excel("./data/trafico_claro.xlsx")
voz.claro <- trafico.claro |> select(Fecha, `VOZ TOTAL TRAFICO Outbound`, `VOZ TOTAL TRAFICO Inbound`)


#---IoT Movi---
iot.movi <- read_excel("./data/iot_movi.xlsx")
iot.movi <- iot.movi[,-5]


#----Minorista----
minoristas <- read_excel("./data/tarifas_minoristas.xlsx")
minoristas <- minoristas |> mutate(Claro = round(Claro, 5), Movistar = round(Movistar, 5))

minoristas2 <- pivot_longer(minoristas, cols=2:4, names_to = 'operadora', values_to = 'tarifas')

hchart(minoristas2, "column", hcaes(SERVICIO, tarifas, group = operadora)) |>
  hc_title(text="<b>Tarifas Minoristas Servicio Roaming Chile-Ecuador</b>") |>
  hc_subtitle(text = 'Country: Ecuador') |> 
  hc_credits(enable = TRUE, text = 'https://github.com/raulavilesrodriguez') |>
  hc_tooltip(sort = TRUE, table = TRUE) |>
  hc_caption(text = 'Elaborated: Byron Raúl Avilés Rodríguez') |>
  hc_add_theme(hc_theme_alone())

minoristas2.datos <- minoristas2 |> filter(SERVICIO == 'DATOS (USD/MB)')

hchart(minoristas2.datos, "column", hcaes(SERVICIO, tarifas, group = operadora)) |>
  hc_title(text="<b>Tarifas Minoristas Servicio de Datos Roaming Chile-Ecuador</b>") |>
  hc_subtitle(text = 'Country: Ecuador') |> 
  hc_credits(enable = TRUE, text = 'https://github.com/raulavilesrodriguez') |>
  hc_tooltip(sort = TRUE, table = TRUE) |>
  hc_caption(text = 'Elaborated: Byron Raúl Avilés Rodríguez') |>
  hc_add_theme(hc_theme_alone())

#---Tarifas Locales----
locales <- read_excel("./data/tarifas_locales.xlsx")
locales <- locales |> mutate(`Average Datos (USD/MB)` = round(`Average Datos (USD/MB)`, 5),
                             `Average Voz (USD/min)` = round(`Average Voz (USD/min)`, 5),
                             `Average SMS (USD/mensaje)` = round(`Average SMS (USD/mensaje)`, 5)
                             )
DT::datatable(locales)

locales2 <- pivot_longer(locales, cols=2:4, names_to = 'indicador', values_to = 'tarifas')

hchart(locales2, "column", hcaes(Operadora, tarifas, group = indicador)) |>
  hc_title(text="<b>Tarifas Minoristas a nivel local Ecuador</b>") |>
  hc_subtitle(text = 'País: Ecuador') |> 
  hc_credits(enable = TRUE, text = 'https://github.com/raulavilesrodriguez') |>
  hc_tooltip(sort = TRUE, table = TRUE) |>
  hc_caption(text = 'Elaborado: Byron Raúl Avilés Rodríguez') |>
  hc_add_theme(hc_theme_alone())


#----Propuesta----
topes <- read_excel("./data/tarifas_topes.xlsx")

for (i in 1:5) {
  topes[,i] <- (lapply(topes[,i], function(x){
    ifelse(str_detect(x, regex("[:digit:]")), round(as.numeric(x), 4), x)
  }))
}







