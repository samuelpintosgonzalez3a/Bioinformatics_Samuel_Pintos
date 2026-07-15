rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
setwd("~/TFM")


#Carga de excels

df_total <- read_excel("LARVAS_PUPAS.xlsx") %>% mutate(FECHA = as.Date(FECHA))
df_1 <- read_excel(paste0("P01.EL PARDO.xlsx")) %>% mutate(FECHA = as.Date(datetime), LOC = "P01. EL PARDO") %>% select(-name, -datetime)
df_2 <- read_excel(paste0("P02.PUERTA DE HIERRO.xlsx")) %>% mutate(FECHA = as.Date(datetime), LOC = "P02. PUERTA DE HIERRO") %>% select(-name, -datetime)
df_3 <- read_excel(paste0("P03.PUENTE DE LOS FRANCESES.xlsx")) %>% mutate(FECHA = as.Date(datetime), LOC = "P03. PUENTE DE LOS FRANCESES") %>% select(-name, -datetime)
df_4 <- read_excel(paste0("P04.PUENTE DE TOLEDO.xlsx")) %>% mutate(FECHA = as.Date(datetime), LOC = "P04. PUENTE DE TOLEDO") %>% select(-name, -datetime)
df_5 <- read_excel(paste0("P05.TANATORIO.xlsx")) %>% mutate(FECHA = as.Date(datetime), LOC = "P05. TANATORIO") %>% select(-name, -datetime)
df_6 <- read_excel(paste0("P06.PRESA IV.xlsx")) %>% mutate(FECHA = as.Date(datetime), LOC = "P06. PRESAIV") %>% select(-name, -datetime)
df_7 <- read_excel(paste0("P07.DEPURADORA GAVIA.xlsx")) %>% mutate(FECHA = as.Date(datetime), LOC = "P07. DEPURADORA GAVIA") %>% select(-name, -datetime)

# Merge
df_clima <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6, df_7)
df <- df_total %>% left_join(df_clima, by = c("LOC", "FECHA")) %>% arrange(-desc(LOC), FECHA)


#Traducimos variables

df <- df %>%
  rename(
    Localización = LOC,
    DENSIDAD_LARVAS_PEQUEÑAS = DENSIDAD_PEQUEÑAS,
    DENSIDAD_LARVAS_MEDIANAS = DENSIDAD_MEDIANAS,
    DENSIDAD_LARVAS_GRANDES = DENSIDAD_GRANDES,
    DENSIDAD_LARVARIA_TOTAL = DENSIDAD_TOTAL,
    densidad_S.erytrocephalum = d_S.erytrocephalum,
    densidad_S.pseudoequinum = d_S.pseudoequinum,
    densidad_S.lineatum = d_S.lineatum,
    densidad_S.ornatum = d_S.ornatum,
    densidad_S.equinum = d_S.equinum,
    densidad_S.ruficorne = d_S.ruficorne,
    densidad_S.rubzovianum = d_S.rubzovianum,
    densidad_S.sergenti = d_S.sergenti,
    densidad_pupas_vacías = d_VACÍAS,
    densidad_pupal_total = d_TOTAL,
    temperatura_máxima = tempmax,
    temperatura_mínima = tempmin,
    temperatura_media = temp,
    sensación_térmica_máxima = feelslikemax,
    sensación_térmica_mínima = feelslikemin,
    sensación_térmica = feelslike,
    rocío = dew,
    humedad = humidity,
    precipitación = precip,
    probabilidad_precipitación = precipprob,
    cobertura_precipitación = precipcover,
    rachas_de_viento = windgust,
    velocidad_del_viento = windspeed,
    nubosidad = cloudcover,
    visibilidad = visibility,
    radiación_solar = solarradiation,
    energía_solar = solarenergy,
    amanecer = sunrise,
    atardecer = sunset,
    fase_lunar = moonphase
  )


# Quitamos variables que no nos interesan

df <- df %>% 
  select(-any_of(c("severerisk", "stations", "description", "icon", "preciptype","winddir", "sealevelpressure", "snow",
                   "snowdepth", "conditions","uvindex")))

#Convertimos todo a numérico salvo las variables que no son continuas

df <- df %>%
  mutate(across(-c(Localización, FECHA, amanecer, atardecer), as.numeric))

#Metemos los tratamientos

library(glmmTMB)
fechas_muestreo <- sort(unique(df$FECHA))
fechas_post <- as.Date(c("2025-05-16", "2025-05-21", "2025-06-12", "2025-08-26", "2025-09-30"))
fechas_pre <- as.Date(sapply(fechas_post, function(t_date) {
  max(fechas_muestreo[fechas_muestreo < t_date], na.rm = TRUE)
}), origin = "1970-01-01")

df <- df %>%
  
  mutate(# Creamos la variable numérica: 0 para Pre y 1 para Post
    Tratamiento_Num = case_when(
      FECHA %in% fechas_post ~ 1,
      FECHA %in% fechas_pre ~ 0,
      TRUE ~ NA_real_))
      

# Fotoperiodo

df <- df %>%
  mutate(
    amanecer_dt = as.POSIXct(amanecer, format = "%Y-%m-%dT%H:%M:%S"),
    atardecer_dt = as.POSIXct(atardecer, format = "%Y-%m-%dT%H:%M:%S"),
    fotoperiodo = as.numeric(difftime(atardecer_dt, amanecer_dt, units = "hours"))
  ) %>%
  select(-amanecer_dt, -atardecer_dt)


# Corrección de d_S.rubzovianum

df <- df %>%
  mutate(densidad_S.rubzovianum = ifelse(densidad_S.rubzovianum > 1000000, densidad_S.rubzovianum / 1e9, densidad_S.rubzovianum))
