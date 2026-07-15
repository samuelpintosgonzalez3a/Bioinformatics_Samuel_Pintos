setwd("~/TRABAJO/AVISOS_RATAS")

library(readxl)
library(dplyr)
library(tidyr)
library(broom)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(glmmTMB)
library(ggpubr)
library(webshot2)

HASTA_2025 <- read_xlsx("AVISOS_RATAS_NUEVOS.xlsx")


df <- HASTA_2025

df <- df %>%
  mutate(across(c(VECTOR_ENTRADA, VECTOR_ENCONTRADO), as.factor))

df <- df %>%
  mutate(PUBLICA = ifelse(MADRIGUERA == 1, PUBLICA, NA))

df <- df %>%
  mutate(PRIVADA = ifelse(MADRIGUERA == 1, PRIVADA, NA))

df <- df %>%
  # Filtro 2: VECTOR_ENCONTRADO debe empezar por "RAT"
  # Usamos str_starts de stringr (incluido en tidyverse)
  filter(str_starts(VECTOR_ENCONTRADO, "RAT"))

df <- df %>% dplyr::select(- starts_with("REG"), - starts_with("RET"), - starts_with("COO"), -`ZONAS VERDES`,-X, -Y)

df <- df %>% dplyr::select((-RATICIDA), -starts_with("RODEN"), -starts_with("CEBO"),
                           -starts_with("PUB"), -starts_with("PRIV"))


# ---------------
# FILTRAR 2025
# ---------------


library(lubridate)

df <- df %>%   # dmy() interpreta automáticamente "día-mes-año"
  mutate(FECHA = dmy(FECHA)) %>% 
  # year() extrae el año como número, es más robusto que formatear a texto
  filter(year(FECHA) == 2025)

