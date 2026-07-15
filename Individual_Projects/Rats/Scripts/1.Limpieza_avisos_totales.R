setwd("~/TRABAJO/AVISOS_RATAS")

library(readxl)
library(dplyr)
library(tidyr)
library(broom)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(glmmTMB) #binomial negativa
library(ggpubr)
library(webshot2)

HASTA_2025 <- read_excel("HASTA_2025.xlsx", 
                         col_types = c("text", "date", "numeric", 
                                       "numeric", "text", "text", "skip", 
                                       "skip", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))
View(HASTA_2025)


df <- HASTA_2025

df <- df %>%
  mutate(across(c(VECTOR_ENTRADA, VECTOR_ENCONTRADO), as.factor))

df <- df %>%
  mutate(PUBLICA = ifelse(MADRIGUERA == 1, PUBLICA, NA))

df <- df %>%
  mutate(PRIVADA = ifelse(MADRIGUERA == 1, PRIVADA, NA))


cols_to_exclude <- c("REF_UTCV", "FECHA", "BAR_COD", "DIS_COD", 
                     "VECTOR_ENTRADA", "VECTOR_ENCONTRADO", "CONTACTO CON SOLICITANTE")

df_preparado <- df%>%
  group_by(FECHA, DIS_COD) %>%
  summarise(
    Total_Avisos = n(),
    # Calculamos la media (proporción) de las intervenciones
    across(-any_of(cols_to_exclude), \(x) mean(as.numeric(x), na.rm = TRUE)),
    .groups = 'drop'
  )



########################
# POSIBLE FILTRADO 2025
########################

df <- df %>%
  mutate(FECHA = as.Date(FECHA)) %>%          
  filter(format(FECHA, "%Y") == "2025")