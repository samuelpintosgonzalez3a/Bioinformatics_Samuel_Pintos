library(tidyverse)


#############################
#############################
#    EVOLUCIÓN TEMPORAL     #
#############################
#############################


# 3. Crear el dataframe agregado (Solo para DENSIDAD_TOTAL)
df_temporal <- df %>%
  group_by(FECHA) %>%
  summarise(
    Larvas = sum(DENSIDAD_LARVARIA_TOTAL, na.rm = TRUE)
  )

# 4. Gráfico con las líneas púrpuras de tratamiento
ggplot(df_temporal, aes(x = FECHA)) +
  # Añadimos las líneas de tratamiento al fondo
  geom_vline(xintercept = as.numeric(fechas_post), 
             color = "purple", 
             linewidth = 1.2, 
             alpha = 0.4) + 
  # Línea y puntos de Larvas (DENSIDAD_TOTAL)
  geom_line(aes(y = Larvas), color = "darkblue", linewidth = 1) +
  geom_point(aes(y = Larvas), color = "darkblue") +
  theme_minimal() +
  labs(title = "Evolución Temporal de la Densidad Total de Larvas",
       subtitle = "Líneas púrpura: Días de tratamiento",
       x = "Fecha", 
       y = " Densidad Larvaria") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Asegúrate de tener cargadas las librerías
library(lme4)
library(lmerTest) # Fundamental para que aparezca el p-valor en lme4
library(dplyr)

# 1. Preparación de datos
df_modelo_tiempo <- df %>%
  # Filtramos posibles NAs en la densidad
  filter(!is.na(DENSIDAD_LARVARIA_TOTAL)) %>%
  mutate(
    # Es crucial convertir la FECHA a factor. 
    # Si la dejas como fecha continua, R buscará una línea recta de tendencia.
    # Como factor, R evalúa las subidas y bajadas (picos) entre cada día de muestreo.
    FECHA_factor = as.factor(FECHA),
    # La localización será nuestro efecto aleatorio (mediciones repetidas)
    Localizacion_factor = as.factor(Localización) 
  )

# 2. Construir el modelo
# Usamos log1p (log(x+1)) para suavizar los picos extremos de densidad
modelo_tiempo <- lmer(log1p(DENSIDAD_LARVARIA_TOTAL) ~ FECHA_factor + (1 | Localizacion_factor), 
                      data = df_modelo_tiempo)

# 3. Extraer el p-valor global mediante ANOVA
resultados_anova <- anova(modelo_tiempo)
print(resultados_anova)





#############################
#############################
# DENSIDAD POR LOCALIZACIÓN #
#############################
#############################


library(tidyverse)

df_barras <- df %>%
  filter(!is.na(DENSIDAD_LARVARIA_TOTAL)) %>%
  group_by(Localización) %>%
  summarise(
    Media = mean(DENSIDAD_LARVARIA_TOTAL),
    # Calculamos la desviación estándar dividida por la raíz cuadrada del número de muestras (n)
    SE = sd(DENSIDAD_LARVARIA_TOTAL) / sqrt(n()) 
  ) %>%
  
  arrange(desc(Media))


grafico_localizaciones <- ggplot(df_barras, 
                                 # reorder() mantiene el orden de mayor a menor que establecimos antes
                                 aes(x = reorder(Localización, -Media), y = Media, fill = Localización)) +
 
  geom_col(color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE), width = 0.2, linewidth = 0.8) +
  theme_minimal() +
  labs(
    title = "Densidad Larvaria Media por Localización",
    subtitle = "Las líneas de error representan el Error Estándar (SE)",
    x = "Punto de Muestreo",
    y = "Densidad Larvaria Media"
  ) +
  theme(
    axis.text.x = element_text(angle = 53, hjust = 1, face = "bold"),
    legend.position = "none" # Ocultamos la leyenda porque los nombres ya están en el eje X
  ) +
  # Usar una paleta de colores amigable (opcional)
  scale_fill_viridis_d(option = "mako")

print(grafico_localizaciones)


library(lme4)
library(lmerTest)
library(emmeans)

df_stats <- df %>%
  filter(!is.na(DENSIDAD_LARVARIA_TOTAL)) %>%
  mutate(
    Localizacion_factor = as.factor(Localización),
    FECHA_factor = as.factor(FECHA)
  )


modelo_loc <- lmer(log1p(DENSIDAD_LARVARIA_TOTAL) ~ Localizacion_factor + (1 | FECHA_factor), 
                   data = df_stats)

# 3. Paso A: ¿Hay diferencias significativas a nivel global?
print("--- ANOVA GLOBAL ---")
print(anova(modelo_loc))

# 4. Paso B: ¿Qué localizaciones son distintas entre sí? (Prueba Post-hoc)
print("--- COMPARACIONES POR PARES (Ajuste de Tukey) ---")
medias_loc <- emmeans(modelo_loc, ~ Localizacion_factor)
comparaciones_loc <- pairs(medias_loc)
print(comparaciones_loc)



#############################
#############################
#    TAMAÑOS LARVARIOS      #
#############################
#############################



library(tidyverse)
library(stringr)

df_tamanos <- df %>%
  # Seleccionamos las columnas clave
  select(FECHA, Localización, matches("DENSIDAD_LARVAS_")) %>%
  # Pivotamos al formato largo
  pivot_longer(
    cols = matches("DENSIDAD_LARVAS_"),
    names_to = "Tamano",
    values_to = "Densidad"
  ) %>%
  # Limpieza robusta de los nombres para evitar problemas con la "Ñ"
  mutate(
    Tamano = case_when(
      str_detect(Tamano, "PEQU") ~ "PEQUEÑAS",
      str_detect(Tamano, "MED")  ~ "MEDIANAS",
      str_detect(Tamano, "GRAN") ~ "GRANDES",
      TRUE ~ Tamano
    ), Tamano = factor(Tamano, levels = c("PEQUEÑAS", "MEDIANAS", "GRANDES")),
    Localización = as.factor(Localización),
    FECHA = as.factor(FECHA)
  ) %>%
  # Quitamos nulos
  filter(!is.na(Densidad))


# Gráfico de barras apiladas al 100% (Proporciones relativas)
grafico_proporciones <- ggplot(df_tamanos, aes(x = FECHA, y = Densidad, fill = Tamano)) +
  
  # ALERTA DE CAMBIO: position = "fill" convierte las densidades en porcentajes
  geom_col(position = "fill", width = 0.8, color = "black", linewidth = 0.2) + 
  
  # Ya no usamos scales = "free_y" porque todos los paneles irán exactamente de 0 a 1
  facet_wrap(~ Localización) +
  
  scale_fill_manual(values = c("PEQUEÑAS" = "#FF9999", 
                               "MEDIANAS" = "#66B2FF", 
                               "GRANDES"  = "#99FF99")) +
  theme_minimal() +
  labs(
    title = "Estructura Poblacional: Proporción de Densidad Larvaria",
    x = "Fecha de Muestreo",
    y = "Proporción de cada tamaño (0 = 0%, 1 = 100%)",
    fill = "Tamaño"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.major.x = element_blank() 
  )

print(grafico_proporciones)


# Significancia

library(lme4)
library(lmerTest)
library(emmeans)

# 1. Construir el modelo
# Usamos log1p para normalizar, igual que con las densidades totales
modelo_tamanos <- lmer(log1p(Densidad) ~ Tamano * Localización + (1 | FECHA), 
                       data = df_tamanos)

# 2. Comprobar si globalmente el tamaño influye
print("--- ANOVA GLOBAL ---")
print(anova(modelo_tamanos))

# 3. Comparaciones por pares DENTRO de cada localización
print("--- COMPARACIONES DE TAMAÑO POR LOCALIZACIÓN ---")
medias_tamanos <- emmeans(modelo_tamanos, ~ Tamano | Localización)

# Calculamos todas las combinaciones posibles (pairwise)
comparaciones_tamanos <- pairs(medias_tamanos)
print(comparaciones_tamanos)




####################################################
####################################################
#    TAMAÑOS LARVARIOS  EN FUNCIÓN DEL TIEMPO      #
####################################################
####################################################



library(tidyverse)

# 2. Preparar el dataframe temporal agregado por tamaños
df_temporal_tamanos <- df_tamanos %>%
  # Volvemos a convertir FECHA a formato fecha real (estaba como factor para el modelo)
  mutate(FECHA = as.Date(as.character(FECHA))) %>%
  # Agrupamos por día y por tamaño larvario
  group_by(FECHA, Tamano) %>%
  # Sumamos la densidad de todas las localizaciones para tener la población total del río
  summarise(Densidad_Total = sum(Densidad, na.rm = TRUE), .groups = "drop")

# 3. Generar el gráfico con los 3 paneles
grafico_lineas_tamanos <- ggplot(df_temporal_tamanos, aes(x = FECHA, y = Densidad_Total, color = Tamano)) +
  
  # Añadimos las líneas púrpuras de los tratamientos AL FONDO
  geom_vline(xintercept = as.numeric(fechas_post), 
             color = "purple", linewidth = 1.2, alpha = 0.4) +
  
  # Líneas y puntos de las densidades
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # LA MAGIA: Crea 3 paneles distintos (uno debajo del otro) y adapta el eje Y a cada uno
  facet_wrap(~ Tamano, ncol = 1, scales = "free_y") +
  
  # Mantenemos los colores consistentes
  scale_color_manual(values = c("PEQUEÑAS" = "#FF9999", 
                                "MEDIANAS" = "#66B2FF", 
                                "GRANDES"  = "#99FF99")) +
  
  theme_minimal() +
  labs(
    title = "Evolución Temporal de la Densidad por Tamaño Larvario",
    subtitle = "Líneas púrpura: Fechas de tratamiento",
    x = "Fecha de Muestreo",
    y = "Densidad Larvaria Total (Individuos)",
    color = "Tamaño"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none" # Ocultamos la leyenda porque los títulos de los paneles ya lo indican
  )

# Mostrar el gráfico
print(grafico_lineas_tamanos)


# Significancia


library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)


# 2. Asegurar el formato Date y extraer las fechas de muestreo
# Asumimos que df_tamanos ya está cargado en el entorno
df_tamanos_fechas <- df_tamanos %>%
  mutate(FECHA = as.Date(as.character(FECHA)))

fechas_muestreo <- sort(unique(df_tamanos_fechas$FECHA))

# 3. NUEVO ALGORITMO: POST = Día del tratamiento | PRE = Muestreo anterior
tabla_momentos <- map_dfr(fechas_post, function(t_date) {
  
  # PRE: El muestreo más cercano que sea estrictamente ANTERIOR al tratamiento
  fecha_pre <- max(fechas_muestreo[fechas_muestreo < t_date], na.rm = TRUE)
  
  # POST: La propia fecha de tratamiento
  fecha_post <- t_date
  
  tibble(
    Tratamiento_ID = paste("Campaña", t_date),
    FECHA = c(fecha_pre, fecha_post),
    Momento = c("PRE", "POST")
  )
}) %>%
  # Limpieza para evitar duplicados si los tratamientos están muy juntos
  distinct(FECHA, Momento, .keep_all = TRUE)

# 4. Filtrar nuestro dataframe grande para usar SOLO estos días clave
df_impacto <- df_tamanos_fechas %>%
  # Añadimos el argumento para silenciar el aviso de seguridad de R
  inner_join(tabla_momentos, by = "FECHA", relationship = "many-to-many") %>%
  # Asegurar que el modelo lee "PRE" antes que "POST"
  mutate(Momento = factor(Momento, levels = c("PRE", "POST")))

# 5. MODELO ESTADÍSTICO (LMM) - CORREGIDO
# Cambiamos 'data = df_tamanos' por 'data = df_impacto'
modelo_impacto <- lmer(log1p(Densidad) ~ Momento * Tamano + 
                         (1 | Localización) + 
                         (1 | Tratamiento_ID), 
                       data = df_impacto)

# 6. Ver resultados del nuevo modelo
summary(modelo_impacto)
library(car)
Anova(modelo_impacto, type = "II")

# 6. PRUEBA POST-HOC DIRECTA (Extracción de p-valores)
print("--- EFICACIA DEL TRATAMIENTO: PRE vs POST POR TAMAÑO ---")
medias_impacto <- emmeans(modelo_impacto, ~ Momento | Tamano)
comparaciones_impacto <- pairs(medias_impacto)

print(comparaciones_impacto)


##################################
##################################
#    EFECTO DEL TRATAMIENTO      #
##################################
##################################


library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# 1. Definir fechas de tratamiento (que ahora son el POST)
fechas_tratamiento <- as.Date(c("2025-05-16", "2025-05-21", "2025-06-12", "2025-08-26", "2025-09-30"))

# 2. Asegurar formato de fechas y extraer las reales de muestreo
df_limpio <- df %>% 
  filter(!is.na(DENSIDAD_LARVARIA_TOTAL)) %>% 
  mutate(FECHA = as.Date(as.character(FECHA)))

fechas_muestreo <- sort(unique(df_limpio$FECHA))

# 3. NUEVO ALGORITMO: POST = Día del tratamiento | PRE = Muestreo anterior
tabla_momentos <- map_dfr(fechas_tratamiento, function(t_date) {
  
  # PRE: El muestreo más cercano que sea estrictamente ANTERIOR a la fecha indicada
  fecha_pre <- max(fechas_muestreo[fechas_muestreo < t_date], na.rm = TRUE)
  
  # POST: La propia fecha indicada
  fecha_post <- t_date
  
  tibble(
    Tratamiento_ID = paste("Campaña", t_date),
    FECHA = c(fecha_pre, fecha_post),
    Momento = c("PRE", "POST")
  )
}) %>% 
  # Evitar duplicados por si los periodos se solapan
  distinct(FECHA, Momento, .keep_all = TRUE)

# 4. Unir con los datos de densidad total
df_impacto_loc <- df_limpio %>%
  inner_join(tabla_momentos, by = "FECHA") %>%
  mutate(
    Momento = factor(Momento, levels = c("PRE", "POST")),
    Localizacion_factor = as.factor(Localización)
  )

# 5. Calcular medias y Errores Estándar (SE) para el gráfico
df_resumen_loc <- df_impacto_loc %>%
  group_by(Localización, Momento) %>%
  summarise(
    Media = mean(DENSIDAD_LARVARIA_TOTAL, na.rm = TRUE),
    SE = sd(DENSIDAD_LARVARIA_TOTAL, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 6. Generar el Gráfico de Barras Pareadas
grafico_pre_post_loc <- ggplot(df_resumen_loc, aes(x = Localización, y = Media, fill = Momento)) +
  geom_col(position = position_dodge(width = 0.8), color = "black", width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = ifelse(Media - SE < 0, 0, Media - SE), ymax = Media + SE),
                position = position_dodge(width = 0.8), width = 0.25, linewidth = 0.8) +
  scale_fill_manual(values = c("PRE" = "#D55E00", "POST" = "#0072B2")) +
  theme_minimal() +
  labs(
    title = "Eficacia del Tratamiento: Densidad Larvaria Total por Localización",
    subtitle = "Comparativa PRE y POST tratamiento (Media ± Error Estándar)",
    x = "Punto de Muestreo",
    y = "Densidad Larvaria Media",
    fill = "Momento"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

print(grafico_pre_post_loc)


# ---------------------------------------------------------
# 7. ANÁLISIS ESTADÍSTICO
# ---------------------------------------------------------

# Modelo mixto
modelo_loc_impacto <- lmer(log1p(DENSIDAD_LARVARIA_TOTAL) ~ Momento * Localizacion_factor + (1 | Tratamiento_ID), 
                           data = df_impacto_loc)

anova(modelo_loc_impacto)
