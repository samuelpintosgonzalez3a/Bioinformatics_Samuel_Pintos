######################
######################
# DENSIDADES POR SPP #
######################
######################

library(dplyr)
library(tidyr)
library(ggplot2)


df_especies <- df %>%
  select(
    `S. pseudoequinum`  = densidad_S.pseudoequinum,
    `S. ornatum`        = densidad_S.ornatum,
    `S. lineatum`       = densidad_S.lineatum,
    `S. rubzovianum`    = densidad_S.rubzovianum,
    `S. sergenti`       = densidad_S.sergenti,
    `S. erytrocephalum` = densidad_S.erytrocephalum,
    `S. ruficorne`      = densidad_S.ruficorne,
    `S. equinum`        = densidad_S.equinum
  ) %>%
  # Pasamos las columnas a formato largo para poder agruparlas
  pivot_longer(cols = everything(), names_to = "Especie", values_to = "Densidad") %>%
  
  # 3. Calcular la suma total acumulada de densidades por especie
  group_by(Especie) %>%
  summarise(Total_Densidad = sum(Densidad, na.rm = TRUE)) %>%
  
  # 4. Reordenar el factor 'Especie' de forma ascendente según su densidad total
  # Esto garantiza que al usar coord_flip() la especie más abundante quede arriba
  mutate(Especie = reorder(Especie, Total_Densidad))

# 5. Diseño y renderizado del gráfico de barras horizontales
grafico_barras_especies <- ggplot(df_especies, aes(x = Especie, y = Total_Densidad, fill = Total_Densidad)) +
  geom_col(show.legend = FALSE, color = "#2b2b2b", width = 0.7) +
  coord_flip() +  # Volteado horizontal para facilitar la lectura de los nombres binomiales
  scale_fill_viridis_c(option = "mako", begin = 0.2, end = 0.8) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribución de la Densidad Acumulada por Especie",
    subtitle = "Suma total de las densidades larvarias registradas (Larvas / kg)",
    x = NULL,
    y = "Suma Acumulada de Densidad"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a1a1a"),
    plot.subtitle = element_text(size = 11, color = "#555555"),
    axis.text.y = element_text(face = "italic", size = 11, color = "#222222"), # Cursiva para nomenclatura científica
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank() # Limpia las líneas horizontales para resaltar las barras
  )

# Imprimir el gráfico en pantalla
print(grafico_barras_especies)



# -significancia

library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(emmeans)

# 1. Preparar los datos en formato largo (aplicando la corrección del outlier que vimos)
df_model_esp <- df %>%
  select(
    Localización, FECHA,
    `S. pseudoequinum`  = densidad_S.pseudoequinum,
    `S. ornatum`        = densidad_S.ornatum,
    `S. lineatum`       = densidad_S.lineatum,
    `S. rubzovianum`    = densidad_S.rubzovianum,
    `S. sergenti`       = densidad_S.sergenti,
    `S. erytrocephalum` = densidad_S.erytrocephalum,
    `S. ruficorne`      = densidad_S.ruficorne,
    `S. equinum`        = densidad_S.equinum
  ) %>%
  # Identificador único para cada unidad muestral (cada visita a cada río)
  mutate(Muestra_ID = paste(Localización, FECHA, sep = "_")) %>%
  pivot_longer(cols = starts_with("S. "), names_to = "Especie", values_to = "Densidad")

# 2. Ajustar el Modelo Mixto
# Usamos log1p por la sobredispersión y Muestra_ID como efecto aleatorio
modelo_especies <- lmer(log1p(Densidad) ~ Especie + (1 | Muestra_ID), data = df_model_esp)

# 3. ANOVA para ver si globalmente "Especie" es una variable significativa
print("--- ANOVA Global ---")
anova(modelo_especies)

# 4. Comparaciones Múltiples Post-Hoc (Test de Tukey)
# Esto te dirá qué especies son significativamente más abundantes que otras
print("--- Comparaciones Post-Hoc (Tukey) ---")
comparaciones <- emmeans(modelo_especies, pairwise ~ Especie, adjust = "tukey")
# Mostramos solo los p-valores de las comparaciones en formato tabla limpia
summary(comparaciones$contrasts)




######################
######################
# ABUNDANCIA POR SPP #
######################
######################


library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(car)
library(emmeans)

# 1. SELECCIÓN Y PREPARACIÓN DE LOS CONTEOS BRUTOS
# Usamos las columnas de individuos totales (comienzan por S.)
df_conteos <- df %>%
  select(
    Localización, FECHA,
    `S. ornatum`        = S.ornatum,
    `S. pseudoequinum`  = S.Pseudoequinum,
    `S. lineatum`       = S.Lineatum,
    `S. rubzovianum`    = S.rubzovianum,
    `S. sergenti`       = S.sergenti,
    `S. erytrocephalum` = S.Erytrocephalum,
    `S. equinum`        = S.equinum,
    `S. ruficorne`      = S.ruficorne
  ) %>%
  # Identificador de muestra único (bloque espacial-temporal)
  mutate(Muestra_ID = paste(Localización, FECHA, sep = "_")) %>%
  # Transformamos a formato largo para el plot y el modelo
  pivot_longer(cols = starts_with("S. "), names_to = "Especie", values_to = "Individuos")

# -------------------------------------------------------------------------
# PARTE 1: GRÁFICO DE BARRAS DE RECUENTO TOTAL
# -------------------------------------------------------------------------

# Agrupamos para calcular la suma absoluta de individuos capturados
df_plot_conteos <- df_conteos %>%
  group_by(Especie) %>%
  summarise(Total_Individuos = sum(Individuos, na.rm = TRUE)) %>%
  mutate(Especie = reorder(Especie, Total_Individuos))

grafico_conteos <- ggplot(df_plot_conteos, aes(x = Especie, y = Total_Individuos, fill = Total_Individuos)) +
  geom_col(show.legend = FALSE, color = "#2b2b2b", width = 0.7) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", begin = 0.2, end = 0.8) + # Cambiado a paleta plasma para diferenciarlo del de densidades
  theme_minimal(base_size = 12) +
  labs(
    title = "Abundancia Relativa por Especie de Simúlido",
    subtitle = "Recuento total de individuos capturados en los muestreos",
    x = NULL,
    y = "Número Total de Individuos"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a1a1a"),
    plot.subtitle = element_text(size = 11, color = "#555555"),
    axis.text.y = element_text(face = "italic", size = 11, color = "#222222"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Renderizar gráfico de barras
print(grafico_conteos)



# -------------------------------------------------------------------------
# PARTE 2: ANOVA Y MODELO MIXTO GENERALIZADO (GLMM)
# -------------------------------------------------------------------------

# Ajustamos el modelo para datos de conteo con sobredispersión (nbinom2)
# Controlamos la estructura repetida del muestreo con (1 | Muestra_ID)
set.seed(1234)
modelo_conteos_esp <- glmmTMB(Individuos ~ Especie + (1 | Muestra_ID), 
                              data = df_conteos, 
                              family = nbinom2)

# ANOVA Global (Prueba de Wald tipo II)
print("--- ANOVA GLOBAL (CONTEOS) ---")
anova_resultados <- Anova(modelo_conteos_esp, type = "II")
print(anova_resultados)

# Comparaciones múltiples Post-Hoc (Test de Tukey)
print("--- COMPARACIONES MÚLTIPLES POST-HOC (TUKEY) ---")
comparaciones_conteos <- emmeans(modelo_conteos_esp, pairwise ~ Especie, adjust = "tukey")

# Extraemos la tabla limpia de p-valores entre las especies
tabla_p_valores <- summary(comparaciones_conteos$contrasts)
print(tabla_p_valores)



######################
######################
# TIEMPO - LOC - SPP #
######################
######################

library(tidyverse)
library(stringr)
library(scales) # Para formatear el eje Y a porcentajes visuales

# 1. Preparación de los datos
df_especies_plot <- df %>%
  # Seleccionamos las columnas clave
  select(FECHA, Localización, 
         `S. pseudoequinum`  = densidad_S.pseudoequinum,
         `S. ornatum`        = densidad_S.ornatum,
         `S. lineatum`       = densidad_S.lineatum,
         `S. rubzovianum`    = densidad_S.rubzovianum,
         `S. sergenti`       = densidad_S.sergenti,
         `S. erytrocephalum` = densidad_S.erytrocephalum,
         `S. ruficorne`      = densidad_S.ruficorne,
         `S. equinum`        = densidad_S.equinum) %>%
  # Pivotamos al formato largo
  pivot_longer(
    cols = starts_with("S. "),
    names_to = "Especie",
    values_to = "Densidad"
  ) %>%
  # Limpieza y factores
  mutate(
    Especie = as.factor(Especie),
    Localización = as.factor(Localización),
    # Usamos FECHA como factor para que las barras salgan separadas e iguales
    FECHA_factor = as.factor(FECHA), 
    # Creamos una variable de tiempo continua para el modelo estadístico posterior
    Dias_Estudio = as.numeric(as.Date(FECHA) - min(as.Date(FECHA), na.rm = TRUE))
  ) %>%
  # Quitamos nulos
  filter(!is.na(Densidad))

# 2. Definir una paleta de 8 colores distintivos (Colorblind-friendly)
colores_especies <- c(
  "S. pseudoequinum"  = "#E69F00",  # Naranja
  "S. ornatum"        = "#56B4E9",  # Azul claro
  "S. lineatum"       = "#009E73",  # Verde
  "S. rubzovianum"    = "#F0E442",  # Amarillo
  "S. sergenti"       = "#0072B2",  # Azul oscuro
  "S. erytrocephalum" = "#D55E00",  # Rojo/Naranja oscuro
  "S. ruficorne"      = "#CC79A7",  # Rosa/Magenta
  "S. equinum"        = "#999999"   # Gris
)

# 3. Gráfico de barras apiladas al 100%
grafico_proporciones_esp <- ggplot(df_especies_plot, aes(x = FECHA_factor, y = Densidad, fill = Especie)) +
  
  # Barras proporcionales al 100%
  geom_col(position = "fill", width = 0.8, color = "black", linewidth = 0.2) + 
  
  # Facetamos por Localización
  facet_wrap(~ Localización, ncol = 2) +
  
  # Aplicamos nuestros 8 colores manuales
  scale_fill_manual(values = colores_especies) +
  
  # Transformamos el eje Y de 0-1 a 0%-100%
  scale_y_continuous(labels = percent_format()) +
  
  theme_minimal(base_size = 11) +
  labs(
    title = "Estructura Poblacional: Proporción de Densidad por Especie",
    x = "Fecha de Muestreo",
    y = "Proporción de la comunidad (%)",
    fill = "Especie"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "italic"), # Nombres de las especies en cursiva
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.grid.major.x = element_blank() 
  )

print(grafico_proporciones_esp)

# Significancia

library(dplyr)
library(glmmTMB)
library(car)

# 1. ESTANDARIZAR LA VARIABLE TEMPORAL (Vital para que el modelo no colapse)
df_especies_plot <- df_especies_plot %>%
  mutate(Dias_Estudio_Std = as.vector(scale(Dias_Estudio)))

# -------------------------------------------------------------------------
# PREGUNTA 1: ¿Cambia la proporción de las especies a lo largo del TIEMPO?
# (Sucesión Ecológica Estacional)
# -------------------------------------------------------------------------
set.seed(1234)
modelo_tiempo <- glmmTMB(
  Densidad ~ Especie * Dias_Estudio_Std + (1 | FECHA_factor),
  data = df_especies_plot,
  family = tweedie(link = "log")
)

print("--- ANOVA: INTERACCIÓN ESPECIE vs TIEMPO ---")
Anova(modelo_tiempo, type = "II")


# -------------------------------------------------------------------------
# PREGUNTA 2: ¿Cambia la dominancia de las especies según la LOCALIZACIÓN?
# (Distribución Espacial en el Río)
# -------------------------------------------------------------------------
# MODELO 2 CORREGIDO: Modelo Aditivo (Efectos principales independientes)
set.seed(1234)
modelo_espacio_aditivo <- glmmTMB(
  Densidad ~ Especie + Localización + (1 | FECHA_factor),
  data = df_especies_plot,
  family = tweedie(link = "log")
)

# Sacamos la tabla ANOVA
print("--- ANOVA: EFECTOS PRINCIPALES (ESPECIE Y LOCALIZACIÓN) ---")
Anova(modelo_espacio_aditivo, type = "II")


library(emmeans)

# 1. Ejecutar el test de Tukey para la variable Localización
print("--- CALCULANDO TEST DE TUKEY PARA LOCALIZACIÓN ---")
tukey_localizacion <- emmeans(modelo_espacio_aditivo, pairwise ~ Localización, adjust = "tukey")

# 2. Extraer y mostrar solo la tabla de contrastes (p-valores)
tabla_tukey_loc <- summary(tukey_localizacion$contrasts)
print("--- RESULTADOS DE LAS COMPARACIONES PAREADAS ---")
print(tabla_tukey_loc)

# Opcional: Ejecutar también el Tukey para ver las diferencias globales entre Especies
tukey_especies <- emmeans(modelo_espacio_aditivo, pairwise ~ Especie, adjust = "tukey")
summary(tukey_especies$contrasts)



#######################
#######################
#### PUPA - TIEMPO ####
#######################
#######################




library(tidyverse)

# 1. Preparar datos 
df_facet <- df %>%
  mutate(FECHA = as.Date(FECHA)) %>%
  group_by(FECHA) %>%
  summarise(across(starts_with("densidad_S."), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with("densidad_S."), 
               names_to = "Especie", 
               values_to = "Densidad") %>%
  mutate(Especie = str_replace(Especie, "densidad_S\\.", "S. "))

# 2. Crear gráficos subdivididos
grafico_especies <- ggplot(df_facet, aes(x = FECHA, y = Densidad)) +
  geom_line(color = "#008080", linewidth = 0.8) + 
  geom_point(color = "#008080", size = 1.5) + 
  geom_vline(xintercept = as.Date(c("2025-05-16", "2025-05-21", "2025-06-12", "2025-08-26", "2025-09-30")), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~Especie, scales = "free_y", ncol = 2) + 
  theme_bw() +
  labs(title = "Evolución Temporal Individual por Densidad de Especie",
       x = "Fecha", 
       y = "Suma de Densidades") +
  theme(strip.background = element_rect(fill = "#2C3E50"),
        strip.text = element_text(color = "white", face = "bold.italic")) 
# Mostrar el gráfico
print(grafico_especies)


#Significancia

library(tidyverse)
library(glmmTMB)
library(car)
library(emmeans)

# 1. PREPARACIÓN DE DATOS (Rápida y limpia)
df_tiempo <- df %>%
  mutate(FECHA = as.Date(FECHA)) %>%
  filter(!is.na(FECHA)) %>%
  # Cogemos las densidades de especies
  select(FECHA, Localización, starts_with("densidad_S.")) %>%
  pivot_longer(
    cols = starts_with("densidad_S."), 
    names_to = "Especie", 
    values_to = "Densidad"
  ) %>%
  mutate(
    # Limpiamos nombres para que quede bonito ("S. ornatum")
    Especie = as.factor(str_replace(Especie, "densidad_S\\.", "S. ")),
    Localización = as.factor(Localización),
    # Creamos la variable de tiempo y la estandarizamos
    Dias_Estudio = as.numeric(FECHA - min(FECHA, na.rm = TRUE)),
    Dias_Estudio_Std = as.vector(scale(Dias_Estudio))
  ) %>%
  filter(!is.na(Densidad))

# 2. MODELO MIXTO GENERALIZADO (GLMM - Tweedie)
# Pregunta: ¿Cambia la densidad según la especie y el paso del tiempo?
set.seed(1234)
modelo_tiempo <- glmmTMB(
  Densidad ~ Especie * Dias_Estudio_Std + (1 | Localización),
  data = df_tiempo,
  family = tweedie(link = "log")
)

# 3. TABLA ANOVA (Significancia global)
print("--- ANOVA: ESPECIE Y TIEMPO ---")
anova_tiempo <- Anova(modelo_tiempo, type = "II")
print(anova_tiempo)

# 4. TEST DE TUKEY (Post-Hoc de Especies)
# ¿Qué especies son significativamente distintas entre sí a lo largo del año?
print("--- TEST DE TUKEY: DIFERENCIAS GLOBALES ENTRE ESPECIES ---")
tukey_especies <- emmeans(modelo_tiempo, pairwise ~ Especie, adjust = "tukey")
print(summary(tukey_especies$contrasts))

# EXTRA PRO PARA TU TFM: Análisis de Tendencias (Pendientes)
# Como el tiempo es continuo, esto te dice qué especies suben en verano y cuáles bajan
print("--- TEST DE TUKEY: DIFERENCIAS EN LA TENDENCIA TEMPORAL ---")
tendencias_tiempo <- emtrends(modelo_tiempo, pairwise ~ Especie, var = "Dias_Estudio_Std", adjust = "tukey")
print(summary(tendencias_tiempo$contrasts))



###########
###########
# HEATMAP #
###########
###########


library(tidyverse)
library(lubridate)

# 1. Preparación de datos y Estandarización Global
df_heatmap <- df %>%
  mutate(FECHA = as.Date(FECHA)) %>%
  select(Localización, FECHA, starts_with("densidad_S.")) %>%
  mutate(MES_NUM = month(FECHA)) %>%
  pivot_longer(cols = starts_with("densidad_S."), 
               names_to = "Especie", 
               values_to = "Densidad") %>%
  mutate(Especie = str_replace(Especie, "densidad_S\\.", "S. ")) %>%
  group_by(MES_NUM, Especie) %>%
  summarise(Densidad_Media_Mes = mean(Densidad, na.rm = TRUE), .groups = "drop") %>%
  group_by(Especie) %>%
  mutate(
    Intensidad = if(max(Densidad_Media_Mes, na.rm = TRUE) == 0) 0 else 
      (Densidad_Media_Mes - min(Densidad_Media_Mes, na.rm = TRUE)) / 
      (max(Densidad_Media_Mes, na.rm = TRUE) - min(Densidad_Media_Mes, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  
  # EL TRUCO VISUAL: Todo valor por debajo de 0.05 (muy cercano a 0) se convierte en NA
  # Puedes ajustar ese 0.05 al umbral que te parezca más limpio visualmente
  mutate(Intensidad = ifelse(Intensidad < 0.05, NA, Intensidad))

# 2. Generar el Mapa de Calor
mapa_calor <- ggplot(df_heatmap, aes(x = factor(MES_NUM), y = Especie, fill = Intensidad)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Configuramos el gradiente y le asignamos el gris a los valores NA
  scale_fill_gradient(
    low = "#FFFFCC", 
    high = "#800026", 
    na.value = "grey37",  # Gris claro para los valores cercanos a 0
    name = "Pico Poblacional"
  ) +
  
  scale_x_discrete(labels = c("Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov")) +
  theme_minimal(base_size = 12) +
  labs(title = "Mapa de Calor Fenológico: Sucesión Temporal por Densidad de Especies",
       subtitle = "Intensidad estandarizada (Gris = Ausencia/Mínimo anual; Rojo = Pico de actividad)",
       x = "Mes de Muestreo", 
       y = NULL) +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "italic"), 
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#555555"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold")
  )

print(mapa_calor)


# Significancia (PERMANOVA)


library(tidyverse)
library(vegan)
library(lubridate)

# 1. Preparar la "Matriz de Comunidad" (Filas = Meses, Columnas = Especies)
df_comunidad <- df %>%
  mutate(MES_NUM = month(as.Date(FECHA))) %>%
  # Sumamos la densidad total de cada especie por mes
  group_by(MES_NUM) %>%
  summarise(across(starts_with("densidad_S."), ~ sum(.x, na.rm = TRUE))) %>%
  column_to_rownames("MES_NUM") # Convertimos la columna MES en los nombres de las filas

# 2. Ejecutar el PERMANOVA (Distancia de Bray-Curtis)
set.seed(1234)
permanova_temporal <- adonis2(df_comunidad ~ as.numeric(rownames(df_comunidad)), 
                              method = "bray", 
                              permutations = 999)

print("--- RESULTADO DEL PERMANOVA TEMPORAL ---")
print(permanova_temporal)



