

############################
############################
#     TABLA DESCRIPTIVA    #
############################
############################


library(dplyr)
library(purrr)
library(ggpubr)
library(grid)



analizar_todo <- function(x, nombre) {
  x_clean <- na.omit(x)
  
  # Validar que haya datos suficientes y variabilidad
  if(length(unique(x_clean)) <= 1 || length(x_clean) < 5) return(NULL)
  
  # Test KS contra una normal teórica (media y sd de la muestra)
  ks_res <- suppressWarnings(ks.test(x_clean, "pnorm", mean(x_clean), sd(x_clean)))
  p_val <- ks_res$p.value
  es_normal <- p_val > 0.05
  
  # 1. Modificar el nombre de la variable (cambiar "_" por " ")
  nombre_limpio <- gsub("_", " ", nombre)
  
  # 2. Formatear el p-valor: si es muy cercano a 0, poner <0.001
  # sprintf("%.3f", p_val) asegura que siempre se muestren 3 decimales (ej: 0.050)
  p_val_formateado <- ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val))
  
  data.frame(
    Variable = nombre_limpio,
    `p` = p_val_formateado,
    Distribucion = if_else(es_normal, "Normal", "No Normal"),
    Estadistico = if_else(es_normal, 
                          paste0(round(mean(x_clean), 2), " ± ", round(sd(x_clean), 2)),
                          paste0(round(median(x_clean), 2), " [", round(IQR(x_clean), 2), "]")),
    check.names = FALSE
  )
}

# 4. Procesar todas las columnas numéricas
df_num <- df %>% select(where(is.numeric))
tabla_completa <- map2_df(df_num, names(df_num), analizar_todo)

# 5. Crear el Plot de la Tabla
plot_final <- ggtexttable(tabla_completa, 
                          rows = NULL, 
                          theme = ttheme(
                            colnames.style = colnames_style(fill = "#2C3E50", color = "white", face = "bold", size = 10),
                            tbody.style = tbody_style(fill = c("#F4F6F6", "#EAEDED"), size = 9),
                            padding = unit(c(3, 3), "mm")
                          )) %>%
  tab_add_title(text = "Análisis de Normalidad y Descriptivos", 
                face = "bold", size = 16, padding = unit(1.5, "line")) %>%
  tab_add_footnote(text = "Nota: Normalidad = Media ± Desviación estándar\ No Normalidad = Mediana [Rango Intercuartílico]", 
                   size = 7, face = "italic")

plot_final


#############################
#############################
# DENSIDAD POR CUATRIMESTRE #
#############################
#############################



# Definimos Cuatrimestre 1 (Ene-Abr), 2 (May-Ago), 3 (Sep-Dic)
df <- df %>%
  mutate(
    FECHA = as.Date(FECHA),
    MES = month(FECHA),
    CUATRIMESTRE = case_when(
      MES <= 4 ~ "C1",
      MES <= 8 ~ "C2",
      TRUE     ~ "C3"
    )
  )

# 2. Preparar los datos (Cálculo de medias y error estándar)
df_plot <- df %>%
  mutate(CUATRIMESTRE = as.factor(CUATRIMESTRE)) %>%
  # Seleccionamos solo las columnas de interés
  select(CUATRIMESTRE, DENSIDAD_LARVARIA_TOTAL, densidad_pupal_total) %>%
  # Transformamos a formato largo
  pivot_longer(
    cols = c(DENSIDAD_LARVARIA_TOTAL, densidad_pupal_total),
    names_to = "Estadio",
    values_to = "Densidad"
  ) %>%
  # Limpiamos los nombres para la leyenda
  mutate(Estadio = if_else(Estadio == "DENSIDAD_LARVARIA_TOTAL", "Larvas", "Pupas")) %>%
  # Agrupamos por cuatrimestre y estadio para calcular la media y el error estándar
  group_by(CUATRIMESTRE, Estadio) %>%
  summarise(
    Media = mean(Densidad, na.rm = TRUE),
    EE = sd(Densidad, na.rm = TRUE) / sqrt(n()), # Cálculo del Error Estándar
    .groups = "drop"
  )

# 3. Crear el gráfico de barras
grafico_barras <- ggplot(df_plot, aes(x = CUATRIMESTRE, y = Media, fill = Estadio)) +
  # Barras (stat="identity" viene por defecto en geom_col)
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9, color = "black") +
  # Barras de error
  geom_errorbar(aes(ymin = Media > 0, ymax = Media + EE), # ymin puede ser Media-EE, pero así no baja de 0
                position = position_dodge(width = 0.8), width = 0.25) +
  # Colores
  scale_fill_manual(values = c("Larvas" = "#2C3E50", "Pupas" = "#E74C3C")) +
  # Tema limpio
  theme_minimal() +
  labs(
    title = "Densidad Larvaria y Pupal Media por Cuatrimestre",
    x = "Cuatrimestre",
    y = expression("Densidad Media ("*ind/kg*") + EE"), 
    fill = "Estadio"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    # Opcional: Elimina las líneas de cuadrícula menores para que se vea más limpio
    panel.grid.minor = element_blank() 
  )

# Mostrar el gráfico
print(grafico_barras)



library(rstatix)

# --- 1. ANÁLISIS PARA DENSIDAD LARVARIA ---

# Test de Kruskal-Wallis (alternativa no paramétrica al ANOVA)
kw_larvas <- df %>% 
  kruskal_test(DENSIDAD_LARVARIA_TOTAL ~ CUATRIMESTRE)

cat("\n--- Test General: Densidad Larvaria ---\n")
print(kw_larvas)

# Si el p-valor de Kruskal-Wallis es < 0.05, hacemos un test Post-Hoc 
# para saber EXACTAMENTE qué cuatrimestres son distintos entre sí (ej: 1 vs 2, 2 vs 3)
posthoc_larvas <- df %>% 
  dunn_test(DENSIDAD_LARVARIA_TOTAL ~ CUATRIMESTRE, p.adjust.method = "bonferroni")

cat("\n--- Diferencias por pares (Larvas) ---\n")
print(posthoc_larvas)


# --- 2. ANÁLISIS PARA DENSIDAD PUPAL ---

kw_pupas <- df %>% 
  kruskal_test(densidad_pupal_total ~ CUATRIMESTRE)

cat("\n--- Test General: Densidad Pupal ---\n")
print(kw_pupas)

posthoc_pupas <- df %>% 
  dunn_test(densidad_pupal_total ~ CUATRIMESTRE, p.adjust.method = "bonferroni")

cat("\n--- Diferencias por pares (Pupas) ---\n")
print(posthoc_pupas)


#############################
#############################
#    EVOLUCIÓN TEMPORAL     #
#############################
#############################


library(tidyverse)
library(lubridate)


datos_limpios <- df %>%
  mutate(FECHA = ymd(FECHA)) %>%
  filter(!is.na(FECHA)) %>%
  pivot_longer(
    cols = c(DENSIDAD_LARVARIA_TOTAL, densidad_pupal_total),
    names_to = "Estadio",
    values_to = "Densidad"
  ) %>%
  mutate(Estadio = dplyr::recode(Estadio, 
                                 "DENSIDAD_LARVARIA_TOTAL" = "Larvas", 
                                 "densidad_pupal_total" = "Pupas"))


grafico_combinado <- ggplot(datos_limpios, aes(x = FECHA, y = Densidad, color = Estadio)) +
  
  # AÑADIR TRATAMIENTOS: Líneas verticales rojas discontinuas
  geom_vline(xintercept = as.numeric(fechas_post), 
             linetype = "dashed", color = "darkred", linewidth = 0.8, alpha = 0.7) +
  
  # Líneas y puntos de las densidades
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(size = 2) +
  
  # SEPARAR POR LOCALIZACIÓN: Crea un mini-gráfico para cada punto de muestreo
  facet_wrap(~ Localización, scales = "free_y") +
  scale_color_manual(values = c("Larvas" = "#1f78b4", "Pupas" = "#33a02c")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  
  theme_minimal() +
  labs(
    title = "Dinámica Poblacional de Simúlidos: Densidad de Larvas vs Pupas",
    subtitle = "Las líneas rojas discontinuas indican las fechas de tratamiento",
    x = "Fecha de Muestreo",
    y = "Densidad",
    color = "Estadio de desarrollo"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10) # Títulos de las localizaciones
  )

print(grafico_combinado)


# Significancia

library(lme4)
library(lmerTest) # Añade p-valores a los modelos de lme4
library(emmeans)  # Fundamental para las comparaciones post-hoc por localización
library(dplyr)


datos_modelo <- datos_limpios %>%
  mutate(
    Estadio = as.factor(Estadio),
    Localización = as.factor(Localización),
    FECHA = as.factor(FECHA) # La fecha será nuestro factor aleatorio
  )


# Usamos log1p (que es log(x + 1)) para normalizar las densidades biológicas
modelo <- lmer(log1p(Densidad) ~ Estadio * Localización + (1 | FECHA), data = datos_modelo)


summary(modelo)

medias_estimadas <- emmeans(modelo, ~ Estadio | Localización)
comparaciones <- pairs(medias_estimadas)
print(comparaciones)
