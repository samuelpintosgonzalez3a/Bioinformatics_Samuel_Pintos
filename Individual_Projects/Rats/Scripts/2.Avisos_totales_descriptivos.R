###################################
# PROPORCIÓN RATICIDA-RODENTICIDA
###################################



# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# Preparar los datos: Sumar los totales de cada producto

df_proporcion <- df %>%
  summarise(
    Alcantarilla = sum(RATICIDA, na.rm = TRUE),
    Exteriores = sum(`RODENTICIDA MADRIG`, na.rm = TRUE)
  ) %>%
  # Pasamos de formato ancho a largo para facilitar el gráfico
  pivot_longer(cols = everything(), 
               names_to = "Producto", 
               values_to = "Cantidad_Total") %>%
  # Porcentaje relativo entre ambos
  mutate(porcentaje = Cantidad_Total / sum(Cantidad_Total))

# 3. Crear el gráfico de barra única al 100%
ggplot(df_proporcion, aes(x = "Total Productos", y = porcentaje, fill = Producto)) +
  # Usamos geom_col para crear la barra apilada
  geom_col(width = 0.6) +
  # Añadir las etiquetas de porcentaje
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            fontface = "bold",
            size = 5) +
  # Formatear el eje Y como porcentaje
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proporción de Rodenticida Utilizado",
    subtitle = "Comparativa entre zonas (2018-2025)",
    x = NULL,
    y = "Porcentaje relativo",
    fill = "Tipo de Producto"
  ) +
  theme_minimal() +
  # Colores personalizados
  scale_fill_manual(values = c("Exteriores" = "#2E86C1", "Alcantarilla" = "#E67E22"))


###################################################
##ANÁLISIS DE MADRIGUERAS VS PÚBLICAS (REGISTRADAS)
###################################################



# Cargar librerías

library(scales)


# 2. Preparar los datos: Filtrar y calcular porcentajes
df_resumen <- df %>%
  filter(MADRIGUERA == 1) %>%
  # Agrupamos por la variable PUBLICA
  group_by(PUBLICA) %>%
  summarise(total = n()) %>%
  # Calculamos la proporción sobre el total de MADRIGUERA == 1
  mutate(porcentaje = total / sum(total)) %>%
  # Etiquetas descriptivas
  mutate(Etiqueta_Publica = case_when(
    PUBLICA == 1 ~ "Pública",
    PUBLICA == 0 ~ "Privada",
    TRUE ~ "No registrado"
  ))

# 3. Crear el gráfico con etiquetas de porcentaje
ggplot(df_resumen, aes(x = "Madrigueras registradas", y = porcentaje, fill = Etiqueta_Publica)) +
  # Usamos geom_col porque ya tenemos los valores calculados
  geom_col() +
  # Añadir el texto del porcentaje encima de cada segmento
  geom_text(aes(label = percent(porcentaje, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), # Centrado en el segmento
            color = "white", # Color del texto
            fontface = "bold") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Madrigueras públicas vs privadas",
    x = NULL,
    y = "Porcentaje (%)",
    fill = "Estado"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")





######################################
#  GRÁFICOS TEMPORALES
######################################


###########################################3
#######################
#############################################
#########################################



# 1. Carga de librerías
library(tidyverse)
library(lubridate)


# Agrupación semanal
df_semanal <- df %>%
  mutate(FECHA = as.Date(FECHA)) %>%
  mutate(semana = floor_date(FECHA, unit = "week")) %>% 
  group_by(semana) %>%
  summarise(
    avisos = n(),
    solares = sum(SOLAR, na.rm = TRUE),
    madrigueras = sum(MADRIGUERA, na.rm = TRUE),
    basura = sum(BASURA, na.rm = TRUE),
    repetidos = sum(REPETIDO, na.rm = TRUE),
    nulos = sum(NULO, na.rm = TRUE)
  )

# ---------------------------------------------------------
# GRÁFICO 1: EVOLUCIÓN SEMANAL DE AVISOS
# ---------------------------------------------------------
p1 <- ggplot(df_semanal, aes(x = semana, y = avisos)) +
  geom_line(color = "steelblue", linewidth = 0.8) + 
  geom_point(color = "steelblue", size = 1, alpha = 0.7) +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  scale_x_date(date_labels = "%W-%y", date_breaks = "4 weeks") +
  labs(title = "Evolución Semanal del Número de Avisos Totales (2018-2025)",
       ,
       x = "Semana - Año",
       y = "Cantidad de Avisos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))

print(p1)

# ---------------------------------------------------------
# 4. GRÁFICO 2: MULTIVARIABLE (Corregido)
# ---------------------------------------------------------

df_long_semanal2 <- df_semanal %>%
  select(semana, basura, repetidos, solares) %>%
  pivot_longer(cols = -semana, names_to = "indicador", values_to = "valor")

p2 <- ggplot(df_long_semanal2, aes(x = semana, y = valor, color = indicador)) +
  geom_line(linewidth = 0.7) + # <--- Cambio aquí: linewidth
  scale_x_date(date_labels = "%W-%y", date_breaks = "4 weeks") +
  scale_color_manual(values = c(
    "basura" = "black", 
    "repetidos" = "#4DAF4A", 
    "solares" = "#984EA3")) +
  labs(title = "Seguimiento Semanal de Indicadores de Campo (2025)",
       x = "Semana - Año",
       y = "Conteo Total",
       color = "Indicador") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
        legend.position = "bottom")

print(p2)


# ------------------------------------
# HISTOGRAMA REPETIDOS VERSUS TOTAL
# ------------------------------------


df_porcentaje_rep <- df %>%
  mutate(FECHA = as.Date(FECHA),
         ANIO = year(FECHA)) %>%
 
  filter(ANIO >= 2018 & ANIO <= 2025) %>%
 
  filter(str_starts(VECTOR_ENCONTRADO, "RAT")) %>% 
  
  mutate(REPETIDO = if_else(REPETIDO == 1, 1, 0)) %>%
  group_by(ANIO) %>%
  summarise(
    total_avisos = n(),
    con_repetido = sum(REPETIDO == 1, na.rm = TRUE),
    porcentaje = (con_repetido / total_avisos) * 100,
    .groups = 'drop' # Buena práctica para desagrupar
  )


p3 <- ggplot(df_porcentaje_rep, aes(x = factor(ANIO), y = porcentaje, fill = porcentaje)) +
  geom_col(show.legend = FALSE, color = "white", width = 0.7) +
  # Añadir etiquetas de porcentaje sobre las barras
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            vjust = -0.5, fontface = "bold", size = 4) +
  # Escala de colores premium (rojo/naranja para diferenciar de madrigueras)
  scale_fill_gradient(low = "#FADBD8", high = "#E74C3C") +
  # Estética del tema
  theme_minimal() +
  labs(
    title = "Porcentaje de Avisos Repetidos (2018 - 2025)",
    x = "Año",
    y = "Porcentaje (%)",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(p3)


# ---------------------------------
# HISTOGRAMA PROPORCIONES RELATIVAS
# ---------------------------------



proporciones <- df %>%
  summarise(
    Madrigueras = mean(MADRIGUERA, na.rm = TRUE) * 100,
    Basura = mean(BASURA, na.rm = TRUE) * 100,
    Nulos = mean(NULO, na.rm = TRUE) * 100,
    Repetidos = mean(REPETIDO, na.rm = TRUE) * 100,
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Porcentaje") %>%
  arrange(desc(Porcentaje)) # Ordenamos de mayor a menor para mejor estética

# 4. Generación del gráfico de barras
ggplot(proporciones, aes(x = reorder(Variable, -Porcentaje), y = Porcentaje, fill = Variable)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  # Añadimos el texto del porcentaje encima de cada barra
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            vjust = -0.5, 
            size = 4, 
            fontface = "bold") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Proporción Relativa sobre el Total de Avisos",
       subtitle = "Porcentaje de incidencia de cada factor en el histórico completo",
       x = "Indicador",
       y = "Porcentaje (%)") +
  # Ajustamos el límite del eje Y para que no se corte el texto
  scale_y_continuous(limits = c(0, max(proporciones$Porcentaje) * 1.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank() # Quitamos líneas verticales para limpieza
  )