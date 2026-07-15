
# ---------------------------------
# TOP 30 BARRIOS RESPECTO AL RESTO
# ---------------------------------


library(tidyverse)
library(knitr)
library(kableExtra)

# 1. Identificar cuáles son los IDs del Top 30
top_30_ids <- df %>%
  count(BAR_COD) %>%
  arrange(desc(n)) %>%
  slice_head(n = 30) %>%
  pull(BAR_COD)


# 1. Obtenemos el dataframe con el conteo
df_counts <- df %>%
  count(BAR_COD, name = "Avisos") %>%
  arrange(desc(Avisos)) %>%
  slice_head(n = 30) %>%
  mutate(Ranking = row_number()) # Añadimos el número de puesto

# 2. Dividimos en dos bloques
top_1_15 <- df_counts %>% slice(1:15)
top_16_30 <- df_counts %>% slice(16:30)

# 3. Los unimos horizontalmente
tabla_horizontal <- bind_cols(top_1_15, top_16_30)

# 4. Mostrar la tabla
# Si usas RStudio, kableExtra la deja impecable:
tabla_horizontal %>%
  kable(col.names = c("Cód. Barrio", "Avisos","Puesto", "Cód. Barrio", "Avisos", "Puesto")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  add_header_above(c("Top 1 al 15" = 3, "Top 16 al 30" = 3))
df_comparativo <- df %>%
  mutate(grupo = ifelse(BAR_COD %in% top_10_ids, "Top 30 Barrios", "Resto de Barrios (101)")) %>%
  group_by(grupo) %>%
  summarise(total = n()) %>%
  mutate(
    prop = total / sum(total) * 100,
    label = paste0(format(total, big.mark="."), "\navisos\n(", round(prop, 1), "%)")
  )


ggplot(df_comparativo, aes(x = grupo, y = total, fill = grupo)) +
  # Barras con bordes limpios
  geom_col(width = 0.6, color = "white", linewidth = 1) +
  # Etiquetas de datos dentro o sobre las barras
  geom_text(aes(label = label), vjust = 1.2, color = "white", fontface = "bold", size = 5) +
  # Colores premium (Azul oscuro vs Gris suave)
  scale_fill_manual(values = c("Top 30 Barrios" = "#2E86C1", "Resto de Barrios (101)" = "#95A5A6")) +
  # Estética del tema
  theme_minimal() +
  labs(
    title = "CARGA DE AVISOS: TOP 30 VS. RESTO DE BARRIOS",
    subtitle = "Comparativa del volumen de avisos acumulado por los 30 barrios más activos",
    x = NULL,
    y = "Número total de avisos"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "none", # Quitamos leyenda porque el eje X ya lo indica
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12)
  )

# -------------------------------------
# RANKING CON BASURA, SOLAR, VEGETACIÓN
# -------------------------------------

library(dplyr)
library(gt)
library(gtExtras)

# 1. Procesamiento (asegúrate de que los nombres aquí coincidan con los de abajo)
tabla_top_30 <- df %>%
  group_by(BAR_COD) %>%
  summarise(
    Total_Avisos = n(),
    Total_Veg_Tupida = sum(`VEG. TUPIDA/TAPIZANTE`, na.rm = TRUE),
    Total_Basura = sum(BASURA, na.rm = TRUE),
    Total_Solar = sum(SOLAR, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Avisos)) %>%
  slice_head(n = 30)

# 2. Creación de la tabla con gt
tabla_final <- tabla_top_30 %>%
  gt() %>%
  tab_header(
    title = md("**Top 30 Barrios con más Avisos (2025)**"),
    subtitle = "Desglose de factores ambientales detectados"
  ) %>%
  # CORRECCIÓN: Los nombres a la izquierda del '=' deben ser los nombres reales del dataframe
  cols_label(
    BAR_COD = "Código Barrio",
    Total_Avisos = "Total Avisos",
    Total_Veg_Tupida = "Veg. Tupida",
    Total_Basura = "Basura",
    Total_Solar = "Solar"
  ) %>%
  gt_theme_538() %>%
  gt_color_rows(Total_Avisos, palette = "Greens") %>%
  tab_options(
    table.font.size = px(14),
    column_labels.font.weight = "bold"
  )

# Ver la tabla
tabla_final


# -------------------------------------
# EVOLUCIÓN MADRIGUERAS HISTÓRICA
# -------------------------------------

df_porcentaje <- df %>%
  # Convertir FECHA a formato Date y extraer el año
  # (Ajustar el formato "d/m/Y" según venga en tu Excel/CSV)
  mutate(FECHA = dmy(FECHA),
         ANIO = year(FECHA)) %>%
  # Filtrar por el periodo solicitado
  filter(ANIO >= 2018 & ANIO <= 2025) %>%
  # Filtrar solo registros de RATAS (como en tu script original)
  filter(str_starts(VECTOR_ENCONTRADO, "RAT")) %>%    #cambiamos aquí por RATA o lo que hay
  # Unificar criterio de MADRIGUERA (1 y 2 se consideran presencia)
  mutate(MADRIGUERA = if_else(MADRIGUERA %in% c(1, 2), 1, 0)) %>%
  # Agrupar por año y calcular el porcentaje
  group_by(ANIO) %>%
  summarise(
    total_avisos = n(),
    con_madriguera = sum(MADRIGUERA == 1, na.rm = TRUE),
    porcentaje = (con_madriguera / total_avisos) * 100
  )

# 3. Crear el gráfico
ggplot(df_porcentaje, aes(x = factor(ANIO), y = porcentaje, fill = porcentaje)) +
  geom_col(show.legend = FALSE, color = "white", width = 0.7) +
  # Añadir etiquetas de porcentaje sobre las barras
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            vjust = -0.5, fontface = "bold", size = 4) +
  # Escala de colores premium (azul)
  scale_fill_gradient(low = "#AED6F1", high = "#2E86C1") +
  # Estética del tema
  theme_minimal() +
  labs(
    title = "Porcentaje de Avisos con Presencia de Madriguera (2018 - 2025)",
    x = "Año",
    y = "Porcentaje (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )
