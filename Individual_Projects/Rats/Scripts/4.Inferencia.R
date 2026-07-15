# ------------------------
# KOLMOGOROV - SMIRNOV
# ------------------------


cols_to_exclude <- c("REF_UTCV", "FECHA", "Y", "CodHex", "BAR_COD", "DIS_COD", 
                     "VECTOR_ENTRADA", "VECTOR_ENCONTRADO", "CONTACTO CON SOLICITANTE")

df_preparado <- df%>%
  group_by(FECHA) %>%
  summarise(
    Total_Avisos = n(),
    # Calculamos la media (proporción) de las intervenciones
    across(-any_of(cols_to_exclude), \(x) mean(as.numeric(x), na.rm = TRUE)),
    .groups = 'drop'
  )

ks_resultado <- ks.test(df_preparado$Total_Avisos, "pnorm", 
                        mean = mean(df_preparado$Total_Avisos, na.rm = TRUE), 
                        sd = sd(df_preparado$Total_Avisos, na.rm = TRUE))

# 2. Ver el resultado
print(ks_resultado)


# ------------------------
# KRUSKAL - WALLIS
# ------------------------


df_clean <- df %>%
  filter(!is.na(DIS_COD)) %>%
  mutate(DIS_COD = as.factor(DIS_COD))

variables_estudio <- names(df_preparado) %>% 
  setdiff(c("FECHA", "DIS_COD", "Total_Avisos", "REF_UTCV", cols_to_exclude))

# 3. Ejecutar el análisis (usamos lm para variables continuas/proporciones)
library(tidyverse)
library(broom)
library(ggpubr)

# 1. Ejecutar Kruskal-Wallis para cada variable de forma automática
resultados_kruskal <- variables_estudio %>%
  map_df(function(var) {
    
    # 1. Extraer la columna para analizarla
    columna <- df_preparado[[var]]
    
    # 2. Contar cuántos valores NO son NA
    datos_presentes <- sum(!is.na(columna))
    
    # 3. Solo proceder si tenemos al menos 2 datos y hay variación (sd > 0)
    # Añadimos is.na(sd(...)) para evitar el error que te salió
    if(datos_presentes > 1) {
      desv_est <- sd(columna, na.rm = TRUE)
      
      if(!is.na(desv_est) && desv_est > 0) {
        
        # Ejecutar el test
        res <- try(kruskal.test(as.formula(paste("Total_Avisos ~ factor(", var, ")")), 
                                data = df_preparado), silent = TRUE)
        
        if (!inherits(res, "try-error")) {
          return(tidy(res) %>%
                   mutate(Variable = var) %>%
                   dplyr::select(Variable, p.value))
        }
      }
    }
    return(NULL) # Si no cumple las condiciones, devuelve vacío
  })

# 2. Validar, Formatear y Ordenar
tabla_final <- resultados_kruskal %>%
  mutate(
    `P-Valor` = format.pval(p.value, digits = 3, eps = 0.001),
    Significativo = ifelse(p.value < 0.05, "SÍ", "no")
  ) %>%
  arrange(p.value) %>% # Los más significativos arriba
  select(Variable, `P-Valor`, Significativo)

# 3. Plottear la tabla visual
p <- ggtexttable(tabla_final, rows = NULL, theme = ttheme("minimal"))

p <- tab_add_title(p,
                   text = "K-S: Variables vs Total de Avisos", 
                   face = "bold", 
                   size = 14,
                   padding = unit(1.5, "line"))

p



# 1. Preparar los datos de la tabla con etiquetas más limpias
tabla_estetica <- tabla_final %>%
  mutate(
    # Limpiamos los nombres de las variables (quitar guiones, poner mayúsculas)
    Variable = str_to_title(str_replace_all(Variable, "_", " ")),
    # Añadimos un indicador visual (estrella) a las significativas
    Significativo = ifelse(Significativo == "SÍ", "★ SÍ", "no")
  )

# 2. Definir un tema personalizado para la tabla
mi_tema <- ttheme(
  colnames.style = colnames_style(fill = "#2C3E50", color = "white", face = "bold", size = 12),
  tbody.style = tbody_style(
    fill = c("#F2F4F4", "#FFFFFF"), # Colores alternos para las filas
    color = "#2C3E50",
    size = 10
  )
)

# 3. Crear el objeto gráfico de la tabla
p <- ggtexttable(tabla_estetica, 
                 rows = NULL, 
                 theme = mi_tema)

# 4. Añadir títulos y formato adicional
p <- p %>%
  tab_add_title(text = "Inferencia Estadística", 
                face = "bold", size = 16, color = "#2C3E50") %>%
  tab_add_title(text = "Test K-Wallis: Avisos ratas 2025", 
                face = "italic", size = 10, color = "#566573", padding = unit(1, "line")) %>%
  tab_add_footnote(text = "* Nivel de significancia alpha = 0.05", 
                   size = 8, face = "italic")

# 5. Dibujar líneas de separación
p <- tab_add_hline(p, at.row = 1:2, row.side = "top", linewidth = 3, linetype = 1)

# Mostrar tabla
print(p)


# ------------------------
# SPEARMAN
# ------------------------

# 1. Calcular Correlación de Spearman, R2 y P-valor
resultados_spearman <- variables_estudio %>%
  map_df(function(var) {
    # Extraemos vectores y quitamos NAs
    x <- as.numeric(df_preparado[[var]])
    y <- as.numeric(df_preparado$Total_Avisos)
    
    validos <- !is.na(x) & !is.na(y)
    
    # Solo si hay varianza en ambos vectores
    if(sum(validos) > 2 && sd(x[validos]) > 0 && sd(y[validos]) > 0) {
      test <- cor.test(x[validos], y[validos], method = "spearman", exact = FALSE)
      
      data.frame(
        Variable = str_to_title(str_replace_all(var, "_", " ")),
        R = as.numeric(test$estimate),
        R2 = as.numeric(test$estimate)^2,
        p_value = as.numeric(test$p.value)
      )
    }
  })

# 2. Preparar la tabla para visualizar
tabla_final <- resultados_spearman %>%
  arrange(p_value) %>%
  mutate(
    R = round(R, 3),
    R2 = round(R2, 3),
    `P-Valor` = format.pval(p_value, digits = 3, eps = 0.001),
    Significativo = ifelse(p_value < 0.05, "SÍ", "no")
  ) %>%
  select(Variable, R, R2, `P-Valor`, Significativo)

# 3. Crear vector de colores para resaltar filas significativas
# Si es significativo (< 0.05), usamos un verde suave; si no, blanco.
colores_filas <- ifelse(tabla_final$Significativo == "SÍ", "#D5F5E3", "#FFFFFF")

# 4. Configurar el tema estético
mi_tema <- ttheme(
  colnames.style = colnames_style(fill = "#2E4053", color = "white", face = "bold"),
  tbody.style = tbody_style(
    fill = colores_filas, # Aplicamos el resaltado condicional aquí
    color = "#2E4053",
    size = 10
  )
)

# 5. Generar y maquetar la tabla
p <- ggtexttable(tabla_final, rows = NULL, theme = mi_tema) %>%
  tab_add_title(text = "Análisis de Correlación de Spearman", 
                face = "bold", size = 16, padding = unit(1, "line")) %>%
  tab_add_title(text = "Variables técnicas vs Volumen total de avisos ratas 2025", 
                face = "italic", size = 11, color = "grey40") %>%
  tab_add_footnote(text = "Filas en verde indican relación estadísticamente significativa (p < 0.05)", 
                   size = 9, face = "italic")

# Visualizar
print(p)


# ------------------------
# BINOMIAL NEGATIVA
# ------------------------

library(MASS)

# 1. Ajustar el Modelo Binomial Negativa

modelo_nb <- glm.nb(Total_Avisos ~ BASURA + SOLAR + `INMUEBLE_ABANDONADO` +
                      `DEFECTOS ESTRUCTURALES` + `HUERTO URBANO` + `ARQUETA SIN TAPA/O ROTA` 
                    +`ALIMENTACION GATOS`+`ALIMENTACION PALOMAS`+ `PLATAFORMAS` + 
                      `ARQUETA SIN TAPA/O ROTA` + `INMUEBLE OCUPADO` +
                      OBRAS + PSH + `VEG. TUPIDA/TAPIZANTE` + `ALIMENTACION GENERAL/RESTOS COMIDA`,
                    data = df_preparado)

coeficientes_tabla <- tidy(modelo_nb, conf.int = TRUE, exponentiate = FALSE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # El IRR es el exponencial del coeficiente (estimate)
    IRR = exp(estimate),
    # Formateamos para la tabla
    beta = round(estimate, 3),
    IRR = round(IRR, 3),
    p_valor_raw = p.value,
    p_valor_txt = format.pval(p.value, digits = 3, eps = 0.001),
    Significativo = ifelse(p.value < 0.05, "SÍ", "no"),
    Variable = str_to_title(str_replace_all(term, "_", " "))
  ) %>%
  # FUERZA EL SELECT DE DPLYR AQUÍ:
  dplyr::select(Variable, beta, IRR, `P-Valor` = p_valor_txt, Significativo)

# 3. Generar la tabla con colores según el signo de Beta
# Verde para impacto positivo en avisos, rojo para impacto negativo
colores_filas <- ifelse(coeficientes_tabla$beta > 0, "red", "green")

p_beta <- ggtexttable(coeficientes_tabla, rows = NULL, 
                      theme = ttheme(
                        colnames.style = colnames_style(fill = "#21618C", color = "white"),
                        tbody.style = tbody_style(fill = colores_filas)
                      )) %>%
  tab_add_title(text = "Coeficientes de Regresión Binomial Negativa", 
                face = "bold", size = 14) %>%
  tab_add_footnote(text = "IRR > 1: Aumenta avisos | IRR < 1: Disminuye avisos", 
                   face = "italic", size = 9)

print(p_beta)



# ------------------------
# ÁRBOL DE DECISIÓN
# ------------------------


library(tidyverse)
library(rpart)
library(rpart.plot)

# 1. Preparación de datos (aseguramos que son enteros para Poisson)
df_arbol_poisson <- df_preparado %>%
  dplyr::select(Total_Avisos, SOLAR, BASURA, INDUCIDOS, Promedio.de.tmed) %>%
  mutate(Total_Avisos = as.integer(round(Total_Avisos))) %>% # Poisson requiere enteros
  drop_na()

# 2. Entrenar el modelo con method = "poisson"
modelo_poisson_arbol <- rpart(
  Total_Avisos ~ SOLAR + BASURA + INDUCIDOS + Promedio.de.tmed, 
  data = df_arbol_poisson, 
  method = "poisson", # <--- CAMBIO CLAVE
  control = rpart.control(minsplit = 10, cp = 0.002)
)

# 3. Visualización
# En el método Poisson, 'extra = 1' muestra la tasa de eventos en el nodo
prp(modelo_poisson_arbol, 
    type = 5, 
    extra = 1, 
    under = TRUE, 
    box.palette = "Oranges", 
    cex = 0.6,
    cex.main = 1.5,
    nn.cex = 1.0,
    border.col = "black",
    compress = TRUE,
    ycompress = TRUE,
    varlen = 0,         
    faclen = 0,          
    main = "Árbol de Regresión de Poisson: Avisos de Ratas",
    sub = "Predicción basada en factores de riesgo y temperatura")

# 4. Importancia de variables
print(modelo_poisson_arbol$variable.importance)
