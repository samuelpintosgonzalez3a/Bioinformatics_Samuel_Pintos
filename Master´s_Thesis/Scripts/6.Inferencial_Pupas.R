

####################
####################
##  MANN-WHITNEY  ##
####################
####################



# 1. Cargar librerías necesarias (si no están ya)
library(tidyverse)
library(ggpubr)
library(broom)

# 2. Definir las variables climáticas a analizar
vars_clima <- c("temperatura_media", "precipitación", "humedad", "temperatura_máxima", "temperatura_mínima", 
                "nubosidad", "rachas_de_viento", "velocidad_del_viento", "fotoperiodo", "radiación_solar")

# 3. Función para ejecutar Mann-Whitney dicotomizando por la mediana
ejecutar_mw <- function(var_nombre, datos) {
  # Creamos una copia temporal para no alterar el df original
  temp_df <- df %>%
    select(densidad_pupal_total, all_of(var_nombre)) %>%
    drop_na()
  
  # Dicotomizamos la variable climática por su mediana
  mediana_val <- median(temp_df[[var_nombre]], na.rm = TRUE)
  temp_df$Grupo <- ifelse(temp_df[[var_nombre]] > mediana_val, "Alto", "Bajo")
  
  # Ejecutamos el test de Wilcoxon (Mann-Whitney)
  test <- wilcox.test(densidad_pupal_total ~ Grupo, data = temp_df)
  
  # Extraemos medianas de cada grupo para la tabla
  medianas <- temp_df %>%
    group_by(Grupo) %>%
    summarise(med = median(densidad_pupal_total, na.rm = TRUE))
  
  # Retornamos los resultados en una fila de data.frame
  data.frame(
    Variable_Climatica = var_nombre,
    Mediana_Grupo_Bajo = round(medianas$med[medianas$Grupo == "Bajo"], 2),
    Mediana_Grupo_Alto = round(medianas$med[medianas$Grupo == "Alto"], 2),
    Estadistico_W = round(test$statistic, 2),
    p_valor = test$p.value
  )
}

# 4. Iterar sobre todas las variables y crear la tabla de resultados
resultados_mw <- map_df(vars_clima, ~ejecutar_mw(.x, df)) %>%
  mutate(
    Significancia = case_when(
      p_valor < 0.001 ~ "***",
      p_valor < 0.01  ~ "**",
      p_valor < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    p_valor = format.pval(p_valor, digits = 3)
  )

# 5. CREAR LA TABLA ESTÉTICA
tabla_estetica2 <- ggtexttable(resultados_mw, 
                               rows = NULL, 
                               theme = ttheme(
                                 colnames.style = colnames_style(fill = "#2C3E50", color = "white", face = "bold"),
                                 tbody.style = tbody_style(fill = c("#F8F9F9", "#EBF5FB"))
                               )) %>%
  tab_add_title(text = "Test de Mann-Whitney: Densidad de Pupas vs Clima", 
                face = "bold", size = 14) %>%
  tab_add_footnote(text = "Nota: Variables dicotomizadas por la mediana. *p < 0.05, **p < 0.01, ***p < 0.001.", 
                   size = 10)

# 6. Visualizar y guardar
print(tabla_estetica2)



library(tidyverse)

# 1. Definir variables
vars_clima <- c("temperatura_media", "precipitación", "humedad", "temperatura_máxima", 
                "temperatura_mínima", "nubosidad", "rachas_de_viento", 
                "velocidad_del_viento", "fotoperiodo", "radiación_solar")

# ==============================================================================
# OPCIÓN A: MANN-WHITNEY (Corregido)
# ==============================================================================
cat("\n--- RESULTADOS MANN-WHITNEY (División por Mediana) ---\n")

resultados_mw <- map_dfr(vars_clima, function(var) {
  # Filtramos NAs
  temp_df <- df %>% select(densidad_pupal_total, all_of(var)) %>% drop_na()
  
  # Usamos >= para evitar que el grupo "Alto" se quede vacío si hay muchos empates (ej. lluvias)
  mediana_val <- median(temp_df[[var]], na.rm = TRUE)
  temp_df$Grupo <- ifelse(temp_df[[var]] >= mediana_val, "Alto", "Bajo")
  
  # Si por algún motivo todos los valores son iguales, nos saltamos el test para evitar error
  if(length(unique(temp_df$Grupo)) < 2) {
    return(data.frame(Variable = var, p_valor = NA, Significativo = "Error: Sin varianza"))
  }
  
  # exact = FALSE evita que R colapse cuando hay muchos valores repetidos (ceros en pupas)
  test <- wilcox.test(densidad_pupal_total ~ Grupo, data = temp_df, exact = FALSE)
  
  data.frame(
    Variable = var,
    p_valor = round(test$p.value, 4),
    Significativo = ifelse(test$p.value < 0.05, "SÍ", "No")
  )
})

print(resultados_mw)


# ==============================================================================
# CORRELACIÓN DE SPEARMAN 
# ==============================================================================
cat("\n--- RESULTADOS SPEARMAN (Continua vs Continua) ---\n")

resultados_spearman <- map_dfr(vars_clima, function(var) {
  temp_df <- df %>% select(densidad_pupal_total, all_of(var)) %>% drop_na()
  
  # Calculamos rho de Spearman (no asume normalidad, perfecto para conteos con ceros)
  test <- cor.test(temp_df$densidad_pupal_total, temp_df[[var]], method = "spearman", exact = FALSE)
  
  data.frame(
    Variable = var,
    Rho_Fuerza = round(test$estimate, 3), # +1 positiva, -1 negativa, 0 nada
    p_valor = round(test$p.value, 4),
    Significativo = ifelse(test$p.value < 0.05, "SÍ", "No")
  )
})

# Ordenamos por las más significativas
print(resultados_spearman %>% arrange(p_valor))



################
################
#####  VIF #####
################
################


library(car)
library(dplyr)
library(ggpubr)

modelo_vif_completo <- lm(log1p(densidad_pupal_total) ~ temperatura_media + precipitación + 
                            nubosidad + rachas_de_viento +
                            + fotoperiodo , 
                          data = df)

# 3. CALCULAR VIF
vif_res <- vif(modelo_vif_completo)

vif_res


################
################
### Elastic ####
################
################

library(tidyverse)
library(caret)
library(glmnet)
library(googledrive)

# 1. Asegurar formato Date en df y filtrar NAs en la variable objetivo
df_limpio <- df %>% 
  filter(!is.na(densidad_pupal_total)) %>% 
  mutate(FECHA = as.Date(as.character(FECHA)))

fechas_muestreo <- sort(unique(df_limpio$FECHA))

# 2. Filtrado y selección de variables predictoras
df_modelo_reducido <- df_limpio %>%
  select(densidad_pupal_total, precipitación, nubosidad, 
         temperatura_media, rachas_de_viento, Tratamiento_Num) %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# 3. Generar la matriz matemática 
dummies <- dummyVars(densidad_pupal_total ~ ., data = df_modelo_reducido)
x_total <- predict(dummies, newdata = df_modelo_reducido)
y_total <- df_modelo_reducido$densidad_pupal_total

# 4. División en Train (80%) y Test (20%)
set.seed(123)
train_index <- createDataPartition(y_total, p = 0.8, list = FALSE)

x_train <- x_total[train_index, , drop = FALSE]
x_test  <- x_total[-train_index, , drop = FALSE]
y_train <- y_total[train_index]
y_test  <- y_total[-train_index]

# 5. Entrenamiento del modelo Elastic Net
control <- trainControl(method = "cv", number = 5) 

set.seed(1234)
enet_model <- train(x_train, y_train, 
                    method = "glmnet", 
                    trControl = control, 
                    preProcess = c("center", "scale"), 
                    tuneLength = 10)

# Ver el resumen del mejor modelo
cat("\n--- RESUMEN DEL MEJOR MODELO ---\n")
print(enet_model)

# 6. Extraer la importancia de las variables
importancia <- varImp(enet_model, scale = TRUE)
print(importancia)

# Mostrar el gráfico en la consola de RStudio
plot(importancia, main = "Importancia de los Predictores en el Elastic Net para Densidad Pupal Total")

# 7. Predicciones y Métricas
predictions <- predict(enet_model, x_test)

metrica_r2 <- R2(predictions, y_test)
metrica_rmse <- RMSE(predictions, y_test)

# Resultados
cat("\n--- RESULTADOS DEL MODELO ELASTIC NET EN TEST ---\n")
cat("R2:", round(metrica_r2, 4), "\n")
cat("RMSE:", round(metrica_rmse, 4), "\n")




#############################
#############################
######## MIXED MODEL ########
#############################
#############################

## creamos pupas_totales ##

df <- df %>%
  mutate(
    # rowSums suma los valores por fila de las columnas seleccionadas
    pupas_totales = rowSums(select(., starts_with("S."), VACÍAS), na.rm = TRUE)
  )


# Cargar librerías necesarias
library(lme4)
library(dplyr)
library(car)
library(purrr)

# -------------------------------------------------------------------------
# 1. PREPARACIÓN DE DATOS PARA PUPAS
# -------------------------------------------------------------------------
df_limpio_pupas <- df %>% 
  # Filtramos NAs usando la variable de pupas
  filter(!is.na(pupas_totales)) %>% 
  # CRÍTICO: Filtramos las muestras sin sustrato (PESO_g == 0)
  filter(PESO_g > 0) %>% 
  mutate(FECHA = as.Date(as.character(FECHA)))

# Dataset estricto
df_mixto_estricto_pupas <- df_limpio_pupas %>%
  mutate(dias_estudio = as.numeric(FECHA - min(FECHA, na.rm = TRUE))) %>%
  mutate(Localización = as.factor(Localización)) %>%
  # Estandarizamos para evitar problemas de convergencia
  mutate(across(c(dias_estudio, temperatura_media, nubosidad, 
                  rachas_de_viento, precipitación, fotoperiodo), 
                ~ as.vector(scale(.))))

# -------------------------------------------------------------------------
# 2. MODELO ESTRICTO: Eficacia Tratamiento (CONTEOS)
# -------------------------------------------------------------------------
modelo_nb_limpio_pupas <- glmer.nb(pupas_totales ~ Tratamiento_Num + 
                                     (1 | Localización), 
                                   data = df_mixto_estricto_pupas)

cat("\n--- MODELO ESTRICTO: PUPAS TOTALES ---\n")
summary(modelo_nb_limpio_pupas)
Anova(modelo_nb_limpio_pupas)
confint(modelo_nb_limpio_pupas, method = "Wald")

# -------------------------------------------------------------------------
# 3. MODELO COMPLETO: Dinámica temporal y climática (CONTEOS)
# -------------------------------------------------------------------------
df_mixto_completo_pupas <- df_limpio_pupas %>%
  mutate(Localización = as.factor(Localización)) %>%
  mutate(across(c(temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo), 
                ~ as.vector(scale(.))))

modelo_nb_completo_pupas <- glmer.nb(pupas_totales ~ Tratamiento_Num + temperatura_media + 
                                       nubosidad + rachas_de_viento + precipitación + fotoperiodo + 
                                       (1 | Localización), 
                                     data = df_mixto_completo_pupas)

cat("\n--- MODELO COMPLETO CLIMA: PUPAS TOTALES ---\n")
summary(modelo_nb_completo_pupas)
Anova(modelo_nb_completo_pupas)
confint(modelo_nb_completo_pupas, method = "Wald")

# -------------------------------------------------------------------------
# 4. MODELOS PARA DENSIDAD PUPAL
# -------------------------------------------------------------------------

# Modelo Estricto - Densidad (Solo Tratamiento)
modelo_tweedie_estricto <- glmmTMB(
  densidad_pupal_total ~ Tratamiento_Num + (1 | Localización), 
  data = df_mixto_completo_pupas,
  family = tweedie(link = "log")
)

cat("\n--- TWEEDIE ESTRICTO: DENSIDAD PUPAL ---\n")
summary(modelo_tweedie_estricto)
Anova(modelo_tweedie_estricto, type = "II")
confint(modelo_tweedie_estricto, method = "Wald")


# Modelo Completo - Densidad (Tratamiento + Clima)
modelo_tweedie_completo <- glmmTMB(
  densidad_pupal_total ~ Tratamiento_Num + temperatura_media + 
    nubosidad + rachas_de_viento + precipitación + fotoperiodo + 
    (1 | Localización),  
  data = df_mixto_completo_pupas,
  family = tweedie(link = "log")
)

cat("\n--- TWEEDIE COMPLETO CLIMA: DENSIDAD PUPAL ---\n")
summary(modelo_tweedie_completo)
Anova(modelo_tweedie_completo, type = "II")
confint(modelo_tweedie_completo, method = "Wald")

##############################
#MIXED MODEL POR LOCALIZACIÓN
##############################


library(tidyverse)
library(glmmTMB)

# 1. Crear una lista vacía para guardar los resultados
resultados_clima_loc <- list()

# 2. Obtener los nombres de todas las localizaciones
localizaciones <- unique(df_mixto_completo_pupas$Localización)

# 3. Bucle para iterar sobre cada río/tramo
for(loc in localizaciones) {
  
  # Filtramos los datos solo para esta localización
  df_sub <- df_mixto_completo_pupas %>% filter(Localización == loc)
  
  # Usamos tryCatch para que, si un tramo falla (ej. exceso de ceros), no pare el código
  tryCatch({
    # Ejecutamos el modelo Tweedie
    modelo_sub <- glmmTMB(
      densidad_pupal_total ~ temperatura_media + Tratamiento_Num + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
      data = df_sub,
      family = tweedie(link = "log")
    )
    
    # Extraemos la tabla de coeficientes y p-valores (quitando el intercepto)
    res <- as.data.frame(summary(modelo_sub)$coefficients$cond) %>%
      rownames_to_column("Variable") %>%
      filter(Variable != "(Intercept)") %>%
      mutate(
        Localización = loc,
        # CÁLCULO DEL IC95% WALD (escala log)
        IC95_inf = Estimate - 1.96 * `Std. Error`,
        IC95_sup = Estimate + 1.96 * `Std. Error`
      )
    
    # Guardamos en la lista
    resultados_clima_loc[[loc]] <- res
    
  }, error = function(e) {
    # Si da error, nos avisa pero sigue con la siguiente localización
    message(paste("Atención: El modelo falló en", loc, "->", e$message))
  })
}

# 4. Unir todos los resultados en una única tabla bonita y fácil de leer
tabla_clima_localizaciones <- bind_rows(resultados_clima_loc) %>%
  rename(
    Coeficiente = Estimate,
    Z_val = `z value`,
    p_valor = `Pr(>|z|)`
  ) %>%
  mutate(
    # Redondeamos los números largos
    Coeficiente = round(Coeficiente, 3),
    IC95_inf = round(IC95_inf, 3),
    IC95_sup = round(IC95_sup, 3),
    p_valor = round(p_valor, 4),
    
    # Creamos una columna unificada para el IC que sea fácil de leer
    `IC95%_Wald(log)` = paste0("[", IC95_inf, " ; ", IC95_sup, "]"),
    
    # Añadimos las clásicas estrellitas de significancia
    Significancia = case_when(
      p_valor < 0.001 ~ "***",
      p_valor < 0.01  ~ "**",
      p_valor < 0.05  ~ "*",
      p_valor < 0.1   ~ ".", # Rozando la significancia (tendencia)
      TRUE            ~ "ns"
    )
  )
# 5. Imprimir la tabla final
cat("\n--- IMPACTO DEL CLIMA EN PUPAS POR LOCALIZACIÓN ---\n")
print(as.data.frame(tabla_clima_localizaciones))


#############################
#############################
####### DECISION TREE #######
#############################
#############################



library(rpart)
library(rpart.plot)
library(dplyr)

# 1. Preparar el dataframe incluyendo TODAS las variables que quieres ahora
df_arbol_pupas <- df %>%
  # AÑADIMOS precipitación y temperatura_media al select
  select(pupas_totales, nubosidad, rachas_de_viento, fotoperiodo, 
         Tratamiento_Num, precipitación, temperatura_media)

# 2. Entrenar el algoritmo con la fórmula corregida (todo con '+')
arbol_modelo_pupas <- rpart(
  pupas_totales ~ nubosidad + rachas_de_viento + fotoperiodo + 
    Tratamiento_Num + precipitación + temperatura_media,
  data = df_arbol_pupas,
  method = "anova",
  control = rpart.control(cp = 0.01, minsplit = 10) 
)

# 3. Dibujar el árbol
rpart.plot(
  arbol_modelo_pupas,
  type = 3,
  extra = 1,
  under = TRUE,
  faclen = 0,
  cex = 0.8,
  main = "Predicción de Abundancia Pupal (Árbol de Decisión)",
  box.palette = "Blues"
)

# 4. Ver la importancia real de cada variable
print(arbol_modelo_pupas$variable.importance)