
#### SELECCIÓN DE VARIABLES CLIMÁTICAS PARA LA INFERENCIA ####


############################
# MANN-WHITNEY Y SPEARMAN ##
############################


library(tidyverse)

# 1. Definir variables
vars_clima <- c("temperatura_media", "precipitación", "humedad", "temperatura_máxima", 
                "temperatura_mínima", "nubosidad", "rachas_de_viento", 
                "velocidad_del_viento", "fotoperiodo", "radiación_solar")

# ==============================================================================
# MANN-WHITNEY  - LARVAS
# ==============================================================================
cat("\n--- RESULTADOS MANN-WHITNEY: DENSIDAD LARVARIA TOTAL ---\n")

resultados_mw_larvas <- map_dfr(vars_clima, function(var) {
  # Filtramos NAs usando la variable de larvas
  temp_df <- df %>% select(DENSIDAD_LARVARIA_TOTAL, all_of(var)) %>% drop_na()
  
  # Dicotomizamos
  mediana_val <- median(temp_df[[var]], na.rm = TRUE)
  temp_df$Grupo <- ifelse(temp_df[[var]] >= mediana_val, "Alto", "Bajo")
  
  # Control de errores si no hay varianza
  if(length(unique(temp_df$Grupo)) < 2) {
    return(data.frame(Variable = var, p_valor = NA, Significativo = "Error: Sin varianza"))
  }
  
  # Test de Mann-Whitney (Wilcoxon independiente)
  test <- wilcox.test(DENSIDAD_LARVARIA_TOTAL ~ Grupo, data = temp_df, exact = FALSE)
  
  data.frame(
    Variable = var,
    p_valor = round(test$p.value, 4),
    Significativo = ifelse(test$p.value < 0.05, "SÍ", "No")
  )
})

print(resultados_mw_larvas)


# ==============================================================================
# CORRELACIÓN DE SPEARMAN - LARVAS
# ==============================================================================
cat("\n--- RESULTADOS SPEARMAN: DENSIDAD LARVARIA TOTAL ---\n")

resultados_spearman_larvas <- map_dfr(vars_clima, function(var) {
  temp_df <- df %>% select(DENSIDAD_LARVARIA_TOTAL, all_of(var)) %>% drop_na()
  
  # Correlación de Spearman
  test <- cor.test(temp_df$DENSIDAD_LARVARIA_TOTAL, temp_df[[var]], method = "spearman", exact = FALSE)
  
  data.frame(
    Variable = var,
    Rho_Fuerza = round(test$estimate, 3), # +1 positiva, -1 negativa, 0 nada
    p_valor = round(test$p.value, 4),
    Significativo = ifelse(test$p.value < 0.05, "SÍ", "No")
  )
})

# Ordenamos por las más significativas para verlo más claro
print(resultados_spearman_larvas %>% arrange(p_valor))


#############################
#############################
#    MATRIZ CORRELACIONES  #
#############################
#############################

df[df == 0] <- NA

# 1. Crear la matriz solo con datos numéricos
matriz_cor <- df %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs", method = "spearman")

# 2. Visualizar (opcional, usando la librería corrplot si la tienes)
# install.packages("corrplot")
library(corrplot)
corrplot(matriz_cor, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.4, tl.srt = 60)


# 1. Cargar librerías
library(Hmisc)

# 1. Limpieza profunda antes de calcular
df_limpio <- df %>%
  select(where(is.numeric)) %>%
  # Eliminamos columnas que tengan TODO NAs
  select(where(~!all(is.na(.)))) %>%
  # Eliminamos columnas con varianza cero (constantes) para evitar NAs en la correlación
  select(where(~sd(., na.rm = TRUE) > 0))

# 2. Calcular correlación y p-valores
# Usamos "pairwise.complete.obs" para aprovechar el máximo de datos
res <- rcorr(as.matrix(df_limpio), type = "spearman")

# 3. SUSTITUIR NAs en la matriz de correlación (Truco para que hclust no falle)
# Si queda algún NA puntual, lo ponemos a 0 para que el gráfico cargue
res$r[is.na(res$r)] <- 0
res$P[is.na(res$P)] <- 1 # Si no hay dato, el p-valor es 1 (no significativo)


# Seleccionamos solo lo que realmente influye
df_top <- df %>% select(temperatura_media, humedad, precipitación, 
      rachas_de_viento, velocidad_del_viento, nubosidad, temperatura_máxima, temperatura_mínima, fotoperiodo)

# Calculamos la correlación solo de estas
res_top <- rcorr(as.matrix(df_top))

library(corrplot)

# 1. INVERTIR LA PALETA DE COLORES
# Creamos un gradiente: Azul (-1) -> Blanco (0) -> Rojo (+1)
paleta_intuitiva <- colorRampPalette(c("#4575b4", "white", "#d73027"))(200)

# 2. DIBUJAR EL GRÁFICO BASE (Sin números ni asteriscos nativos)
# Lo guardamos en el objeto 'cp' para que R nos devuelva las coordenadas de cada celda dibujada
cp <- corrplot(res_top$r, 
               method = "color", 
               type = "upper", 
               col = paleta_intuitiva, # Aplicamos la nueva paleta
               tl.col = "black", 
               tl.srt = 45, 
               diag = FALSE)

# 3. CREAR Y COLOCAR EL TEXTO COMBINADO (Número + Asterisco lado a lado)
# Recorremos cada cuadrado que ha dibujado corrplot mediante sus coordenadas
for(k in 1:nrow(cp$corrPos)) {
  
  # Obtenemos el nombre de la fila y columna de esa celda específica
  fila <- cp$corrPos$yName[k]
  columna <- cp$corrPos$xName[k]
  
  # Extraemos la correlación y el p-valor exacto
  r_val <- res_top$r[fila, columna]
  p_val <- res_top$P[fila, columna]
  
  # Formateamos el número a 2 decimales para que todos midan lo mismo
  texto_numero <- sprintf("%.2f", r_val)
  
  # Determinamos el nivel de significancia para añadir 1, 2 o 3 asteriscos
  asteriscos <- ""
  if (!is.na(p_val)) {
    if (p_val < 0.05) { asteriscos <- "*" }
  }
  
  # Unimos el número y el asterisco de forma lineal
  # El resultado será una única "palabra" (ej: "0.46**"), impidiendo que se superpongan
  texto_final <- paste0(texto_numero, asteriscos)
  
  # Imprimimos el texto final en las coordenadas exactas de esa celda
  text(cp$corrPos$x[k], cp$corrPos$y[k], 
       labels = texto_final, 
       cex = 0.8, 
       col = "black")
}



#############################
#############################
########     VIF     ########
#############################
#############################





# 1. CARGAR LIBRERÍAS
library(car)
library(dplyr)
library(ggpubr)

# 2. DEFINIR EL MODELO LINEAL
# Incluimos todas las variables atmosféricas para la evaluación inicial
# Usamos log1p en la variable dependiente para mejorar la linealidad
modelo_vif_completo <- lm(log1p(DENSIDAD_LARVARIA_TOTAL) ~ temperatura_media + precipitación + 
                            nubosidad + rachas_de_viento + fotoperiodo , 
                          data = df)

# 3. CALCULAR VIF
vif_res <- vif(modelo_vif_completo)

# 4. ORGANIZAR RESULTADOS EN UN DATAFRAME
tabla_vif_data <- data.frame(
  Variable = names(vif_res),
  VIF = as.numeric(vif_res)
) %>%
  mutate(Estado = case_when(
    VIF > 10 ~ "Crítico (Eliminar)",
    VIF > 5  ~ "Alto (Precaución)",
    TRUE     ~ "Aceptable"
  )) %>%
  arrange(desc(VIF))



##############################
##############################
######## ESTANDARIZAR ########
##############################
##############################


cols_a_estandarizar <- df %>% 
  select(where(is.numeric)) %>% 
  names()

# 2. Aplicar la estandarización
# scale() devuelve una matriz, por eso usamos as.data.frame()
df_std <- df %>%
  mutate(across(all_of(cols_a_estandarizar), ~ as.vector(scale(.x))))



#############################
#############################
######## ELASTIC NET########
#############################
#############################



library(tidyverse)
library(caret)
library(glmnet)


# Asegurar formato Date en df
df_limpio <- df %>% 
  filter(!is.na(DENSIDAD_LARVARIA_TOTAL)) %>% 
  mutate(FECHA = as.Date(as.character(FECHA)))

fechas_muestreo <- sort(unique(df_limpio$FECHA))


# 3. Filtrado y creación de variable única "Tratamiento" (0 = PRE, 1 = POST)
df_modelo_reducido <- df_limpio %>%
  select(DENSIDAD_LARVARIA_TOTAL, precipitación, nubosidad, 
         temperatura_media, rachas_de_viento, Tratamiento_Num) %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# 4. Generar la matriz matemática 
# Como "Tratamiento" ya es numérica, no se dividirá en varias columnas
dummies <- dummyVars(DENSIDAD_LARVARIA_TOTAL ~ ., data = df_modelo_reducido)
x_total <- predict(dummies, newdata = df_modelo_reducido)
y_total <- df_modelo_reducido$DENSIDAD_LARVARIA_TOTAL

# 5. División en Train (80%) y Test (20%)
set.seed(123)
train_index <- createDataPartition(y_total, p = 0.8, list = FALSE)

x_train <- x_total[train_index, , drop = FALSE]
x_test  <- x_total[-train_index, , drop = FALSE]
y_train <- y_total[train_index]
y_test  <- y_total[-train_index]

# 6. Entrenamiento del modelo Elastic Net
control <- trainControl(method = "cv", number = 5) 

set.seed(1234)
enet_model <- train(x_train, y_train, 
                    method = "glmnet", 
                    trControl = control, 
                    preProcess = c("center", "scale"), 
                    tuneLength = 10)

# Ver el resumen del mejor modelo
print(enet_model)

# 7. Extraer la importancia de las variables
importancia <- varImp(enet_model, scale = TRUE)
print(importancia)
plot(importancia, main = "Importancia de los Predictores en el Elastic Net para Densidad Larvaria Total")

# 9. Predicciones y Métricas
# Ahora x_test tendrá exactamente las mismas 13 variables que x_train
predictions <- predict(enet_model, x_test)

metrica_r2 <- R2(predictions, y_test)
metrica_rmse <- RMSE(predictions, y_test)

# 10. Resultados
cat("Resultados del Modelo Elastic Net (Pre vs Post):\n")
cat("R2:", round(metrica_r2, 4), "\n")
cat("RMSE:", round(metrica_rmse, 4), "\n")



#############################
#############################
######## MIXED MODEL ########
#############################
#############################


# Cargar librerías necesarias
library(lme4)
library(dplyr)
library(car)
library(purrr)


df_limpio <- df %>% 
  filter(!is.na(LARVAS_TOTALES)) %>% 
  # CRÍTICO: Si el peso es 0, el logaritmo da -Infinito y el modelo se rompe. 
  # Filtramos las muestras sin sustrato (PESO_g == 0)
  filter(PESO_g > 0) %>% 
  mutate(FECHA = as.Date(as.character(FECHA)))


df_mixto_estricto <- df_limpio %>%
  mutate(dias_estudio = as.numeric(FECHA - min(FECHA, na.rm = TRUE))) %>%
  mutate(Localización = as.factor(Localización)) %>%
  # Estandarizamos para evitar problemas de convergencia
  mutate(across(c(dias_estudio, temperatura_media, nubosidad, 
                  rachas_de_viento, precipitación, fotoperiodo), 
                ~ as.vector(scale(.))))

# -------------------------------------------------------------------------
# 2. MODELO ESTRÍCTO (Eficacia del Tratamiento PRE vs POST)
# -------------------------------------------------------------------------

# Usamos glmer.nb (Generalized Linear Mixed Model con Binomial Negativa)

modelo_nb_limpio <- glmer.nb(LARVAS_TOTALES ~ Tratamiento_Num + 
                               (1 | Localización), 
                             data = df_mixto_estricto)

summary(modelo_nb_limpio)
Anova(modelo_nb_limpio)

confint(modelo_nb_limpio, method = "Wald")
exp(confint(modelo_nb_limpio, method = "Wald"))


# -------------------------------------------------------------------------
# 3. MODELO COMPLETO (Dinámica temporal y climática - Opcional)
# -------------------------------------------------------------------------
# Si quieres correr el modelo general (corrigiendo la colinealidad quitando dias_estudio):

 df_mixto_completo <- df_limpio %>%
 mutate( Localización = as.factor(Localización)) %>%
 mutate(across(c(temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo), ~ as.vector(scale(.))))

modelo_nb_completo <- glmer.nb(LARVAS_TOTALES ~ Tratamiento_Num + temperatura_media + 
                                                                   nubosidad + rachas_de_viento + precipitación + fotoperiodo +
                                                                   + (1 | Localización), 
                                                                 data = df_mixto_completo)

summary(modelo_nb_completo)
Anova(modelo_nb_completo)

confint(modelo_nb_completo, method = "Wald")
exp(confint(modelo_nb_completo, method = "Wald"))


###densidad


modelo_nb_completo2 <- glmer.nb(DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + 
                                 nubosidad + rachas_de_viento + precipitación + fotoperiodo +
                                 + (1 | Localización),  data = df_mixto_completo)
summary(modelo_nb_completo2)
Anova(modelo_nb_completo2)

confint(modelo_nb_completo2, method = "Wald")
exp(confint(modelo_nb_completo2, method = "Wald"))



modelo_nb_completo3 <- glmer.nb(DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num +(1 | Localización), 
                                data = df_mixto_completo)
summary(modelo_nb_completo3)
Anova(modelo_nb_completo3)

confint(modelo_nb_completo3, method = "Wald")
exp(confint(modelo_nb_completo3, method = "Wald"))


#############################
#############################
####### DECISION TREE #######
#############################
#############################



library(rpart)
library(rpart.plot)
library(dplyr)

# 1. Preparar el dataframe incluyendo TODAS las variables que quieres ahora
df_arbol <- df %>%
  # AÑADIMOS precipitación y temperatura_media al select
  select(LARVAS_TOTALES, nubosidad, rachas_de_viento, fotoperiodo, 
         Tratamiento_Num, precipitación, temperatura_media)

# 2. Entrenar el algoritmo con la fórmula corregida (todo con '+')
arbol_modelo <- rpart(
  LARVAS_TOTALES ~ nubosidad + rachas_de_viento + fotoperiodo + 
    Tratamiento_Num + precipitación + temperatura_media,
  data = df_arbol,
  method = "anova",
  control = rpart.control(cp = 0.01, minsplit = 10) 
)

# 3. Dibujar el árbol
rpart.plot(
  arbol_modelo,
  type = 2,
  extra = 1,
  under = TRUE,
  faclen = 0,
  cex = 0.8,
  main = "Predicción de Abundancia Larvaria (Árbol de Decisión)",
  box.palette = "Blues"
)

# 4. Ver la importancia real de cada variable
print(arbol_modelo$variable.importance)



############################
# MIXED MODEL TOTAL
############################



library(tidyverse)
library(glmmTMB)

# -------------------------------------------------------------------------
# 1. CREACIÓN DE LOS 7 DATAFRAMES INDEPENDIENTES
# -------------------------------------------------------------------------
df1 <- df %>% filter(Localización == "P01. EL PARDO")
df2 <- df %>% filter(Localización == "P02. PUERTA DE HIERRO")
df3 <- df %>% filter(Localización == "P03. PUENTE DE LOS FRANCESES")
df4 <- df %>% filter(Localización == "P04. PUENTE DE TOLEDO")
df5 <- df %>% filter(Localización == "P05. TANATORIO")
df6 <- df %>% filter(Localización == "P06. PRESAIV")
df7 <- df %>% filter(Localización == "P07. DEPURADORA GAVIA")

# -------------------------------------------------------------------------
# 2. EJECUCIÓN MANUAL Y DIAGNÓSTICO (Ejemplos con df1 y df2)
# -------------------------------------------------------------------------

### TRAMO 1: EL PARDO
# 1. Comprobamos cuántas filas útiles (sin NA) le van a llegar realmente al modelo
filas_utiles_df1 <- df1 %>% drop_na(Tratamiento_Num, temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo) %>% nrow()
cat("Filas sin NA en P01. EL PARDO:", filas_utiles_df1, "\n")

# 2. Ejecutamos el modelo
modelo1 <- glmmTMB(
  DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
  data = df1,
  family = tweedie(link = "log")
)
summary(modelo1)
confint(modelo1, method = "Wald")

### TRAMO 2: PUERTA DE HIERRO
filas_utiles_df2 <- df2 %>% drop_na(Tratamiento_Num, temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo) %>% nrow()
cat("\nFilas sin NA en P02. PUERTA DE HIERRO:", filas_utiles_df2, "\n")

modelo2 <- glmmTMB(
  DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
  data = df2,
  family = tweedie(link = "log")
)

summary(modelo2)
confint(modelo2, method = "Wald")

### TRAMO 3
filas_utiles_df3 <- df3 %>% drop_na(Tratamiento_Num, temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo) %>% nrow()
cat("\nFilas sin NA en P03", filas_utiles_df3, "\n")

modelo3 <- glmmTMB(
  DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
  data = df3,
  family = tweedie(link = "log")
)

summary(modelo3)
confint(modelo3, method = "Wald")

### TRAMO 4
filas_utiles_df4 <- df4 %>% drop_na(Tratamiento_Num, temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo) %>% nrow()
cat("\nFilas sin NA en P04:", filas_utiles_df4, "\n")

modelo4 <- glmmTMB(
  DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
  data = df4,
  family = tweedie(link = "log")
)

summary(modelo4)
confint(modelo4, method = "Wald")

### TRAMO 5:
filas_utiles_df5 <- df5 %>% drop_na(Tratamiento_Num, temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo) %>% nrow()
cat("\nFilas sin NA en P05:", filas_utiles_df5, "\n")

modelo5 <- glmmTMB(
  DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
  data = df5,
  family = tweedie(link = "log")
)

summary(modelo5)
confint(modelo5, method = "Wald")

### TRAMO 6: 
filas_utiles_df6 <- df6 %>% drop_na(Tratamiento_Num, temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo) %>% nrow()
cat("\nFilas sin NA en P06:", filas_utiles_df6, "\n")

modelo6 <- glmmTMB(
  DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
  data = df6,
  family = tweedie(link = "log")
)

summary(modelo6)
confint(modelo6, method = "Wald")

### TRAMO 7:
filas_utiles_df7 <- df7 %>% drop_na(Tratamiento_Num, temperatura_media, nubosidad, rachas_de_viento, precipitación, fotoperiodo) %>% nrow()
cat("\nFilas sin NA en P07:", filas_utiles_df7, "\n")

modelo7 <- glmmTMB(
  DENSIDAD_LARVARIA_TOTAL ~ Tratamiento_Num + temperatura_media + nubosidad + rachas_de_viento + precipitación + fotoperiodo,
  data = df7,
  family = tweedie(link = "log")
)

summary(modelo7)
confint(modelo7, method = "Wald")



