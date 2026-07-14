# --- Carga de librerías y datos ---
library(tidyverse)
library(caret)
library(pROC)

data_cancer <- read.csv("data.csv")

# 1. Limpieza: Se eliminan las columnas innecesarias (ID) y se convierte Diagnosis a Factor
data_cancer <- data_cancer %>% 
  select(-ID) %>%
  mutate(Diagnosis = factor(Diagnosis, levels = c("B", "M")))

# 2. Partición de datos (80% entrenamiento, 20% prueba)
set.seed(123) # Para que se pueda reproducir
trainIndex <- createDataPartition(data_cancer$Diagnosis, p = .8, list = FALSE)
trainData <- data_cancer[trainIndex, ]
testData  <- data_cancer[-trainIndex, ]

# 3. Entrenamiento con validación cruzada (CV)

knnModel <- train(Diagnosis ~ .,
                  data = trainData,
                  method = "knn",
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess = c("center", "scale"),
                  tuneLength = 20) # Prueba 20 valores distintos de K

# Visualizar el modelo
print(knnModel)
plot(knnModel)
# Es un modelo muy bueno donde el número de vecindarios tiene una k bastante pequeña para que la accuracy sea máxima
# Luego se ve cómo va decayendo la k a medida que aumenta la k

# 4. Predicciones
predictions <- predict(knnModel, newdata = testData)

# 5. Matriz de Confusión
cm <- confusionMatrix(predictions, testData$Diagnosis)
print(cm)

#La sensibilidad nos daría la posibilidad de acertar los casos malignos, que es un 100%, lo cual es muy bueno
#La precisión del modelo es de un 99%, lo que es un resultado también muy positivo
#La especificidad es la capacidad de descartar casos benignos, la cual es muy alta también
#Esto queda plasmado en que cuando el modelo ha predicho 72 benignos y solo 1 ha resultado ser maligno
#Cuando ha predicho 41 malignos, los ha acertado todos.

# 6. Curva ROC y AUC
probabilities_knn <- predict(knnModel, newdata = testData, type = "prob")
# Usamos la columna "M" (Maligno) para la probabilidad
roc_knn <- roc(testData$Diagnosis, probabilities_knn[, "M"], levels= c("B", "M"), direction = "<")
auc_knn <- auc(roc_knn)

plot(roc_knn, main = paste("Curva ROC k-NN (AUC =", round(auc_knn, 3), ")"), col = "blue")

#La curva ROC tiene su ángulo recto arriba a la izquierda, lo que hace que la relación sensibilidad-especificidad sea casi perfecta
#El parámetro AUC se acerca muchísimo a 1, por lo que se ha usado un modelo óptimo para este conjunto de datos
