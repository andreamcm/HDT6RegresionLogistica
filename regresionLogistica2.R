#-----------------------------------------------------------------------------------------------------------------------------------------------
# Universidad del Valle de Guatemala
# Autores: Andrea Maria Cordon Mayen, 16076
#          Cristopher Sebastian Recinos RamÃ???rez, 16005
# Fecha: 18/03/2019
# arboles.R
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Librerias a utilizar
library(caret)
install.packages("fmsb")
library(fmsb)
install.packages("e1071")
library(e1071)
install.packages("mlr")
library(mlr)
install.packages("dummy")
library(dummy)
install.packages("h2o")
library(h2o)
install.packages("plyr")
library(plyr)
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Set de ambientes y variables generales
# --------------------------------------

# Set del working directory de Andrea
setwd("~/2019/UVG/Primer Semestre/Minería de Datos/Laboratorios/Laboratorio6/HDT6RegresionLogistica/Datos")

# Set del working directory de Sebastian
setwd("C:/Users/sebas/Documents/UVG/201901/Mineria/Laboratorio5/HDT5NaiveBayes/Datos")

# Se cargan todos los datos 
datos <- read.csv("train2.csv")
str(datos) # Tipos de variables de las columnas de la base de datos
misDatos <- datos
sum(is.na(datos))
datos <- na.omit(datos)
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Regresión Logistica
#-----------------------------------------------------------------------------------------------------------------------------------------------

datos$VelocidadAdopcion <- plyr::revalue(datos$AdoptionSpeed, c("0" = "1", "1" = "1", "2" = "1", "3" = "0", "4" = "0"))


datos$AdoptionSpeed <- factor(datos$AdoptionSpeed)
datos$AdoptionSpeed

porcentaje<-0.7
set.seed(123)

# datos<-cbind(datos,dummy(datos,verbose = T))


# Datos de entrenamiento y prueba
corte <- sample(nrow(datos), nrow(datos)*porcentaje)
train <- datos[corte, ]
test <- datos[-corte, ]

modelo <- glm(AdoptionSpeed ~ Type + Age, data = train, family = binomial(), maxit = 10000)
#summary(modelo)
#test$AdoptionSpeed

pred <- predict(modelo, newdata = test, type = "response")
pred
prediccion <- ifelse(pred == 1, 1, 0)
prediccion
confusionMatrix(as.factor(test$AdoptionSpeed), as.factor(prediccion))



