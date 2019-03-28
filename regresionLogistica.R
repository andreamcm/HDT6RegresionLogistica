#-----------------------------------------------------------------------------------------------------------------------------------------------
# Universidad del Valle de Guatemala
# Autores: Andrea Maria Cordon Mayen, 16076
#          Cristopher Sebastian Recinos RamÃ?rez, 16005
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

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Regresión Logistica
#-----------------------------------------------------------------------------------------------------------------------------------------------

porcentaje<-0.7
set.seed(123)

datos<-cbind(datos,dummy(datos,verbose = T))

# Datos de entrenamiento y prueba
corte <- sample(nrow(datos), nrow(datos)*porcentaje)
train <- datos[corte, ]
test <- datos[-corte, ]

modelo <- glm(AdoptionSpeed~., data = train[,c(1, 3:18)],family = binomial(), maxit = 10000)

modelo<-glm(Species_virginica~., data = train[,c(1:4,8)],family = binomial(), maxit=100)

#-------------------------------------------------
# Regresión Logistica 
#-------------------------------------------------

##Modelo con todas las variables
pred<-predict(modelo,newdata = test[,1:4], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$Species_virginica),as.factor(prediccion))