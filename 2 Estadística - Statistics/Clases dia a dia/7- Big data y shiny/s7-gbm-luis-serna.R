############################################################
#
# MBD - Clustering supervisado (II)
# GBM
############################################################
rm(list=ls())

############################################################
#
# Variables
#
############################################################
# Objetivo: clasificar los productos
# id: identificador del producto
# feat_XX: caracteristica XX                                 
# target: tipo de producto

############################################################
# Cargar paquetes
############################################################
# install.packages('randomForest')
# install.packages('party')
# install.packages('e1071')
install.packages("dplyr")
install.packages("gbm")
install.packages("ROCR")
install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
install.packages("DataExplorer")

#library(randomForest)
#library(party)
#library(e1071)
library(gbm)
library(dplyr)       # Manipulación de datos
library(knitr)       # Para formato de tablas
library(ROCR)        # Rendimiento del modelo y curva ROC
library(caret)       # División de muestra, Clasificación y regresión
library(DataExplorer)# Análisis descriptivo con gráficos


############################################################
# Leer datos
############################################################
setwd('~/UNI/ESTADISTICA/')
datos <- read.csv2('products.csv',header=TRUE, stringsAsFactors = TRUE)
d <- datos[,-1]

# model <- gbm(
#   formula(d),
#   d,
#   distribution = "gaussian",
#   n.trees = 100,
#   interaction.depth = 3
# )

set.seed(100)  # Para reproducir los mismos resultados

d <- mutate_if(d, is.integer, as.factor)

partition <- createDataPartition(y = d$feat_6, p = 0.7, list = FALSE)
train <- d[partition,]
test <- d[-partition,]
table(train$feat_6)   # counting occurrences

## Modelo GBM
set.seed(1)
gbm <- gbm(formula = target ~ .,
           distribution = "multinomial",
           data = train,
           n.trees = 500)
print(gbm)

test

# Prediccion Test

pred.rf2 <- predict(gbm, test, type ='response')
pred_class <- apply(pred.rf2, 1, which.max)

pred_class
pred.rf2

(t <- table(pred_class, test$target ))
sum(diag(t)/sum(t))

train$target

# Prediccion Train
pred.rf3 <- predict(gbm, train, type ='response')
pred_class_train <- apply(pred.rf3, 1, which.max)

(t2 <- table(pred_class_train, train$target ))
sum(diag(t2)/sum(t2))

## Degradacion es comparar el valor del accuracy dle modelo con datos de test y train
sum(diag(t)/sum(t)) # 0.78
sum(diag(t2)/sum(t2)) # 0.77

## degradación de 0.01