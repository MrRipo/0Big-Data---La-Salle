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

# Entrenamiento con 5000 árboles.
set.seed(1)
gbm_1 <- gbm (
  formula = train$feat_6 ~ .,
  distribution = "multinomial",
  data = train,
  n.trees = 500
)
print(gbm_1)

# previous code
pred1 <- predict(gbm_1, train) #, type="response")       # prediccion
t1 <- table(pred1, train$feat_6)                     # tabla de confusion 
sum(diag(t1))/sum(t1)   
