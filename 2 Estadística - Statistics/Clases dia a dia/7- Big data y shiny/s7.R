############################################################
#
# MBD - Clustering supervisado (II)
#
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
library(randomForest)
library(party)
library(e1071)

############################################################
# Leer datos
############################################################
setwd('...')
datos <- read.csv2('products.csv',header=TRUE, stringsAsFactors = TRUE)

#-----------------------------------------------------------
#
# Arboles condicionales
#
#-----------------------------------------------------------
############################################################
# Dividir la muestra
############################################################
d <- datos[,-1]                            # Eliminamos identificador
p <- 0.7                                   # Proporcion en muestra de entrenamiento
n <- nrow(d)                               # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Visualizacion
############################################################
##-- Construirlo
ct.mod <- ctree(target ~ ., train,controls=ctree_control(maxdepth=3)) # Poco profundo para poder graficarlo

##-- Visualizarlo
windows()
plot(ct.mod,type='extended')
plot(ct.mod,type='simple')

############################################################
# Evaluar capacidad predictiva
############################################################
ct.mod <- ctree(target ~ ., train,controls=ctree_control(maxdepth=0)) # Profundidad maxima
pred <- predict(ct.mod,test,type="response")                          # prediccion de la respuesta
(t <- table(pred,test$target))                                        # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)                                                   # porcentaje de acierto: 0.7037383

############################################################
# Capacidad predictiva por clase
############################################################
barplot(diag(prop.table(t,2)))

############################################################
# Mejora: Podar el arbol --> No mejora
############################################################
##-- Con libreria partykit
library(partykit)
ct.mod <- ctree(target ~ ., train,control=ctree_control(maxdepth=Inf))
nid <- nodeids(ct.mod)                                                               # id de los nodos
iid <- nid[!(nid %in% nodeids(ct.mod, terminal = TRUE))]                             # Prune manual: eliminar los nodos terminales

# P valores
pval <- unlist(nodeapply(ct.mod, ids = iid, FUN = function(n) info_node(n)$p.value)) # p-valores resultantes del test de los nodos no terminales
quantile(log(pval,10),seq(0,1,0.1))

# Modelo podado
ct.mod2 <- nodeprune(ct.mod, ids = iid[log(pval,10) > -95])
pred2 <- predict(ct.mod2,test,type="response")                                       
(t2 <- table(pred2,test$target))                                                     
sum(diag(t2))/sum(t2) 

table(pred2,test$target,useNA='alw')

##-- Con libreria tree --> No mejora
library(tree)
tr <- tree(target ~ ., train)
cv.tr <- cv.tree(tr, FUN = prune.misclass)
plot(cv.tr)

# Modelo podado
ct.prune <- prune.misclass (tr ,best = 10)              # modelo
plot(ct.prune); text(ct.prune,pretty =0)                # arbol podado
pred3 <- predict (ct.prune ,test ,type="class")         # prediccion
t3 <- table(pred3,test$target)                          # tabla de confusion 
sum(diag(t3))/sum(t3)                                   # porcentaje de acierto 

############################################################
# Ejercicio 7-1
############################################################

##-- 1. Estudia como varia la proporción de acierto del ctree en función de algun parámetro que tu escojas dentro de ctree_control




#-----------------------------------------------------------
#
# Random forest
#
#-----------------------------------------------------------
set.seed(12345)
rf.mod <- randomForest(target~.,train,importance=TRUE,ntree=50,do.trace=TRUE)  
rf.mod
pred.rf <- predict(rf.mod,test)
(t <- table(pred.rf,test$target))                         # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)                                       # porcentaje de acierto

############################################################
# Es necesario el conjunto de entrenamiento?
############################################################
##-- Comparacion de errores de clasificacion
EE1 <- c(1-sum(diag(t))/sum(t),1-diag(prop.table(t,2)))    # Error de prediccion observado con muestra test (Global y por clase) --> En random forest no se necesita muestra test
EE2 <- rf.mod$err.rate[50,]                                # Error de prediccion estimado con modelo (Global y por clase)
plot(EE1,type='b',col=1,lwd=2,pch=15,xlab='',ylab='OOB',ylim=0:1,lty=1)
lines(EE2,type='b',col=2,lwd=2,pch=15,xlab='',lty=2)
legend('topright',c('OBB test','OBB RF'),col=1:2,lty=1:2,lwd=2) 

##-- Comparacion de la matriz de confusion
t2 <- rf.mod$confusion
t2
round(prop.table(t(t),1),2)
round(prop.table(t2[,1:9],1),2)

############################################################
# Necesitamos mas arboles?
############################################################
plot(rf.mod, type="l")

############################################################
# Importancia de las variables --> Interpretabilidad
############################################################
varImpPlot(rf.mod)
v.imp0 <- importance(rf.mod)

##-- Importancia Global
ord <- order(v.imp0[,'MeanDecreaseAccuracy'],decreasing=TRUE)
v.imp0[ord,c('MeanDecreaseAccuracy','MeanDecreaseGini')]

##-- Importancia para una clase concreta
ord <- order(v.imp0[,'Class_1'],decreasing=TRUE)
v.imp0[ord,c('Class_1')]

############################################################
# "Tunear" el parametro mtry
############################################################
mtry.par <- tuneRF(d[,1:93],d$target)
set.seed(12345)
rf.mod1 <- randomForest(target~.,train,importance=TRUE,ntree=50,do.trace=TRUE,mtry=18)
pred.rf1 <- predict(rf.mod1,test)
(t <- table(pred.rf1,test$target))                        
sum(diag(t))/sum(t)                

############################################################
# Ejercicio 7-2
############################################################

##-- 1. Ajusta un random-forest con 500 arboles y usando todos los datos. 
## Cual es el error de prediccion? Se necesitan mas arboles?


#-----------------------------------------------------------
#
# SVM
#
#-----------------------------------------------------------

############################################################
# Escoger solo 4000 registros
# Transformar para solo tener 2 clases
# Dividir la muestra 50/50
############################################################
##-- Dividir en muestra de entrenamiento y muestra test (iguales)
p <- 0.5
set.seed(12345)
n0 <- 4000
sel <- sample(1:nrow(d),n0)
d1 <- d[sel,]
d1$target2 <- factor(d1$target=='Class_5') # Class_1
d1$target <- NULL
train.sel <- sample(c(FALSE,TRUE),n0,rep=TRUE,prob=c(1-p,p))
train <- d1[train.sel,]
test <- d1[!train.sel,]

############################################################
# Ajustar modelo
############################################################
mod.svm <- svm(target2~.,data = train,cost=1)

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.svm,test)
t <- table(pr,test$target2)
sum(diag(t))/sum(t)
t

############################################################
# Tunear
############################################################
##-- Tunear el parametro de sobreajuste (cost)
mod.tune <- tune(svm,target2~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune)
mod.tune$best.parameters

##-- Escoger el mejor modelo
mod.best <- mod.tune$best.model
summary(mod.best)

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best,test)
t <- table(pr,test$target2)
t
sum(diag(t))/sum(t)

############################################################
# Kernels polynomial
############################################################
mod.tune1 <- tune(svm,target2~.,data=train,kernel="polynomial",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune1)
mod.best1 <- mod.tune1$best.model

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best1,test)
t <- table(pr,test$target2)
sum(diag(t))/sum(t)

############################################################
# Kernel radial
############################################################
mod.tune2 <- tune(svm,target2~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune2)
mod.best2 <- mod.tune2$best.model

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best2,test)
t <- table(pr,test$target2)
sum(diag(t))/sum(t)

############################################################
# Que kernel escoger?
############################################################
mod.tune3 <- tune(svm,target2~.,data=train,cost=1,ranges=list(kernel=c('linear','polynomial','radial','sigmoid')))
summary(mod.tune3)
mod.best3 <- mod.tune3$best.model


############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best3,test)
t <- table(pr,test$target2)
t
sum(diag(t))/sum(t)


############################################################
# Ejercicio 7-3
############################################################

##-- 1. Mejora la capacidad predictiva en la clasificacion de todas las clases usando la funcion "tune" 
## (Prueba con un numero ligeramente mayor de filas)