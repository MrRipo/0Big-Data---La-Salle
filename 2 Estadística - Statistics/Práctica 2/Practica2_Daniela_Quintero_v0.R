############################################################
#
# MUBD - PRACTICA 2 - Clusterizacion
#
############################################################

rm(list=ls())

############################################################
# Instalamos y cargamos los paquetes necesarios
############################################################
library(NbClust)        # Function NbClust
library(ggplot2)
library(factoextra)     # Several clustering graphics
library(clustertend)    # Hopkins index
library(FactoMineR)     # Factor analysis
library(dendextend)     # Comparar dendogramas
library(corrplot)       # Graficos de correlaciones
library(cluster)        # pam
library(scatterplot3d)  
library(flexclust)
library(kernlab)        # kkmeans
library(clValid)        # clValid
############################################################
# Carga de datos
############################################################

#Cargamos lo datos de entrenamiento y los datos Test que utilizaremos a lo largo de este archivo de codigo.

DatosTrain <- read.table('Datos de entrenamiento.txt',header=TRUE,sep="\t",dec=".", stringsAsFactors = TRUE)
DatosTest <- read.table('Datos Test.txt',header=TRUE,sep="\t",dec=".", stringsAsFactors = TRUE)

#Eliminamos datos no relevantes para ejecutar comandos de Clusterizacion No Supervisada.

#datos_cns: Datos de entrenamiento sin variables subject y activity

datos_cns <- DatosTrain[,-which(names(DatosTrain) %in% c('subject','activity'))]  

############################################################
#
# OBJETIVO 1
#
############################################################
############################################################
#
# K-means
#
############################################################

############################################################
# Numero de grupos
############################################################
##-- Ejecutamos una prueba con la Regla del codo para darnos una idea del numero de clusters optimo.
VE <- c()
K <- seq(1,10,1)
for (k in K){
  km <- kmeans(datos_cns,centers=k,nstart=10)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")
cbind(K,VE)


##-- Regla del codo (Inercia intra) --> Equivalente al anterior

fviz_nbclust(datos_cns,kmeans,method="wss")


##-- Numero de clusteres segun indicadores

ncluster <- NbClust(datos_cns, min.nc=2, max.nc=10, method="kmeans")
ncluster

barplot(table(ncluster$Best.n[1,]))
heatmap(scale(ncluster$All.index),Rowv=NA,Colv = NA)

############################################################
# Representacion grafica
############################################################
##-- 2 grupos
km2 <- kmeans(datos_cns,centers=2,nstart=10)

table(DatosTrain$activity,km2$cluster)

fviz_cluster(list(data = datos_cns, cluster = km2$cluster),ellipse.type = "convex",
             repel = FALSE,                                        # Avoid label overplotting (slow)
             show.clust.cent = TRUE, ggtheme = theme_minimal(),pointsize = 1,
             labelsize = 0, main="K-Means 2 Clusters")




##-- 4 grupos
km4 <- kmeans(datos_cns,centers=4,nstart=10)

table(DatosTrain$activity,km4$cluster)

fviz_cluster(list(data = datos_cns, cluster = km4$cluster),ellipse.type = "convex",
             repel = FALSE,                                        # Avoid label overplotting (slow)
             show.clust.cent = TRUE, ggtheme = theme_minimal(),pointsize = 1,
             labelsize = 0, main="K-Means 4 Clusters")


##-- Calculo de la variabilidad explicada
var.exp.2<-with(km2,betweenss/totss) #2 Clusters
var.exp.2

var.exp.4<-with(km4,betweenss/totss) #4 Clusters
var.exp.4


############################################################
# K-mediods
############################################################
#2 Clusters
kmediods2 <- pam(datos_cns,2) 

fviz_cluster(list(data = datos_cns, cluster = kmediods2$cluster),ellipse.type = "convex",
             repel = FALSE,                                        
             show.clust.cent = FALSE, ggtheme = theme_minimal(),pointsize = 1,
             labelsize = 0, main="K-Mediods 2 Clusters")


#4 Clusters
kmediods4 <- pam(datos_cns,4)

fviz_cluster(list(data = datos_cns, cluster = kmediods4$cluster),ellipse.type = "convex",
             repel = FALSE,                                        
             show.clust.cent = FALSE, ggtheme = theme_minimal(),pointsize = 1,
             labelsize = 0, main="K-Mediods 4 Clusters")


#2 Clusters
randIndex(table(km2$cluster,kmediods2$cluster))   

#4 Clusters
randIndex(table(km4$cluster,kmediods4$cluster))   


############################################################
# Similitud con etiquetas iniciales
############################################################
#2 Clusters
randIndex(table(km2$cluster,DatosTrain$activity))

#4 Clusters
randIndex(table(km4$cluster,DatosTrain$activity))


############################################################
#
# OBJETIVO 2
#
############################################################

############################################################
#
# MUBD - Clustering supervisado (I)
#
############################################################


############################################################
#
# KNN
#
############################################################

############################################################
# Cargar paquetes
############################################################
# install.packages('class')
# install.packages('deldir')
# install.packages('kknn')
library(deldir)
library(kknn)
library(class)

############################################################
# Eliminar variables irrelevantes para funciones KNN y Naive Bayes
############################################################

#d0<-DatosTrain

d <- DatosTrain[,-which(names(DatosTrain) %in% 'subject')]


#Varibles para crear una tabla con todas las probabilidades
v1.nombres<-c()
v2.valores<-c()

############################################################
# Tranformar todas las variables en numericas
############################################################
# La funcion knn trabaja con numericas
for (i in 1:ncol(d)) d[,i] <- as.numeric(d[,i])

############################################################
# Dividir la muestra
############################################################
##-- Dividir en muestra de entrenamiento y muestra test
p <- 0.8                 # Proporcion en muestra de entrenamiento
n <- dim(d)[1]           # numero de observaciones 
set.seed(100)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Comparar capacidad predictiva con un k=1
############################################################
##-- 1-NN Train + Test
knn1 <- knn(train[,-ncol(d)], test=test[,-ncol(d)], cl=train$activity, k = 1)
t <- table(knn1,test$activity)
p01.knn1<-sum(diag(t))/sum(t)
p01.knn1

#Vectores con los nombres y valores de las predicciones
v1.nombres[1]<-c("Pred. 01 - 1-NN Train + Test, k=1")
v2.valores[1]<-c(p01.knn1)

##-- 1-NN Cross-validation
knn2 <- knn.cv(d[,-ncol(d)], cl=d$activity, k = 1)
t <- table(knn2,d$activity)
p02.knn2<-sum(diag(t))/sum(t)
p02.knn2

v1.nombres[2]<-c("Pred. 02 - 1-NN Cross-validation, k=1")
v2.valores[2]<-c(p02.knn2)

##-- Opcion Naive (Asignar a la categoria mayoritaria)
table(test$activity)
p03.knnNaive<-max(prop.table(table(test$activity)))
p03.knnNaive

v1.nombres[3]<-c("Pred. 03 - Opcion Naive (Asignar a la categoria mayoritaria)")
v2.valores[3]<-c(p03.knnNaive)



############################################################
# PROBANDO CON DIFERENTES K
############################################################
##-- 1-NN Train + Test - PRUEBA CON DIFERENTES K
p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn3 <- knn(train[,-ncol(d)], test=test[,-ncol(d)], cl=train$activity, k = k)
  t <- table(knn3,test$activity)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

#Obtenemos la mejor prediccion con los diferentes k probados.
p04.knn3<-as.data.frame(cbind(K,p))
p04.knn3 <- p04.knn3[with(p04.knn3, order(-p04.knn3$p)), ]
p04.knn3<-(p04.knn3$p[1])
p04.knn3

v1.nombres[4]<-c("Pred. 04 - 1-NN Train + Test - PRUEBA CON DIFERENTES K")
v2.valores[4]<-c(p04.knn3)

##-- 1-NN Cross-validation - PRUEBA CON DIFERENTES K
p <- c()
K <- seq(1,21,2)
t<-NULL
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn4 <- knn.cv(d[,-ncol(d)], cl=d$activity, k = k)
  t <- table(knn4,d$activity)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

#Obtenemos la mejor prediccion con los diferentes k probados.
p05.knn4<-as.data.frame(cbind(K,p))
p05.knn4 <- p05.knn4[with(p05.knn4, order(-p05.knn4$p)), ]
p05.knn4<-(p05.knn4$p[1])
p05.knn4

v1.nombres[5]<-c("Pred. 05 - 1-NN Cross-validation - PRUEBA CON DIFERENTES K")
v2.valores[5]<-c(p05.knn4)

############################################################
# Usar ACP --> Reduccion dimensionalidad previo a KNN
############################################################
res.acp0 <- princomp(d[,-ncol(d)])
screeplot(res.acp0,type='lines', main='Reduccion Dimensional')
res.acp <- res.acp0$scores[,1]
train2 <- data.frame(c1=res.acp[train.sel])
test2 <- data.frame(c1=res.acp[!train.sel])


#Volvemos a probar con distintos valores de k luego de la reduccion dimensional
K <- seq(1,21,2)
p <- c()
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn5 <- knn(train2, test2, cl=train$activity, k = k)
  t <- table(knn5,test$activity)
  p <- c(p,sum(diag(t))/sum(t))
}
plot(K,p,pch=19,type='b')
cbind(K,p)

#Obtenemos la mejor prediccion con los diferentes k probados.
p06.knn5<-as.data.frame(cbind(K,p))
p06.knn5 <- p06.knn5[with(p06.knn5, order(-p06.knn5$p)), ]
p06.knn5<-(p06.knn5$p[1])
p06.knn5

v1.nombres[6]<-c("Pred. 06 - Reduccion dimensionalidad previo a KNN")
v2.valores[6]<-c(p06.knn5)

############################################################
# Anyadir un minimo de votos (parametro l)
############################################################
#Realizamos un barrido con diferentes k con un minimo de votos=2 para descubrir y eliminar
#ks que nos muestren valores NAs.

K <- seq(1,10,1)
p <- c()
lista<-c()
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn6 <- knn(train[,-562], test[,-562], cl=train$activity, l=2, k = k)
  tmiss <- table(knn6,test$activity,useNA = 'always')
  lista[k]<-list(tmiss)
}

#Obtenemos una lista en la cual podemos observar las tablas que tienen valores
#NAs y las descartamos para realizar la funcion knn, con el numero de votos en 
#utilizando diferentes k.

lista

set.seed(100)
K <- seq(4,10,1)
p <- c()
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn6 <- knn(train[,-562], test[,-562], cl=train$activity, l=2, k = k)
  t <- table(knn6,test$activity)
  p<-c(p,sum(diag(t))/sum(t))
  
}
plot(K,p,pch=19,type='b')
cbind(K,p)


#Obtenemos la mejor prediccion con los diferentes k probados.
p07.knn6<-as.data.frame(cbind(K,p))
p07.knn6 <- p07.knn6[with(p07.knn6, order(-p07.knn6$p)), ]
p07.knn6<-(p07.knn6$p[1])
p07.knn6

v1.nombres[7]<-c("Pred. 07 - Anyadir un minimo de votos KNN")
v2.valores[7]<-c(p07.knn6)



############################################################
# Kernel
############################################################
##-- Con datos originales
kknn1 <- kknn(factor(activity)~., train, test,k=1)
fit <- fitted(kknn1)
t <- table(fit,test$activity)
sum(diag(t))/sum(t)     

p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  kknn <- kknn(factor(activity)~., train, test,k=k)
  t <- table(fitted(kknn),test$activity)
  p <- c(p,sum(diag(t))/sum(t))
}
plot(K,p,pch=19,type='b')
cbind(K,p)

#Obtenemos la mejor prediccion con los diferentes k probados.
p08.kknn1<-as.data.frame(cbind(K,p))
p08.kknn1 <- p08.kknn1[with(p08.kknn1, order(-p08.kknn1$p)), ]
p08.kknn1<-(p08.kknn1$p[1])
p08.kknn1

v1.nombres[8]<-c("Pred. 08 - Kernel KNN")
v2.valores[8]<-c(p08.kknn1)

##-- Con ACP --> NO mejora
kknn3 <- kknn(factor(train$activity)~., train2, test2,k=2)
fit <- fitted(kknn3)
t <- table(fit,test$activity)
sum(diag(t))/sum(t)

K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  kknn <- kknn(factor(train$activity)~., train2, test2,k=k)
  t <- table(fitted(kknn),test$activity)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

#Obtenemos la mejor prediccion con los diferentes k probados.
p09.kknn2<-as.data.frame(cbind(K,p))
p09.kknn2 <- p09.kknn2[with(p09.kknn2, order(-p09.kknn2$p)), ]
p09.kknn2<-(p09.kknn2$p[1])
p09.kknn2

v1.nombres[9]<-c("Pred. 09 - Kernel ACP KNN")
v2.valores[9]<-c(p09.kknn2)


############################################################
#
# Naive Bayes
#
############################################################


############################################################
# Cargar paquetes
############################################################
# install.packages('e1071')
# install.packages('ineq')
library(e1071)
library(ineq)

############################################################
# Inspeccionar datos
############################################################
dim(DatosTrain)                                 # Dimension
summary(DatosTrain)                             # Descriptiva
table(apply(apply(DatosTrain,2,is.na),2,sum))   # Tabla con numero de missings

############################################################
# Premisa de independencia
############################################################
cor.matrix <- cor(datos_cns)              # Matriz de correlaciones
cor.num <- as.numeric(cor.matrix)                           # Todas las correlaciones en un vector
t.cor <- table(cut(cor.num[cor.num!=1],br=seq(-1,1,0.1)))/2 # Categorazion de las correlaciones en intervalos de 0.1
t.cor
barplot(t.cor)

############################################################
# Volvemos a cargar variables y eliminar irrelevantes
############################################################
d <- DatosTrain[,-which(names(DatosTrain) %in% 'subject')]
p <- 0.8                 # Proporcion en muestra de entrenamiento
n <- dim(d)[1]           # numero de observaciones 
set.seed(100)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Aplicar bayes
############################################################
nb <- naiveBayes(activity ~ ., train)
nb$levels       # Clases
nb$apriori      # A priori
nb$tables       # A posteriori

############################################################
# Capacidad predictiva
############################################################
##-- Global
preds <- predict(nb, newdata = test)
t <- table(preds, test$activity)
t
p10.Nb1 <- sum(diag(t))/sum(t)
p10.Nb1

v1.nombres[10]<-c("Pred. 10 - Naive Bayes")
v2.valores[10]<-c(p10.Nb1)


##-- Se pueden pedir las probabilidades de cada clase para inspecci?n visual
preds2 <- predict(nb, newdata = test,type = "raw")
head(preds2)
heatmap(preds2[1:50,],Rowv=NA,Colv=NA,col = cm.colors(256))

##-- Proporcion de acierto por clase
barplot(diag(prop.table(t,2)))

############################################################
# Intento de mejora 1: Quitar variables correlacionadas
############################################################
##-- Sistema 1: Correlaciones grandes
#Probamos diferentes magnitudes de correlacion para encontrar la optima.
K <- seq(0.5,0.99,0.01)
p <- c()

for(k in K){
  
  high.corr <- which(cor.matrix>k,arr.ind = TRUE)
  t.high.corr <- sort(table(as.numeric(high.corr))/2,decreasing=TRUE)
  t.high.corr
  sel.rm <- names(t.high.corr)[t.high.corr>=2] 
  train2 <- train[,-which(names(train) %in% paste0('feat',sel.rm))]
  
  ##-- Aplicar bayes nuevamente
  nb2 <- naiveBayes(activity ~ ., train2, type="class")
  preds <- predict(nb2, newdata = test)
  t <- table(preds, test$activity)
  p <- c(p,sum(diag(t))/sum(t))
}
p
cbind(K,p)

#Obtenemos la mejor prediccion con las diferentes correlaciones probadas.
p11.Nb2<-as.data.frame(cbind(K,p))
p11.Nb2 <- p11.Nb2[with(p11.Nb2, order(-p11.Nb2$p)), ]
p11.Nb2<-(p11.Nb2$p[1])
p11.Nb2

v1.nombres[11]<-c("Pred. 11 - Naive Bayes-Quitar variables correlacionadas")
v2.valores[11]<-c(p11.Nb2)

##-- Sistema 2: Suma de correlaciones
cor.sum <- sort(apply(cor.matrix,2,sum),decreasing=TRUE)
cor.sum
sel.rm <- names(cor.sum)[1:30]
train3 <- train[,-which(names(train) %in% sel.rm)]

##-- Aplicar bayes nuevamente
nb3 <- naiveBayes(activity ~ ., train3, type="class")
preds <- predict(nb3, newdata = test)
t <- table(preds, test$activity)
p12.Nb3 <- sum(diag(t))/sum(t)
p12.Nb3

v1.nombres[12]<-c("Pred. 12 - Naive Bayes-Suma de correlaciones")
v2.valores[12]<-c(p12.Nb3)


############################################################
# Intento de mejora 2: Anyadir correccion de Laplace sobre datos no correlacionados --> No funciona
############################################################
##-- Aplicar bayes nuevamente
#Probamos diferentes valores de Laplace, pero la probabilidad no cambia puesto
#que los predictores son continuos.
K <- seq(0.1,0.9,0.1)
p <- c()

for(k in K){
  
  nb4 <- naiveBayes(activity ~ ., train, type="class",laplace=k)
  preds <- predict(nb4, newdata = test)
  t <- table(preds, test$activity)
  p <- c(p,sum(diag(t))/sum(t))
}
p    # No cambia porque todos los predictores son continuos

p13.Nb4<-as.data.frame(cbind(K,p))
p13.Nb4 <- p13.Nb4[with(p13.Nb4, order(-p13.Nb4$p)), ]
p13.Nb4<-(p13.Nb4$p[1])
p13.Nb4

v1.nombres[13]<-c("Pred. 13 - Naive Bayes-Correccion de Laplace")
v2.valores[13]<-c(p13.Nb4)

############################################################
# Intento de mejora 3: Kernel en vez de distr. Gaussiana           --> No funciona
############################################################
library(naivebayes)
nb5 <- naive_bayes(activity ~ ., train,usekernel = TRUE)
preds <- predict(nb5, newdata = test)
t <- table(preds, test$activity)
p14.Nb5 <- sum(diag(t))/sum(t)
p14.Nb5

v1.nombres[14]<-c("Pred. 14 - Naive Bayes-Kernel en vez de distr. Gaussiana")
v2.valores[14]<-c(p14.Nb5)



############################################################
#
# MBD - Clustering supervisado (II)
#
############################################################

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

#-----------------------------------------------------------
#
# Arboles condicionales
#
#-----------------------------------------------------------
##############################################################################
# Cargamos los datos nuevamente
##############################################################################
d <- DatosTrain[,-which(names(DatosTrain) %in% 'subject')]                            
p <- 0.8                                   # Proporcion en muestra de entrenamiento
n <- nrow(d)                               # Numero de observaciones 
set.seed(100)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Visualizacion
############################################################
##-- Construirlo
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(maxdepth=3)) # Poco profundo para poder graficarlo

##-- Visualizarlo
windows()
plot(ct.mod,type='extended')
plot(ct.mod,type='simple')

############################################################
# Evaluar capacidad predictiva
############################################################
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(maxdepth=0)) # Profundidad maxima
pred <- predict(ct.mod,test,type="response")                          # prediccion de la respuesta
(t <- table(pred,test$activity))                                        # tabla de predicciones vs respuesta real
p15.CondTree1<-sum(diag(t))/sum(t)                                                   # porcentaje de acierto: 0.7037383
p15.CondTree1

v1.nombres[15]<-c("Pred. 15 - Arboles condicionales")
v2.valores[15]<-c(p15.CondTree1)

############################################################
# Capacidad predictiva por clase
############################################################
barplot(diag(prop.table(t,2)))

############################################################
# Mejora: Podar el arbol 
############################################################
##-- Con libreria partykit
library(partykit)
ct.mod <- ctree(activity ~ ., train,control=ctree_control(maxdepth=Inf))
nid <- nodeids(ct.mod)                                                               # id de los nodos
iid <- nid[!(nid %in% nodeids(ct.mod, terminal = TRUE))]                             # Prune manual: eliminar los nodos terminales

# P valores
pval <- unlist(nodeapply(ct.mod, ids = iid, FUN = function(n) info_node(n)$p.value)) # p-valores resultantes del test de los nodos no terminales
quantile(log(pval,10),seq(0,1,0.1))

# Modelo podado
ct.mod2 <- nodeprune(ct.mod, ids = iid[log(pval,10) > -95])
pred2 <- predict(ct.mod2,test,type="response")                                       
(t2 <- table(pred2,test$activity))                                                     
p16.CondTree2<-sum(diag(t2))/sum(t2) 
p16.CondTree2

v1.nombres[16]<-c("Pred. 16 - Arboles condicionales-Con libreria partykit")
v2.valores[16]<-c(p16.CondTree2)

table(pred2,test$activity,useNA='alw')

##-- Con libreria tree 
library(tree)
tr <- tree(activity ~ ., train)
cv.tr <- cv.tree(tr, FUN = prune.misclass)
plot(cv.tr)

# Modelo podado
ct.prune <- prune.misclass (tr ,best = 10)              # modelo
plot(ct.prune); text(ct.prune,pretty =0)                # arbol podado
pred3 <- predict (ct.prune ,test ,type="class")         # prediccion
t3 <- table(pred3,test$activity)                          # tabla de confusion 
p17.CondTree3<-sum(diag(t3))/sum(t3)                                   # porcentaje de acierto 
p17.CondTree3

v1.nombres[17]<-c("Pred. 17 - Arboles condicionales-Con libreria tree")
v2.valores[17]<-c(p17.CondTree3)

#-----------------------------------------------------------
#
# Random forest
#
#-----------------------------------------------------------
set.seed(100)
rf.mod <- randomForest(activity~.,train,importance=TRUE,ntree=500,do.trace=TRUE)  
rf.mod
pred.rf <- predict(rf.mod,test)
(t <- table(pred.rf,test$activity))                         # tabla de predicciones vs respuesta real
p18.RamFor1<-sum(diag(t))/sum(t)                                       # porcentaje de acierto
p18.RamFor1

v1.nombres[18]<-c("Pred. 18 - Random forest")
v2.valores[18]<-c(p18.RamFor1)


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


############################################################
# "Tunear" el parametro mtry
############################################################
mtry.par <- tuneRF(d[,1:93],d$activity)
set.seed(100)
rf.mod1 <- randomForest(activity~.,train,importance=TRUE,ntree=500,do.trace=TRUE,mtry=18)
pred.rf1 <- predict(rf.mod1,test)
(t <- table(pred.rf1,test$activity))                        
p19.RamFor2.Tune.mtry<-sum(diag(t))/sum(t)                
p19.RamFor2.Tune.mtry

v1.nombres[19]<-c("Pred. 19 - Random forest-Tunear el parametro mtry")
v2.valores[19]<-c(p19.RamFor2.Tune.mtry)

#-----------------------------------------------------------
#
# SVM
#
#-----------------------------------------------------------
##############################################################################
# Cargamos los datos nuevamente
##############################################################################
d <- DatosTrain[,-which(names(DatosTrain) %in% 'subject')]                            
p <- 0.85                                   # Proporcion en muestra de entrenamiento
n <- nrow(d)                               # Numero de observaciones 
set.seed(100)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Ajustar modelo
############################################################
mod.svm <- svm(activity~.,data = train,cost=1)

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.svm,test)
t <- table(pr,test$activity)
p20.svm1<-sum(diag(t))/sum(t)
p20.svm1

v1.nombres[20]<-c("Pred. 20 - SVM")
v2.valores[20]<-c(p20.svm1)

############################################################
# Tunear
############################################################
##-- Tunear el parametro de sobreajuste (cost)
mod.tune <- tune(svm,activity~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.1,0.2,1,5,10,100)))
summary(mod.tune)
mod.tune$best.parameters

##-- Escoger el mejor modelo
mod.best <- mod.tune$best.model
summary(mod.best)

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best,test)
t <- table(pr,test$activity)
p21.svm2.TL<-sum(diag(t))/sum(t)
p21.svm2.TL

v1.nombres[21]<-c("Pred. 21 - SVM - Kernel Linear")
v2.valores[21]<-c(p21.svm2.TL)

############################################################
# Kernels polynomial
############################################################
mod.tune1 <- tune(svm,activity~.,data=train,kernel="polynomial",ranges=list(cost=c(0.01,0.1,0.2,1,5,10,100)))
summary(mod.tune1)
mod.best1 <- mod.tune1$best.model

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best1,test)
t <- table(pr,test$activity)
p22.svm3.TP<-sum(diag(t))/sum(t)
p22.svm3.TP

v1.nombres[22]<-c("Pred. 22 - SVM - Kernel Polynomial")
v2.valores[22]<-c(p22.svm3.TP)
############################################################
# Kernel radial
############################################################
mod.tune2 <- tune(svm,activity~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.1,0.2,1,5,10,100)))
summary(mod.tune2)
mod.best2 <- mod.tune2$best.model

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best2,test)
t <- table(pr,test$activity)
p23.svm4.TR<-sum(diag(t))/sum(t)
p23.svm4.TR

v1.nombres[23]<-c("Pred. 23 - SVM - Kernel Radial")
v2.valores[23]<-c(p23.svm4.TR)

############################################################
# Kernel sigmoid
############################################################

mod.tune3 <- tune(svm,activity~.,data=train,kernel="sigmoid",ranges=list(cost=c(0.01,0.1,0.2,1,5,10,100)))
summary(mod.tune3)
mod.best3 <- mod.tune3$best.model

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best3,test)
t <- table(pr,test$activity)
p24.svm4.TS<-sum(diag(t))/sum(t)
p24.svm4.TS

v1.nombres[24]<-c("Pred. 24 - SVM - Kernel Sigmoid")
v2.valores[24]<-c(p24.svm4.TS)



############################################################
# Kernel radial ALTERNATIVO
############################################################

p <- 0.9                                   # Proporcion en muestra de entrenamiento
n <- nrow(d)                               # Numero de observaciones 
set.seed(100)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]




mod.tune2_A <- tune(svm,activity~.,data=train,kernel="radial",cost=10)
summary(mod.tune2_A)
mod.best2_A <- mod.tune2_A$best.model
summary(mod.best2_A)
############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best2_A,test)
t <- table(pr,test$activity)
p23.svm4.TR_A<-sum(diag(t))/sum(t)
p23.svm4.TR_A

v1.nombres[25]<-c("Pred. 23 - SVM - Kernel Radial ALTERNO")
v2.valores[25]<-c(p23.svm4.TR_A)


############################################################
# Compilacion de resultados
############################################################

cbind(v1.nombres,v2.valores)

#Ordenamos de mejor a peir prediccion.
p.Totales<-as.data.frame(cbind(v1.nombres,as.numeric(v2.valores)))
p.Totales <- p.Totales[with(p.Totales, order(-v2.valores)), ]
p.Totales

############################################################
# APLICACION DEL MEJOR MODELO PREDICTIVO
############################################################


PR<- predict(mod.best2_A,DatosTest)

DatosTest_page <- data.frame(activity=PR) 
write.table(DatosTest_page, 'p2.txt', row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE)

