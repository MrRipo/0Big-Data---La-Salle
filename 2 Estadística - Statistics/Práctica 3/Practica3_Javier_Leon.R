rm(list=ls())

############################################################
# Instalar y cargar paquetes
############################################################
library(scatterplot3d)  
library(flexclust)
library(NbClust)        # NbClust
library(cluster)
library(factoextra)     # fviz_***
library(kernlab)        # kkmeans
library(clValid)        # clValid
library(cluster)        # pam
library(hopkins)    # Hopkins index

############################################################
# Lectura de datos y inspeccion
############################################################
##-- Leer los datos
setwd('/Users/javier.leon/Documents/personal/Master/Estadistica/P3-GESTURE-STUDENTS')                                                                       # directorio de trabajo
datos <- read.table('p3_train.txt',header=TRUE,sep='\t',stringsAsFactors = TRUE) # lectura de los datos

##-- Visualizar los datos y descriptiva con summary
#View(datos)             # Ver los datos
summary(datos)          # Descriptiva de todas las variables

d_training = datos[,1:64]

#dev.new(14,7)
#heatmap(as.matrix(d_training))                  # Heatmap sin escalar
heatmap(as.matrix(scale(d_training)))           # Heatmap escalando

#calcular la matriz de distacias
#m.distancia <- get_dist(d_training, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
#fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

##-- Prueba simple
km0 <- kmeans(d_training,centers=3)
km0$cluster                         # asignacion a los clusteres
km0$centers                         # coordenadas de los centros de gravedad
km0$totss                           # Inercia total
km0$withinss                        # Inercia intra para cada cluster
km0$tot.withinss                    # Inercia intra (global)
km0$betweenss                       # Inercia entre
km0$size                            # Tamanyo de los clusteres
km0$iter                            # Iteraciones para converger

##-- Calculo de la variabilidad explicada
with(km0,betweenss/totss)

############################################################
# Numero de grupos
############################################################
VE <- c()
for (k in seq(2, 30, 2)){
  km <- kmeans(d_training,centers=k,nstart=30)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")

##-- Regla del codo (Inercia intra) --> Equivalente al anterior
fviz_nbclust(d_training,kmeans,k.max=30,method="wss")
fviz_nbclust(d_training,kmeans,k.max=30,method="silhouette")
fviz_nbclust(d_training,kmeans,k.max=30,method="gap_stat")

resnumclust<-NbClust(d_training, distance = "euclidean", min.nc=2, max.nc=30, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

# Hopkins
set.seed(12345)
hop <- hopkins(d_training, n = nrow(d_training)-1)
hop 

##-- Numero de clusteres segun indicadores
set.seed(12345)
ncluster <- NbClust(d_training, min.nc=2, max.nc=30, method="kmeans")
ncluster
barplot(table(ncluster$Best.n[1,]))
heatmap(scale(ncluster$All.index),Rowv=NA,Colv = NA)

######################################################################################################################

############################################################
# Clustering supervisado
############################################################

############################################################
# KNN
############################################################
rm(list=ls())

library(deldir)
library(kknn)
library(class)

# Leer e inspeccionar los datos
setwd('/Users/javier.leon/Documents/personal/Master/Estadistica/P3-GESTURE-STUDENTS')
d_knn <- read.table('p3_train.txt',header=TRUE,sep='\t',stringsAsFactors = TRUE)
t_knn <- read.table('p3_test.txt',header=TRUE,sep='\t',stringsAsFactors = TRUE)

dim(d_knn)

# Tranformar todas las variables en numericas
transform(d_knn,
          y = as.integer(as.factor(y))
          )
# ok: 1, piedra: 2, papel:3, tijeras: 4


# Dividir la muestra
##-- Dividir en muestra de entrenamiento y muestra test
p <- 0.7                 # Proporcion en muestra de entrenamiento
n <- dim(d_knn)[1]           # numero de observaciones 
set.seed(12345)          # dejar estatico el primer set de datos de entrenamiento.
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d_knn[train.sel,]
test <- d_knn[!train.sel,]

# Comparar capacidad predictiva
##-- 1-NN Train + Test
knn1 <- knn(train = train[1:(length(train)-1)], test = test[1:(length(test)-1)], cl=train$y, k = 6)
t <- table(knn1,test$y)
t
sum(diag(t))/sum(t)

##-- 1-NN Cross-validation
knn2 <- knn.cv(d_knn[1:(length(d_knn)-1)], cl=d_knn$y, k = 1)
t <- table(knn2,d_knn$y)
t
sum(diag(t))/sum(t)

##-- Opcion Naive (Asignar a la categoria mayoritaria)
table(test$y)
max(prop.table(table(test$y)))

# Numero de grupos para KNN Train + Test
p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn <- knn(train = train[1:(length(train)-1)], test = test[1:(length(test)-1)], cl=train$y, k = k)
  t <- table(knn,test$y)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

# Numero de grupos para CV
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knncv <- knn.cv(d_knn[1:(length(d_knn)-1)], cl=d_knn$y, k = k)
  t <- table(knncv,d_knn$y)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

# KNN Train + Test con la mejor K encontrada
knn7 <- knn(train = d_knn[1:(length(d_knn)-1)], test = t_knn, cl=d_knn$y, k = 7)
t <- table(knn7,d_knn$y)
t
sum(diag(t))/sum(t)

# Generacion de Fichero
knn7_pred <- data.frame(y=knn7)
# Diferente nombre ya que no es el mejor método
write.table(knn7_pred, 'p3_knn.txt', row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE) 

############################################################
# Naive Bayes
############################################################
rm(list=ls())
library(e1071)
library(ineq)
library(psych)
library(corrplot)

setwd('/Users/javier.leon/Documents/personal/Master/Estadistica/P3-GESTURE-STUDENTS')
d_naive <- read.table('p3_train.txt',header=TRUE,sep='\t',stringsAsFactors = TRUE)
t_naive <- read.table('p3_test.txt',header=TRUE,sep='\t',stringsAsFactors = TRUE)

# Inspeccionar datos
dim(d_naive)                                 # Dimension
summary(d_naive)                             # Descriptiva
#table(apply(apply(datos,2,is.na),2,sum))   # Tabla con numero de missings


# Premisa de independencia
cor.matrix <- cor(d_naive[,-c(1,length(d_naive))])          # Matriz de correlaciones
cor.num <- as.numeric(cor.matrix)                           # Todas las correlaciones en un vector
t.cor <- table(cut(cor.num[cor.num!=1],br=seq(-1,1,0.1)))/2 # Categorazion de las correlaciones en intervalos de 0.1
t.cor
barplot(t.cor)
corPlot(cor.matrix, main = "Matriz de correlación")


# Dividir la muestra
p <- 0.7                  # Proporcion en muestra de entrenamiento
n <- nrow(d_naive)        # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d_naive[train.sel,]
test <- d_naive[!train.sel,]


# Aplicar bayes
nb <- naiveBayes(y ~ ., train)
nb$levels       # Clases
nb$apriori      # A priori
nb$tables       # A posteriori

# Capacidad predictiva
##-- Global
preds <- predict(nb, newdata = test)
t <- table(preds, test$y)
t <- as.table(t)
t
p.acierto <- sum(diag(t))/sum(t)
p.acierto

##-- Se pueden pedir las probabilidades de cada clase para inspecci?n visual
preds2 <- predict(nb, newdata = test,type = "raw")
head(preds2)
dev.off()
heatmap(preds2[1:100,],Rowv=NA,Colv=NA,col = cm.colors(256))

##-- Proporcion de acierto por clase
barplot(diag(prop.table(t,2)))

############################################################
# Random forest
############################################################
library(randomForest)
library(party)
library(e1071)

setwd('/Users/javier.leon/Documents/personal/Master/Estadistica/P3-GESTURE-STUDENTS')
d_rforest <- read.table('p3_train.txt',header=TRUE,sep='\t',stringsAsFactors = TRUE)
t_rforest <- read.table('p3_test.txt',header=TRUE,sep='\t',stringsAsFactors = TRUE)

# Dividir la muestra
p <- 0.7              # Proporcion en muestra de entrenamiento
n <- nrow(d_rforest)          # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d_rforest[train.sel,]
test <- d_rforest[!train.sel,]

set.seed(12345)
rf.mod <- randomForest(y~.,train,importance=TRUE,ntree=50,do.trace=TRUE)  
rf.mod
pred.rf <- predict(rf.mod,test)
(t <- table(pred.rf,test$y))                         # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)                                       # porcentaje de acierto

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
round(prop.table(t2[,1:4],1),2)

# Necesitamos mas arboles?
plot(rf.mod, type="l")

# Importancia de las variables --> Interpretabilidad
varImpPlot(rf.mod)
v.imp0 <- importance(rf.mod)

##-- Importancia Global
ord <- order(v.imp0[,'MeanDecreaseAccuracy'],decreasing=TRUE)
v.imp0[ord,c('MeanDecreaseAccuracy','MeanDecreaseGini')]

##-- Importancia para una clase concreta
ord <- order(v.imp0[,'ok'],decreasing=TRUE)
v.imp0[ord,c('ok')]

ord <- order(v.imp0[,'papel'],decreasing=TRUE)
v.imp0[ord,c('papel')]

ord <- order(v.imp0[,'piedra'],decreasing=TRUE)
v.imp0[ord,c('piedra')]

ord <- order(v.imp0[,'tijeras'],decreasing=TRUE)
v.imp0[ord,c('tijeras')]

# "Tunear" el parametro mtry
mtry.par <- tuneRF(d_rforest[,1:64],d_rforest$y)
set.seed(12345)
rf.mod1 <- randomForest(y~.,train,importance=TRUE,ntree=50,do.trace=TRUE,mtry=18)
pred.rf1 <- predict(rf.mod1,test)
(t <- table(pred.rf1,test$y))                        
sum(diag(t))/sum(t)  

#Generacion de la prediccion y archivo
pred.test.rf <- predict(rf.mod,t_rforest)
pred_test <- data.frame(y=pred.test.rf) # pr: predicciones
write.table(pred_test, 'p3.txt', row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE)
