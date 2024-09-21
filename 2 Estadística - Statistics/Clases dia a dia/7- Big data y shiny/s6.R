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
# Variables
############################################################
# Datos de clientes de una companyia telefonica
# age: edad                                 
# annualincome: ingresos anuales
# calldroprate: tasa de llamadas perdidas                          
# callfailurerate: tasa de llamadas cortadas                     
# callingnum: numero de telefono                 
# customerid: identificador del cliente                          
# customersuspended:                    
# education: formacion educativa                           
# gender: sexo                               
# homeowner: propietario de vivienda                           
# maritalstatus: estado civil                        
# monthlybilledamount: factura mensual                 
# noadditionallines: ?                    
# numberofcomplaints: numero de quejas                  
# numberofmonthunpaid: numero de meses sin pagar                  
# numdayscontractequipmentplanexpiring: numero de dias para expirar el contrato
# occupation: ocupacion                           
# penaltytoswitch: penalizacion por cambio de linea                     
# state: estado de USA                               
# totalminsusedinlastmonth: minutos en ultimo mes            
# unpaidbalance: balance de impagos                        
# usesinternetservice: servicio de internet contratado                                      
# usesvoiceservice: servicio de voz contratado                     
# percentagecalloutsidenetwork        
# totalcallduration: duracion total de las llamadas                    
# avgcallduration: duracion media de las llamadas                     
# churn: baja del cliente                                
# year: anyo                                
# month: mes    

rm(list=ls())

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
# Leer e inspeccionar los datos
############################################################
setwd('...')
d0 <- read.table('edw.csv',header=TRUE,sep=',',stringsAsFactors = TRUE)
dim(d0)
summary(d0)

############################################################
# Eliminar variables irrelevantes y transformar algunas
############################################################
var.rm <- c('customerid','noadditionallines','state','year','month','callingnum')
d <- d0[,-which(names(d0) %in% var.rm)]

d$education <- relevel(d$education,'High School or below')
d$occupationTech <- ifelse(d$occupation=='Technology Related Job',1,0) 
d$occupationNonTech <- ifelse(d$occupation=='Non-technology Related Job',1,0)
d$occupation <- NULL
d <- d[,names(d)[c(1:21,23,24,22)]] # Poner la variable respuesta la ultima

############################################################
# Tranformar todas las variables en numericas
############################################################
# La funcion knn trabaja con numericas
for (i in 1:ncol(d)) d[,i] <- as.numeric(d[,i])


############################################################
# Dividir la muestra
############################################################
##-- Dividir en muestra de entrenamiento y muestra test
p <- 0.7                 # Proporcion en muestra de entrenamiento
n <- dim(d)[1]           # numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Comparar capacidad predictiva 
############################################################
##-- 1-NN Train + Test
knn1 <- knn(train[,-ncol(d)], test=test[,-ncol(d)], cl=train$churn, k = 1)
t <- table(knn1,test$churn)
sum(diag(t))/sum(t)

##-- 1-NN Cross-validation
knn2 <- knn.cv(d[,-ncol(d)], cl=d$churn, k = 1)
t <- table(knn2,d$churn)
sum(diag(t))/sum(t)

##-- Opcion Naive (Asignar a la categoria mayoritaria)
table(test$churn)
max(prop.table(table(test$churn)))

############################################################
# Numero de grupos
############################################################
p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn <- knn(train[,-ncol(d)], test=test[,-ncol(d)], cl=train$churn, k = k)
  t <- table(knn,test$churn)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

############################################################
# Usar ACP --> Reduccion dimensionalidad previo a KNN
############################################################
res.acp0 <- princomp(d[,-ncol(d)])
screeplot(res.acp0,type='lines')
res.acp <- res.acp0$scores[,1]
train2 <- data.frame(c1=res.acp[train.sel])
test2 <- data.frame(c1=res.acp[!train.sel])


K <- seq(1,21,2)
p <- c()
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn <- knn(train2, test2, cl=train$churn, k = k)
  t <- table(knn,test$churn)
  p <- c(p,sum(diag(t))/sum(t))
}
plot(K,p,pch=19,type='b')
cbind(K,p)

##-- 1-NN
knn1 <- knn(train2, test2, cl=train$churn, k = 1)
t <- table(knn1,test$churn)
t
sum(diag(t))/sum(t)

############################################################
# Anyadir un minimo de votos (parametro l)
############################################################
knn2 <- knn(train2, test2, cl=train$churn, l=2, k = 2)
t <- table(knn2,test$churn)
sum(diag(t))/sum(t)
tmiss <- table(knn2,test$churn,useNA = 'always')
tmiss


############################################################
# Kernel
############################################################
##-- Con datos originales
kknn1 <- kknn(factor(churn)~., train, test,k=1)
fit <- fitted(kknn1)
t <- table(fit,test$churn)
sum(diag(t))/sum(t)     

p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  kknn <- kknn(factor(churn)~., train, test,k=k)
  t <- table(fitted(kknn),test$churn)
  p <- c(p,sum(diag(t))/sum(t))
}
plot(K,p,pch=19,type='b')
cbind(K,p)
    
##-- Con ACP --> NO mejora
kknn3 <- kknn(factor(train$churn)~., train2, test2,k=2)
fit <- fitted(kknn3)
t <- table(fit,test$churn)
sum(diag(t))/sum(t)

K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  kknn <- kknn(factor(train$churn)~., train2, test2,k=k)
  t <- table(fitted(kknn),test$churn)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

############################################################
# Ejercicio 6-1
############################################################

##-- 1. Lee los datos "bank-additional-full.csv"


##-- 2. Realiza una depuracion
# a. Ordena las categoricas ordinales
# b. Convierte en dummies las categoricas nominales
# c. Todas las variables deben ser de clase "numeric"
# d. Elimina varible pdays
# Puedes usar la siguiente funcion manual para pasar una variable a dummy
to.dummy <- function(x,ordinal,ord=NULL){
  if(ordinal){
    y <- as.numeric(factor(x,levels=ord))
  }else{
    x <- as.factor(x)
    nl <- nlevels(x)
    y <- matrix(ncol=nl-1,nrow=length(x))
    for(i in 1:(nl-1)){
      y[,i] <- ifelse(x==levels(x)[i],1,0)
    }
    colnames(y) <- paste0(levels(x)[1:(nl-1)],'.dummy')
  }
  return(y)
}

##-- 3. Divide la muestra: Train + test en una submuestra de 10000


##-- 4. Estudia el numero de grupos idoneo en una submuestra de 10000




############################################################
#
# Naive Bayes
#
############################################################
rm(list=ls())

############################################################
# Variables
############################################################
# Objetivo: clasificar los productos
# id: identificador del producto
# feat_XX: caracteristica XX                                 
# target: tipo de producto

############################################################
# Cargar paquetes
############################################################
# install.packages('e1071')
# install.packages('ineq')
library(e1071)
library(ineq)

############################################################
# Leer datos
############################################################
setwd('...')
datos <- read.csv2('products.csv',header=TRUE, stringsAsFactors = TRUE)

############################################################
# Inspeccionar datos
############################################################
dim(datos)                                 # Dimension
summary(datos)                             # Descriptiva
table(apply(apply(datos,2,is.na),2,sum))   # Tabla con numero de missings

############################################################
# Premisa de independencia
############################################################
cor.matrix <- cor(datos[,-c(1,length(datos))])              # Matriz de correlaciones
cor.num <- as.numeric(cor.matrix)                           # Todas las correlaciones en un vector
t.cor <- table(cut(cor.num[cor.num!=1],br=seq(-1,1,0.1)))/2 # Categorazion de las correlaciones en intervalos de 0.1
t.cor
barplot(t.cor)

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
# Aplicar bayes
############################################################
nb <- naiveBayes(target ~ ., train)
nb$levels       # Clases
nb$apriori      # A priori
nb$tables       # A posteriori

############################################################
# Capacidad predictiva
############################################################
##-- Global
preds <- predict(nb, newdata = test)
t <- table(preds, test$target)
t
p.acierto <- sum(diag(t))/sum(t)
p.acierto

##-- Se pueden pedir las probabilidades de cada clase para inspecci?n visual
preds2 <- predict(nb, newdata = test,type = "raw")
head(preds2)
heatmap(preds2[1:50,],Rowv=NA,Colv=NA,col = cm.colors(256))

##-- Proporcion de acierto por clase
barplot(diag(prop.table(t,2)))


############################################################
# Importancia de las variables
############################################################
# Medir la importancia de las variables en funcion del indice Gini
graphics.off()                  # Cerrar ventanas
nbt <- nb$tables                # Tablas con medias
var.imp <- c()                  # Gini

##-- Calcular indice y graficar distribucion
for (i in 1:(ncol(d)-1)){
  ##-- Crear ventana grafica cada 9 graficos
  if((i-1) %% 9==0){
    windows()
    par(mfrow=c(3,3))
  }
  
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])  # grafico
  var.imp[i] <- ineq(x)        # GINI         
}

##-- Los mas predictores
sel.pr <- order(var.imp, decreasing=TRUE)[1:9]
windows()
par(mfrow=c(3,3))
for (i in sel.pr){
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])
}

##-- Los menos predictores
sel.pr <- order(var.imp, decreasing=FALSE)[1:9]
windows()
par(mfrow=c(3,3))
for (i in sel.pr){
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])
}

############################################################
# Intento de mejora 1: Seleccionar mejores predictores
############################################################
##-- Selecionar los mejores predictores
hist(var.imp)
sel.pr <- which(var.imp>0.5)
train1 <- train[,c(paste0('feat_',sel.pr),'target')]

##-- Aplicar bayes nuevamente
nb1 <- naiveBayes(target ~ ., train1, type="class")
preds <- predict(nb1, newdata = test)
t <- table(preds, test$target)
p.acierto1 <- sum(diag(t))/sum(t)
p.acierto1

############################################################
# Intento de mejora 2: Quitar variables correlacionadas
############################################################
##-- Sistema 1: Correlaciones grandes
high.corr <- which(cor.matrix>0.8,arr.ind = TRUE)
t.high.corr <- sort(table(as.numeric(high.corr))/2,decreasing=TRUE)
t.high.corr
sel.rm <- names(t.high.corr)[t.high.corr>=2] 
train2 <- train[,-which(names(train) %in% paste0('feat_',sel.rm))]

##-- Aplicar bayes nuevamente
nb2 <- naiveBayes(target ~ ., train2, type="class")
preds <- predict(nb2, newdata = test)
t <- table(preds, test$target)
p.acierto2 <- sum(diag(t))/sum(t)
p.acierto2

##-- Sistema 2: Suma de correlaciones
cor.sum <- sort(apply(cor.matrix,2,sum),decreasing=TRUE)
cor.sum
sel.rm <- names(cor.sum)[1:30]
train3 <- train[,-which(names(train) %in% sel.rm)]

##-- Aplicar bayes nuevamente
nb3 <- naiveBayes(target ~ ., train3, type="class")
preds <- predict(nb3, newdata = test)
t <- table(preds, test$target)
p.acierto3 <- sum(diag(t))/sum(t)
p.acierto3

##-- Sistema 3: R2 --> No mejora (0.57) --> Muy costoso.
R2.max <- 0.5
d <- train[,-94]
ite <- 1
while(R2.max>=0.5){
  p <- ncol(d)
  R2 <- c()
  for(i in 1:p){
    res <- names(d)[i]
    mod.lm <- lm(as.formula(paste(res,'~.')),d)
    s.mod.lm <- summary(mod.lm)
    R2[i] <- s.mod.lm$r.squared
    cat('Iteration:',ite,'Model:',i,'R2:',R2[i],'\n')
  }
  R2.max <- max(R2)
  d <- d[,-which.max(R2)]
  
  ite <- ite + 1
}

##-- Aplicar bayes nuevamente
train4 <- d
train4$target <- train$target
test4 <- test[,names(train4)]
nb4 <- naiveBayes(target ~ ., train4, type="class")
preds <- predict(nb4, newdata = test4)
t <- table(preds, test$target)
p.acierto4 <- sum(diag(t))/sum(t)
p.acierto4

############################################################
# Intento de mejora 3: Anyadir correccion de Laplace sobre datos no correlacionados --> No funciona
############################################################
##-- Aplicar bayes nuevamente
nb5 <- naiveBayes(target ~ ., train3, type="class",laplace=0.1)
preds <- predict(nb5, newdata = test)
t <- table(preds, test$target)
p.acierto5 <- sum(diag(t))/sum(t)
p.acierto5    # No cambia porque todos los predictores son continuos


############################################################
# Intento de mejora 4: Kernel en vez de distr. Gaussiana           --> No funciona
############################################################
library(naivebayes)
nb6 <- naive_bayes(target ~ ., train,usekernel = TRUE)
preds <- predict(nb6, newdata = test)
t <- table(preds, test$target)
p.acierto6 <- sum(diag(t))/sum(t)
p.acierto6

plot(nb6,45,arg.num=list(xlim=c(-1,1)))
abline(v=0)

############################################################
# Ejercicio 6-2
############################################################
##-- 1. Reduce el numero de variables predictoras a 20 y aplica el KNN con diferente numero de vecinos
## ?Mejora la capacidad predictiva medida por la proporci?n de aciertos?

##-- 2. Obten una medida probabilistica del KNN y promediala con la obtenida con el NB. Asigna a un grupo segun esta nueva probabilidad
## ?Mejora la capacidad predictiva medida por la proporci?n de aciertos? (15')

