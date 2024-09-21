############################################################
#
# MBD - Regresion logistica
#
############################################################
############################################################
# 
# Variables en el conjunto de datos:
# 
# status: Status of existing checking account 
# duration: Duration in month 
# credit.hist: Credit history 
# purpose: Purpose 
# credit.amo: Credit amount 
# savings: Savings account/bonds 
# employed.time: Present employment since 
# installment: Installment rate in percentage of disposable income 
# status.sex: Personal status and sex 
# debtors: Other debtors / guarantors 
# residence.time: Present residence since 
# property: Property 
# age: Age in years 
# installment2: Other installment plans 
# housing: Housing 
# num.credits: Number of existing credits at this bank 
# job: Job 
# num.people: Number of people being liable to provide maintenance for 
# telephone: Telephone 
# foreign: foreign worker 
# y: credit paid
#
############################################################

############################################################
#
# Parte 1. Construir modelo
#
############################################################
##-- Borrar los objetos en memoria
rm(list=ls())

############################################################
# Lectura de datos y inspeccion
############################################################
##-- Leer los datos
setwd('...')                                                                       # directorio de trabajo
datos <- read.table('bank0_train.txt',header=TRUE,sep=';',stringsAsFactors = TRUE) # lectura de los datos

##-- Visualizar los datos y descriptiva con summary
View(datos)             # Ver los datos
summary(datos)          # Descriptiva de todas las variables



############################################################
# Descriptiva bivariante
############################################################
##-- Variables categoricas
sapply(datos,class)
var.cat <- which(sapply(datos,class)=="factor" & names(datos)!="y")  # variables que son categoricas (factores)
##--Mosaicplot para las categoricas
for(vc in var.cat)  mosaicplot(datos[,vc]~datos$y,main=names(datos)[vc],col=2:3,las=1)


##--Densidad para las variables numericas
var.num <- which(sapply(datos,class) %in% c("numeric","integer"))    # variables que son numericas
for(vn in var.num) cdplot(datos$y~datos[,vn],main=names(datos)[vn],n=512)


############################################################
# Establecer categorias de referencia en variables categoricas
############################################################
##-- La instruccion relevel cambia la categoria de referencia en las variables categoricas
##-- Se escoge la referencia no por criterios estadisticos, sino de interpretabilidad
datos$status <- relevel(datos$status,ref="no checking account")
datos$credit.hist <- relevel(datos$credit.hist,ref="no credits taken/all credits paid back duly")
datos$purpose <- relevel(datos$purpose,ref="others")
datos$savings <- relevel(datos$savings,ref="unknown/no savings account")
datos$employed.time <- relevel(datos$employed.time,ref="unemployed")
datos$status.sex <- relevel(datos$status.sex,ref="male:single")
datos$debtors <- relevel(datos$debtors,ref="none")
datos$property <- relevel(datos$property,ref="unknown/no property")
datos$installment2 <- relevel(datos$installment2,ref="none")
datos$housing <- relevel(datos$housing,ref="for free")
datos$job <- relevel(datos$job,ref="unskilled - resident")
datos$telephone <- relevel(datos$telephone,ref="none")
datos$foreign <- relevel(datos$foreign,ref="no")

############################################################
# Estimar modelos con todas las variables
############################################################
##-- Modelo completo
mod.glm0 <- glm(y~.,datos,family=binomial)    # estimacion del modelo
summary(mod.glm0)                             # resumen del modelo

############################################################
# Seleccion automatica
############################################################
##-- Missings
apply(apply(datos,2,is.na),2,sum)             # cuantos missings tiene cada variable --> Step no se puede aplicar con missings

##-- Funcion step (requiere que no haya missings si ha de quitar una variable que tenga)
mod.glm1 <- step(mod.glm0)
summary(mod.glm1)

##-- Importancia de las categorias/covariables segun su significacion estadistica
# install.packages('caret')
library(caret)
View(varImp(mod.glm1))

############################################################
# Validacion 
############################################################
##-- Por inspeccion visual
br <- quantile(fitted(mod.glm1),seq(0,1,0.1))                                # Se crean los puntos de corte para los intervalos (br) de las probabilidades predichas
int <- cut(fitted(mod.glm1),br)                                              # Se crea una variable con el intervalo al que pertenece cada individuo
obs <- tapply(mod.glm1$y,int,sum)                                            # Los pagos observados en cada intervalo
exp <- tapply(fitted(mod.glm1),int,sum)                                      # Los pagos esperados en cada intervalo  
plot(1:10+0.05,exp,type='h',xlab="Intervalos",ylab="Frecuencias",lwd=2)      # Grafico de los pagos esperados
lines(1:10-0.05,obs,type='h',col=2,lwd=2)                                    # Se anyade los pagos observados
legend("topleft",c("Pagan - esperados", "Pagan - observados"),lwd=2,col=1:2) # Se anyade una leyenda

##--test de Hosmer-Lemeshow
# install.packages('ResourceSelection')
library(ResourceSelection)
hoslem.test(mod.glm1$y, fitted(mod.glm1))  # si el p-valor es inferior a 0.05 quedaria en duda el modelo                              

############################################################
# Estimacion de un Odds Ratio
############################################################
##-- Variable categorica
exp(mod.glm1$coef["foreignyes"])    # Los extranjeros tienen un oddsratio de 0.17 respecto a los no extranjeros. Es decir, las probabilidades de pagar son un 0.17 la de los nacionales 

##-- Variable numerica
exp(mod.glm1$coef["age"])           # Por cada anyo de mas de la persona se incrementa en un 2% (aprox) la probabilidad de que acabe pagando

##--Intervalos de confianza
IC <- confint(mod.glm1)             # Intervalos de  confianza para los coeficientes
round(exp(IC),2)                    # Intervalos de confianza para los ORs redondeados a 2 decimales

############################################################
# Estimacion de la probabilidad de pago
############################################################
##-- Probabilidades predichas
pr <- predict(mod.glm1,datos,type="response")
pr

##--Probabilidad maxima y minima
pos.max <- which.max(pr)        # posicion del individuo con mayor probabilidad de pagar
pr[pos.max]                     # probabilidad de dicho individuo 
datos$y[pos.max]                # pago?

pos.min <- which.min(pr)        # posicion del individuo con menor probabilidad de pagar
pr[pos.min]                     # probabilidad de dicho individuo 
datos$y[pos.min]                # pago?

boxplot(pr~y,datos)

############################################################
# Curva ROC y AUC
############################################################
##-- Instalar libreria AUC
# install.packages('AUC')
library(AUC)

##-- Curva ROC y AUC
pr <- predict(mod.glm1,type='response')
roc.curve <- roc(pr,datos$y)
plot(roc.curve)
AUC::auc(roc.curve)

############################################################
# Calibracion del modelo
############################################################
# install.packages('PresenceAbsence')
library(PresenceAbsence)
df.calibra <- data.frame(plotID=1:nrow(datos), Observed = as.numeric(datos$y)-1  , Predicted1 = pr)
calibration.plot(df.calibra, N.bins = 10,ylab='Observed probabilities')
detach('package:PresenceAbsence')

############################################################
#
# Parte 2. Testear resultados
#
############################################################
test <- read.table('bank0_test.txt',header=TRUE,sep=';', stringsAsFactors = TRUE)

############################################################
# Calcular predicciones y compararlas con valores reales
############################################################
pr <- predict(mod.glm1,test)   # probabilidades predichas
boxplot(pr~test$y)             # Como son estas probabilidades en ambos grupos de respuesta
roc.curve <- roc(pr,test$y)    # Calculo de la curva ROC
plot(roc.curve)                # Dibujo de la curva ROC
AUC::auc(roc.curve)                 # AUC de la curva ROC

##-- Sensibilidad y especificidad para un punto de corte concreto
s <- AUC::sensitivity(pr,test$y)
e <- AUC::specificity(pr,test$y)
a <- AUC::accuracy(pr,test$y)
df <- data.frame(cutpoints=s$cutoffs,sens=s$measure,esp=e$measure,acc=a$measure)
View(round(df,3))

##-- Escoger un punto de corte --> Matriz de confusion
test$doy.credito <- ifelse(pr>0.5,'si','no')  # Doy credito a aquellos con un probabilidad predicha de pagar superior a 0.5
with(test,table(doy.credito,y))
with(test,round(100*prop.table(table(doy.credito,y),1),1))

 

