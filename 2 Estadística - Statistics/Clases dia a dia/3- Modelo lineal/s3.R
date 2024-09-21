############################################################
#
# MBD - Modelo lineal
#
############################################################



############################################################
# Lectura de datos y descriptiva
############################################################
##-- Lectura de los datos 
setwd('...')
datos <- read.table('Concrete_train.txt',sep="\t",header=TRUE)

##--Inspeccion de los datos
dim(datos)                  # num filas y columnas
View(datos)                 # ver datos
summary(datos)              # descriptiva de todas las variables 
boxplot(datos,las=2)        # boxplot de todas las variables

##-- Explorar todos los pares de datos
pairs(datos)                # descriptiva bivariante


##-- Descriptiva bivariante para la variable cemento
plot(Strength~Cement,datos)                       # puntos
with(datos,lines(lowess(Strength~Cement),col=2))  # estimacion no parametrica de la relacion

##-- Descriptiva bivariante para todas las variables
par(mfrow=c(2,4))
for(i in 1:8){
  plot(datos$Strength~datos[,i],main=names(datos)[i],xlab=names(datos)[i],ylab="Strength")
  with(datos,lines(lowess(Strength~datos[,i]),col=2))
}

############################################################
# Ajuste del modelo lineal simple
############################################################
mod.lm0 <- lm(Strength~Cement,datos)
summary(mod.lm0)
par(mfrow=c(1,1))
plot(Strength~Cement,datos)
abline(mod.lm0,col="red")

############################################################
# Ajuste del modelo multivariado
############################################################
mod.lm1 <- lm(Strength~Cement + BlastFurnaceSlag + FlyAsh +      
                       Water + Superplasticizer + CoarseAggregate +
                       FineAggregate + Age,datos)                   # Ajuste del modelo
mod.lm1 <- lm(Strength~.,datos)                                     # Instruccion equivalente a la anterior
mod.lm1                                                             # Ver coeficientes
summary(mod.lm1)                                                    # Resumen del modelo

############################################################
# Seleccion de variables 
############################################################
mod.lm2 <- step(mod.lm1)                   # Seleccionar variables
summary(mod.lm2)                           # Modelo con variables seleccionadas

############################################################
# Validacion 
############################################################
par(mfrow=c(2,2))                          # ventana para 4 gr?ficos
plot(mod.lm2)                              # graficos para valorar premisas

############################################################
# Nueva descriptiva
############################################################
## Alternativa 1. Residuos versus variables predictoras
par(mfrow=c(2,4))
for(i in 1:8){
  plot(resid(mod.lm2)~datos[,i],main=names(datos)[i],xlab=names(datos)[i],ylab="Residuals")
  with(datos,lines(lowess(resid(mod.lm2)~datos[,i]),col=2))
}

## Alternativa 2. Residuos versus variables predictoras
# Instalacion del paquete car
install.packages('car')
library(car)
residualPlots(mod.lm2)

############################################################
# Transformaciones polinomicas sobre las predictoras con poly 
############################################################
##- Con poly se incluyen terminos polinomicos de orden mayor
mod.lm4 <- lm(Strength ~ poly(Cement,2) + poly(BlastFurnaceSlag,2) + poly(FlyAsh,2)+
                         poly(Water,2) + poly(Superplasticizer,2) + CoarseAggregate +
                         poly(FineAggregate,2) + poly(Age,2),datos)
summary(mod.lm4)

mod.lm5 <- step(mod.lm4)
summary(mod.lm5)

############################################################
# Colinealidad
############################################################
vif(mod.lm5)     # Valores superiores a 5 indica variables que se han de eliminar

############################################################
# Validacion 
############################################################
par(mfrow=c(2,2))
plot(mod.lm5)
residualPlots(mod.lm5)

############################################################
# Transformacion Boxcox sobre la respuesta
############################################################
##-- Calculamos la lambda para la transformacion Boxcox
par(mfrow=c(1,1))
bc <- boxCox(mod.lm5)
lamb <- powerTransform(mod.lm5)$lambda # Similar a --> bc$x[which.max(bc$y)]  
datos$Strength2 <- datos$Strength^lamb
mod.lm6 <- lm(Strength2~poly(Cement,2) + poly(BlastFurnaceSlag,2) + poly(FlyAsh,2) +
                        poly(Water,2) + poly(Superplasticizer,2) +
                        poly(FineAggregate,2) + poly(Age,2), datos)
summary(mod.lm6)

############################################################
# Validacion 
############################################################
par(mfrow=c(2,2))
plot(mod.lm6)

## Y transformaciones logaritmicas sobre las predictoras? Probadlo en casa!

############################################################
# Observaciones influyentes
############################################################
influenceIndexPlot(mod.lm5)           # Las observaciones 81, 147 tienen mucha influencia a posteriori. La 248 est? muy mal explicada por el modelo. La 65 y 81 tienen mucha influencia a priori.
par(mfrow=c(1,1))
influencePlot(mod.lm5)                # Eliminar las 2 mas influyentes a posteriori (81 y 147) y la peor explicada (248)
obs.rm <- c(81,147,248)
View(datos[obs.rm,])
col.points <- rep(rgb(0,0,0,0.1),nrow(datos))        # Vector de colores
col.points[obs.rm] <- 2:4                            # Colores distintos para las observaciones influyentes
pairs(datos[,-10],col=col.points,pch=19,cex=0.8)     # Dibujo por pares de las observaciones influyentes
datos.rm <- datos[-obs.rm ,]                         # Nos creamos un nuevo data.frame sin estas observaciones

##--Ajuste del modelo sin observaciones influyentes
mod.lm7 <- lm(Strength~poly(Cement,2) + poly(BlastFurnaceSlag,2) + poly(FlyAsh,2)+
                       poly(Water,2) + poly(Superplasticizer,2)  +
                       poly(FineAggregate,2) + poly(Age,2),datos.rm)

##--Comparar nuevo modelo con antiguo
summary(mod.lm7)   # Modelo nuevo sin outliers y observaciones influyentes
summary(mod.lm5)   # Modelo antiguo

##-- Modelo final
mod.final <- mod.lm5

##-- Efectos
library(effects)
plot(allEffects(mod.final))


############################################################
# Testearlo con nuevos datos
############################################################
# setwd('...')
test <- read.table('Concrete_test.txt',sep="\t",header=TRUE)

##-- Volver a hacer transformaciones en test (por si la necesito)
test$Strength2 <- test$Strength^lamb

##-- Predicciones
pr <- predict(mod.final,test)   # Predicciones para los nuevos valores
par(mfrow=c(1,1))           
plot(pr,test$Strength,asp=1)    # Predicciones vs valores observados 
abline(0,1,col=2,lwd=2)         # Bisectriz

##--EQM (Error Cuadratico Medio)
n <- dim(test)[1]                                       # Tamanyo muestral
EQM <- sum((pr-test$Strength)^2)/n                      # Error Cuadratico Medio
sqrt(EQM)                                               # Incertidumbre a posteriori                                                
sd(test$Strength)                                       # Incertidumbre a priori --> El modelo me reduce a la mitad la incertidumbre en las predicciones
summary(test$Strength)
