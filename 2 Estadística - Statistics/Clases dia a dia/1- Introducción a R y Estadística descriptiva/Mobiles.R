## Instalar y cargar paquete MindOnStats
install.packages("MindOnStats")
library(MindOnStats)

## Cargar datos
setwd("C:/Users/alber/Desktop/0Big Data - La Salle/Estadística/Curs 22-23/4- Modelo respuesta binaria/")
Mobiles <- read.delim("Mobiles.txt")
View(Mobiles)

## Inspeccionar datos
dim(Mobiles) # Número de filas y columnas
names(Mobiles) # Nombre de las variables
summary(Mobiles) # Descriptiva univariante de las variables

# Paràmetres estadístics
Mobiles
mean(Mobiles$Age)
var(Mobiles$Age) 
sd(Mobiles$Age) 
IQR(Mobiles$Age)

# Boxplot simple y más elaborado 
par(mfrow=c(1,2)) # Dos ventanas gráficas
boxplot(Mobiles$Age) # Boxplot simple
boxplot(Mobiles$Age,col="blue",horizontal=TRUE,main="Edad",las=1) # Complejo

Q1 <- 18
Q3 <- 24
Bot <- Q1-1.5*IQR(Mobiles$Age)
Bot
Top <- Q3+1.5*IQR(Mobiles$Age)
Top

## Histograma simple y más elaborado 
par(mfrow=c(1,2)) 
hist(Mobiles$Age)
hist(Mobiles$Age,col=3,xlab="Años",main="Edad",freq=FALSE) # True->Freqüència absoluta, False->

## Tabla de frecuencias, proporciones y diagrama de barras 
table(Mobiles$PlanType) 
Plan Prepaid
5 76 98 
prop.table(table(Mobiles$PlanType)) 
Plan Prepaid
0.02793296 0.42458101 0.54748603 
barplot(table(Mobiles$PlanType))


# Tabla contingencia
with(Mobiles,table(PlanType,PrimaryUse)) 
with(Mobiles,prop.table(table(PlanType,PrimaryUse),1))

##-- Cat vs Cat - Barplot 
par(mfrow=c(1,2)) 
with(Mobiles,barplot(table(PrimaryUse,PlanType)[,3:1],col=1:4,legend=TRUE)) 
with(Mobiles,barplot(table(PrimaryUse,PlanType)[,3:1],col=1:4,legend=TRUE,beside=TRUE))

par(mfrow=c(1,1)) 
Mobiles.Clean <- droplevels(subset(Mobiles,Mobiles$PlanType!="",drop=TRUE)) 
with(Mobiles.Clean,mosaicplot(PrimaryUse~PlanType,col=2:3,las=1, main="Uso 
según plan"))

##-- Num vs Cat - Boxplot estratificado
par(mfrow=c(1,1))
boxplot(Bill~PlanType,Mobiles,col="steelblue",main="Factura según plan")


