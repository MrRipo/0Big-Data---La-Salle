############################################################
#
# Sesion 1 - MUBD
#
############################################################

############################################################
#
# R como calculadora
#
############################################################
2+3               # operaciones aritmeticas
3^2               # potencias
log(5)            # logaritmo natural
a <- 2*5          # asignaciones
a

############################################################
#
# Objetos por defecto
#
############################################################
pi                # valor de pi
letters           # letras minusculas
LETTERS           # letras mayusculas
month.name        # nombre de los meses


############################################################
#
# Tipos de objetos
#
############################################################
############################################################
# Vectores: serie ordenada de valores del mismo tipo --> c()
############################################################
##-- Ejemplos
v1 <- c(32,40,61,71,39)       # edades de personas (vector numerico)
v1
v2 <- 1:5                     # enteros del 1 al 5 (vector entero)
v2
v3 <- c("h","m","h","m","m")  # genero de personas (vector de caracteres)
v3
class(v1)
class(v2)
class(v3)

##-- Acceso y modificacion
v1[3]          # Ver 3r elemento
v1[3] <- 62    # Modificar 3r elemento
v1[3]          # Ver 3r elemento

v1[2:4]        # Ver los elementos del 2 al 4
v1[c(1,2,4)]   # Ver los elementos 1,2 y 4

############################################################
# Matrices: serie de columnas numericas ordenadas y de igual longitud --> matrix 
############################################################
##-- Ejemplos
m1 <- matrix(c(10,20,30,40),nrow=2,byrow=TRUE)
m1
m11 <- matrix(c(1,2,3,4,5,6,7,8),nrow=2,byrow=TRUE)
m11
m2 <- cbind(v1,v2) # assignar a columnes
m2
m3 <- rbind(v1,v2) # assignar a files
m3
##-- Acceso y modificacion
m1[2,1]       # Elemento de la fila 2, columna 1       
m1[2,]        # Elementos de la fila 2
m1[,1]        # Elementos de la columna 1
m1[2,1] <- 35 # Modificar el elemento de la fila 2, columna 1
m1

colnames(m2) <- c("edad","orden")  # Modificar el nombre de las columnas 
rownames(m2) <- letters[1:5]       # Modificar el nombre de las filas
colnames(m2)                       # Consultar el nombre de las columnas 
rownames(m2)                       # Consultar el nombre de las filas
m2

############################################################
# Data.frames: serie de columnas de cualquier tipo ordenadas y de igual longitud --> data.frame
############################################################
##-- Ejemplos
df1 <- data.frame(x=letters[1:10],y=1:10)   # Letras del 1 al 10 y numeros del 1 al 10
df1
df2 <- data.frame(v3,v1)
df2

##-- Acceso y modificacion
df1$x          # Columna llamada "x"
df1[,"x"]      # Columna llamada "x" (alternativa a la anterior opcion)
df1[,1]        # Primera columna     (alternativa a la anterior opcion)

df1[2,1]       # Elemento de la fila 2, columna 1
df1$x[2]       # Elemento de la fila 2, columna 1 (alternativa a la anterior opcion)

df1[2,]        # Elementos de la fila 2
df1[2,2] <- 35 # Modificar el elemento de la fila 2, columna 2
df1

df1[3,1] <- "k" # Atencion: Modificar el elemento de la fila 3, columna 1
df1

df1[3,1] <- "j" # Modificar el elemento de la fila 3, columna 1. Ahora si, porque "j" es un nivel del factor.
df1

############################################################
# Listas: cualquier combinacion de los elementos anteriores --> list 
############################################################
##-- Ejemplos 
l1 <- list(v1,m1,df1)
l1

##-- Acceso y modificacion
l1[1]               # Un corchete para simplificar la lista
class(l1[1])

l1[[1]]             # Doble corchete para acceder al elemento
class(l1[[1]])


##-- Ejercicio 1.1:
# 1. Crear un data.frame con los siguientes datos
#     marca modelo consumo
#      audi     A3     6.8
# volswagen   Polo     6.8
#       bmw     X5    12.5
#   renault megane     4.7
#      fiat  punto     5.0
# 2. Modificar el consumo del fiat punto a 4.5



############################################################
#
# Instrucciones utiles
#
############################################################
##-- Ayuda
?mean                         # interrogante para pedir ayuda de una funcion
??geometric                   # doble interrogante para pedir ayuda general --> equivalente a help.search("geometric")

##-- Busqueda
which(df1$x=='e')
which.max(df1$y)
which.min(df1$y)

##-- Programacion
if(v1[1]==32) print("La edad del primer individuo es 32")  # if --> condicion logica
for(i in 1:5) print(v1[i])                                 # Bucle 


##-- Gestion de objetos
ls()              # Objetos en memoria
object.size(v1)   # Tamanyo que ocupa un objeto en memoria
rm(v1)            # Borrar (remove) algun objeto en memoria
rm(list=ls())     # Borrar todos los objetos en memoria


############################################################
#
# Lectura de datos
#
############################################################
##-- De un fichero de texto
setwd('C:/Users/alber/Desktop/0Big Data - La Salle/Estadística/Curs 22-23/2- Inferencia estadística/')                                          # Poner aqui el path donde esten los datos separando las carpetas con "/"
datos <- read.table('Mobiles.txt',header=TRUE,sep=";",dec=".",na.strings="NA", stringsAsFactors = TRUE)

##-- Inspeccion de los datos
dim(datos)               # numero de filas y de columnas
head(datos)              # cabecera de los datos
summary(datos)           # descriptiva de los datos

##-- Hay missings no codificados correctamente. Se repite la lectura
datos <- read.table('Mobiles.txt',header=TRUE,sep=";",dec=".",na.strings=c("NA",""), stringsAsFactors = TRUE)
summary(datos)

##-- Ejercicio 1.2:
# 1. Leer el fichero flights.txt cambiando los parametros de lectura oportunos y guardar en datos2 (OJO con el simbolo de los missings)
# 2. Inspeccionar los datos: num. filas y columnas, visualizar los datos y descriptiva global
# 3. ¿Cuanto tiempo estuvo en el aire el vuelo 251 y con numero de cola N855UA?
setwd('C:/Users/alber/Desktop/0Big Data - La Salle/Estadística/Curs 22-23/2- Inferencia estadística/')                                          # Poner aqui el path donde esten los datos separando las carpetas con "/"
datos <- read.table('short_flights_test.txt',header=TRUE,sep=";",dec=".",na.strings="NA", stringsAsFactors = TRUE)
flights_mini <- read.delim("C:/Users/alber/Desktop/0Big Data - La Salle/Estadística/Curs 22-23/2- Inferencia estadística/flights_mini.txt")
View(flights_mini)
dim(flights_mini)
tailnum_col <- flights_mini[,9]
flight_col <- flights_mini[,10]

filter(flights_mini,tailnum=="N855UA")
filter(flights_mini,251)
filter(flights_mini,tailnum=="N855UA" & flight=="251")

s <- subset(flights_mini, tailnum=="N855UA" & flight==251)
s[13]
############################################################
#
# Escritura de datos a un fichero
#
############################################################
##-- A un fichero de texto
setwd('...')
write.table(datos,"Mobiles_test.txt",quote=FALSE,row.names = FALSE,sep="\t")

##-- Copiar el subconjunto de moviles Nokia a un fichero llamado Nokia_test
write.table(subset(datos,Brand=="Nokia"),"Nokia_test.txt",quote=FALSE,row.names = FALSE,sep="\t")


##-- Ejercicio 1.3:
# 1. Copiar los vuelos que estuvieron volando menos de 1 hora en un fichero llamado short_flights_test.txt
# [El fichero sin usar comillas para las categorias,sin nombre de columnas ni de filas y columnas separadas por tabuladores]


############################################################
#
# Descriptiva Univariante
#
############################################################
############################################################
# Variable numerica
############################################################
##-- Estadisticos principales de una variable
summary(datos$Bill)                  

##-- ¿Que representan los cuartiles (1st Qu.,Median(2nd Qu.),3rd Qu.)?
n <- dim(datos)[1]                   # numero de observaciones
sum(datos$Bill<=25.00,na.rm=TRUE)/n  # proporcion de observaciones por debajo del 1st Qu.
sum(datos$Bill<=35.00,na.rm=TRUE)/n  # proporcion de observaciones por debajo del 2nd Qu.
sum(datos$Bill<=60.00,na.rm=TRUE)/n  # proporcion de observaciones por debajo del 3rd Qu.

##-- Desviacion tipica y IQR
sd(datos$Bill)                       # atencion: no funciona por tener missings
sd(datos$Bill,na.rm=TRUE)            # se deben eliminar los missings
(iqr <- IQR(datos$Bill,na.rm=TRUE))  # rango intercuartilico: distancia entre Q1 y Q3

##-- Graficos
hist(datos$Bill,col="blue")
boxplot(datos$Bill,main="Facturas ($)")

##-- ?Que representan las lineas del boxplot?
abline(h=25.00,lty=2,col=1)          # 1st Qu.
abline(h=35.00,lty=2,col=2)          # 2nd Qu.
abline(h=60.00,lty=2,col=3)          # 3rd Qu.
abline(h=0,lty=2,col=4)              # Min (=bigote inferior)
abline(h=60.00+1.5*iqr,lty=2,col=5)  # Bigote superior de 1.5 IQR

############################################################
# Variable categorica
############################################################
##-- Tabla de frecuencias y proporciones
table(datos$Brand)                           # Tabla de frecuencias
prop.table(table(datos$Brand))               # Tabla de proporciones
round(100*prop.table(table(datos$Brand)),1)  # Tabla de porcentajes redondeados

# Ver https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/ para alternativas

##-- Diagrama de barras
barplot(table(datos$Brand))                  

# Diagrama de barras mejorado
windows()
barplot(sort(table(datos$Brand)),las=2)                  

##-- Ejercicio 1.4
# 1. Realiza la descriptiva de la variable tiempo de vuelo (air_time) del conjunto de datos flights.txt
# 2. Realiza la descriptiva de la variable categorica origen (origin) del conjunto de datos flights.txt

############################################################
#
# Descriptiva bivariante
#
############################################################
############################################################
# Numerica vs Numerica
############################################################
##-- Correlacion: relacion lineal entre variables (entre -1 y 1)
cor(datos$Age,datos$Bill,use="complete.obs")    # alternativa 1
with(datos,cor(Age,Bill,use="complete.obs"))    # alternativa 2

##-- Grafico bivariante (hacer ?par para ver parametros)
plot(datos$Age,datos$Bill)
plot(datos$Age,datos$Bill,xlab="Edad",ylab="Factura",main="Relacion de Edad y Gasto")
plot(Bill~Age,datos,xlab="Edad",ylab="Factura",main="Relacion de Edad y Gasto",
     col=4,pch=19,cex=0.8,las=1)                                                    
plot(jitter(log(Bill+0.1))~jitter(log(Age)),datos,
     col=4,pch=19,cex=0.8,las=1,
     xlab="Edad",ylab="Factura",main="Relacion de Edad y Gasto")  

############################################################
# Numerica vs Categorica
############################################################
##-- Descriptiva numerica estratificada: tapply
summary(datos$Bill)                             # Descriptiva global
tapply(datos$Bill,datos$PlanType,summary)       # Descriptiva estratificada segun plan
tapply(datos$Bill,datos$PlanType,sd,na.rm=TRUE) # Desviacion segun plan

##-- Boxplot estratificado
boxplot(datos$Bill~datos$PlanType)

############################################################
# Categorica vs Categorica
############################################################
##-- Descriptiva categorica vs categorica: table y prop.table
with(datos,table(PlanType,PrimaryUse))               # Frecuencias
with(datos,prop.table(table(PlanType,PrimaryUse)))   # Proporciones globales
with(datos,prop.table(table(PlanType,PrimaryUse),1)) # Proporciones por filas
with(datos,prop.table(table(PlanType,PrimaryUse),2)) # Proporciones por columnas

##-- Barplot y mosaicplot
windows(10,7)
par(mfrow=c(1,2),las=1)
with(datos,barplot(table(PrimaryUse,PlanType),col=1:3,legend=TRUE,args.legend=list(x='topleft')))
mosaicplot(PlanType~PrimaryUse,datos,col=1:3,main="Uso segun uso de contrato")


##-- Ejercicio 1.5
# 1. Realiza la descriptiva del retraso en la llegada segun el retraso en la salida. 
# Calcula la correlacion y haz el diagrama bivariante. Tambien usa la instruccion smoothScatter para hacer un plot alternativo 
# 
# 2. Realiza la descriptiva del retraso en la llegada segun el mes del anyo
# Haz el summary del retraso segun el mes y el boxplot estratificado
#
# Crea un subconjunto de datos2 llamado datos2.delay para aquellos vuelos con retraso en la llegada
# Vuelve a hacer la descriptiva anterior
# 
# 
# 3. Realiza la descriptiva de la hora de llegada segun la hora de salida (hour) como si fuese una categorica  
# 
# Crea una nueva variable dentro de datos 2 que sea una categorizacion de la variable hour en 3 categorias: [0-8],[9-16],[17-24]
# Repite la descriptiva anterior (mira la ayuda de la instruccion ?cut)
