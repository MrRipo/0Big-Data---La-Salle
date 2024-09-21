############################################################
#
# Sesion 2 - MUBD
#
############################################################

############################################################
#
# Lectura de datos
#
############################################################
setwd("C:/Users/alber/Desktop/0Big Data - La Salle/Estadística/Curs 22-23/4- Modelo respuesta binaria/")
datos <- read.table('Mobiles.txt',header=TRUE,sep=";",dec=".",na.strings=c("NA",""), stringsAsFactors = TRUE)
View(datos)

############################################################
#
# Estimacion de la media poblacional
#
############################################################
summary(datos$Bill)      # Descriptiva de los datos numerica
hist(datos$Bill)         # Descriptiva de los datos grafica
t.test(datos$Bill)       # Estimacion de la media con su IC y hipotesis de que la media es 0 (por defecto)
t.test(datos$Bill,mu=50) # Hipotesis de que la media poblacional es 50

############################################################
#
# Estimacion de la desviacion poblacional (Opcional)
#
############################################################
##-- Usando formula ya que no hay una funcion en el paquete base que lo calcule
##-- Formula: ver diapositiva   
s2 <- var(datos$Bill,na.rm=TRUE)         # varianza de las facturas
n <- sum(!is.na(datos$Bill))             # tamano muestral. Valores que no son missings
chi2 <- qchisq(0.025,n-1)                # cuantil 0.025 de una chi-cuadrado con (n-1) grados de libertad
chi1 <- qchisq(0.975,n-1)                # cuantil 0.975 de una chi-cuadrado con (n-1) grados de libertad
(LI <- (n-1)*s2/chi1)                    # Limite inferior del IC de la varianza
(LS <- (n-1)*s2/chi2)                    # Limite superior del IC de la varianza  
sqrt(c(LI,LS))                           # IC para la desviacion tipica (sqrt = raiz cuadrada)

##-- ¿Se cumple la Normalidad de los datos? --> No se cumple
qqnorm(datos$Bill)                       # Distribucion empirica vs teorica
qqline(datos$Bill,col=2)                 # Los puntos al no situarse sobre la linea, no son Normales

##-- Alternativa: bootstrap
##-- Generamos 10000 muestras de la muestra original con reposicion
set.seed(12345)                                                        # Se fija una semilla para que todas las simulaciones sean iguales
m <- replicate(10000, sd(sample(datos$Bill, replace=TRUE),na.rm=TRUE)) # Se generan 10000 muestras y para cada una de ellas se calcula la desviacion
hist(m)                                                                # Histograma de las desviaciones 
quantile(m,probs=c(0.025, 0.975))                                      # Se calcula el IC95% cogiendo los cuantiles 0.025 y 0.975

############################################################
#
# Estimacion de una proporcion
#
############################################################
##-- Probabilidad de que un estudiante tenga una tarjeta prepago
(e <- sum(datos$PlanType=='Prepaid',na.rm=TRUE)) # Numero de estudiantes con tarjeta prepago
(n <- sum(!is.na(datos$PlanType)))               # Numero de estudiantes totales que no tienen missing esta variable (OJO: no n <- length(datos$PlanType))
prop.test(e,n)                                   # Estimacion de una probabilidad

##-- Funcion para pocos datos (no asume normalidad)
binom.test(e,n)

##-- Ejercicio 2.1:
# 1. Leer el fichero flights.txt y guardar en datos2:
# 2. Estimar el IC95% para la media del tiempo de retraso en las llegadas (arr_delay) con la instruccion t.test
# 3. (Opcional) Verificar que la variable NO es Normal con la instruccion qqnorm y estimar el IC90% (no IC95%) para la 
#    desviacion tipica del tiempo de retraso en las llegadas con bootstrap
# 4. Estimar el IC99% (no IC95%) para la proporcion de vuelos que llegan con retraso

############################################################
#
# Comparacion de 2 medias
#
############################################################
############################################################
# Independientes
############################################################
boxplot(Bill~PlanType,datos)                                     # Descriptiva de los datos (boxplot)
stripchart(Bill~PlanType,datos)                                  # Descriptiva de los datos (stripchart)
t.test(Bill~PlanType,datos,var.equal=TRUE)                       # Comparacion de medias
t.test(Bill~PlanType,datos,var.equal=TRUE,conf=0.99)             # Comparacion de medias. IC(99% confianza)
t.test(Bill~PlanType,datos,var.equal=TRUE,alternative="greater") # Comparacion de medias. Test unilateral.
# Ver este aplicativo: http://onlinestatbook.com/stat_sim/sampling_dist/

############################################################
# Datos apareados: 
# Sex: a factor with levels F M
# HrsSleep: how many hours of sleep the subject gets per night
# SQpick: a factor with levels Q S. Each subject was presented with two letters (S and Q), and asked to pick one. This variable indicates which letter the subject picked.
# Height: height in inches
# RandNumb: a numeric vector: Each subject was asked to choose randomly an integer from 1 to 10.
# Fastest: highest speed, in mph, at which subject has ever driven a car
# RtSpan: span of the right hand, in centimeters.
# LftSpan: span of the left hand, in centimeters.
# Form: a factor with levels QorS SorQ. The order of presentation of the S and Q options to the subject varied from one survey form to another. This variable indicates which letter was presented first on the form.
############################################################
##-- Cargar nuevos datos para prueba apareada
PennState1 <- read.table("PennState1.txt",header=TRUE,sep="\t")
View(PennState1)                                        # Descripcion: http://sites.stat.psu.edu/~dhunter/250h/data/readme.html

##-- Comparar los tamanos de las palmas izquierda y derecha de la mano
boxplot(PennState1$LftSpan,PennState1$RtSpan)           # Descriptiva de los datos (boxplot)
plot(RtSpan~LftSpan,PennState1)                         # Descriptiva de los datos (bivariante)
abline(0,1)                                             # Trazar bisectriz
with(PennState1,t.test(RtSpan,LftSpan,var.equal=TRUE))  # OJO! Esto esta mal --> muestras no son independientes
with(PennState1,t.test(RtSpan,LftSpan,paired=TRUE))     # Comparacion de medias en muestras apareadas

##--Graficos para datos apareados
install.packages("PairedData")                          # Instalar paquete para graficos de datos apareadaos
library(PairedData)                                     # Cargar paquete

?paired
?plot.paired

##-- Graficos descriptivos para datos apareados
x=PennState1$LftSpan
y=PennState1$RtSpan
p.data <- paired(x,y)
plot(p.data,type="correlation")
plot(p.data,type="BA")
plot(p.data,type="McNeil")
plot(p.data,type="profile")

##-- Eliminando los valores muy similares para visualizar mejor
PennState1.sub <- subset(PennState1,round(RtSpan)!=round(LftSpan))
x=PennState1.sub$LftSpan
y=PennState1.sub$RtSpan
p.data <- paired(x,y)
plot(p.data,type="correlation")
plot(p.data,type="BA")
plot(p.data,type="McNeil")
plot(p.data,type="profile")


##-- Ejercicio 2.2:
# 1. Crea una nueva variable llamada origin2 que valga "JFK" si el aeropuerto de origen es el JFK
#    o "Other" en caso de que sea otro aeropuerto con la instruccion ifelse (?ifelse)
# 2. Compara las medias de retraso en las llegadas entre el aeropuerto JFK y el resto
# 3. Compara las medias de retrasos entre salidas y llegadas para todos los aeropuertos. Obten el IC
#    con un 99.9% de confianza
# 4. Con las funciones paired y plot del paquete PairedData dibuja el grafico de Bland-Altman (BA) 
#    para los 10000 primeros vuelos con retraso en la llegada inferior a 180 minutos


############################################################
#
# Comparacion de 2 varianzas (Opcional)
#
############################################################
boxplot(Bill~PlanType,datos)             # Descriptiva de las facturas
(vt <- var.test(Bill~PlanType,datos))    # Comparacion de varianzas en muestras independientes
sqrt(vt$conf.int)                        # Intervalo para el cociente de desviaciones                               

##-- Ejercicio 2.3:
# 1. Realiza el boxplot estratificado de los tiempos de llegada segun si el aeropuerto de partida era el JFK o no
# 2. Realiza otro boxplot unicamente sobre los datos con retrasos positivos y convierte la escala 
# vertical en logaritmica (usa el parametro log='y')
# 3. Compara las varianzas y calcula el intervalo de confianza del 99% con la funcion var.test


############################################################
#
# Comparacion de 2 proporciones
#
############################################################
##-- Estudio del tipo de contrato segun el genero
(t1 <- with(datos,table(Gender,PlanType)))   # Tabla de frecuencias 2x2 (genero vs contrato)
mosaicplot(PlanType~Gender,datos,col=2:3)    # Mosaicplot (genero vs contrato)
prop.test(t1)                                # Comparacion de proporciones    

##-- Ejercicio 2.4:
# 1. Crea tres variables dentro de datos2 (datos de vuelos):
# - date: una variable que contenga la fecha completa (usa las funciones as.Date y paste). Las fechas han de tener un formato YYYY-MM-DD
# - weekend: variable que valga TRUE si el dia es fin de semana y false en caso contrario (usa la funcion weekdays)
# - arr_delay_cat: variable que valga TRUE si el vuelo tuvo retraso
# Pistas:
# datos2$date <- with(datos2,as.Date(paste(...,"-",...,"-",...,sep="")))
# datos2$weekend <- weekdays(datos2$date) %in% c("...","...")
# datos2$arr_delay_cat <- ...
#
# 2. Realiza un mosaicplot de los vuelos con retraso (arr_delay_cat) y si es fin de semana (weekend)
#
# 3. Realiza un test (prop.test) para comprobar si la proporcion de vuelos sin retraso es igual 
# en fines de semana y el resto de dias




############################################################
#
# Errores tipo I y tipo II (Opcional)
#
############################################################
############################################################
# Simulacion Error Tipo I: Proporcion de resultados positivos sin haber diferencias
############################################################
n <- 100            # Tamanyo muestral
nsim <- 1000        # numero de simulaciones
p <- c()            # vector donde se guardan los p-valores de las simulaciones

##-- Realizacion de las simulaciones
set.seed(12345)
for(i in 1:nsim){
  RetrasosPrat <- rnorm(n,mean=10,sd=3)                  # Generacion de retrasos con media 10 y desviacion 3
  RetrasosBarajas <- rnorm(n,mean=10,sd=3)               # Generacion de retrasos con media 10 y desviacion 3
  tt <- t.test(RetrasosPrat,RetrasosBarajas,var.equal=TRUE)
  p[i] <- tt$p.value
}
sum(p<0.05)/nsim    # alpha

############################################################
# Simulacion Error Tipo II (beta): Proporcion de resultados negativos habiendo diferencias
############################################################
##-- Ejercicio 2.5:
# 1. Por simulacion, calcula el error de tipo II que se tendria con un tamanyo muestral de 393 
# observaciones en cada grupo para una diferencia de medias de 0.2 y una varianza igual a 1 
# ambos grupos. 
# n <- ...            # Tamanyo muestral
# nsim <- ...         # numero de simulaciones
# p <- c()            # vector donde se guardan los p-valores de las simulaciones
# 
# ##-- Realizacion de las simulaciones
# set.seed(12345)
# for(i in 1:nsim){
#   RetrasosPrat <- rnorm(...)
#   RetrasosBarajas <- rnorm(...)
#   tt <- t.test(...)
#   p[i] <- ...
# }
# sum(...)/nsim    # beta (probabilidad de error tipo II)
#
# 2. La potencia de un analisis es igual a (1-beta). Calcula la potencia en la situacion anterior
# con la funcion power.t.test y comprueba si cuadra con los resultados de la simulacion
# power.t.test(...)