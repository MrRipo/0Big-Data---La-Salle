############################################################
#
# Sesion 9 - MUBD
#
############################################################
##-- Instalar paquetes
install.packages('data.table')
install.packages('biganalytics')
install.packages('bigmemory')
install.packages('biglm')
install.packages('snowfall')



############################################################
#
# data.table
#
############################################################
##-- Cargar paquetes
library(data.table)

##-- Leer un fichero de datos en una pagina url
url <- 'https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv'
system.time(fly.df <- read.csv(url(url)))
system.time(fly.dt <- fread(url))

##-- Gestion de la memoria
format(object.size(fly.df),units = "MB") 
format(object.size(fly.dt),units = "MB") 

format(object.size(fly.df[-1,]),units = "MB")
format(object.size(fly.dt[-1]),units = "MB")

##-- Gran ventaja del data.table --> Es data.table y data.frame
class(fly.dt)

##-- Seleccionar elementos (SQL: WHERE --> Primera componente)
fly.dt.AA <- fly.dt[carrier=='AA']
fly.dt.AA

##-- Seleccionar variables (SQL: SELECT --> Segunda componente)
fly.dt.dates <- fly.dt[,.(year,month,day)]
fly.dt.dates

##-- Agrupar (SQL: GROUP BY --> Parametro by) y contar (SQL: COUNT --> .N)
fly.dt.carrier <- fly.dt[,.N,by=carrier]
fly.dt.carrier

##-- Agrupar y hacer la media y la mediana de los tiempos de vuelo
fly.dt.airtime <- fly.dt[,.(media=mean(air_time),mediana=median(air_time)),by=carrier]
fly.dt.airtime

##-- Crear una nueva variable que sea la diferencia entre los retrasos de la llegada y de la salida
fly.dt[,dif:=arr_delay-dep_delay]
fly.dt

##-- Eliminar una variable
fly.dt[,dif:=NULL]
fly.dt

##-- Ordenar por una clave (p.ej. retrasos en la llegada)
setkey(fly.dt,arr_delay)
fly.dt

##-- Ejercicio 9.1. Da la velocidad media en km/h de los vuelos segun companyia y ordenalos segun este valor


############################################################
#
# biglm
#
############################################################
# Inconvenientes de biglm o bigglm:
# No estima el modelo si algun coeficiente no es estimable
# step no es aplicable a bigglm
# Se debe escribir la formula completa
# Necesita que la respuesta sea 0/1 o FALSE/TRUE en el caso de respuesta binaria

##-- Cargar paquete
library(biglm)

##-- Lectura de datos
setwd('...')
datos2 <- read.table('p1b_train.csv',header=TRUE,sep=';', stringsAsFactors = TRUE)
datos2$id <- NULL
datos2$loan <- NULL                      # se elimina porque no se ejecuta con bigglm 
datos2$y <- datos2$y=='yes'              # Transformo respuesta en TRUE or FALSE

##-- Tiempo de ejecucion del modelo
system.time(mod.glm1 <- glm(y~.,datos2,family=binomial))
form <- as.formula(paste0('y~',paste0(names(datos2)[-which(names(datos2)=='y')],collapse='+')))
system.time(mod.glm2 <- bigglm(form,datos2,family=binomial()))
summary(mod.glm2) # Converge para la mayoria de coeficientes a pesar del warning

##-- Con un objeto 10 veces mayor...
bigdatos2 <- rbind(datos2,datos2,datos2,datos2,datos2,datos2,datos2,datos2,datos2,datos2)
system.time(mod.glm1 <- glm(y~.,bigdatos2,family=binomial))
system.time(mod.glm2 <- bigglm(form,bigdatos2,family=binomial()))
round(cbind(coef(mod.glm1),coef(mod.glm2)),3)

##-- Ejercicio 9.2. Haz las predicciones de la respuesta en el mismo conjunto de entrenamiento con predict y
##-- compara los tiempos con ambos metodos

############################################################
#
# biganalytics
#
############################################################
##-- Cargar paquete
library(biganalytics)
library(bigmemory)

##-- Lectura de datos
setwd('...')
datos3 <- read.table('products.csv',header=TRUE,sep=';', stringsAsFactors = TRUE)
datos3$id <- datos3$target <- NULL

##-- Comparar tama?os de objetos
datos3.mat <- as.big.matrix(datos3)
format(object.size(datos3),units="MB")
format(object.size(datos3.mat),units="Kb")

##-- kmeans tradicional vs kmeans
set.seed(12345)
system.time(km1 <- kmeans(datos3,3,nstart=10))
system.time(km2 <- bigkmeans(datos3.mat,3,nstart=10))
table(km1$cluster,km2$cluster)  # Verificar que hay resultados similares

##-- Ejercicio 9.3. Compara lo que hace la instruccion summary aplicada a los dos objetos datos3 y datos3.mat. En que se diferencian?


############################################################
#
# snowfall
#
############################################################
##-- Cargar paquete
library(snowfall)
library(party)
library(randomForest)

##-- Inicializar cluster
sfInit(parallel=TRUE, cpus=8)
sfLibrary(party)
sfLibrary(randomForest)

##-- Comparar rendimiento
system.time(result <- lapply(1:5000000,log))    # mirar rendimiento de cpu
system.time(result <- sfLapply(1:5000000, log)) # mirar rendimiento de cpu 
gc()

##-- Aplicabilidad
kmeans2 <- function(grupos,datos){kmeans(datos,grupos)}
km <- list()
system.time(km1 <- lapply(2:10,kmeans2,datos3))
system.time(km2 <- sfLapply(2:10,kmeans2,datos3))

##-- Ejercicio 9.4. Influye el parametro mtry en la prediccion en los random forest? Utiliza la funcion sfLapply
# y la funcion randomForest2 para saberlo
train <- read.table('products_train.csv',header=TRUE,sep=';',stringsAsFactors = TRUE)
test <- read.table('products_test.csv',header=TRUE,sep=';',stringsAsFactors = TRUE)[1:1000,]
train$id <- NULL
randomForest2 <- function(mtry,train,test){
  rf <- randomForest(target~.,train,ntree=10,mtry = mtry,trace=TRUE)
  class <- predict(rf,newdata=test,type="response")   
  t <- table(class,test$target)
  return(sum(diag(t))/sum(t))
}
MTRY <- seq(10,90,20)

...


# stop cluster
sfStop()

############################################################
#
# ggplot2 --> Graphics
#
############################################################
##-- Cargar paquete
library(ggplot2)

##-- Seleccionar una submuestra
fly.mini <- fly.df[1:1000,]

##-- Graficos bivariantes de puntos
ggplot(data=fly.mini) + geom_point(mapping=aes(x=dep_delay,y=arr_delay))               # Basico
ggplot(data=fly.mini) + geom_point(mapping=aes(x=dep_delay,y=arr_delay),color="red")   # Color
ggplot(data=fly.mini) + geom_point(mapping=aes(x=dep_delay,y=arr_delay,color=origin))  # Color by origin
ggplot(data=fly.mini) + geom_point(mapping=aes(x=dep_delay,y=arr_delay,shape=origin))  # Forma by origin
ggplot(data=fly.mini) + geom_point(mapping=aes(x=dep_delay,y=arr_delay,size=air_time)) # Tama?o by air_time
ggplot(data=fly.mini) + geom_point(mapping=aes(x=dep_delay,y=arr_delay,alpha=air_time))# Transparencia by air_time

##-- Facets
# 1 variable
ggplot(data=fly.mini) +                     
  geom_point(mapping=aes(x=dep_delay,y=arr_delay)) +
  facet_wrap(~ origin, nrow = 1)

# 2 variables
ggplot(data=fly.mini) + 
  geom_point(mapping=aes(x=dep_delay,y=arr_delay)) +
  facet_grid(origin ~ carrier)

##-- Otras geometrias
ggplot(data=fly.mini) + geom_line(mapping=aes(x=dep_delay,y=arr_delay))                  # Linea
ggplot(data=fly.mini) + geom_smooth(mapping=aes(x=dep_delay,y=arr_delay))                # Suavizado
ggplot(data=fly.mini) + geom_smooth(mapping=aes(x=dep_delay,y=arr_delay,linetype=origin))# Suavizado by origin
ggplot(data=fly.mini) +                                                                  # 2 geometrias
  geom_point(mapping=aes(x=dep_delay,y=arr_delay,color=origin)) +
  geom_smooth(mapping=aes(x=dep_delay,y=arr_delay,color=origin))
ggplot(data=fly.mini,aes(x=dep_delay,y=arr_delay,color=origin)) +                        # Idem que el anterior
  geom_point() + geom_smooth()


##-- Diagramas de barras
ggplot(data=fly.mini) + geom_bar(mapping=aes(x=origin))                                  # univariante
ggplot(data=fly.mini) + geom_bar(mapping=aes(x=origin,fill=arr_delay>0))                 # bivariante
ggplot(data=fly.mini) +                                                                  # bivariante varias categorias
  geom_bar(mapping=aes(x=origin,
                       fill=cut(arr_delay,fivenum(arr_delay),include.lowest = TRUE)),
                       position = "dodge") + 
  theme(legend.title = element_blank())

##-- Estadisticos resumen
ggplot(data=fly.mini) +
  stat_summary(
    mapping = aes(x = origin, y = arr_delay),
    fun.min = min,
    fun.max = max,
    fun = median
  )

##-- Mapas
library(maps)
wd <- map_data("world")
ggplot(wd, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

############################################################
# Ejercicio 9.5
############################################################

## Mira si hay diferencias entre la relacion de la velocidad del avion y la distancia de recorrido
## Entre distintos carriers

