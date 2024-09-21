############################################################
#
# MUBD - Clusterizacion no supervisada
#
############################################################

rm(list=ls())

############################################################
# Instalar y cargar paquetes
############################################################
library(NbClust)        # Function NbClust
library(factoextra)     # Several clustering graphics
library(clustertend)    # Hopkins index
library(FactoMineR)     # Factor analysis
library(dendextend)     # Comparar dendogramas
library(corrplot)       # Graficos de correlaciones
library(cluster)        # pam

############################################################
# Inspeccionar datos
############################################################
data(iris)                                 # Cargar datos
View(iris)                                 # Ver datos
iris2 <- iris[,1:4]                        # Eliminar variable respuesta (no supervisado)

windows()
pairs(iris2)                               # Plots 2 a 2
heatmap(as.matrix(iris2))                  # Heatmap sin escalar
heatmap(as.matrix(scale(iris2)))           # Heatmap escalando


############################################################
#
# Clustering jerarquico
#
############################################################
##-- 1. Matriz de distancias
d <- dist(iris2, method = "euclidean")     # matriz de distancias
View(as.matrix(d),'dist')                  # ver matriz de distancias


##-- 2. Clusterizacion
hc <- hclust(d,method = "complete")        # Jerarquizacion


##-- 3. Graficar jerarquizacion
windows(14,7)                              # Abrir ventana grafica
plot(hc,cex=0.7)                           # Dibujar dendograma             


##-- 4. Evaluacion de la calidad del arbol
cop.dist <- cophenetic(hc)
cor(d,cop.dist)


##-- 5. Particion. Cuantos clusteres?
fviz_nbclust(iris2,hcut,method=c("wss"))  # Elbow rule
NB <- NbClust(iris2,method = 'ward.D2')   # Numero de clusteres segun distintos criterios
fviz_nbclust(NB)                          # Numero de clusteres segun distintos criterios
ct <- cutree(hc,3)                        # Seleccionar numero de clusteres
fviz_dend(hc, k = 3,rect=TRUE,lwd=2)


##-- Todo a la vez
HC <- HCPC(iris2,method = "complete")

##-- 5. Representacion grafica
# Por pares
pairs(iris2,col=ct)                        

# Componentes principales --> Representacion en las 2 primeras componentes que recogen mayor variabilidad
pr <- princomp(iris2)                       
x <- pr$scores[,1]
y <- pr$scores[,2]
plot(x,y,col=ct,pch=19)

# Con elipses (Avoid label overplotting --> slow)
fviz_cluster(list(data = iris2, cluster = ct),ellipse.type = "convex",
             repel = TRUE,                                              
             show.clust.cent = FALSE, ggtheme = theme_minimal())


##-- 6. Porcentaje de acierto (en general, no aplica a clusterizacion no jerarquica)
table(iris$Species,ct)
cl <- ifelse(ct==1,1,ifelse(ct==2,3,2))
(t.pred <- table(iris$Species,cl))
sum(diag(t.pred))/sum(t.pred)
pairs(iris2,cex=0.8,col=cl,pch=as.numeric(iris$Species))

##-- 7. Comparacion de 2 dendogramas
hc1 <- hclust(d,method = "complete")       # Jerarquizacion
hc2 <- hclust(d,method = "average")        # Jerarquizacion

tanglegram(hc1,hc2)
dend_list <- dendlist(as.dendrogram (hc1), as.dendrogram (hc2))

# Correlacion
cor.dendlist(dend_list, method = "cophenetic")

##-- 8. Comparacion de 4 dendogramas
hc3 <- hclust(d,method = "ward.D2")       # Jerarquizacion
hc4 <- hclust(d,method = "single")        # Jerarquizacion

dend_list <- dendlist(as.dendrogram (hc1), as.dendrogram (hc2),
                      as.dendrogram (hc3), as.dendrogram (hc4))

par(mfrow=c(1,1))
corrplot(cor.dendlist(dend_list, method = "cophenetic"), "pie", "lower")

##-- 9. Tendencia cluster
# Hopkins
set.seed(12345)
hop <- hopkins(iris2, n = nrow(iris2)-1)
hop                                       # Valor proximo a 0 --> Hay clusters



############################################################
# Ejercicio 5-1
############################################################
# Aplicar la clusterizacion jerarquica al conjunto de datos de 
# wine2.txt para clasificarlos segun tipologia (variable Type)
# 1. Lee los datos


# 2. Haz una descriptiva de los datos sin escalar


# 3. Escala los datos y haz un heatmap


# 4. Compara los 4 dendogramas resultantes de usar la distancia euclidea/manhattan y el metodo de wald y completo
# Que influye mas el tipo de distancia o el tipo de agrupacion?


# 5. Segun los 4 dendogramas, Cuantos grupos de vino crees que hay? Haz la particion segun 
# el numero de grupos que creas conveniente para cada uno de los sistemas


# 6. Escoge una particion de las anteriores y calcula el % de variabilidad explicada


# 7. Define las caracteristicas mas relevantes de cada grupo de vinos segun la clasificacion escogida. 
# En que caracteristica difieren menos?


############################################################
#
# K-means
#
############################################################

############################################################
# Instalar y cargar paquetes
############################################################
# install.packages('scatterplot3d')
# install.packages('flexclust')
# install.packages('NbClust')
library(scatterplot3d)  
library(flexclust)
library(NbClust)        # NbClust
library(cluster)
library(factoextra)     # fviz_***
library(kernlab)        # kkmeans
library(clValid)        # clValid
library(cluster)        # pam

############################################################
# Objetos dentro del K-means
############################################################
##-- Prueba simple
km0 <- kmeans(iris2,centers=3)
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
##-- Regla del codo
VE <- c()
for (k in 2:10){
  km <- kmeans(iris2,centers=k,nstart=10)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")

##-- Regla del codo (Inercia intra) --> Equivalente al anterior
fviz_nbclust(iris2,kmeans,method="wss")

##-- Numero de clusteres segun indicadores
set.seed(12345)
ncluster <- NbClust(iris2, min.nc=2, max.nc=15, method="kmeans")
ncluster
barplot(table(ncluster$Best.n[1,]))
heatmap(scale(ncluster$All.index),Rowv=NA,Colv = NA)

############################################################
# Representacion grafica
############################################################
##-- 3 grupos
km3 <- kmeans(iris2,centers=3,nstart=10)

##-- Por pares
pairs(iris2,pch=19,cex=0.8,col=km3[[1]])

##-- En las 2 primeras componentes
pr.comp <- princomp(iris2)
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km3$cluster)

# Con elipses
fviz_cluster(list(data = iris2, cluster = km3$cluster),ellipse.type = "convex",
             repel = TRUE,                                        
             show.clust.cent = FALSE, ggtheme = theme_minimal())

##-- Graficar en 3D
scatterplot3d(iris2$Sepal.Length,iris2$Sepal.Width,iris2$Petal.Length,
              type="h",pch=as.numeric(iris$Species),angle=60,color=km3$cluster)

##-- 2 grupos
km2 <- kmeans(iris2,centers=2,nstart=10)

# Por pares
pairs(iris2,pch=19,cex=0.8,col=km2[[1]])

# En las 2 primeras componentes
plot(x,y,pch=19,col=km2$cluster)

fviz_cluster(list(data = iris2, cluster = km2$cluster),ellipse.type = "convex",
             repel = TRUE,                                        # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

############################################################
# K-mediods
############################################################
kmediods3 <- pam(iris2,3)
fviz_cluster(list(data = iris2, cluster = kmediods3$cluster),ellipse.type = "convex",
             repel = TRUE,                                        # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())
randIndex(table(km3$cluster,kmediods3$cluster))                   # rand = 1 --> Imply same clustering 

############################################################
# Similitud con etiquetas iniciales
############################################################
randIndex(table(km3$cluster,iris$Species))



############################################################
# Conjuntos no convexos --> No funciona K-means
############################################################
n <- 500
set.seed(12345)

##-- Generar datos en circunferencia de radio 1
r1 <- runif(n,0,1)
alfa1 <- runif(n,0,2*pi)
x1 <- r1*cos(alfa1)
y1 <- r1*sin(alfa1)

##-- Generar datos en corona circular de 2 a 3
r2 <- runif(n,2,3)
alfa2 <- runif(n,0,2*pi)
x2 <- r2*cos(alfa2)
y2 <- r2*sin(alfa2)

# Juntar datos
df <- data.frame(x=c(x1,x2),y=c(y1,y2))
plot(y~x,df,pch=19)
km2 <- kmeans(scale(df),2)
plot(y~x,df,col=km2$cluster,pch=19)

##-- kernel k-means clustering
library(kernlab)
kk <- kkmeans(scale(df),2)
plot(y~x,df,col=as.numeric(kk),pch=19)

############################################################
# Outliers
############################################################
set.seed(12345)
n <- 1000                                                    # Tamaño de datos "normales"
nout <- 3                                                    # numero de outliers
x1 <- c(rnorm(n/2,10,1),rnorm(n/2,100,1),rnorm(nout,10^5))   # primera dimension (variable)
x2 <- c(rnorm(n,10),rnorm(nout,10^5))                        # segunda dimension (variable)

df <- data.frame(x1,x2)

par(mfrow=c(2,2))

##-- Datos
plot(x1,x2,log='xy',main='DATA')

##-- K-means
km1 <- kmeans(df,2,nstart=1)
plot(x1,x2,log='xy',col=km1$cluster+1,main='K-MEANS')

##-- K-mediods
km2 <- pam(df,2)
plot(x1,x2,log='xy',col=km2$cluster+1,main='PAM')

##-- K-mediods variante (CLARA)
km3 <- clara(df, 2)
plot(x1,x2,log='xy',col=km3$cluster+1,main='CLARA')

############################################################
# Ejercicio 5-2
############################################################
##-- Variables
# FRESH: annual spending (m.u.) on fresh products (Continuous); 
# MILK: annual spending (m.u.) on milk products (Continuous); 
# GROCERY: annual spending (m.u.)on grocery products (Continuous); 
# FROZEN: annual spending (m.u.)on frozen products (Continuous) 
# DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous) 
# DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous); 

##-- 1.Lee el fichero Wholesale customers data.csv


##-- 2.Haz una descriptiva de los datos y escalalos con la funcion "scale"
# Valora si conviene scalar los datos originales o los datos transformados logaritmicamente 
# Para la transformacion logaritmica (suma 1 a todos los valores para evitar tener ceros)



##-- 3. Decide el numero de clusteres segun algun criterio. Cuantos clusteres son los ideales?
##-- Regla del codo o varios indicadores (NbClust)



##-- 4. Compara los siguientes 4 metodos:
# kmeans con algoritmo Lloyd y n-start=1
# kmeans con algoritmo Hartigan-Wong y n-start=1
# kmeans con algoritmo Hartigan-Wong y n-start=10
# kmeans despues de hacer una clusterizacion jerarquica y fijando los centros segun esta

# Para un numero de clusters de 5 a 8. Obten los siguientes indicadores/graficos
# El indice de Dunn (funcion dunn del paquete clValid)
# El indice silhouette (funcion del silhouette paquete cluster)

# Contesta las siguientes preguntas
# a. Segun los indices de Dunn y de silhouette, Cuantos clusteres se deberan escoger entre 5 y 8?
# b. La decision es independiente del sistema empleado??la decision es independiente del indice empleado?


# ##-- Matrices para guardar resultados
# DUN <- SIL <- matrix(nrow=4,ncol=4)                 
# 
# ##-- Bucle para un numero de clusteres de 5 a 8
# set.seed(12345)                                             # Semilla para replicar los resultados
# D <- dist(datos2)                                           # Calcular distancias
# for (i in 5:8){
#   
#   ##-- Clusterizacion jerarquica para obtener los centros
#   hc <- hclust(...,method = 'ward.D') 
#   cl <- cutree(hc,...)
#   centers <- apply(datos2,2,tapply,cl,mean)
#   
#   ##-- 4 metodos
#   km_1 <- kmeans(...)       # Metodo 1
#   km_2 <- kmeans(...)       # Metodo 2
#   km_3 <- kmeans(...)       # Metodo 3
#   km_4 <- kmeans(...)       # Metodo 4
#   
#   ##-- Calcular indices y guardarlos en una matrix
#   DUN[i-4,] <- c(dunn(D,km_1$cluster),
#                  dunn(D,km_2$cluster),
#                  ...,
#                  ...)
#   SIL[i-4,] <- c(summary(silhouette(km_1$cluster,D))[[4]],
#                  ...,
#                  ...,
#                  ...)
#   
# }
# round(DUN,3)
# round(SIL,3)

##-- 5. Compara la similitud de los clusteres con el RAND INDEX para escoger 2 y 5 
# clusteres con el sistema m?s simple (Lloyd,nstart=1) y el sistema por defecto 
# con 10 inicios (Hartigan-Wong,nstart=10)

# # 2 Clusteres
# set.seed(1234)
# km1 <- kmeans(...)
# km2 <- kmeans(...)
# randIndex(...) 
# 
# # 5 Clusteres
# set.seed(1234)
# km1 <- kmeans(...)
# km2 <- kmeans(...)
# randIndex(...) 


##-- 6. Describe los grupos segun la agrupacion escogida
# Hacer descriptiva, por ejemplo, segun boxplot
# par(mfrow=c(...,...),mar=c(5,5,5,1))
# for(i in 1:...) boxplot(datos2[,...]~...,main=colnames(datos2)[i])
