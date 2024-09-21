####################################################################
# Ejercicio 1.1
####################################################################
# 1
df11 <- data.frame(marca=c("audi","volswagen","bmw","renault","fiat"),
                   modelo=c("A3","Polo","X5","megane","punto"),
                   consumo=c(6.8,6.8,12.5,4.7,5.0))
# 2
df11$consumo[5] <- 4.5
df11

####################################################################
# Ejercicio 1.2
####################################################################
# 1
setwd('mi_directorio')      # Entre comillas (simples o dobles) y separando las carpetas por "/"
datos2 <- read.table('flights.txt',header=TRUE,sep="\t",dec=".",na.strings="@", stringsAsFactors = TRUE)

# 2
dim(datos2)
View(datos2)
summary(datos2)

# 3
sel.flight <- which(datos2$flight==251 & datos2$tailnum=="N855UA")
datos2[sel.flight,"air_time"]

####################################################################
# Ejercicio 1.3
####################################################################
# 1
write.table(subset(datos2,air_time<60),"short_flights_test.txt",quote = FALSE,col.names = FALSE,row.names = FALSE,sep="\t")


####################################################################
# Ejercicio 1.4
####################################################################
# 1
summary(datos2$air_time)
sd(datos2$air_time,na.rm=TRUE)
IQR(datos2$air_time,na.rm=TRUE)
hist(datos2$air_time)
boxplot(datos2$air_time)
boxplot(datos2$air_time~datos2$dest,las=2,cex.axis=0.5)
abline(v=seq(1,100,2),col='grey',lty=2)

# 2
table(datos2$origin)
prop.table(table(datos2$origin))
barplot(table(datos2$origin))


####################################################################
# Ejercicio 1.5
####################################################################
# 1
windows(10,10)
par(mfrow=c(1,1),las=1)
with(datos2,cor(dep_delay,arr_delay, use="complete.obs"))
plot(arr_delay~dep_delay,datos2,pch=19,col=rgb(1,0,0,0.05,max=1),
     xlab="Retraso salida",ylab="Retraso llegada",main="Retrasos en los vuelos")
smoothScatter(datos2$arr_delay~datos2$dep_delay,nbin=1024,col=datos2$origin)

# 2
with(datos2,tapply(arr_delay,month,summary))
boxplot(arr_delay~month,datos2)

datos2.delay <- subset(datos2,arr_delay>0)  
with(datos2.delay,tapply(arr_delay,month,summary))
boxplot(arr_delay~month,datos2.delay,log="y")


# 3 
with(datos2,table(hour,origin))
with(datos2,prop.table(table(hour,origin),1))
mosaicplot(origin~hour,datos2,col=1:24)

datos2$time.slot <- cut(datos2$hour,c(0,8,16,24),include.lowest = TRUE)
with(datos2,table(time.slot,origin))
with(datos2,prop.table(table(time.slot,origin),2))
mosaicplot(origin~time.slot,datos2,col=1:24)
