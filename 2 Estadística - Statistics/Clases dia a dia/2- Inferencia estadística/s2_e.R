####################################################################
# Ejercicio 2.1
####################################################################
# 1
datos2 <- read.table('flights.txt',header=TRUE,sep="\t",dec=".",na.strings="@") 


# 2
t.test(datos2$arr_delay)

# 3
qqnorm(datos2$arr_delay)
qqline(datos2$arr_delay,col=2)

set.seed(12345)
m <- replicate(1000, sd(sample(datos2$arr_delay, replace=TRUE),na.rm=TRUE))
quantile(m,probs=c(0.05, 0.95))

# 4
e <- sum(datos2$arr_delay>0,na.rm=TRUE)
n <- sum(!is.na(datos2$arr_delay))
prop.test(e,n,conf.level=0.99)

####################################################################
# Ejercicio 2.2
####################################################################
# 1
datos2$origin2 <- ifelse(datos2$origin=="JFK","JFK","Other")

# 2
t.test(arr_delay~origin2,datos2,var.equal=TRUE)

# 3
with(datos2,t.test(arr_delay,dep_delay,paired=TRUE,conf=0.999))
attach(datos2)
t.test(arr_delay,dep_delay,paired=TRUE,conf=0.999)
detach(datos2)
t.test(datos2$arr_delay,datos2$dep_delay,paired=TRUE,conf=0.999)



# 4 
library(PairedData)
n <- 10000
datos2.sub <- subset(datos2,arr_delay<180)
x <- datos2.sub$dep_delay[1:n]
y <- datos2.sub$arr_delay[1:n]
pd <- paired(x,y)
plot(pd,type="BA") + xlim(-40,20)


####################################################################
# Ejercicio 2.3
####################################################################
# 1
boxplot(arr_delay~origin2,datos2)

# 2
datos2.sub <- subset(datos2, datos2$arr_delay>0)
boxplot(arr_delay~origin2,datos2.sub,log="y")

# 3
var.test(arr_delay~origin2,datos2,conf=0.99)


####################################################################
# Ejercicio 2.4
####################################################################
# 1
datos2$date <- with(datos2,as.Date(paste(year,"-",month,"-",day,sep="")))
datos2$weekend <- weekdays(datos2$date) %in% c("sábado","domingo")
datos2$arr_delay_cat <- datos2$arr_delay>0


# 2
t1 <- with(datos2,table(weekend,arr_delay_cat))
mosaicplot(t1,col=3:2)

# 3
prop.test(t1)

####################################################################
# Ejercicio 2.5
####################################################################
# 1
n <- 393            # Tamanyo muestral
nsim <- 1000        # numero de simulaciones
p <- c()            # vector donde se guardan los p-valores de las simulaciones

##-- Realizacion de las simulaciones
set.seed(12345)
for(i in 1:nsim){
  muestraA <- rnorm(n,mean=0,sd=1)
  muestraB <- rnorm(n,mean=0.2,sd=1)
  tt <- t.test(muestraA,muestraB,var.equal=TRUE)
  p[i] <- tt$p.value
}
sum(p>0.05)/nsim    # beta (probabilidad de error tipo II)

# 2
power.t.test(n=393,delta=0.2,sd=1,sig.level=0.05)

