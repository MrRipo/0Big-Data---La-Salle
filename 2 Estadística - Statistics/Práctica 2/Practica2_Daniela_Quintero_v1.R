############################################################
rm(list=ls())
############################################################
# install.packages("devtools")
# install.packages("caret")
# install.packages('ResourceSelection')
library(randomForest)
library(party)
library(e1071)
library(corrplot)
library(minerva)
library(lattice)
library(devtools)
library(caret)
library(ggplot2)
library(ResourceSelection)
library(AUC)
library(PresenceAbsence)

# rm(list=ls())
# Read data
setwd('/Users/dandr/Documents/Estadistica/P2-BANK-STUDENTS')
training <- read.csv2('p2_train.csv',header=TRUE, stringsAsFactors = TRUE)
testing <- read.csv2('p2_test.csv', header=TRUE, dec=".", stringsAsFactors = TRUE)

# Convert all training to numeric
training$age <- as.numeric(training$age)
training$campaign <- as.numeric(training$campaign)
training$pdays <- as.numeric(training$pdays)
training$previous <- as.numeric(training$previous)
training$emp.var.rate<- as.numeric(training$emp.var.rate)
training$cons.price.idx<- as.numeric(training$cons.price.idx)
training$cons.conf.idx<- as.numeric(training$cons.conf.idx)
training$euribor3m<- as.numeric(training$euribor3m)
training$nr.employed<- as.numeric(training$nr.employed)
# Convert all testing to numeric
testing$campaign <- as.numeric(testing$campaign)
testing$pdays <- as.numeric(testing$pdays)
testing$previous <- as.numeric(testing$previous)
testing$emp.var.rate<- as.numeric(testing$emp.var.rate)
testing$cons.price.idx<- as.numeric(testing$cons.price.idx)
testing$cons.conf.idx<- as.numeric(testing$cons.conf.idx)
testing$euribor3m<- as.numeric(testing$euribor3m)
testing$nr.employed<- as.numeric(testing$nr.employed)

summary(training) # no :25362, yes: 3283
summary(testing)

### Balancear la Variable respuesta Y, porque tenemos muchos valores de "no"
# Condición para filtrar las filas que cumplen esta condición
condicion <- training$y == "no"
# Obtener los índices de las filas que cumplen la condición
delete_indices <- which(condicion)
# Eliminar max 10000 filas
delete_rows <- min(12000, length(delete_indices))
# Obtener indices de las filas que se eliminan
delete_rows <- sample(delete_indices, delete_rows)
# Eliminar las filas del training dataset
training <- training[-delete_rows, ]

summary(training) # no :13362, yes: 3283

# Create a new data frame without the specified columns
training <- training[, !(names(training) %in% "id")]
training_feat <- training[, !(names(training) %in% "y")]
training_target <- training
testing <- testing[, !(names(testing) %in% "id")]

# Use lapply to convert the to factor
cols_to_convert <- c("job", "marital", "education", "default")
training_feat[cols_to_convert] <- lapply(training_feat[cols_to_convert], factor)
training_target[cols_to_convert] <- lapply(training_target[cols_to_convert], factor)
testing[cols_to_convert] <- lapply(testing[cols_to_convert], factor)

# Plot histogram of all variables
plot(y~job,training_target)
plot(y~education,training_target)
plot(y~housing,training_target)
plot(y~loan,training_target)
plot(y~campaign,training_target)
plot(y~pdays,training_target)
plot(y~previous,training_target)
plot(y~poutcome,training_target)
plot(y~marital,training_target)

training$nr.employed<- as.numeric(training$nr.employed)

# Densidad en las variables numericas
var.num <- which(sapply(training,class) %in% c("numeric","integer"))
for(vn in var.num) cdplot(training$y~training[,vn],main=names(training)[vn],n=512)

# Descripcion bivariante de todas las variables
par(mfrow=c(5,4))
for(i in 1:19){
    plot(training$y~training[,i],main=names(training)[i],xlab=names(training)[i],ylab="y")
    with(training,lines(lowess(y~training[,i]),col=2))
}

# modelo multivariado con todas las variables
mod.glm0 <- glm(y~., training_target, family=binomial)    # estimacion del modelo            
mod.glm0                                        
summary(mod.glm0)  # AIC: 16287

# Seleccion automatica
############################################################
##-- Identificar cuantos Missings tiene cada variables. 
# La funcion Step no se puede aplicar con missings
apply(apply(training_feat,2,is.na),2,sum)

mod.glm1 <- step(mod.glm0)
summary(mod.glm1) # AIC: 16268
# Best model according to step function
# glm(formula = y ~ job + default + contact + month + day_of_week + 
#campaign + pdays + poutcome + cons.price.idx + euribor3m + 
#    nr.employed, family = binomial, data = training_target)


# Importancia de las categorias/covariables segun su significacion estadistica
feat_import = varImp(mod.glm1)

# Validacion 
############################################################
## Por inspeccion visual
br <- quantile(fitted(mod.glm1),seq(0,1,0.1))                                # Se crean los puntos de corte para los intervalos (br) de las probabilidades predichas
int <- cut(fitted(mod.glm1),br)                                              # Se crea una variable con el intervalo al que pertenece cada individuo
obs <- tapply(mod.glm1$y,int,sum)                                            # Los pagos observados en cada intervalo
exp <- tapply(fitted(mod.glm1),int,sum)                                      # Los pagos esperados en cada intervalo  
plot(1:10+0.05,exp,type='h',xlab="Intervalos",ylab="Frecuencias",lwd=2)      # Grafico de los pagos esperados
lines(1:10-0.05,obs,type='h',col=2,lwd=2)                                    # Se anyade los pagos observados
legend("topleft",c("Se suscriben - esperados", "No se suscriben - observados"),lwd=2,col=1:2) # Se anyade una leyenda

############################################################
# Validacion 
## test de Hosmer-Lemeshow
hoslem.test(mod.glm1$y, fitted(mod.glm1))  # si el p-value es inferior a 0.05 quedaria en duda el modelo   
# p-value = p-value = 0.0069. 
# If p-value > 0.05: Fail to reject the null hypothesis. Good fit.

############################################################
# Estimacion de probabilidad de suscribirse al deposito
## Probabilidades predichas
pr1 <- predict(mod.glm1, training, type="response")
pr1

##--Probabilidad maxima y minima
pos.max <- which.max(pr1)   # posicion del individuo con mayor probabilidad de suscribirse
pr1[pos.max]                # 0.95 probabilidad de dicho individuo 
training$y[pos.max]         # yes

pos.min <- which.min(pr1)   # posicion del individuo con menor probabilidad de suscribirse
pr1[pos.min]                # 2.577634e-05 probabilidad de dicho individuo 
training$y[pos.min]         # no

boxplot(pr1~y, training)

##-- Curva ROC y AUC
pr <- predict(mod.glm1, type='response')
roc.curve <- roc(pr, training_target$y)
plot(roc.curve)
AUC::auc(roc.curve)  # AUC: 0.7870663

##-- Sensibilidad y especificidad para un punto de corte concreto
s <- AUC::sensitivity(pr, training_target$y)
e <- AUC::specificity(pr, training_target$y)
a <- AUC::accuracy(pr, training_target$y)
df <- data.frame(cutpoints=s$cutoffs,sens=s$measure,esp=e$measure,acc=a$measure)
View(round(df,3))

##-- Escoger un punto de corte --> Matriz de confusion
training$se.suscribe.glm1 <- ifelse(pr1>0.5,'yes','no')  # Doy credito a aquellos con un probabilidad predicha de pagar superior a 0.5
with(training, table(se.suscribe.glm1, y))
with(training, round(100*prop.table(table(se.suscribe.glm1, y),1),1))

## Calibracion del modelo
library(PresenceAbsence)
df.calibra <- data.frame(plotID=1:nrow(training_target), Observed = as.numeric(training_target$y)-1  , Predicted1 = pr)
calibration.plot(df.calibra, N.bins = 10, ylab='Observed probabilities')
detach('package:PresenceAbsence')


### Reajuste del modelo: quitar variables correlacionadas o sin importancia

# Correlation matrix using numerical variables
# Specify the column names you want to include in the correlation matrix
selected_columns <- c("age","campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")
# Subset the data frame to include only the selected columns
selected_data <- training_target[selected_columns]
# Compute the correlation matrix
correlation_matrix <- cor(selected_data)
# Print or view the correlation matrix
print(correlation_matrix)
# Visualize the correlation matrix with colors
par(mfrow=c(1,1))
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(20), addCoef.col = "black", tl.col="black")

#Variables numéricas independientes
##############################################################

# Specify the column names you want to include in the correlation matrix
selected_columns <- c("age","campaign", "previous", "cons.price.idx", "cons.conf.idx")
# Subset the data frame to include only the selected columns
selected_data <- training_feat[selected_columns]
# Compute the correlation matrix
correlation_matrix <- cor(selected_data)
# Print or view the correlation matrix
print(correlation_matrix)
# Visualize the correlation matrix with colors
par(mfrow=c(1,1))
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(20), addCoef.col = "black", tl.col="black")

### Modelo con variables independientes
mod.glm2 <- glm(y~age +job + marital + default + housing + loan + contact + 
                    month + day_of_week + campaign + previous + cons.price.idx + 
                    cons.conf.idx, training_target,family=binomial) # estimacion del modelo # Instruccion equivalente a la anterior
mod.glm2  # Ver coeficientes
summary(mod.glm2)  # AIC: 14073

##-- Funcion step (selección automática)
mod.glm3 <- step(mod.glm2)
summary(mod.glm3)  # AIC: 14068

# Validation 
############################################################
##-- Por inspeccion visual
br <- quantile(fitted(mod.glm3),seq(0,1,0.1))                                # Se crean los puntos de corte para los intervalos (br) de las probabilidades predichas
int <- cut(fitted(mod.glm3),br)                                              # Se crea una variable con el intervalo al que pertenece cada individuo
obs <- tapply(mod.glm3$y,int,sum)                                            # Las llamadas observadas en cada intervalo
exp <- tapply(fitted(mod.glm3),int,sum)                                      # Las llamadas esperadas en cada intervalo  
plot(1:10+0.05,exp,type='h',xlab="Intervalos",ylab="Frecuencias",lwd=2)      # Grafico de las llamadas esperadas
lines(1:10-0.05,obs,type='h',col=2,lwd=2)                                    # Se anyade las llamadas observadas
legend("topleft",c("Se suscriben - esperadas", "No se suscriben - observadas"),lwd=2,col=1:2) # Se anyade una leyenda

##--test de Hosmer-Lemeshow
hoslem.test(mod.glm3$y, fitted(mod.glm3)) # p-value < 2.2e-16

# Estimacion de probabilidad de suscribirse al deposito
############################################################
## Probabilidades predichas
pr <- predict(mod.glm3, training_target,type="response")
pr

## Probabilidad maxima y minima
pos.max <- which.max(pr)        # posicion del individuo con mayor probabilidad de suscribirse
pr[pos.max]                     # 0.95 probabilidad de dicho individuo 
training_target$y[pos.max]      # yes

pos.min <- which.min(pr)        # posicion del individuo con menor probabilidad de suscribirse
pr[pos.min]                     # 2.57e-05 probabilidad de dicho individuo 
training_target$y[pos.min]      # no

boxplot(pr~y, training_target)

### Curva ROC y AUC
pr <- predict(mod.glm3, type='response')
roc.curve <- roc(pr,training_target$y)
plot(roc.curve)
AUC::auc(roc.curve)

### Calibracion del modelo GLM3
library(PresenceAbsence)
df.calibra <- data.frame(plotID=1:nrow(training_target), Observed = as.numeric(training_target$y)-1  , Predicted1 = pr)
calibration.plot(df.calibra, N.bins = 10,ylab='Observed probabilities')
detach('package:PresenceAbsence')

### Testear resultados
# Display the structure of the data frame
str(testing)

## Missing
apply(apply(testing,2,is.na),2,sum) # cuantos missings tiene cada variable --> Step no se puede aplicar con missings

# Calcular predicciones y obtener fichero con id y probabilidad
############################################################
pr <- predict(mod.glm1, testing, type="response")   # probabilidades predichas
# Dataframe con el ID y las probabilidades
resultados <- data.frame(id = testing$id, probabilidad = pr)
# Guardar dataframe de resultados
write.table(resultados, file = "/Users/dandr/Documents/Estadistica/P2-BANK-STUDENTS/test_results.txt",  sep = "\t", row.names = FALSE)





