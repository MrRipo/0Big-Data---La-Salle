rm(list=ls())

library(minerva)
library(corrplot)
library(car)

##-- Leer los datos
setwd('/Users/javier.leon/Documents/personal/Master/Estadistica/P1-BIKES-STUDENTS')                                                                       # directorio de trabajo
d_train <- read.table('p1_train.csv',header=TRUE,sep=';',stringsAsFactors = TRUE)
d_test <- read.table('p1_test.csv',header=TRUE,sep=';',stringsAsFactors = TRUE)

# Convert all training to numeric
d_train$hour <- as.numeric(d_train$hour)
d_train$temp <- as.numeric(d_train$temp)
d_train$atemp <- as.numeric(d_train$atemp)
d_train$windspeed <- as.numeric(d_train$windspeed)
# Convert all testing to numeric
d_test$hour <- as.numeric(d_test$hour)
d_test$temp <- as.numeric(d_test$temp)
d_test$atemp <- as.numeric(d_test$atemp)
d_test$windspeed <- as.numeric(d_test$windspeed)

# Create a new data frame without the specified column
d_train <- d_train[, !(names(d_train) %in% "id")]
train_no_count <- d_train[, !(names(d_train) %in% c("year", "count"))]
train_target <- d_train[, !(names(d_train) %in% "year")]
d_test <- d_test[, !(names(d_test) %in% c("year", "id"))]

# Plot histogram of all variables
boxplot(train_target~train_no_count)
boxplot(count~hour,train_target)
boxplot(count~season,train_target)
boxplot(count~holiday,train_target)
boxplot(count~workingday,train_target)
boxplot(count~weather,train_target)
boxplot(count~temp,train_target)
boxplot(count~atemp,train_target)
boxplot(count~humidity,train_target)
boxplot(count~windspeed,train_target)

# Use lapply to apply the factor function to each specified column
cols_to_convert <- c("season", "holiday", "workingday", "weather") # we let "hour" as continue. Only categorical 
train_no_count[cols_to_convert] <- lapply(train_no_count[cols_to_convert], factor)
train_target[cols_to_convert] <- lapply(train_target[cols_to_convert], factor)
d_test[cols_to_convert] <- lapply(d_test[cols_to_convert], factor)

#### Correlation matrix using numerical varirbles
# Convert count as numeric
train_target$count <- as.numeric(train_target$count)
# Specify the column names you want to include in the correlation matrix
selected_columns <- c("hour", "temp", "atemp", "humidity", "windspeed")
# Subset the data frame to include only the selected columns
selected_data <- train_target[selected_columns]
# Compute the correlation matrix
correlation_matrix <- cor(selected_data)
# Print or view the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix with colors
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(20), addCoef.col = "black", tl.col = "black")

### Calculate MIC (Maximum Information Coefficient) for non linear correlations
mic_result <- mine(selected_data)
# Access the MIC value
mic_value <- mic_result$MIC
# Print or use the MIC value as needed
print(mic_value)
corrplot(mic_value, method = "color", col = colorRampPalette(c("blue", "white", "red"))(20), addCoef.col = "black", tl.col = "black")

# Plot variables that are highly correlated
plot(train_target$temp, train_target$atemp)

# Lineal model 1 with temp
mod.lm1 <- lm(count~ hour + season + holiday +      
                workingday + weather + temp +
                humidity + windspeed, train_target)
summary(mod.lm1)

# Lineal model 2. categorical variables is shown as 1, 2, 3
# Shows only the categorical ones with less values, it has 3 categories, shows only 2. 2 shows 1
mod.lm2 <- lm(count~ hour + season +weather + temp +
                humidity + windspeed, train_target)
summary(mod.lm2)

# Lineal model 3
mod.lm3 <- lm(count~ hour + season + weather + atemp + humidity + 
                windspeed, train_target)    
summary(mod.lm3)

# Lineal model 4
# Shows only the categorical ones with less values, it has 3 categories, shows only 2. 2 shows 1
mod.lm4 <- lm(count~ hour + season +weather + atemp +
                humidity + windspeed, train_target)
summary(mod.lm4)

# Lineal model 5 with temp and workingday
mod.lm5 <- lm(count~ hour + season +
                workingday + weather + temp +
                humidity, train_target)
summary(mod.lm5)

# Lineal model 1 with temp and holiday
mod.lm6 <- lm(count~ hour + season + holiday +      
                weather + temp +
                humidity, train_target)
summary(mod.lm6)

# Lineal model 7 with temp and holiday cut
mod.lm7 <- lm(count~ hour + season +      
                weather + temp +
                humidity, train_target)
summary(mod.lm7)

##- Con poly se incluyen terminos polinomicos de orden mayor
mod.lm71 <- lm(count ~ poly(hour,2) + poly(season,2) + poly(weather,2)+
                poly(temp,2) + poly(humidity,2) ,train_target)
summary(mod.lm71)

mod.lm72 <- step(mod.lm71)
summary(mod.lm72)

# Colinealidad
vif(mod.lm1)
vif(mod.lm2)

# Lineal model 1 optimization
mod.lm11 <- lm(count~ hour + season + holiday +      
                workingday + weather + temp +
                humidity + windspeed + (temp*humidity), train_target)
summary(mod.lm11)

# Lineal model 2 optimization
mod.lm21 <- lm(count~ hour + season +weather + temp +
                humidity + windspeed + (temp*humidity), train_target)
summary(mod.lm21)

##-- Efectos
library(effects)
plot(allEffects(mod.lm1))
plot(allEffects(mod.lm21))
plot(allEffects(mod.lm72))


################## Predicciones ###################################
pr <- predict(mod.lm21, d_test)   # Predicciones para los nuevos valores
par(mfrow=c(1,1))           
plot(pr, d_test$count, asp=1)    # Predicciones vs valores observados 
abline(0,1, col=2, lwd=2)         # Bisectriz

pr72 <- predict(mod.lm72, d_test)   # Predicciones para los nuevos valores
par(mfrow=c(1,1))           
plot(pr72, d_test$count, asp=1)    # Predicciones vs valores observados 
abline(0,1, col=2, lwd=2)         # Bisectriz


################## EQM (Error Cuadratico Medio) ###################################
n <- dim(d_test)[1]                                       # Tamaño muestral
EQM <- sum((pr-d_test$count)^2)/n                      # Error Cuadratico Medio
sqrt(EQM)                                               # Incertidumbre a posteriori                                                
sd(d_test$count)                                       # Incertidumbre a priori --> El modelo me reduce a la mitad la incertidumbre en las predicciones
summary(d_test$count)

EQM2 <- sum((pr72-d_test$count)^2)/n                      # Error Cuadratico Medio
sqrt(EQM2)                                               # Incertidumbre a posteriori                                                
sd(d_test$count)                                       # Incertidumbre a priori --> El modelo me reduce a la mitad la incertidumbre en las predicciones
summary(d_test$count)
