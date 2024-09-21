############################################################
rm(list=ls())
############################################################
install.packages('randomForest')
install.packages('party')
install.packages('e1071')
install.packages('minerva')
install.packages("corrplot")
install.packages("lattice")
install.packages('car')
library(randomForest)
library(party)
library(e1071)
library(corrplot)
library(minerva)
library(lattice)
library(car)

# Read data
setwd('C:/Users/alber/Desktop/0Big Data - La Salle/Estadística/Práctica 1')
training <- read.csv2('p1_train.csv',header=TRUE, stringsAsFactors = TRUE)
testing <- read.csv2('p1_test.csv',header=TRUE, stringsAsFactors = TRUE)
summary(training)
summary(testing)

# Convert all training to numeric
training$hour <- as.numeric(training$hour)
training$temp <- as.numeric(training$temp)
training$atemp <- as.numeric(training$atemp)
training$windspeed <- as.numeric(training$windspeed)
# Convert all testing to numeric
testing$hour <- as.numeric(testing$hour)
testing$temp <- as.numeric(testing$temp)
testing$atemp <- as.numeric(testing$atemp)
testing$windspeed <- as.numeric(testing$windspeed)

# Create a new data frame without the specified column
training <- training[, !(names(training) %in% "id")]
training_feat <- training[, !(names(training) %in% c("year", "count"))]
training_target <- training[, !(names(training) %in% "year")]
testing <- testing[, !(names(testing) %in% c("year", "id"))]
# Convert count as numeric
training_target$count <- as.numeric(training_target$count)

# Plot histogram of all variables
boxplot(count~hour,training_target)
boxplot(count~season,training_target)
boxplot(count~holiday,training_target)
boxplot(count~workingday,training_target)
boxplot(count~weather,training_target)
boxplot(count~temp,training_target)
boxplot(count~atemp,training_target)
boxplot(count~humidity,training_target)
boxplot(count~windspeed,training_target)

# Use lapply to convert the to factor
cols_to_convert <- c("season", "holiday", "workingday", "weather") # we let "hour" as continue
training_feat[cols_to_convert] <- lapply(training_feat[cols_to_convert], factor)
training_target[cols_to_convert] <- lapply(training_target[cols_to_convert], factor)
testing[cols_to_convert] <- lapply(testing[cols_to_convert], factor)



#### Calculate R coef with Correlation matrix using numerical variables
# Specify the column names to include in the correlation matrix
selected_columns <- c("hour", "temp", "atemp", "humidity", "windspeed")
# Compute the correlation matrix
correlation_matrix <- cor(training_feat[selected_columns])
# Visualize the correlation matrix with colors
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(20), addCoef.col = "black", tl.col = "black", title = "R Coef")

### Calculate MIC (Maximum Information Coefficient) for non linear correlations
mic_result <- mine(training_feat[selected_columns])
# Access the MIC value
mic_value <- mic_result$MIC
# Visualize the correlation matrix with colors
corrplot(mic_value, method = "color", col = colorRampPalette(c("blue", "white", "red"))(20), addCoef.col = "black", tl.col = "black", title = "MIC Coef")

# Plot variables that are highly correlated
plot(training_target$temp, training_target$atemp, xlab = "temp", ylab = "atemp",)

# Otras dependencias
plot(training_target$holiday, training_target$workingday,xlab = "holiday", ylab = "workingday")
plot(training_target$temp, training_target$humidity,xlab = "temp", ylab = "humidity")
plot(training_target$atemp, training_target$humidity,xlab = "atemp", ylab = "humidity")
plot(training_target$temp, training_target$windspeed,xlab = "temp", ylab = "windspeed")
plot(training_target$atemp, training_target$windspeed,xlab = "atemp", ylab = "windspeed")

############ LINEAL MODELS #################################################################################################


#### LM1. 8v (all) -> hour + season + holiday + workingday + weather + temp + humidity + windspeed
mod.lm1 <- lm(count~ hour + season + holiday +      
                workingday + weather + temp +
                humidity + windspeed, training_target)
summary(mod.lm1)

# Residuos versus variables predictoras
residualPlots(mod.lm1)
plot(mod.lm1$fitted.values, residualPlots(mod.lm1), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)

#### LM2. 6v (temp) -> hour + season  + weather + temp + humidity + windspeed
# Shows only the categorical ones with less values, it has 3 categories, shows only 2. 2 shows 1
mod.lm2 <- lm(count~ hour + season +weather + temp +
                humidity + windspeed, training_target)
summary(mod.lm2)

# Residuos versus variables predictoras
residualPlots(mod.lm2)
plot(mod.lm1$fitted.values, residualPlots(mod.lm2), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)

#### LM3. 6v (atemp) -> hour + season + weather + atemp + humidity +  windspeed
mod.lm3 <- lm(count~ hour + season + weather + atemp + humidity + 
                windspeed, training_target)    
summary(mod.lm3)

# Residuos versus variables predictoras
residualPlots(mod.lm3)
plot(mod.lm1$fitted.values, residualPlots(mod.lm3), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)

# Predicciones
pr <- predict(mod.lm3, testing)   # Predicciones para los nuevos valores
par(mfrow=c(1,1))           
plot(pr, testing$count, asp=1)    # Predicciones vs valores observados 
abline(0,1, col=2, lwd=2)         # Bisectriz


#### LM4 6v (temp, workingday) -> hour + season + workingday + weather + temp + humidity
mod.lm4 <- lm(count~ hour + season +
                workingday + weather + temp +
                humidity, train_target)
summary(mod.lm4)

#### LM5 6v (temp and holiday) -> hour + season + holiday + weather + temp + humidity
mod.lm5 <- lm(count~ hour + season + holiday +      
                weather + temp +
                humidity, train_target)
summary(mod.lm5)

#### LM6 5v (without holiday) -> hour + season + weather + temp + humidity
mod.lm6 <- lm(count~ hour + season +      
                weather + temp +
                humidity, train_target)
summary(mod.lm6)

#### LM7 5v (Poly) -> hour + season + weather + temp + humidity. Se incluyen terminos polinomicos de orden mayor
mod.lm7 <- lm(count ~ poly(hour,2) + poly(season,2) + poly(weather,2)+
                 poly(temp,2) + poly(humidity,2) ,train_target)
summary(mod.lm7)

mod.lm7_step <- step(mod.lm7)
summary(mod.lm7_step)

############ DATA MINING #################################################################################################
#### Remove the wrong data → Data with more than 15ºC diference between atemp and temp.
training_target_mined <- training_target[abs(training_target$atemp - training_target$temp) <= 15, ]
plot(training_target_mined$temp, training_target_mined$atemp)

## REDO LM1
# Lineal model 1
mod.lm1 <- lm(count~ hour + season + holiday +      
                workingday + weather + temp +
                humidity + windspeed, training_target_mined)
summary(mod.lm1)
# Residuos versus variables predictoras
residualPlots(mod.lm1)
plot(mod.lm1$fitted.values, residualPlots(mod.lm1), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)


## REDO LM2
# Lineal model 2. categorical variables is shown as 1, 2, 3
# Shows only the categorical ones with less values, it has 3 categories, shows only 2. 2 shows 1
mod.lm2 <- lm(count~ hour + season +weather + temp +
                humidity + windspeed, training_target)
summary(mod.lm2)
# Residuos versus variables predictoras
residualPlots(mod.lm2)
plot(mod.lm1$fitted.values, residualPlots(mod.lm2), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)



#### Diferencia entre working day y no working day
# Plot Hour vs Count, colored by Workingday 
plot(training_target$hour, training_target$count, col = training_target$workingday,pch = 16, xlab = "Hour", ylab = "Count")

# Subset data with workingday and not workingday
training_target_work <- training_target[training_target$workingday == 1, ]
training_target_free <- training_target[training_target$workingday == 0, ]

#### Transformar datos houras por polinomio
x2<-x^2
x3<-x^3
z<-(x-6.5)^3
x4<-ifelse(z<0,0, z)
w<-(x-13)^3
x5<-ifelse(w<0,0,w)
B<-data.frame(x,x2,x3,x4,x5,y)
