############################################################
rm(list=ls())
############################################################
install.packages('randomForest')
install.packages('party')
install.packages('e1071')
install.packages('minerva')
library(randomForest)
library(party)
library(e1071)
library(corrplot)
library(minerva)

# Read data
setwd('/Users/danielaquintero/Downloads/Estadistica/data')
training <- read.csv2('p1_train.csv',header=TRUE, stringsAsFactors = TRUE)
testing <- read.csv2('p1_test.csv',header=TRUE, stringsAsFactors = TRUE)

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

# Plot histogram of all variables
boxplot(training_target~training_feat)
boxplot(count~hour,training_target)
boxplot(count~season,training_target)
boxplot(count~holiday,training_target)
boxplot(count~workingday,training_target)
boxplot(count~weather,training_target)
boxplot(count~temp,training_target)
boxplot(count~atemp,training_target)
boxplot(count~humidity,training_target)
boxplot(count~windspeed,training_target)

# Use lapply to apply the factor function to each specified column
cols_to_convert <- c("season", "holiday", "workingday", "weather") # we let "hour" as continue
training_feat[cols_to_convert] <- lapply(training_feat[cols_to_convert], factor)
training_target[cols_to_convert] <- lapply(training_target[cols_to_convert], factor)
testing[cols_to_convert] <- lapply(testing[cols_to_convert], factor)

#### Correlation matrix using numerical variables
# Convert count as numeric
training_target$count <- as.numeric(training_target$count)
# Specify the column names you want to include in the correlation matrix
selected_columns <- c("hour", "temp", "atemp", "humidity", "windspeed")
# Subset the data frame to include only the selected columns
selected_data <- training_target[selected_columns]
# Compute the correlation matrix
correlation_matrix <- cor(selected_data)
# Print or view the correlation matrix
print(correlation_matrix)

# See correlation of "Count" variable against the other numerical variables
sel_columns <- c("count", "hour", "temp", "atemp", "humidity", "windspeed")
selected_data <- training_target[sel_columns]
# Calculate the correlation matrix
correlation_matrix <- cor(selected_data)
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
plot(training_target$temp, training_target$atemp)

# Lineal model 1
mod.lm1 <- lm(count~ hour + season + holiday +      
                workingday + weather + temp +
                humidity + windspeed, training_target)
summary(mod.lm1)

# Residuos versus variables predictoras
# install.packages('car')
library(car)
residualPlots(mod.lm1)

# Lineal model 2. categorical variables is shown as 1, 2, 3
# Shows only the categorical ones with less values, it has 3 categories, shows only 2. 2 shows 1
mod.lm2 <- lm(count~ hour + season +weather + temp +
                humidity + windspeed, training_target)
summary(mod.lm2)

# Lineal model 3
mod.lm3 <- lm(count~ hour + season + weather + atemp + humidity + 
                windspeed, training_target)    
summary(mod.lm3)

##-- Predicciones
pr <- predict(mod.lm3, testing)   # Predicciones para los nuevos valores
par(mfrow=c(1,1))           
plot(pr, testing$count, asp=1)    # Predicciones vs valores observados 
abline(0,1, col=2, lwd=2)         # Bisectriz

