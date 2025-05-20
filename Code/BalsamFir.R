#Please dowload the packages if you don´t have them already. 

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

## 1. First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH


##Specie: Balsam Fir Abies 

rsqp<-0.993 ##Published R^2 value 
minDBH<-6.4516 #From Jenkin´s Unit: Cm 
maxDBH<-129.032 #From Jenkin´s Unit: Cm 
B0<- -2.5187 #From Ry´s paper
B1<- 2.416  #From Ry´s paper
CF<- 1.005 #Should we include it?

## 1.1 CREATE 10,000 RANDOM DBH

test <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)

## 1.2 Create Random DBHs over the range sampled 


dbhBalFir <- minDBH + (maxDBH - minDBH) * runif(10000, min = 0, max = 1)

## 2. CALCULATE BIOMASS##

## calculate the biomass using the published equation form

meany <- (exp(B0 + B1 * log(dbhBalFir)))*CF #Should I multiply for the CF?

## 3. Introduce Random Error into calculated biomass

##this next part fuzzies the biomasses to create 1000 different populations ## 

ys <- matrix(rep(meany, times = 1000), nrow = length(meany), ncol = 1000)

## creating the steps used to generate wider ranges of fuzzed biomasses. This progression
## will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
## as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
stdevs <- seq(0.1, 100, length.out=1000)  #works better

stdevs2 <- matrix(rep(stdevs, each = 10000), nrow = 10000, ncol = length(stdevs))  
dbh2 <- matrix(rep(dbhBalFir, times = 1000), nrow = length(dbhBalFir), ncol = 1000)

#psuedys=ys+stdevs2.*(test);%this makes the new biomasses if no heteroscedasticity #


#this makes the new biomasses with heteroscedasticity #
psuedys <- ys + stdevs2 * test * dbh2



rsq2 <- numeric(1000)  # memory allocation is all, speeds up

for (i in 1:1000) {  # get stats on the datasets using the fit vs. psuedo-population
  
  sst <- sum((psuedys[, i] - mean(psuedys[, i]))^2)  # Suma de cuadrados total // Total sum of squares
  sse <- sum((psuedys[, i] - meany)^2)  # Suma de cuadrados del error // Sum of squares of the error
  rsq2[i] <- 1 - (sse / sst)  # Coeficiente de determinación R² // Coefficient of determination R²
}


## To search for closest dataset to the published R^2, subtract the R^2 from
## the R^2 of the equation and select the diff closest to zero.  
## This is vectorized saving a for loop


diffs <- abs(rsq2 - rsqp)
I <- which.min(diffs)  # Find the index of the minimum value
BMBalFir<- psuedys[, I]  # Select corresponding column

## Create figure for checking if result is reasonable ##


plot(dbhBalFir, BMBalFir, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "BalFir")

# Write the data in an Excel file

PseudoDataBalFir <- data.frame(dbhBalFir, BMBalFir)
PseudoDataBalFir <- subset(PseudoDataBalFir, BMBalFir>1)

plot(PseudoDataBalFir$dbhBalFir, PseudoDataBalFir$BMBalFir, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "BalFir")

# Specifies the full path to save the file

write.csv(PseudoDataBalFir, file = "BalFir.csv", row.names = FALSE)

## print(sse)
## mean(rsq2)


## Logaritmic differences

noiter<-10000
coefficients1 <- data.frame(intercept=rep(NA,noiter),slope=rep(NA,noiter))
for(i in 1:noiter){
  datatofit<- sample_n(PseudoDataBalFir,200,replace=FALSE)
  modelfit <- lm(log(BMBalFir) ~ log(dbhBalFir), data = na.omit(datatofit)) #Just add the other part
  
  
  coefficients1[i,] <- unname(coef(modelfit))
  
  
}

View(coefficients1)
print(coefficients1)


InterAbies<-mean(coefficients1$intercept)
SlopeAbies<-mean(coefficients1$slope)


any(is.na(datatofit)) #NA revision in the data


View(coefficients1)


SDInterAbies<-sd(coefficients1$intercept) #standar deviation intercept

SDSlopeAbies<-sd(coefficients1$slope)


### NEW COVARIANCE ##

library(MASS)

cov_matrix_Abies <- cov(coefficients1)
mean_vector_Abies <- colMeans(coefficients1)


View(cov_matrix_Abies)

# Simulate new pairs of a and b         Simular nuevos pares de a y b
sim_ab_Abies <- as.data.frame(mvrnorm(n = 10, mu = mean_vector_Abies, Sigma = cov_matrix_Abies))



# Name columns for clarity              Nombrar columnas para claridad
colnames(sim_ab_Abies) <- c("intercept_Abies", "slope_Abies")

sim_ab_Abies$correlative <- seq_len(nrow(sim_ab_Abies))
View(sim_ab_Abies)


## Is it true "? 



# Original Data Datos originales
plot(coefficients1$intercept, coefficients1$slope, 
     main = "Original vs Simulated Balsam Fir", col = "blue", pch = 16, cex = 0.5,
     xlab = "Intercepto", ylab = "Pendiente")

# Agregar simulaciones
points(sim_ab_Abies$intercept, sim_ab_Abies$slope, col = rgb(1, 0, 0, 0.3), pch = 16)
legend("topright", legend = c("Original", "Simulated"),
       col = c("blue", "red"), pch = 16)


