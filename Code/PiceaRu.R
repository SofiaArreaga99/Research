#Picea rubens PiceaRu

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

## First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH


##Specie: Balsam Fir 

rsqp<-0.981 ##Published R^2 value 
minDBH<-5 #From Jenkin´s
maxDBH<-65 #From Jenkin´s
B0<- -1.6885 #From Ry´s paper
B1<- 2.1752  #From Ry´s paper
CF<- 1.008

##CREATE 10,000 RANDOM DBH

test <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)

##Create Random DBHs over the range sampled 


#Cm unit

dbhPiceaRu <- minDBH + (maxDBH - minDBH) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS##

## calculate the biomass using the published equation form

meany <- (exp(B0 + B1 * log(dbhPiceaRu))*CF) #Should I multiply for the CF?

##Introduce Random Error into calculated biomass

##this next part fuzzies the biomasses to create 1000 different populations ## 

ys <- matrix(rep(meany, times = 1000), nrow = length(meany), ncol = 1000)

## creating the steps used to generate wider ranges of fuzzed biomasses. This progression
## will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
## as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
stdevs <- seq(0.1, 100, length.out=1000)  #works better

stdevs2 <- matrix(rep(stdevs, each = 10000), nrow = 10000, ncol = length(stdevs))  
dbh2 <- matrix(rep(dbhPiceaRu, times = 1000), nrow = length(dbhPiceaRu), ncol = 1000)

#psuedys=ys+stdevs2.*(test);%this makes the new biomasses if no heteroscedasticity #

psuedys <- ys + stdevs2 * test * dbh2

#this makes the new biomasses with heteroscedasticity #

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
BMPiceaRu<- psuedys[, I]  # Select corresponding column

## Create figure for checking if result is reasonable ##


plot(dbhPiceaRu, BMPiceaRu, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "PiceaRu")

# Write the data in an Excel file

PseudoDataPiceaRu <- data.frame(dbhPiceaRu, BMPiceaRu)
PseudoDataPiceaRu <- subset(PseudoDataPiceaRu, BMPiceaRu>1)

plot(PseudoDataPiceaRu$dbhPiceaRu, PseudoDataPiceaRu$BMPiceaRu, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "PiceaRu")

# Specifies the full path to save the file

write.csv(PseudoDataPiceaRu, file = "PiceaRu.csv", row.names = FALSE)

## print(sse)
## mean(rsq2)


## Logaritmic differences

noiter<-10000
coefficients7 <- data.frame(intercept=rep(NA,noiter),slope=rep(NA,noiter))
for(i in 1:noiter){
  datatofit<- sample_n(PseudoDataPiceaRu,200,replace=FALSE)
  modelfit <- lm(log(BMPiceaRu) ~ log(dbhPiceaRu), data = na.omit(datatofit)) #Just add the other part
  
  
  coefficients7[i,] <- unname(coef(modelfit))
  
  
}



InterPicea<-mean(coefficients7$intercept)
SlopePicea<-mean(coefficients7$slope)

any(is.na(datatofit)) #NA revision in the data


View(PseudoDataPiceaRu)


View(coefficients7)


SDInterPicea<-sd(coefficients7$intercept) #standar deviation intercept

SDSlopePicea<-sd(coefficients7$slope)


### NEW COVARIANCE ##

library(MASS)

cov_matrix_PiceaRu <- cov(coefficients7)
mean_vector_PiceaRu <- colMeans(coefficients7)


View(cov_matrix_AcerSac)

# Simulate new pairs of a and b      Simular nuevos pares de a y b
sim_ab_PiceaRu <- as.data.frame(mvrnorm(n = 10, mu = mean_vector_PiceaRu, Sigma = cov_matrix_PiceaRu))



# Name columns for clarity            Nombrar columnas para claridad
colnames(sim_ab_PiceaRu) <- c("intercept_PiceaRu", "slope_PiceaRu")
sim_ab_PiceaRu$correlative <- seq_len(nrow(sim_ab_PiceaRu))
View(sim_ab_PiceaRu)


## Is it true "? 



# Originial Data Datos originales
plot(coefficients7$intercept, coefficients7$slope, 
     main = "Original vs Simulated Picea Rubrens", col = "blue", pch = 16, cex = 0.5,
     xlab = "Intercepto", ylab = "Slope")

# Add SImulations Agregar simulaciones
points(sim_ab_PiceaRu$intercept, sim_ab_PiceaRu$slope, col = rgb(1, 0, 0, 0.3), pch = 16)
legend("topright", legend = c("Original", "Simulated"),
       col = c("blue", "red"), pch = 16)





