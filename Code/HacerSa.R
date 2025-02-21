#Fagus grandifolia

#Calling packages

library(fitdistrplus)
library(distr)
library(dplyr)

## First Step  CREATE 10,000 RANDOM DBH // Large Numer DBH


##Specie: #Fagus grandifolia FagusGr

rsqp<-0.994 ##Published R^2 value 
minDBH<-6.4516 #From Jenkin´s
maxDBH<-167.7416 #From Jenkin´s
B0<- -1.7448 #From Ry´s paper
B1<- 2.3613  #From Ry´s paper
CF<- 1.01 #Should we include it?

##CREATE 10,000 RANDOM DBH

test <- matrix(rnorm(10000 * 1000), nrow = 10000, ncol = 1000)

##Create Random DBHs over the range sampled 


#Cm unit

dbhFagusGr <- minDBH + (maxDBH - minDBH) * runif(10000, min = 0, max = 1)

## CALCULATE BIOMASS##

## calculate the biomass using the published equation form

meany <- exp(B0 + B1 * log(dbhFagusGr)) #Should I multiply for the CF?

##Introduce Random Error into calculated biomass

##this next part fuzzies the biomasses to create 1000 different populations ## 

ys <- matrix(rep(meany, times = 1000), nrow = length(meany), ncol = 1000)

## creating the steps used to generate wider ranges of fuzzed biomasses. This progression
## will create 1000 different biomass sets. It can be adjusted to larger or smaller steps
## as needed to match as close as possible the published R-square.

#stdevs <- seq(1, 200.8, by = 0.2)            
stdevs <- seq(0.1, 100, length.out=1000)  #works better

stdevs2 <- matrix(rep(stdevs, each = 10000), nrow = 10000, ncol = length(stdevs))  
dbh2 <- matrix(rep(dbhFagusGr, times = 1000), nrow = length(dbhFagusGr), ncol = 1000)

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
BMFagusGr<- psuedys[, I]  # Select corresponding column

## Create figure for checking if result is reasonable ##


plot(dbhFagusGr, BMFagusGr, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "FagusGr")

# Write the data in an Excel file

PseudoDataFagusGr <- data.frame(dbhFagusGr, BMFagusGr)
PseudoDataFagusGr <- subset(PseudoDataFagusGr, BMFagusGr>1) ##We ignore the values less than 1

plot(PseudoDataFagusGr$dbhFagusGr, PseudoDataFagusGr$BMFagusGr, pch = 16, xlab = "DBH (cm)", ylab = "Biomass (kg)", main = "FagusGr")

# Specifies the full path to save the file

write.csv(PseudoDataFagusGr, file = "FagusGr.csv", row.names = FALSE)

## print(sse)
## mean(rsq2)


## Logaritmic differences

noiter<-10000
coefficients <- data.frame(intercept=rep(NA,noiter),slope=rep(NA,noiter))
for(i in 1:noiter){
  datatofit<- sample_n(PseudoDataFagusGr,200,replace=FALSE)
  modelfit <- lm(log(BMFagusGr) ~ log(dbhFagusGr), data = na.omit(datatofit)) #Just add the other part
  
  
  coefficients[i,] <- unname(coef(modelfit))
  
  
}



mean(coefficients$intercept)
mean(coefficients$slope)


any(is.na(datatofit)) #NA revision in the data


View(coefficients)


sd(coefficients$intercept) #standar deviation intercept

sd(coefficients$slope)
