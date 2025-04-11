
#install.packages("doBy")

#Packages 
library(readxl)
library(plyr)
library(doBy)
library(dplyr)

# Import data frame of coefficients // Importar datos de los coeficientes

frameCoeff <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Process_data/DataFrame.xlsx")
#View(Coefficientes)

# Import test data // Importar datos de prueba

Plot1HWF <- read_excel("Raw_data/Plot1HWF.xlsx")
#View(Plot1HWF)

# data frame to save results of each iteration // data frame para guardar resultados de cada iteración

resultados_df <- data.frame(Plot = character(), PloKg_Ha.sum = numeric(), stringsAsFactors = FALSE)

# Repeat the process n times // Repetir el proceso n veces 

for (j in 1:5) {
  frameCoeff$MCintercept <- sapply(1:nrow(frameCoeff), function(i) rnorm(1, mean = frameCoeff$intercepto[i], sd = frameCoeff$sd_intercepto[i]))
  frameCoeff$MCslope <- sapply(1:nrow(frameCoeff), function(i) rnorm(1, mean = frameCoeff$pendiente[i], sd = frameCoeff$sd_pendiente[i]))
  
  # Create results dataframe of the different iterations//Crear dataframe de resultados de las diferentes iteraciones
  MatrixResult <- frameCoeff[, c("especie", "ecuacion", "MCintercept", "MCslope", "CF")]
  
  #View(MatrixResult)
  
  # Unir datos // merge data (Match species with the slope and intercept) #plyr package
  
  CombinationSI <- merge(MatrixResult, Plot1HWF, by.x = "ecuacion", by.y = "Equation") #Two because of the different name xd
  
  #View(CombinationSI)
  
  # Estimación de biomasa // Biomass estimation
  
  CombinationSI$Y_kg <- (exp(CombinationSI$MCintercept + CombinationSI$MCslope * log(CombinationSI$DBHcm))) * CombinationSI$CF
  
  #View(CombinationSI)

  #1. Sum the biomass of poles an saw in each plot Calculate the total Y_kg by PlotSize and then scale it to hectares // Calcula el total de Y_kg por PlotSize para luego escalarlo a hectáreas
#doBy


SumBioPlots<-summaryBy(Y_kg~Plot+PlotSize, 
          data=CombinationSI, FUN=sum)
#View(SumBioPlots)

# 2. Convert to Kg/ha calling plot area to hectarea // Escalación de área de parcela a hectárea
#dplyr


SumBioPlots <- SumBioPlots %>%
  mutate(PloKg_Ha = ifelse(PlotSize == "POLE", ((Y_kg.sum*10000)/202.343), 
                                       ifelse(PlotSize == "SAW", ((Y_kg.sum*10000)/809.372), NA)))

# 3. Sum PLot (Pole+Saw) across plots 

Sum_A_Plots<-summaryBy(PloKg_Ha~Plot, 
                       data=SumBioPlots, FUN=sum)
#View(Sum_A_Plots)


# 4. Average across plots- Mean of the sum of the plots  

Average_A_Plots <- mean(Sum_A_Plots$PloKg_Ha, na.rm = TRUE)

#print(Average_A_Plots)


# 5. Add the results of Average_A_Plots to the data frame // Agregar los resultados de Average_A_Plots al data frame

resultados_df <- rbind(resultados_df, Average_A_Plots)


}


# View the results of all iterations // Ver los resultados de todas las iteraciones 
colnames(resultados_df) <- "PloKg_Ha"
print(resultados_df)



# Average the results of all iterations // Sumar los resultados de todas las iteraciones
total_mean_PloHa <- mean(resultados_df$PloKg_Ha, na.rm = TRUE)

print(total_mean_PloHa)
# Standar deviation
total_sd_PloHa <- sd(resultados_df$PloKg_Ha, na.rm = TRUE)
print(total_sd_PloHa)



