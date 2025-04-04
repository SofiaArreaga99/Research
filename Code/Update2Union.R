## Simulations

# Definir especies y ecuaciones // Define species and equations
species <- c("AbiesBal", "PiceaRubens", "PinusStrobus", "TsugaCana", "AcerRu", "AcerSacch", "BetulaAlle", "FagusGran", "FraxisAme")
equations <- 1:9

# Crear dataframe de coeficientes // Create coefficient dataframe
frameCoeff <- data.frame(
  especie = species,
  ecuacion = equations,
  intercepto = c(InterAbies, InterPicea, InterPinus, InterTsuga, InterAcer, InterAcerSac, InterBetula, InterFagus, Interfraxis),
  sd_intercepto = c(SDInterAbies, SDInterPicea, SDInterPinus, SDInterTsuga, SDInterAcer, SDInterAcerSac, SDInterBetula, SDInterFagus, SDInterceptfraxis),
  pendiente = c(SlopeAbies, SlopePicea, SlopePinus, SlopeTsuga, SlopeAcer, SlopeAcerSac, SlopeBetula, SlopeFagus, Slopefraxis),
  sd_pendiente = c(SDSlopeAbies, SDSlopePicea, SDSlopePinus, SDSlopeTsuga, SDSlopeAcer, SDSlopeAcerSac, SDSlopeBetula, SDSlopeFagus, SDSlopefraxis),
  CF = c(1.005, 1.008, 1.003, 1.003, 1.006, 1.005, 1.007, 1.01, 1.004)
)

# Importar datos de prueba // Import test data
library(readxl)
Plot1HWF <- read_excel("Raw_data/Plot1HWF.xlsx")

# Lista para guardar resultados de cada iteración // List to save results of each iteration
df_list <- list()

# Repetir el proceso n veces // Repeat the process n times
for (j in 1:5) {
  frameCoeff$MCintercept <- sapply(1:nrow(frameCoeff), function(i) rnorm(1, mean = frameCoeff$intercepto[i], sd = frameCoeff$sd_intercepto[i]))
  frameCoeff$MCslope <- sapply(1:nrow(frameCoeff), function(i) rnorm(1, mean = frameCoeff$pendiente[i], sd = frameCoeff$sd_pendiente[i]))
  
  # Crear dataframe de resultados //Create results dataframe
  MatrixResult <- frameCoeff[, c("especie", "ecuacion", "MCintercept", "MCslope", "CF")]
  
  # Unir datos // merge data (Match species with the slope and intercept) 
  library(dplyr)
  Combination2 <- merge(MatrixResult, Plot1HWF, by.x = "ecuacion", by.y = "Equation") #Two because of the different name xd
  
  # Estimación de biomasa // Biomass estimation
  
  Combination2$Y_kg <- (exp(Combination2$MCintercept + Combination2$MCslope * log(Combination2$DBHcm))) * Combination2$CF
  
  ## Calcula la media de Y_kg por PlotSize para luego escalarlo a hectáreas
  #La idea es tener el promedio entre TIPO de parcela para escalarlo
  #a hectarea despues
  ## Calculate the mean of Y_kg by PlotSize 
  
  mean_by_plot <- ddply(Combination2, .(PlotSize), summarize,
                        Total_Y_kg = mean(Y_kg, na.rm = TRUE))
  
  # Escalación de área de parcela a hectárea //Scaling plot area to hectare
  
  # Plot Size in HA 
  # 1 Ha = 10,000 m2
  # SAW 1/5 acre 0.08096 Ha or 809.372 m2
  # POLE 1/20 acre 0.02035 Ha 202.343 m2
  
  mean_by_plot <- mean_by_plot %>%
    mutate(PloHa = ifelse(PlotSize == "POLE", ((Total_Y_kg*10000)/202.343), 
                          ifelse(PlotSize == "SAW", ((Total_Y_kg*10000)/809.372), NA)))
  
  # Calcular el total de PloHa // Calculate the total of PloHa
  total_PloHa <- sum(mean_by_plot$PloHa, na.rm = TRUE)
  
  
  # Guardar cada iteración en la lista // Save each iteration in the list
  df_list[[j]] <- total_PloHa
}

# Ver los resultados de todas las iteraciones // View the results of all iterations

print(df_list)

# Si quieres convertir la lista en un vector para análisis adicional
# converting the list to a vector for further analysis

total_PloHa_results <- unlist(df_list)
print(total_PloHa_results)

# Sumar los resultados de todas las iteraciones/sum the results of all iterations

total_mean_PloHa <- mean(total_PloHa_results, na.rm = TRUE)
print(total_mean_PloHa)

# Standar deviation

total_sd_PloHa <- sd(total_PloHa_results, na.rm = TRUE)
print(total_sd_PloHa)


