#Union Clean one :'v #

### Simulations

# Definir especies y ecuaciones
species <- c("AbiesBal", "PiceaRubens", "PinusStrobus", "TsugaCana", "AcerRu", "AcerSacch", "BetulaAlle", "FagusGran", "FraxisAme")
equations <- 1:9

# Crear dataframe de coeficientes
frameCoeff <- data.frame(
  especie = species,
  ecuacion = equations,
  intercepto = c(InterAbies, InterPicea, InterPinus, InterTsuga, InterAcer, InterAcerSac, InterBetula, InterFagus, Interfraxis),
  sd_intercepto = c(SDInterAbies, SDInterPicea, SDInterPinus, SDInterTsuga, SDInterAcer, SDInterAcerSac, SDInterBetula, SDInterFagus, SDInterceptfraxis),
  pendiente = c(SlopeAbies, SlopePicea, SlopePinus, SlopeTsuga, SlopeAcer, SlopeAcerSac, SlopeBetula, SlopeFagus, Slopefraxis),
  sd_pendiente = c(SDSlopeAbies, SDSlopePicea, SDSlopePinus, SDSlopeTsuga, SDSlopeAcer, SDSlopeAcerSac, SDSlopeBetula, SDSlopeFagus, SDSlopefraxis),
  CF = c(1.005, 1.008, 1.003, 1.003, 1.006, 1.005, 1.007, 1.01, 1.004)
)

# Importar datos de prueba
library(readxl)
Plot1HWF <- read_excel("Raw_data/Plot1HWF.xlsx")


# Lista para guardar resultados
df_list <- list()

# Repetir el proceso 10 veces
for (j in 1:5) {
  frameCoeff$MCintercept <- sapply(1:nrow(frameCoeff), function(i) rnorm(1, mean = frameCoeff$intercepto[i], sd = frameCoeff$sd_intercepto[i]))
  frameCoeff$MCslope <- sapply(1:nrow(frameCoeff), function(i) rnorm(1, mean = frameCoeff$pendiente[i], sd = frameCoeff$sd_pendiente[i]))
  
  # Crear dataframe de resultados
  MatrixResult <- frameCoeff[, c("especie", "ecuacion", "MCintercept", "MCslope", "CF")]
  

  # Unir datos
  library(dplyr)
  Combination2 <- merge(MatrixResult, Plot1HWF, by.x = "ecuacion", by.y = "Equation")
  
  # Estimación de biomasa
  Combination2$Y_kg <- (exp(Combination2$MCintercept + Combination2$MCslope * log(Combination2$DBHcm))) * Combination2$CF
  
  # Agregar número de iteración
  Combination2$Iteration <- j
  
  # Calcular la media de Y_kg para cada iteración
  Iteration_Mean <- mean(Combination2$Y_kg, na.rm = TRUE)
  
  # Agregar la media al dataframe de resultados
  Combination2$Iteration_Mean <- Iteration_Mean
  
  # Guardar cada iteración en la lista
  df_list[[j]] <- Combination2
}

# Combinar todas las iteraciones en un solo dataframe
FinalResult <- do.call(rbind, df_list)

# Escribir los resultados en un archivo CSV
write.csv(FinalResult, "results_with_Ykg_and_iteration.csv", row.names = FALSE)

# Graficar
plot(Y_kg ~ DBHcm, col = as.factor(especie), data = FinalResult)

# Resumen por parcela
library(plyr)
ddply(FinalResult, .(Plot), summarize,
      Total = round(sum(Y_kg, na.rm = TRUE), 2),
      Mean = round(mean(Y_kg, na.rm = TRUE), 2))


View(FinalResult)

dim(FinalResult)
