CFI2021


#Packages 
library(readxl)
library(plyr)
library(doBy)
library(dplyr)
library(knitr)
library(ggplot2)
library(RColorBrewer)
library(readr)

# THIS WAS DONE TO MAKE THE FIRST BIG MERGE OF THE PAIRS OF THE COEFFICIENTS #


## Using the covariance for the biomass estimation, Let´s see!! 

# 1. Create a list with all of the simulations, in this case just 10, to start. 


#lista_MEGAMerge <- list(coefficients1, coefficients2, coefficients3, coefficients4, coefficients5, coefficients6, coefficients7, coefficients8, coefficients9)
#View(lista_MEGAMerge)

# 2. Merge all of the elements, because they are more than 2 I need to use Reduce function
#Which is merging my elements by correlative that is just a number. 

#MergeCov <- Reduce(function(x, y) merge(x, y, by = "correlative"), lista_MEGAMerge)
#View(MergeCov)

#str(MergeCov)

# 1.     I wrote the excel but this is not neccesary technically 

#write.csv(MergeCov, file = "C:/Users/vanco/Desktop/ResearchR/Research/MergeCov.csv", row.names = FALSE)


# 3. Try to apply the same method ! I will suppose that everything is normal for now 

# 3.1 Import data frame of coefficients (All of them, for now just 10) 


frameCoeff2 <- read_delim("C:/Users/vanco/Desktop/ResearchR/Research/MergeCov.csv", delim = ",", 
                          escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
View(frameCoeff2)
#frameCoeff2 <-MergeCov

# 3.2 Import inventory data

Plot1HWF <- read.csv("C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI2021N.csv")
View(Plot1HWF)


# NEW import code for the plots, relative to each compartment  #

ComPlot <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/ComPlot.xlsx")




# 3.3 data frame to save results of each iteration

resultados_df <- data.frame(Plot = character(), PloKg_Ha.sum = numeric(), stringsAsFactors = FALSE)

# 3.4 Build the data frame with intercepts and slopes for this iteration

#n <-10

#for (i in 1:N)

for (i in 1:nrow(frameCoeff2)) {
  
  Extracction <- frameCoeff2[i, ]
  
  frameCoeffCV <- data.frame( especie = c("AbiesBal", "PiceaRubens","PinusStrobus","TsugaCana", "AcerRu", "AcerSacch", "BetulaAlle", "FagusGran", "FraxisAme"), 
                              Equation = c("1","2","3","4","5","6","7","8","9"),
                              MCintercept = c(Extracction$intercept_Abies, Extracction$intercept_PiceaRu, Extracction$intercept_Pinus, Extracction$intercept_Tsuga, Extracction$intercept_AcerRu, Extracction$intercept_AcerSac, Extracction$intercept_BetulAll, Extracction$intercept_Fagus, Extracction$intercept_Fraxis),
                              MCslope = c(Extracction$slope_Abies, Extracction$slope_PiceaRu, Extracction$slope_Pinus, Extracction$slope_Tsuga, Extracction$slope_AcerRu, Extracction$slope_AcerSac, Extracction$slope_BetulAll, Extracction$slope_Fagus, Extracction$slope_Fraxis),
                              CF = c(1.005, 1.008, 1.003, 1.003, 1.006, 1.005, 1.007, 1.010, 1.004))
  
  
  # 3.5 Create results dataframe of the different iterations//Crear dataframe de resultados de las diferentes iteraciones
  MatrixResult <- frameCoeffCV
  
  # 3.6 Merge data (Match species with the slope and intercept) #plyr package
  
  CombinationSI <- merge(MatrixResult, Plot1HWF, by.x = "Equation") #Two because of the different name xd
  
  # 3.7 Biomass estimation
  
  CombinationSI$Y_kg <- (exp(CombinationSI$MCintercept + CombinationSI$MCslope * log(CombinationSI$DBHcm))) * CombinationSI$CF
  
  
  # 4 Sum the biomass of poles an saw in each plot Calculate the total Y_kg by PlotSize and then scale it to hectares // Calcula el total de Y_kg por PlotSize para luego escalarlo a hectáreas
  #doBy
  
  
  SumBioPlots<-summaryBy(Y_kg~Plot+PlotSize, 
                         data=CombinationSI, FUN=sum)
  
  # 5. Convert to Kg/ha calling plot area to hectarea 
  
  
  SumBioPlots <- SumBioPlots %>%
    mutate(PloKg_Ha = ifelse(PlotSize == "POLE", ((Y_kg.sum*10000)/202.343), 
                             ifelse(PlotSize == "SAW", ((Y_kg.sum*10000)/809.372), NA)))
  
  
  
  #                                                    ACROSS PLOTS & Compartment       #
  
  
  # 6. Sum PLot (Pole+Saw) across plots 
  
  Sum_A_Plots<-summaryBy(PloKg_Ha~Plot, 
                         data=SumBioPlots, FUN=sum)
  View(Sum_A_Plots)
  
  
  #  NEW                                              ACROSS COMPARTMENTS             #
  
  # Merge data (Match plot with Compartment) plyr package
  
  Plots_Compa <- merge(Sum_A_Plots, ComPlot, by.x = "Plot") #Two because of the different name xd
  
  
  #View(Plots_Compa)
  
  
  # average by compartment 
  
  Average_Comp<-summaryBy(PloKg_Ha.sum~Comp, 
                         data=Plots_Compa, FUN=mean)
  
  #View(Average_Comp)
  
  #Where should I save the information collected here? 
  
  
  
  
  
  # NORMAL                                                ACROSS PLOTS                    #
  
  # 7. Average across plots- Mean of the sum of the plots  
  
  Average_A_Plots <- mean(Sum_A_Plots$PloKg_Ha, na.rm = TRUE)
  
  #print(Average_A_Plots)
  
  
  # 8. Add the results of Average_A_Plots to the data frame // Agregar los resultados de Average_A_Plots al data frame
  
  resultados_df <- rbind(resultados_df, Average_A_Plots)
  
}


# 9. View the results of all iterations // Ver los resultados de todas las iteraciones 



colnames(resultados_df) <- "PloKg_Ha"
resultados_df$mg_ha<- ((resultados_df$PloKg_Ha/1000)/2) #mg/ha
print(resultados_df)





# 10. Average the results of all iterations // Sumar los resultados de todas las iteraciones
total_mean_PloHa <- mean(resultados_df$PloKg_Ha, na.rm = TRUE)
total_mean_mg_ha <- mean(resultados_df$mg_ha, na.rm = TRUE)
print(total_mean_PloHa)
print(total_mean_mg_ha)
# 11. Standar deviation
total_sd_PloHa <- sd(resultados_df$PloKg_Ha, na.rm = TRUE)
total_sd_mgha <- sd(resultados_df$mg_ha, na.rm = TRUE)
print(total_sd_PloHa)
print(total_mean_mg_ha)


# 12. Totals
print(total_sd_mgha)
print(total_mean_mg_ha)

#coefficient of variation (CV) 
#deviation / mean *100

CV<-((total_sd_mgha/total_mean_mg_ha))*100

print(CV)
# round to two decimal places

total_mean_mg_ha <- round(total_mean_mg_ha, 2)
total_sd_mgha <- round(total_sd_mgha, 2)
CV<-((total_sd_mgha/total_mean_mg_ha))*100

print(paste("Results for wildlife forest of Huntington 2001:", 
            "mean =", total_mean_mg_ha, "mg_ha",
            ",","sd", total_sd_mgha, "mg_ha",
            ", min =", total_mean_mg_ha - total_sd_mgha,"mg_ha", 
            ", max =", total_mean_mg_ha + total_sd_mgha, "mg_ha",
            ", CV =", round(CV, 2), "%"))

######Graphs ######

#hist(resultados_df$PloKg_Ha)
#hist(resultados_df$mg_ha)

histmg_ha <- qplot(resultados_df$mg_ha, 
                   geom = "histogram",      # Tipo de gráfico: histograma
                   fill = I("forestgreen"),     # Color de las barras
                   color = I("black"),      # Color del borde de las barras
                   bins = 20)               # Número de bins


histmg_ha <- histmg_ha + 
  ggtitle("Biomass (mg per hectare)") +  # Título del gráfico
  xlab("Biomass in mg/ha") +             # Título del eje X
  ylab("Frequency") +                    # Título del eje Y
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título

# Show the graph - Mostrar el gráfico
print(histmg_ha)

