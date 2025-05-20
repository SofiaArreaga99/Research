Update5Covarianza1



### Prueba 2 


lista_MEGAMerge <- list(sim_ab_Abies, sim_ab_BetulAll, sim_ab_Fagus, sim_ab_Fraxis,
                  sim_ab_AcerRu, sim_ab_AcerSac, sim_ab_PiceaRu, sim_ab_Pinus, sim_ab_Tsuga)

MergeCov <- Reduce(function(x, y) merge(x, y, by = "correlative"), lista_MEGAMerge)
View(MergeCov)

write.csv(MergeCov, file = "C:/Users/vanco/Desktop/ResearchR/Research/MergeCov.csv", row.names = FALSE)


getwd()
