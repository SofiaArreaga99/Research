install.packages("dplyr")
install.packages("UsingR")
colnames<-(DataBaseHWF)
colnames(DataBaseHWF)
colnames<-("HWF")

View(DataBaseHWF)
DataBaseHWF$Y_kg2 <- (exp(DataBaseHWF$`B^0`+ DataBaseHWF$`B^1` * log(DataBaseHWF$`DBH cm`))) * DataBaseHWF$CF
write.csv(DataBaseHWF, "resultados_con_Ykg2.csv", row.names = FALSE)

summary(DataBaseHWF$Y_kg2)
sum(DataBaseHWF$Y_kg2, na.rm = TRUE)
