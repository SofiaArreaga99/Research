
#Graphics first step#

library(ggplot2)

# data frame
datos <- data.frame(
  Year = c(1970, 1981, 1991, 2001, 2010, 2020),
  MgC = c(83.18, 73.74, 78.75, 87.27,94.16, 112),
  MinError = c(82.35, 72.85, 77.67, 86.05,92.82,109.61),
  MaxError = c(84.01, 74.63, 79.83, 88.49, 95.5, 112.79)
)


ggplot(datos, aes(x = Year, y = MgC)) +
  geom_line(color = "#2C3E50", size = 1) +  # Línea entre puntos
  geom_point(size = 3, color = "#E67E22", shape = 20, fill = "#E67E22") +  # Puntos
  geom_errorbar(aes(ymin = MinError, ymax = MaxError), width = 0.5, color = "#34495E", size = 1) +  # Barras de error 
  coord_cartesian(ylim = c(70, 125)) +  
  theme_minimal(base_size = 14) +  
  labs(
    x = "Year",
    y = "Mg C per Year",
    title = "Forest Biomass Trends\nHuntington Wildlife Forest"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2C3E50"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(color = "#2C3E50"),
    panel.grid.minor = element_blank()
  )

## Linear model #



model <- lm(MgC ~ Year, data = datos)
summary(model)



## Graphic of the linear model ##


ggplot(datos, aes(x = Year, y = MgC)) +
  geom_point(size = 4, color = "#E67E22", shape = 20, fill = "#E67E22") +  # Puntos
  geom_errorbar(aes(ymin = MinError, ymax = MaxError), width = 0.2, color = "#34495E", size = 1.5) +  # Barras de error
  geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", fill = "lightblue", alpha = 0.3) +  
  coord_cartesian(ylim = c(70, 125)) +
  labs(
    x = "Year",
    y = "Mg C per Year",
    title = "Linear Regression of Biomass over Time\nHuntington Wildlife Forest"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2C3E50"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(color = "#2C3E50"),
    panel.grid.minor = element_blank()
  )




