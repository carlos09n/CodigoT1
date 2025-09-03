# Instalar paquetes (solo la primera vez)
install.packages(c("readxl", "ggplot2", "dplyr"))

# Cargar librerías necesarias
library(readxl)
library(ggplot2)
library(dplyr)

# Leer el archivo Excel (asegúrate de tener la ruta correcta)
datos <- read_excel("datasets.xlsx")

# Gráfico simple: Instancias vs Variables
ggplot(datos, aes(x =Variables , y = Instancias, color = Instancias)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "lightblue", high = "red") +
  labs(
    x = "Número de variables",
    y = "Número de instancias",
    color = "Instancias"
  ) +
  theme_minimal()