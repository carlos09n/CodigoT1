library(readxl)
library(dplyr)
library(ggplot2)

# ============================
# Leer datos
# ============================
df <- read_excel("tabla_comparativa-f.xlsx")

# Corregir formato de números
df <- df %>%
  mutate(Tiempo = as.numeric(gsub(",", ".", as.character(Tiempo)))) %>%
  mutate(Algoritmo = gsub("^SCK1$", "SCK", Algoritmo))

# ============================
# Definir colores
# ============================
colores_algoritmos <- c(
  "CSCLP"       = "#a2fb8f",  
  "HCAKC"       = "#ffbe0a",  
  "K-MeansACO"  = "#e892f5",  
  "K-MeansPSO"  = "#e98cab",  
  "K-MedoidsSC" = "#8ffae8",  
  "KMeansBA"    = "#fe8282",  
  "MILP-KM"     = "#ffe968",  
  "SCK"         = "#8391fa"   
)

# ============================
# Gráfico de tiempo de ejecución
# ============================
plot_tiempo <- ggplot(df, aes(x = id_d, y = Tiempo, color = Algoritmo, group = Algoritmo)) +
  geom_line(size = 0.9, alpha = 0.9) +
  geom_point(size = 1.8, alpha = 0.9) +
  scale_color_manual(values = colores_algoritmos) +
  labs(x = "ID Dataset", y = "Tiempo (s)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Mostrar gráfico
print(plot_tiempo)
