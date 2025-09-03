library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# ============================
# Leer datos
# ============================
df <- read_excel("tabla_comparativa-f.xlsx")

# Corregir formatos numéricos y nombres
df <- df %>%
  mutate(across(c(ARI, AMI, NMI, Silhouette),
                ~ as.numeric(gsub(",", ".", as.character(.))))) %>%
  mutate(Algoritmo = gsub("^SCK1$", "SCK", Algoritmo))

# ============================
# Reestructurar datos a formato largo
# ============================
df_long <- df %>%
  pivot_longer(cols = c("ARI", "AMI", "NMI","Silhouette"),
               names_to = "Metrica",
               values_to = "Valor")

# ============================
# Función para graficar cada métrica
# ============================
colores_algoritmos <- c(
  "CSCLP"       = "#a2fb8f",  # 
  "HCAKC"       = "#ffbe0a",  # 
  "K-MeansACO"  = "#e892f5",  # 
  "K-MeansPSO"  = "#e98cab",  # 
  "K-MedoidsSC" = "#8ffae8",  # 
  "KMeansBA"    = "#fe8282",  # 
  "MILP-KM"     = "#ffe968",  #
  "SCK"         = "#8391fa"   #
)
graficar_lineas <- function(metrica_nombre) {
  ggplot(filter(df_long, Metrica == metrica_nombre),
         aes(x = id_d, y = Valor, color = Algoritmo, group = Algoritmo)) +
    geom_line(size = 0.9, alpha = 0.9) +
    geom_point(size = 1.8, alpha = 0.9) +   # <<--- aquí agregamos los puntos
    scale_color_manual(values = colores_algoritmos) +
    labs(x = "ID Dataset", y = metrica_nombre, title = paste("Resultados", metrica_nombre)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

# ============================
# Generar los gráficos
# ============================
plot_ARI <- graficar_lineas("ARI")
plot_AMI <- graficar_lineas("AMI")
plot_NMI <- graficar_lineas("NMI")
plot_S <- graficar_lineas("Silhouette")
# Mostrar en pantalla
print(plot_ARI)
print(plot_AMI)
print(plot_NMI)
print(plot_S)

