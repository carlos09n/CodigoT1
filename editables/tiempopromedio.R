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
# 2) Gráfico de tiempo promedio (codo)
# ============================
df_promedios <- df %>%
  group_by(Algoritmo) %>%
  summarise(TiempoPromedio = mean(Tiempo, na.rm = TRUE)) %>%
  arrange(desc(TiempoPromedio))   # <<--- ordenar de mayor a menor

plot_promedio <- ggplot(df_promedios, aes(x = reorder(Algoritmo, -TiempoPromedio), 
                                          y = TiempoPromedio, 
                                          group = 1)) +
  geom_line(color = "#4c80b2", size = 1) +   # línea azul
  geom_point(color = "#4c80b2", size = 3) +  # puntos azules
  labs(x = "Algoritmo", y = "Tiempo promedio (segundos)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


print(plot_promedio)
