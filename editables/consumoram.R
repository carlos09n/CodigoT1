library(readxl)
library(dplyr)
library(ggplot2)

# ============================
# Leer datos de Peak RAM
# ============================
df_ram <- read_excel("resultados_peakRAM.xlsx")

# Convertir a numérico si es necesario
df_ram <- df_ram %>%
  mutate(Peak_RAM = as.numeric(gsub(",", ".", as.character(Peak_RAM_Used_MiB)))) %>%
  mutate(Algoritmo = gsub("^SCK1$", "SCK", Algoritmo))

# ============================
# Gráfico de barras (todo azul, sin glosario)
# ============================
plot_ram <- ggplot(df_ram, aes(x = reorder(Algoritmo, -Peak_RAM), 
                               y = Peak_RAM)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#4c80b2") +
  labs(x = "Algoritmo", 
       y = "RAM (MiB)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# Mostrar gráfico
print(plot_ram)
