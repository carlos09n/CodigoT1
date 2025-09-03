library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Leer datos
df <- read_excel("tabla_comparativa-f.xlsx")

# Corregir formatos numéricos y nombres
df <- df %>%
  mutate(across(c(ARI, AMI, NMI, Silhouette),
                ~ as.numeric(gsub(",", ".", as.character(.))))) %>%
  mutate(Algoritmo = gsub("^SCK1$", "SCK", Algoritmo))

# Calcular cuántas veces cada algoritmo tuvo el mayor índice de silueta
mejor_sil <- df %>%
  group_by(id_d) %>%
  filter(Silhouette == max(Silhouette, na.rm = TRUE)) %>%
  ungroup() %>%
  count(Algoritmo, name = "Veces_mejor_Silhouette")

# Asegurar que todos los algoritmos aparezcan (aunque con 0)
todos_algoritmos <- df %>%
  distinct(Algoritmo) %>%
  pull()

mejor_sil_completo <- mejor_sil %>%
  complete(Algoritmo = todos_algoritmos, fill = list(Veces_mejor_Silhouette = 0))

# Graficar
ggplot(mejor_sil_completo,
       aes(x = reorder(Algoritmo, -Veces_mejor_Silhouette),
           y = Veces_mejor_Silhouette,
           fill = Algoritmo)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    x = "Algoritmo",
    y = "Frecuencia S(i)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")
