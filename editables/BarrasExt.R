library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# ============================
# Leer datos desde Excel
# (si tu archivo tiene varias pestañas, ajusta "sheet")
# ============================
df <- read_excel("tabla_comparativa-f.xlsx")  

# Corregir formatos numéricos y nombres
df <- df %>%
  mutate(across(c(ARI, AMI, NMI, Silhouette),
                ~ as.numeric(gsub(",", ".", as.character(.))))) %>%
  mutate(Algoritmo = gsub("^SCK1$", "SCK", Algoritmo))

# ============================
# Calcular "victorias" en cada métrica
# ============================
victorias_ari <- df %>%
  group_by(id_d) %>%
  filter(ARI == max(ARI, na.rm = TRUE)) %>%
  ungroup() %>%
  count(Algoritmo, name = "ARI")

victorias_ami <- df %>%
  group_by(id_d) %>%
  filter(AMI == max(AMI, na.rm = TRUE)) %>%
  ungroup() %>%
  count(Algoritmo, name = "AMI")

victorias_nmi <- df %>%
  group_by(id_d) %>%
  filter(NMI == max(NMI, na.rm = TRUE)) %>%
  ungroup() %>%
  count(Algoritmo, name = "NMI")

# ============================
# Unir todo en un solo dataframe
# ============================
victorias <- full_join(victorias_ari, victorias_ami, by = "Algoritmo") %>%
  full_join(victorias_nmi, by = "Algoritmo") %>%
  replace(is.na(.), 0)

# Pasar a formato largo
victorias_long <- victorias %>%
  pivot_longer(cols = c("ARI", "AMI", "NMI"),
               names_to = "Metrica",
               values_to = "Victorias")

# ============================
# Graficar barras agrupadas
# ============================
ggplot(victorias_long,
       aes(x = Algoritmo, y = Victorias, fill = Metrica)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.7) +
  labs(
    x = "Algoritmo",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")
