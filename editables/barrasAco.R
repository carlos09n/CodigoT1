library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Leer datos
df <- read_excel("tabla_comparativa-f.xlsx")

# Función para limpiar las listas
limpiar_card <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, "c\\(|\\)")
  x <- str_replace_all(x, ",", " ")
  x <- str_squish(x)
  as.numeric(unlist(str_split(x, " ")))
}

df$Card_Pred_List <- lapply(df$Card_Pred, limpiar_card)
df$Card_Real_List <- lapply(df$Card_Real, limpiar_card)

# Calcular porcentaje total de error por fila
calcular_porcentaje_error_total <- function(pred, real) {
  if (length(pred) != length(real)) return(NA)
  total_real <- sum(real, na.rm = TRUE)
  total_error <- sum(abs(pred - real), na.rm = TRUE)
  if (total_real == 0) return(NA)
  (total_error / total_real) * 100
}

df$Error_Pct_Total <- mapply(calcular_porcentaje_error_total, df$Card_Pred_List, df$Card_Real_List)

# Filtrar datos válidos
df_filtrado <- df %>% filter(!is.na(Error_Pct_Total) & Error_Pct_Total > 0)

# Ordenar por mayor porcentaje de error
df_filtrado <- df_filtrado %>%
  arrange(desc(Error_Pct_Total))

# Crear factor ordenado para eje X
df_filtrado$Ndataset <- factor(df_filtrado$id_d, levels = df_filtrado$id_d)

# Graficar
ggplot(df_filtrado, aes(x = Ndataset, y = Error_Pct_Total, fill = Error_Pct_Total)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(
    colors = colorRampPalette(c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"))(100)
  ) +
  labs(x = "Dataset", y = "Porcentaje Total de Error (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_blank(),
    legend.position = "right"
  )
