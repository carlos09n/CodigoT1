if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}
library(rstudioapi)
Execute_Test <- function (numalt){
  if (interactive()) {
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    print(script_dir)
    # Comprobar si la carpeta existe
    if (file.exists(script_dir)) {
      setwd(script_dir)
    } else {
      cat("El directorio no existe.\n")
    }
  }
  
  # Verifica el directorio de trabajo actual
  #getwd()
  source("Openml.R")
  switch(
    as.character(numalt),
    "1" = { source("Bat.R"); },
    "2" = { source("CSCLP.R"); },
    "3" = { source("Kmedoids.R"); },
    "4" = { source("ACO.R"); },
    "5" = { source("PSO.R"); },
    "6" = { source("HCAKC.R"); },
    "7" = { source("PSO.R");
      source("HCAKC.R"); },
    warning("Opción no válida.")
  )
      
}
Execute_Test(6)
