# Instala y carga paquetes necesarios
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}
if (!requireNamespace("peakRAM", quietly = TRUE)) {
  install.packages("peakRAM")
}

library(rstudioapi)
library(parallel)
library(peakRAM)

# Función para ejecutar un algoritmo y registrar su uso de recursos
run_algoritmo <- function(numalt) {
  log_file <- paste0("log_algoritmo_", numalt, "_", Sys.getpid(), ".txt")
  write(paste("Ejecutando algoritmo", numalt, "en PID:", Sys.getpid(), Sys.time()),
        file = log_file, append = TRUE)
  
  # Nombres de algoritmos y scripts asociados
  algoritmo_nombre <- switch(
    as.character(numalt),
    "1" = "BAT",
    "2" = "CSCLP",
    "3" = "K-Medoids",
    "4" = "ACO",
    "5" = "PSO",
    "6" = "HCAKC",
    "7" = "KM-MILP",
    "8" = "SCK1",
    NA
  )
  
  archivo_algoritmo <- switch(
    as.character(numalt),
    "1" = "Bat.R",
    "2" = "CSCLP.R",
    "3" = "Kmedoids.R",
    "4" = "ACO.R",
    "5" = "PSO.R",
    "6" = "HCAKC.R",
    "7" = "KM-MILP.R",
    "8" = "SCK1_final.R",
    NA
  )
  
  if (!is.na(archivo_algoritmo)) {
    # Medir tiempo y RAM con peakRAM
    resultado <- peakRAM(source(archivo_algoritmo, local = TRUE))
    
    # Añadir info extra
    resultado$Algoritmo <- algoritmo_nombre
    resultado$PID <- Sys.getpid()
    resultado$Fecha <- Sys.time()
    
    # Guardar en CSV
    write.table(resultado, file = "resultados_peakRAM.csv",
                sep = ",", row.names = FALSE,
                col.names = !file.exists("resultados_peakRAM.csv"),
                append = TRUE)
    
    write(paste("Finalizado algoritmo", numalt, "PID:", Sys.getpid(), Sys.time()),
          file = log_file, append = TRUE)
    
  } else {
    warning(paste("Opción no válida:", numalt))
  }
}

# Función principal
Execute_Test <- function(numalt) {
  if (interactive()) {
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    print(script_dir)
    if (file.exists(script_dir)) {
      setwd(script_dir)
    } else {
      cat("El directorio no existe.\n")
    }
  }
  
  # Carga de datasets
  source("Openml.R")
  
  # Selección del algoritmo o ejecución paralela
  switch(
    as.character(numalt),
    "1" = { source("Bat.R") },
    "2" = { source("CSCLP.R") },
    "3" = { source("Kmedoids.R") },
    "4" = {source("ACO.R")},
    "5" = { source("PSO.R") },
    "6" = { source("HCAKC.R") },
    "7" = { source("KM-MILP.R") },
    "8" = { source("SCK1_final.R") },
    "9" = {
      cat("Ejecutando algoritmos 1 a 8 en paralelo...\n")
      opciones <- 1:8
      num_cores <- min(length(opciones), detectCores() - 1)
      cl <- makeCluster(num_cores)
      
      # 1. Cargar 'peakRAM' en todos los nodos del clúster
      clusterEvalQ(cl, {
        if (!requireNamespace("peakRAM", quietly = TRUE)) {
          install.packages("peakRAM", repos = "https://cloud.r-project.org")
        }
        library(peakRAM)
      })
      
      # 2. Exportar las funciones y variables necesarias
      clusterExport(cl, varlist = c("odatasets_unique", "run_algoritmo"), envir = environment())
      
      # 3. Ejecutar en paralelo
      parLapply(cl, opciones, run_algoritmo)
      
      stopCluster(cl)
      cat("Ejecución paralela finalizada.\n")
    },
    {
      warning("Opción no válida.")
    }
  )
  
}

# Ejecutar todo
Execute_Test(4)
