library(cluster)
library(proxy)
library(mlr3oml)
library(mlr3)
library(pryr)
library(dplyr) 
library(aricode)
library(ggplot2)
library(corrplot)
library(clValid)
library(RColorBrewer)
library(factoextra) # For PCA visualization
#----------------------------------------------------------------------------------------------#
# HCAKC
#----------------------------------------------------------------------------------------------#
# Function to prepare data with validation of the dependent variable
prepare_data <- function(dataset) {
  # Ensure that the dataset is a data.frame
  dataset <- as.data.frame(dataset)
  
  # Check if the "class" column exists
  if ("class" %in% colnames(dataset)) {
    # Separate the "class" variable from the rest of the columns
    y <- dataset$class
    X <- dataset[, setdiff(colnames(dataset), "class")]
  } 
  # Validate if the last column is categorical or integer
  else if (is.factor(dataset[, ncol(dataset)]) || is.integer(dataset[, ncol(dataset)])) {
    X <- dataset[, -ncol(dataset)]
    y <- dataset[, ncol(dataset)]
  } 
  # If no condition is met, return NULL to ignore this dataset
  else {
    return(NULL)
  }
  
  # Return a list with variables X and y
  list(X = X, y = y)
}
run_HCAKC <- function(X, y, target_cardinality, dataset_name) {
  library(cluster)
  library(mclust)
  library(aricode)
  
  # Initial validations
  if (is.null(X) || is.null(y) || is.null(target_cardinality)) {
    stop("Datos nulos detectados. Verifique que X, y y target_cardinality no sean NULL.")
  }
  
  if (nrow(X) != length(y)) {
    stop("El número de filas en X no coincide con la longitud de y.")
  }
  
  if (sum(target_cardinality) != nrow(X)) {
    stop("La suma de la cardinalidad objetivo no coincide con el número de instancias.")
  }
  
  K <- length(target_cardinality)
  cat("Número de clusters (K):", K, "\n")
  
  # Default basic constraint matrices
  M <- matrix(0, K, K)
  C <- matrix(0, K, K)
  
  # Improved Silhouette feature
  IS <- function(x, cluster, centroids) {
    a <- sqrt(sum((x - centroids[cluster, ])^2))
    b <- min(sapply(1:nrow(centroids), function(i) {
      if (i == cluster) return(Inf)
      sqrt(sum((x - centroids[i, ])^2))
    }))
    return((b - a) / max(a, b))
  }
  
  # CUCMC: updates cohesion matrix based on constraints
  CUCMC <- function(X, M, C) {
    for (i in seq_len(nrow(X))) {
      for (j in seq_len(ncol(X))) {
        if (M[i, j] == 1) X[i, j] <- 1
        else if (C[i, j] == 1) X[i, j] <- 0
      }
    }
    return(X)
  }
  
  # Main HCAKC algorithm
  HCAKC_specified <- function(data, K, cluster_sizes, M, C) {
    n <- nrow(data)
    p <- ncol(data)
    
    clusters <- integer(n)
    centroids <- matrix(NA, nrow = K, ncol = p)
    
    # Simple initialization: sequential allocation of instances
    start_idx <- 1
    for (k in seq_len(K)) {
      end_idx <- start_idx + cluster_sizes[k] - 1
      if (end_idx > n) {
        stop("Índice de asignación fuera de los límites. Verifique target_cardinality.")
      }
      clusters[start_idx:end_idx] <- k
      centroids[k, ] <- colMeans(data[start_idx:end_idx, , drop = FALSE])
      start_idx <- end_idx + 1
    }
    
    # Calculation of IS for each instance
    IS_values <- sapply(seq_len(n), function(i) IS(data[i, ], clusters[i], centroids))
    
    # Cohesion matrix
    Xmat <- matrix(0, K, K)
    for (i in seq_len(K)) {
      for (j in seq_len(K)) {
        IS_i <- IS_values[clusters == i]
        IS_j <- IS_values[clusters == j]
        if (length(IS_i) == 0 || length(IS_j) == 0) next
        min_len <- min(length(IS_i), length(IS_j))
        Xmat[i, j] <- sum(head(IS_i, min_len) * head(IS_j, min_len), na.rm = TRUE)
      }
    }
    
    Xmat <- CUCMC(Xmat, M, C)
    return(list(clusters = clusters, centroids = centroids))
  }
  
  # Run algorithm
  resultado <- tryCatch({
    start_time <- Sys.time()
    result_specified <- HCAKC_specified(X, K, target_cardinality, M, C)
    end_time <- Sys.time()
    exec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    clusters <- result_specified$clusters
    centroids <- result_specified$centroids
    
    # Validations
    if (length(unique(clusters)) != K) {
      warning("El número de clusters encontrados no coincide con K.")
    }
    
    # evaluation
    dist_matrix <- proxy::dist(as.matrix(X), method = "cosine")
    sil <- silhouette(clusters, dist_matrix)
    mean_silhouette <- mean(sil[, 3])
    
    ARI_val <- adjustedRandIndex(clusters, y)
    AMI_val <- AMI(clusters, y)
    NMI_val <- NMI(clusters, y)
    
    size_real <- table(y)
    size_calc <- table(clusters)
    
    cat("Tamaño real:", paste(size_real, collapse = ", "), "\n")
    cat("Tamaño calculado:", paste(size_calc, collapse = ", "), "\n")
    
    # Exportar resultados
    result_df <- data.frame(
      dataset = dataset_name,
      ARI = ARI_val,
      AMI = AMI_val,
      NMI = NMI_val,
      Mean_Silhouette = mean_silhouette,
      num_clusters = K,
      num_features = ncol(X),
      num_instances = nrow(X),
      cardinality_pred = paste(as.vector(size_calc), collapse = ", "),
      cardinality_real = paste(as.vector(size_real), collapse = ", "),
      exec_time_sec = exec_time
    )
    
    output_path <- "global_results_HCAKC.csv"
    if (file.exists(output_path)) {
      write.table(result_df, output_path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    } else {
      write.table(result_df, output_path, sep = ",", row.names = FALSE, col.names = TRUE)
    }
    
    return(result_df)
  }, error = function(e) {
    cat("Error interno durante run_HCAKC para dataset", dataset_name, ":", e$message, "\n")
    return(NULL)
  })
  
  return(resultado)
}



# Main function to run everything
run_clustering <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  X <- data$X
  y <- data$y
  
  # Medir solo la ejecución del algoritmo BAT:
  start_algo <- Sys.time()
  results <- run_HCAKC(X, y, target_cardinality, dataset_name)
  end_algo <- Sys.time()
  algo_time <- as.numeric(difftime(end_algo, start_algo, units = "secs"))
  
  # Devolver el tiempo de ejecución del algoritmo
  return(algo_time)
}
#----------------------------------------------------------------------------------------------#
# Algorithm Execution
#----------------------------------------------------------------------------------------------#

# Vector to store the algorithm execution times
algorithm_times <- numeric(nrow(odatasets_unique))
for (i in 1:nrow(odatasets_unique)) {
  cat("\n\n--- Executing for dataset at position:", i, "---\n")
  
  tryCatch({
    # Extract dataset, name and target cardinality
    dataset <- odatasets_unique[i]$dataset[[1]]
    dataset_name <- odatasets_unique[i]$name
    prepared_data <- prepare_data(dataset)
    if (is.null(prepared_data)) {
      cat("Dataset at position", i, "is not in a valid format. Skipping.\n")
      algorithm_times[i] <- NA
      next
    }
    
    target_cardinality <- odatasets_unique[i]$class_distribution_vector[[1]]
    if (is.null(target_cardinality)) {
      cat("Target cardinality not available for position", i, ". Skipping.\n")
      algorithm_times[i] <- NA
      next
    }
    
    # Ejecutar clustering y medir solo la parte del algoritmo de BAT
    algo_time <- run_clustering(dataset, target_cardinality, dataset_name)
    algorithm_times[i] <- algo_time
    cat("Algorithm execution time for position", i, ":", algo_time, "seconds\n")
    
  }, error = function(e) {
    cat("Error processing dataset at position", i, ":", e$message, "\n")
    algorithm_times[i] <- NA
  })
}
