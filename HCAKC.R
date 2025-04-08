
  algorithm_times <- numeric(nrow(odatasets_unique))
  
  run_HCAKC<- function(X, y, target_cardinality,distancia){
    # HCAKC Algorithm in R
    
    # Improved Silhouette (IS) function
    IS <- function(x, cluster, centroids) {
      a <- sqrt(sum((x - centroids[cluster, ])^2))
      b <- min(sapply(1:nrow(centroids), function(i) sqrt(sum((x - centroids[i, ])^2))))
      (b - a) / max(a, b)
    }
    
    # CUCMC (Constraint-based Update of Cohesion Matrix between Clusters) function
    CUCMC <- function(X, M, C) {
      for (i in 1:nrow(X)) {
        for (j in 1:ncol(X)) {
          if (M[i, j] == 1) {
            X[i, j] <- 1
          } else if (C[i, j] == 1) {
            X[i, j] <- 0
          }
        }
      }
      X
    }
    
    # HCAKC Algorithm with Specified Cluster Sizes
    
    HCAKC_specified <- function(data, K, cluster_sizes, M, C) {
      # Initialization
      centroids <- matrix(NA, nrow = K, ncol = ncol(data))
      clusters <- integer(nrow(data))
      start_index <- 1
      
      for (k in seq_along(cluster_sizes)) {
        # Generate cluster k
        end_index <- start_index + cluster_sizes[k] - 1
        clusters[start_index:end_index] <- k
        start_index <- end_index + 1
        
        # Update centroids for cluster k
        centroids[k, ] <- colMeans(data[clusters == k, ])
      }
      
      # Calculate the IS values for each centroid
      IS_values <- sapply(1:nrow(data), function(i) IS(data[i, ], clusters[i], centroids))
      
      # Calculate the IS values for all pairs of centroids
      X <- matrix(0, nrow(centroids), nrow(centroids))
      for (i in 1:nrow(centroids)) {
        for (j in 1:nrow(centroids)) {
          IS_i <- IS_values[clusters == i]
          IS_j <- IS_values[clusters == j]
          # Ensure both vectors have the same length
          if (length(IS_i) != length(IS_j)) {
            # If lengths are not equal, pad the shorter vector with NA
            max_len <- max(length(IS_i), length(IS_j))
            IS_i <- c(IS_i, rep(NA, max_len - length(IS_i)))
            IS_j <- c(IS_j, rep(NA, max_len - length(IS_j)))
          }
          X[i, j] <- sum(IS_i * IS_j, na.rm = TRUE)
        }
      }
      
      # Apply CUCMC
      X <- CUCMC(X, M, C)
      
      # Hierarchical clustering
      while (nrow(centroids) > length(cluster_sizes)) {
        # Extract the maximal chs (C_i, C_j)
        max_chs <- which.max(X)
        i <- max_chs %/% nrow(X)
        j <- max_chs %% nrow(X)
        
        # Merge clusters
        centroids <- rbind(centroids, colMeans(rbind(centroids[i, ], centroids[j, ])))
        clusters[clusters == i] <- nrow(centroids)
        clusters[clusters == j] <- nrow(centroids)
        X <- CUCMC(X, M, C)
      }
      
      list(clusters = clusters, centroids = centroids)
    }
    
    # Example usage:
    specified_cluster_sizes <-target_cardinality
    K = length(specified_cluster_sizes)
    M <- matrix(0, K, K)
    M[1, 2] <- 1
    C <- matrix(0, K, K)
    C[2, 3] <- 1
    result_specified <- HCAKC_specified(X, K, target_cardinality, M, C)
    
    print(result_specified$clusters)
    print(result_specified$centroids)
    
    # Calcular y mostrar el tamaño de los grupos reales
    size_real <- table(y)
    cat("Tamaño de los Grupos Real:\n")
    print(size_real)
    
    # Calcular y mostrar el tamaño de los grupos calculados
    size_calc <- table(result_specified$clusters)
    cat("Tamaño de los Grupos Calculado:\n")
    print(size_calc)
    
    # Visualize the clusters
    plot(X, col = result_specified$clusters)
    #distancia=dist(X)
    #sil <- silhouette(result_specified$cluster, distancia)
    #silm<-mean(sil[, 3])  # La tercera columna de 'sil' es el índice de silueta de cada punto
    # Calcular ARI
    #vectorpre <- as.numeric(size_calc)
    #vectorreal <- as.numeric(size_real)
    #print(size_calc)
    #print(vectorpre)
    # ARI <- ARI( vectorreal,vectorpre)
    # cat("Adjusted Rand Index (ARI): ", ARI, "\n")
    #
    # # Calcular AMI
    # AMI <- AMI( vectorreal,vectorpre)
    # cat("Adjusted Mutual Information (AMI): ", AMI, "\n")
    #
    # # Calcular NMI
    # NMI <- NMI( vectorreal,vectorpre)
    # global_results_hcack <- data.frame(
    #   aRI = ARI,
    #   aMI = AMI,
    #   aMI = NMI
    #   #Mean_Silhouette = silm
    # )
    # if (file.exists("global_results_hcack.csv")) {
    #   write.table(global_results_hcack, "global_results_hcack.csv", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    # } else {
    #   write.table(global_results_PSO, "global_results_hcack.csv", sep = ",", row.names = FALSE, col.names = TRUE)
    # }
  }
  run_clustering <- function(dataset, target_cardinality, dataset_name) {
    data <- prepare_data(dataset)
    X <- data$X
    y <- data$y
    
    # Medir solo la ejecución del algoritmo BAT:
    start_algo <- Sys.time()
    results <- run_HCAKC(X, y, target_cardinality)
    end_algo <- Sys.time()
    algo_time <- as.numeric(difftime(end_algo, start_algo, units = "secs"))
    
    # Imprimir resultados (incluye la escritura de silhouette_results.csv)
    #print_results(results, y, X, D, target_cardinality, dataset_name)
    
    # Devolver el tiempo de ejecución del algoritmo
    return(algo_time)}
  for (i in 1:nrow(odatasets_unique)) {
    cat("\n\n--- Executing for dataset at position:", i, "---\n")
    algorithm_times <- numeric(nrow(odatasets_unique))
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