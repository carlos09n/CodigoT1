if (!require("cluster")) install.packages("cluster")
library(cluster)
if (!require("pso")) install.packages("pso")
library(pso)  
#----------------------------------------------------------------------------------------------#
# PSO
#----------------------------------------------------------------------------------------------#
# Function to prepare data with validation of the dependent variable
algorithm_times <- numeric(nrow(odatasets_unique))
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
  run_PSO<- function(X, y, target_cardinality,dataset_name){
    # Cargar conjunto de datos iris
    # Seleccionar las columnas numéricas para el análisis
    # Definir el número de elementos en cada clúster
    n2 <- target_cardinality
    k = length(n2)
    algorithm_times <- numeric(nrow(odatasets_unique))
    # Calcular la matriz de distancias
    D <- proxy::dist(as.matrix(X), method = "cosine")
    D = as.matrix(D)
    distancia = D
    #===========================================================================================================


    r <- nrow(D) # Número total de documentos
    cost_function <- function(par, dist_matrix, cluster_sizes, k) {
      n <- length(par)
      cluster_assignment <- integer(n)
      start_idx <- 1

      # Asignar clusters basados en los valores de par
      cluster_indices <- order(par)

      for (i in 1:k) {
        end_idx <- start_idx + cluster_sizes[i] - 1
        cluster_assignment[cluster_indices[start_idx:end_idx]] <- i
        start_idx <- end_idx + 1
      }

      total_distance <- 0
      # Calcular distancias intra-cluster
      for (i in 1:k) {
        cluster_indices <- which(cluster_assignment == i)
        if (length(cluster_indices) > 1) {
          cluster_distances <- dist_matrix[cluster_indices, cluster_indices]
          total_distance <- total_distance + sum(cluster_distances[lower.tri(cluster_distances, diag = FALSE)])
        }
      }

      return(total_distance)
    }
    # Ejecutar PSO
    set.seed(22)
    tiempo <- system.time(
      pso_result <- psoptim(
        par = runif(nrow(X)),
        fn = cost_function,
        dist_matrix = D,
        cluster_sizes = n2,
        k = k,
        lower = 0,
        upper = k,
        control = list(
          maxit = 20,     # Número de iteraciones máximas
          s = 40,         # Número de partículas
          w = 0.7,        # Coeficiente de inercia
          c.p = 2.5,      # Coeficiente de aprendizaje cognitivo
          c.g = 2.5       # Coeficiente de aprendizaje social
        )
      )
    )
    print(tiempo)

    label_pred = rep(0, length(pso_result$par))
    sorted_indices = order(pso_result$par)
    start_idx = 1
    for (i in 1:k) {
      end_idx = start_idx + n2[i] - 1
      label_pred[sorted_indices[start_idx:end_idx]] = i
      start_idx = end_idx + 1
    }

    index = unlist(lapply(1:k, function(i) which(label_pred == i)))
    # Cronometrar tiempo
    tiempo <- system.time({
      pso_result <- psoptim(
        par = runif(nrow(X)),
        fn = cost_function,
        dist_matrix = D,
        cluster_sizes = target_cardinality,
        k = k,
        lower = 0,
        upper = k,
        control = list(maxit = 20, s = 40, w = 0.7, c.p = 2.5, c.g = 2.5)
      )
    })
    exec_time <- tiempo[["elapsed"]]
   
    silhouette_values <- silhouette(x = label_pred, dist = as.dist(distancia))
    mean_silhouette <- mean(silhouette_values[, "sil_width"])

    #=================================================================================================
    # Calcular y mostrar el tamaño de los grupos reales
    size_real <- table(y)
    cat("Tamaño de los Grupos Real:\n")
    print(size_real)

    # Calcular y mostrar el tamaño de los grupos calculados
    size_calc <- table(label_pred)
    cat("Tamaño de los Grupos Calculado:\n")
    print(size_calc)
    #=============================================================================================
    if (!require("aricode")) install.packages("aricode")

    # Cargar bibliotecas necesarias
    library(aricode)

    # Calcular ARI
    ARI <- ARI(y, label_pred)
    cat("Adjusted Rand Index (ARI): ", ARI, "\n")

    # Calcular AMI
    AMI <- AMI(y, label_pred)
    cat("Adjusted Mutual Information (AMI): ", AMI, "\n")

    # Calcular NMI
    NMI <- NMI(y, label_pred)
    cat("Normalized Mutual Information (NMI): ", NMI, "\n")
    cat(mean_silhouette)
    global_results_PSO <- data.frame(
      dataset = dataset_name,
      ARI = ARI,
      AMI = AMI,
      NMI = NMI,
      Mean_Silhouette = mean_silhouette,
      num_clusters = k,
      num_features = ncol(X),
      num_instances = nrow(X),
      cardinality_pred = paste(as.vector(size_calc), collapse = ", "),
      cardinality_real = paste(as.vector(size_real), collapse = ", "),
      execution_time = exec_time
     )
     if (file.exists("global_results_PSO.csv")) {
        write.table(global_results_PSO, "global_results_PSO.csv", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      } else {
        write.table(global_results_PSO, "global_results_PSO.csv", sep = ",", row.names = FALSE, col.names = TRUE)
      }
  }
  run_clustering <- function(dataset, target_cardinality, dataset_name) {
    data <- prepare_data(dataset)
    X <- data$X
    y <- data$y
    dataset_name<- dataset_name
    # Medir solo la ejecución del algoritmo BAT:
    start_algo <- Sys.time()
    results <- run_PSO(X, y, target_cardinality,dataset_name)
    end_algo <- Sys.time()
    algo_time <- as.numeric(difftime(end_algo, start_algo, units = "secs"))

    # Devolver el tiempo de ejecución del algoritmo
    return(algo_time)}
  #----------------------------------------------------------------------------------------------#
  # Algorithm Execution
  #----------------------------------------------------------------------------------------------# 
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

