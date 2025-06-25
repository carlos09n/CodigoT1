library(cluster)
library(proxy)
#----------------------------------------------------------------------------------------------#
# ACO
#----------------------------------------------------------------------------------------------#
# Function to prepare data with validation of the dependent variable
prepare_data <- function(dataset) {
  dataset <- as.data.frame(dataset)
  
  if ("class" %in% colnames(dataset)) {
    y <- dataset$class
    X <- dataset[, setdiff(colnames(dataset), "class")]
  } else if (is.factor(dataset[, ncol(dataset)]) || is.integer(dataset[, ncol(dataset)])) {
    X <- dataset[, -ncol(dataset)]
    y <- dataset[, ncol(dataset)]
  } else {
    return(NULL)
  }
  list(X = X, y = y)
}

global_results_Aco <- data.frame(
  name = character(),
  Best_Seed = integer(),
  ARI = numeric(),
  AMI = numeric(),
  NMI = numeric(),
  Mean_Silhouette = numeric(),
  Clusters = integer(),
  number_features = integer(),
  number_instances = integer(),
  cardinality_ACO = I(list()),
  cardinality_REAL = I(list()),
  stringsAsFactors = FALSE
)
run_ACO<- function(X, y, target_cardinality){
  # Parameters for ACO
  n_ants <- 50
  max_iterations <- 20
  q <- 0.5
  eps <- 0.5
  penalty_weight <- 100
  D <- proxy::dist(as.matrix(X), method = "cosine")
  D <- as.matrix(D)
  distancia = D
  evaluate_solution <- function(cluster_assignment, X) {
    ss <- silhouette(cluster_assignment, dist(X))
    penalty <- 0
    for (j in 1:length(target_cardinality)) {
      cardinality_diff <- abs(sum(cluster_assignment == j) - target_cardinality[j])
      if (cardinality_diff > 0) {
        penalty <- penalty + cardinality_diff * penalty_weight
      }
    }
    return(list(ss = mean(ss) - penalty))
  }
  
  # Function to generate an initial solution (cluster assignment) using kmeans
  generate_initial_solution <- function() {
    km <- kmeans(X, centers = length(target_cardinality))
    cluster_assignment <- km$cluster
    for (j in 1:length(target_cardinality)) {
      while (sum(cluster_assignment == j) > target_cardinality[j]) {
        idx <- which(cluster_assignment == j)
        cluster_assignment[sample(idx, 1)] <- sample(1:length(target_cardinality), 1)
      }
    }
    return(cluster_assignment)
  }
  
  # Function to perturb a solution (cluster assignment)
  perturb_solution <- function(cluster_assignment) {
    new_cluster_assignment <- cluster_assignment
    for (j in 1:nrow(X)) {
      if (runif(1) < 0.1) {
        new_cluster_assignment[j] <- sample(1:length(target_cardinality), 1)
      }
    }
    # Ensure that the perturbed solution meets the desired group size
    for (j in 1:length(target_cardinality)) {
      while (sum(new_cluster_assignment == j) > target_cardinality[j]) {
        idx <- which(new_cluster_assignment == j)
        new_cluster_assignment[sample(idx, 1)] <- sample(1:length(target_cardinality), 1)
      }
    }
    return(new_cluster_assignment)
  }
  
  # Function to apply pheromone update rule
  apply_pheromone_update <- function(X, D, cluster_assignment, pheromone_update) {
    for (j in 1:nrow(X)) {
      if (cluster_assignment[j] %in% 1:3) {
        D[j, ] <- D[j, ] * (1 - pheromone_update)
      }
    }
  }
  
  # Run ACO iterations
  best_ss <- -Inf
  best_cluster_assignment <- NULL
  for (iteration in 1:max_iterations) {
    # Initialize ants
    ants <- list()
    for (i in 1:n_ants) {
      # Generate a new solution by perturbing the initial solution
      cluster_assignment <- generate_initial_solution()
      cluster_assignment <- perturb_solution(cluster_assignment)
      ants[[i]] <- cluster_assignment
    }
    
    # Evaluate solutions
    for (i in 1:n_ants) {
      cluster_assignment <- ants[[i]]
      result <- evaluate_solution(cluster_assignment, X)
      ss <- result$ss
      
      # Update best solution
      if (ss > best_ss) {
        best_cluster_assignment <- cluster_assignment
        best_ss <- ss
      }
    }
    
    # Apply pheromone update rule
    for (i in 1:n_ants) {
      cluster_assignment <- ants[[i]]
      pheromone_update <- 1 / (1 + sum(abs(table(cluster_assignment) - target_cardinality)))
      pheromone_update <- pheromone_update * q
      apply_pheromone_update(X, D, cluster_assignment, pheromone_update)
    }
  }
  

  distancia <- proxy::dist(as.matrix(X), method = "cosine")
  silhouette_values <- silhouette(x = best_cluster_assignment, dist = as.dist(distancia))
  mean_silhouette <- mean(silhouette_values[, "sil_width"])

  
  if (!require("aricode")) install.packages("aricode")
  
  # Cargar bibliotecas necesarias
  library(aricode)
  cat("silueta:",mean_silhouette,"\n")
  # Calcular ARI
  ARI <- ARI(y, best_cluster_assignment)
  cat("Adjusted Rand Index (ARI): ", ARI, "\n")
  
  # Calcular AMI
  AMI <- AMI(y, best_cluster_assignment)
  cat("Adjusted Mutual Information (AMI): ", AMI, "\n")
  
  # Calcular NMI
  NMI <- NMI(y, best_cluster_assignment)
  cat("Normalized Mutual Information (NMI): ", NMI, "\n")
  #guardar_resultados(AMI,ARI,NMI,mean_silhouette,"resulst_ACO.csv")
  tabla <- table(best_cluster_assignment)
  tabla <- paste(as.numeric(tabla), collapse = " ")
  cat("alineamiento:",tabla ,"\n")
  list(best_solution = best_cluster_assignment,mean_silhouette=mean_silhouette,AMI=AMI,ARI=ARI,NMI=NMI,Cluster=length(unique(best_cluster_assignment)),cardinality_real=target_cardinality,cardinality_pred=best_cluster_assignment)
}
run_clustering <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  X <- data$X
  y <- data$y
  # Store the data of the algorithm execusion:
  start_algo <- Sys.time()
  results <- run_ACO(X, y, target_cardinality)
  end_algo <- Sys.time()
  algo_time <- as.numeric(difftime(end_algo, start_algo, units = "secs"))
  num_variables <- ncol(X)
  num_instances <- nrow(X)
  num_clusters <- results$Cluster
  class_dist <- as.numeric(table(results$best_solution))
  best_seed <- NA  # ACO no usa semilla explícita
  global_results_Aco <<- rbind(global_results_Aco, data.frame(
    name = dataset_name,
    Best_Seed = best_seed,
    ARI = results$ARI,
    AMI = results$AMI,
    NMI = results$NMI,
    Mean_Silhouette = results$mean_silhouette,
    Clusters = num_clusters,
    number_features = num_variables,
    number_instances = num_instances,
    cardinality_ACO = I(list(class_dist)),
    cardinality_REAL = I(list(target_cardinality)),
    execution_time = algo_time
  ))

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
    
    # Ejecutar clustering
    algo_time <- run_clustering(dataset, target_cardinality, dataset_name)
    algorithm_times[i] <- algo_time
    cat("Algorithm execution time for position", i, ":", algo_time, "seconds\n")
    
  }, error = function(e) {
    cat("Error processing dataset at position", i, ":", e$message, "\n")
    algorithm_times[i] <- NA
  })
}

  write.table(global_results_Aco, file = archivo, sep = ",", row.names = FALSE, col.names = TRUE)
