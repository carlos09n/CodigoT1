# Instalar si no están disponibles
packages <- c("flexclust", "lpSolve", "mclust", "aricode", "cluster","proxy")
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) install.packages(to_install)

library(flexclust)
library(lpSolve)
library(mclust)     # ARI
library(aricode)    # AMI, NMI
library(cluster)    # Silhouette
library(readr)
library(proxy)
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
global_results_MILP <- data.frame(
  name = character(),
  ARI = numeric(),
  AMI = numeric(),
  NMI = numeric(),
  Mean_Silhouette = numeric(),
  Clusters = integer(),
  number_features = integer(),
  number_instances = integer(),
  cardinality_MILP = I(list()),
  cardinality_REAL = I(list()),
  stringsAsFactors = FALSE
)
#----------------------------------------------------------------------------------------------#
# MILP-KM
#----------------------------------------------------------------------------------------------#
run_MILP<- function(X, y, target_cardinality){

  list(best_solution = best_cluster_assignment,mean_silhouette=mean_silhouette,AMI=AMI,ARI=ARI,NMI=NMI,Cluster=length(unique(best_cluster_assignment)),cardinality_real=target_cardinality,cardinality_pred=best_cluster_assignment)
}
# ---------- Función MILP ----------
solve_milp_assignment <- function(data, centroids, size_constraints) {
  n <- nrow(data)
  k <- nrow(centroids)
  
  cost_matrix <- proxy::dist(data, centroids, method = "cosine")
  cost_matrix <- as.matrix(cost_matrix)
  cost_vec <- as.vector(t(cost_matrix))  # vector fila por fila
  
  # Variables de decisión: x_ij ∈ {0,1}, tamaño n*k
  f.obj <- cost_vec
  
  # Restricción 1: cada punto debe ser asignado a un solo clúster
  constr1 <- matrix(0, n, n*k)
  for (i in 1:n) {
    constr1[i, ((i - 1) * k + 1):(i * k)] <- 1
  }
  
  # Restricción 2: cada clúster tiene que tener un tamaño dentro del límite
  constr2 <- matrix(0, k, n*k)
  for (j in 1:k) {
    constr2[j, seq(j, n*k, by = k)] <- 1
  }
  
  # Juntar restricciones
  f.con <- rbind(constr1, constr2)
  f.dir <- c(rep("=", n), rep("<=", k))
  f.rhs <- c(rep(1, n), size_constraints)
  
  # Restricción inferior de tamaño (opcional)
  lower_bounds <- rep(0, n*k)
  upper_bounds <- rep(1, n*k)
  types <- rep("binary", n*k)
  
  result <- lp("min", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
  
  if (result$status != 0) {
    stop("MILP no encontró solución")
  }
  
  # Obtener asignaciones: p[i] ∈ {1, ..., k}
  x_opt <- matrix(result$solution, nrow = n, byrow = TRUE)
  p <- apply(x_opt, 1, which.max)  # asignación de clúster
  q <- NULL  # placeholder, no usamos variables duales aquí
  
  return(list(p = p, q = q))
}

# ---------- Función principal ----------
clustering_with_size_constraints <- function(data, size_constraints, max_iter = 100, tol = 1e-6) {
  data <- as.matrix(data)
  colnames(data) <- NULL
  n <- nrow(data)
  d <- ncol(data)
  k <- length(size_constraints)
  # Paso 1: Inicialización de los centroides 
  random_indices <- sample(1:nrow(data), k)
  centroids <- data[random_indices, , drop = FALSE]
  converged <- FALSE
  iteration <- 0
  while (!converged && iteration < max_iter) {
    iteration <- iteration + 1
    
    # Paso 5: Resolver MILP
    milp_result <- solve_milp_assignment(data, centroids, size_constraints)
    p <- milp_result$p
    q <- milp_result$q

    # Paso 6: Actualización de centroides
    new_centroids <- matrix(0, nrow = k, ncol = d)
    for (j in 1:k) {
      cluster_points <- data[p == j, , drop = FALSE]
      if (nrow(cluster_points) > 0) {
        new_centroids[j, ] <- colMeans(cluster_points)
      } else {
        new_centroids[j, ] <- centroids[j, ]  # mantener si vacío
      }
    }
    
    # Paso 7: Convergencia
    if (max(abs(centroids - new_centroids)) < tol) {
      converged <- TRUE
    }
    
    centroids <- new_centroids
  }
  
  return(list(p = p, centroids = centroids))
}



run_clustering <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  X <- data$X
  y <- data$y
  # Store the data of the algorithm execusion:
  start_algo <- Sys.time()
  resultado <- clustering_with_size_constraints(X, target_cardinality)
  end_algo <- Sys.time()
  pred_labels <- resultado$p
  centroids <- resultado$centroids
  ari <- adjustedRandIndex(y, pred_labels)
  ami <- AMI(y, pred_labels)
  nmi <- NMI(y, pred_labels)
  
  # Índice de silueta
  d <- proxy::dist(X, method = "cosine")
  sil <- silhouette(pred_labels, d)
  sil_mean <- mean(sil[, 3])
  algo_time <- as.numeric(difftime(end_algo, start_algo, units = "secs"))
  num_variables <- ncol(X)
  num_instances <- nrow(X)
  num_clusters <- length(unique(pred_labels))
  #class_dist <- as.numeric(table(results$best_solution))
  cat("Métricas de evaluación:\n")
  cat(sprintf("ARI: %.4f\n", ari))
  cat(sprintf("AMI: %.4f\n", ami))
  cat(sprintf("NMI: %.4f\n", nmi))
  cat(sprintf("Índice de silueta: %.4f\n", sil_mean))
  pred_labels <- table(pred_labels)
  true_labels <- table(y)
  global_results_MILP <<- rbind(global_results_MILP, data.frame(
    name = dataset_name,
    ARI = ari,
    AMI = ami,
    NMI = nmi,
    Mean_Silhouette = sil_mean,
    Clusters = num_clusters,
    number_features = num_variables,
    number_instances = num_instances,
    cardinality_MILP = paste(as.numeric(pred_labels), collapse = " "),
    cardinality_REAL = paste(as.numeric(true_labels), collapse = " "),
    execution_time = algo_time
  ))
  return(algo_time)}
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

# Nombre del archivo CSV
archivo_csv <- "MILP.csv"

# Convertir listas a texto si las hay
global_results_MILP <- as.data.frame(lapply(global_results_MILP, function(x) {
  if (is.list(x)) {
    sapply(x, toString)
  } else {
    x
  }
}), stringsAsFactors = FALSE)

# Si el archivo ya existe, lo cargamos y combinamos
if (file.exists(archivo_csv)) {
  datos_anteriores <- read.csv(archivo_csv, stringsAsFactors = FALSE)
  nuevos_datos <- rbind(datos_anteriores, global_results_MILP)
} else {
  nuevos_datos <- global_results_MILP
}

# Guardar en CSV (sobrescribe el archivo anterior)
write.csv(nuevos_datos, archivo_csv, row.names = FALSE)