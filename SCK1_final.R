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
global_results_SCK1 <- data.frame(
  name = character(),
  ARI = numeric(),
  AMI = numeric(),
  NMI = numeric(),
  Mean_Silhouette = numeric(),
  Clusters = integer(),
  number_features = integer(),
  number_instances = integer(),
  cardinality_SCK1 = I(list()),
  cardinality_REAL = I(list()),
  stringsAsFactors = FALSE
)
# Función para resolver asignación ILP con restricciones de tamaño fijo
solve_ilp_assignment <- function(cost_mat, sizes) {
  k <- nrow(cost_mat)
  n <- ncol(cost_mat)
  num_vars <- k * n
  cost_vector <- as.vector(t(cost_mat))  # aplanar fila por fila
  
  # Restricción 1: cada punto asignado a un solo cluster
  A1 <- matrix(0, nrow = n, ncol = num_vars)
  for (i in 1:n) {
    A1[i, seq(i, by = n, length.out = k)] <- 1
  }
  dir1 <- rep("=", n)
  rhs1 <- rep(1, n)
  
  # Restricción 2: cada cluster con tamaño exacto sizes[j]
  A2 <- matrix(0, nrow = k, ncol = num_vars)
  for (j in 1:k) {
    A2[j, ((j - 1) * n + 1):(j * n)] <- 1
  }
  dir2 <- rep("=", k)
  rhs2 <- sizes
  
  # Unir restricciones
  A <- rbind(A1, A2)
  dir <- c(dir1, dir2)
  rhs <- c(rhs1, rhs2)
  
  # Resolver ILP
  result <- lp("min", cost_vector, A, dir, rhs, all.bin = TRUE)
  if (result$status != 0) stop("No se pudo encontrar solución ILP factible.")
  
  matrix(result$solution, nrow = k, byrow = TRUE)
}

# Función iterativa SCK1 con ILP para restricciones cardinales fijas
sck1_iterativo <- function(data, k, initial_centroids, sizes) {
  n <- nrow(data)
  d <- ncol(data)
  centroids <- initial_centroids
  
  # 1. Calcular matriz de costos (distancia euclidiana al cuadrado)
  cost_mat <- proxy::dist(centroids, data, method = "cosine")
  cost_mat <- as.matrix(cost_mat)
  
  # 2. Resolver el problema ILP de asignación con restricciones de tamaño
  assignment <- solve_ilp_assignment(cost_mat, sizes)
  
  # 3. Reorganizar resultados
  re <- matrix(0, nrow = n, ncol = 2)
  pos <- 1
  for (i in 1:k) {
    indices <- which(assignment[i, ] == 1)
    for (j in indices) {
      re[pos, ] <- c(i, j)
      pos <- pos + 1
    }
  }
  
  # 4. Recalcular centroides
  for (i in 1:k) {
    members <- re[re[, 1] == i, 2]
    if (length(members) > 0) {
      centroids[i, ] <- colMeans(data[members, , drop = FALSE])
    }
  }
  
  # 5. Etiquetar cada punto
  cluster_assignments <- rep(0, n)
  for (i in 1:n) {
    cluster_assignments[i] <- re[re[, 2] == i, 1]
  }
  return(list(
    centroids = centroids,
    assignments = cluster_assignments
  ))
}
run_clustering <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  X <- data$X
  y <- data$y
  # Store the data of the algorithm execusion:
  k<-length(target_cardinality)
  initial_centroids <- X[sample(1:nrow(X), k), ]
  start_algo <- Sys.time()
  result <- sck1_iterativo(X, k, initial_centroids, target_cardinality)
  end_algo <- Sys.time()
  pred_labels <- result$assignments
  centroids <- result$centroids
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
  global_results_SCK1 <<- rbind(global_results_SCK1, data.frame(
    name = dataset_name,
    ARI = ari,
    AMI = ami,
    NMI = nmi,
    Mean_Silhouette = sil_mean,
    Clusters = num_clusters,
    number_features = num_variables,
    number_instances = num_instances,
    cardinality_SCK1 = paste(as.numeric(pred_labels), collapse = " "),
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
archivo_csv <- "SCK.csv"

# Convertir listas a texto si las hay
global_results_SCK1 <- as.data.frame(lapply(global_results_SCK1, function(x) {
  if (is.list(x)) {
    sapply(x, toString)
  } else {
    x
  }
}), stringsAsFactors = FALSE)

# Si el archivo ya existe, lo cargamos y combinamos
if (file.exists(archivo_csv)) {
  datos_anteriores <- read.csv(archivo_csv, stringsAsFactors = FALSE)
  nuevos_datos <- rbind(datos_anteriores, global_results_SCK1)
} else {
  nuevos_datos <- global_results_SCK1
}

# Guardar en CSV (sobrescribe el archivo anterior con todos los datos combinados)
write.csv(nuevos_datos, archivo_csv, row.names = FALSE)
