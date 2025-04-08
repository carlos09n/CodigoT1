# -----------------------------------------------------------------------------
# Load necessary libraries
# -----------------------------------------------------------------------------
if (!require("cluster")) install.packages("cluster")
library(cluster)
if (!require("proxy")) install.packages("proxy")
library(proxy)
if (!require("aricode")) install.packages("aricode")
library(aricode)
if (!require("lpSolve")) install.packages("lpSolve")
library(lpSolve)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# -----------------------------------------------------------------------------
# CSCLP Function: clustering with size constraints using linear programming
# -----------------------------------------------------------------------------
# global_results <<- data.frame(
#   name = dataset_name,
#   Best_Seed = best_seed,
#   ARI = ARI_value,
#   AMI = AMI_value,
#   NMI = NMI_value,
#   Mean_Silhouette = mean_silhouette,
#   Clusters = num_clusters,
#   number_features = num_variables,
#   number_instances = num_instances,
#   cardinality_BAT = I(list(class_dist)),
#   cardinality_REAL = I(list(target_cardinality))
# ))

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
run_CSCLP <- function(dataset, target_cardinality, dataset_name) {
  # Prepare data
  prepared <- prepare_data(dataset)
  if (is.null(prepared)) stop("The dataset is not in a valid format.")
  X <- prepared$X
  y <- prepared$y
  
  # Calculate the distance matrix using cosine distance
  D_full <- proxy::dist(as.matrix(X), method = "cosine")
  D_full <- as.matrix(D_full)
  distancia <- D_full  # This will be used for silhouette calculation
  
  r <- nrow(D_full)  # Total number of documents
  k <- length(target_cardinality)  # Desired number of clusters
  
  # For the formulation, use n2 = (real cardinality - 1) because centroids are excluded
  n2 <- matrix(target_cardinality - 1, ncol = 1)
  
  # Get indices of centroids using PAM (similar to KMedoidsSC)
  pam_result <- pam(X, k)
  c <- pam_result$id.med  # Indices of the centroids
  
  # Get the vector of indices of documents that are NOT centroids
  v <- 1:r
  v <- v[!v %in% c]
  
  # Adjust the distance matrix:
  # Remove rows of D_full corresponding to non-centroid documents
  D <- D_full[-v, ]
  # Convert rows to vector (although f1 is not used later)
  f1 <- as.vector(t(D))
  # Remove columns corresponding to the centroids
  D <- D[, -c]
  # Convert the resulting matrix to a vector for linear programming
  f <- as.vector(t(D))
  
  # Build the constraint matrix (Aeq)
  ncolD <- ncol(D)
  Aeq1 <- matrix(0, nrow = k, ncol = length(f))
  for (i in 1:ncolD) {
    for (j in 1:k) {
      Aeq1[j, i + (j - 1) * ncolD] <- 1
    }
  }
  
  Aeq2 <- diag(ncolD)
  if (k > 1) {
    for (i in 1:(k - 1)) {
      Aeq2 <- cbind(Aeq2, diag(ncolD))
    }
  }
  Aeq <- rbind(Aeq1, Aeq2)
  beq <- c(n2, rep(1, ncolD))
  
  # Configure the binary linear programming model
  A <- Aeq
  dir <- rep("==", nrow(Aeq))
  rhs <- beq
  
  # Measure only the time for solving the LP model and reconstructing the assignment
  start_alg <- Sys.time()
  result_lp <- lp(direction = "min", objective.in = f, const.mat = A, 
                  const.dir = dir, const.rhs = rhs, all.bin = TRUE)
  end_alg <- Sys.time()
  alg_time <- as.numeric(difftime(end_alg, start_alg, units = "secs"))
  
  x <- result_lp$solution
  fval <- result_lp$objval
  
  s1 <- matrix(x, nrow = k, byrow = TRUE)
  s2 <- diag(k)
  st <- matrix(0, nrow = k, ncol = r)
  st[, c] <- s2
  st[, v] <- s1
  
  label_pred <- numeric(r)
  for (i in 1:k) {
    ind <- which(st[i, ] == 1)
    label_pred[ind] <- i
  }
  
  # Calculate the silhouette coefficient using the original distance matrix
  silhouette_values <- silhouette(x = label_pred, dist = as.dist(distancia))
  mean_silhouette <- mean(silhouette_values[, "sil_width"])
  
  # Calculate ARI, AMI, and NMI
  ARI_value <- ARI(y, label_pred)
  AMI_value <- AMI(y, label_pred)
  NMI_value <- NMI(y, label_pred)
  
  # Get the predicted cardinality (number of elements per cluster)
  cardinality_pred <- as.integer(table(label_pred))
  
  # Calculate the violations in size constraints
  violations <- sum(abs(cardinality_pred - target_cardinality))
  
  # Number of clusters, instances, and features
  num_clusters <- length(unique(label_pred))
  num_instances <- nrow(X)
  num_features <- ncol(X) + 1
  
  return(list(
    dataset_name = dataset_name,
    ARI = ARI_value,
    AMI = AMI_value,
    NMI = NMI_value,
    Mean_Silhouette = mean_silhouette,
    Clusters = num_clusters,
    number_features = num_features,
    number_instances = num_instances,
    cardinality_pred = cardinality_pred,
    cardinality_real = target_cardinality,
    Violations = violations,
    Execution_Time = alg_time  # Measured time of the CSCLP algorithm
  ))
}

# -----------------------------------------------------------------------------
# Global data frame to store CSCLP results
# -----------------------------------------------------------------------------
global_results_CSCLP <- data.frame(
  name = character(),
  ARI = numeric(),
  AMI = numeric(),
  NMI = numeric(),
  Mean_Silhouette = numeric(),
  Clusters = integer(),
  number_features = integer(),
  number_instances = integer(),
  cardinality_pred = I(list()),
  cardinality_real = I(list()),
  Violations = numeric(),
  Execution_Time = numeric(),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Run the CSCLP algorithm on the list of datasets (odatasets_unique)
# -----------------------------------------------------------------------------
# It is assumed that odatasets_unique is available and contains the following columns:
# - dataset: list of datasets (each accessible as odatasets_unique[i]$dataset[[1]])
# - name: dataset name
# - class_distribution_vector: vector with the real cardinality

start_time_total <- Sys.time()

for (i in 1:nrow(odatasets_unique)) {
  cat("\n\n--- Running CSCLP for dataset at position:", i, "---\n")
  start_time <- Sys.time()
  
  tryCatch({
    # Extract dataset, name, and real cardinality
    dataset <- odatasets_unique[i, ]$dataset[[1]]
    dataset_name <- odatasets_unique[i, ]$name
    target_cardinality <- odatasets_unique[i, ]$class_distribution_vector[[1]]
    
    if (is.null(dataset) || is.null(target_cardinality)) {
      cat("Dataset or cardinality not available at position", i, ". Skipping...\n")
      next
    }
    
    # Run CSCLP and obtain the execution time of the algorithm (measured in run_CSCLP)
    result <- run_CSCLP(dataset, target_cardinality, dataset_name)
    
    # Print size constraint violations to the console
    cat("Size constraint violations:", result$Violations, "\n")
    
    # Add results to the global data frame, including the Execution_Time column
    global_results_CSCLP <- rbind(global_results_CSCLP, data.frame(
      name = result$dataset_name,
      ARI = result$ARI,
      AMI = result$AMI,
      NMI = result$NMI,
      Mean_Silhouette = result$Mean_Silhouette,
      Clusters = result$Clusters,
      number_features = result$number_features,
      number_instances = result$number_instances,
      cardinality_pred = I(list(result$cardinality_pred)),
      cardinality_real = I(list(result$cardinality_real)),
      Violations = result$Violations,
      Execution_Time = result$Execution_Time,
      stringsAsFactors = FALSE
    ))
    
    end_time <- Sys.time()
    cat("Execution time for position", i, ":", as.numeric(difftime(end_time, start_time, units = "secs")), "seconds\n")
    
  }, error = function(e) {
    cat("Error processing dataset at position", i, ":", e$message, "\n")
  })
}

end_time_total <- Sys.time()
total_execution_time <- as.numeric(difftime(end_time_total, start_time_total, units = "secs"))
cat("\nTotal execution time (including data preparation and iteration over datasets):", total_execution_time, "seconds.\n")

# -----------------------------------------------------------------------------
# Prepare the results for visualization and save to CSV
# -----------------------------------------------------------------------------
# violations_data <- data.frame(
#   Violations = sapply(1:nrow(global_results), function(i) {
#     real <- unlist(global_results$cardinality_REAL[i])
#     bat <- unlist(global_results$cardinality_BAT[i])
#     sum(abs(real - bat))
#   })
# )
# global_results_CSCLP$cardinality_pred <- sapply(global_results_CSCLP$cardinality_pred, paste, collapse = ", ")
global_results_CSCLP$cardinality_real <- sapply(global_results_CSCLP$cardinality_real, paste, collapse = ", ")

# (Optional) Filter by common names if necessary:
# global_results_total <- cbind(violations_data, global_results)
# common_names <- intersect(global_results_total$name, global_results_CSCLP$name)
# global_results_CSCLP <- global_results_CSCLP[global_results_CSCLP$name %in% common_names, ]

write.csv(global_results_CSCLP, "results_CSCLP.csv", row.names = FALSE)
