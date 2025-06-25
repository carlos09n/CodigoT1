# -----------------------------------------------------------------------------
# Load necessary libraries
# -----------------------------------------------------------------------------
if (!require("cluster")) install.packages("cluster")
library(cluster)
if (!require("proxy")) install.packages("proxy")
library(proxy)
if (!require("aricode")) install.packages("aricode")
library(aricode)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# -----------------------------------------------------------------------------
# Function to prepare data with dependent variable validation
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# SC_medoids Function: clustering using KmedoidsSC
# -----------------------------------------------------------------------------
SC_medoids <- function(D, k, E, C = NULL) {
  if (is.null(C)) {
    C <- sample(1:nrow(D), k)
  }
  
  # Initially assign each point to the nearest medoid
  cl <- max.col(-D[, C, drop = FALSE])
  
  # Order points by their distance to the nearest medoid
  sorted_points <- order(apply(D[, C, drop = FALSE], 1, min))
  
  # Assign the first E[i] points to each group i
  for (i in 1:k) {
    cl[sorted_points[1:E[i]]] <- i
    sorted_points <- sorted_points[-(1:E[i])]
  }
  
  # Assign the remaining points to the nearest medoid
  for (point in sorted_points) {
    cl[point] <- which.min(D[point, C])
  }
  
  # Generate cluster labels for each point
  labels <- numeric(nrow(D))
  for (i in 1:k) {
    ii <- which(cl == i)
    labels[ii] <- i
  }
  
  return(list(medoids = C, clustering = cl, labels = labels))
}

# -----------------------------------------------------------------------------
# Function to run KmedoidsSC on a dataset
# -----------------------------------------------------------------------------
run_KmedoidsSC <- function(dataset, target_cardinality, dataset_name) {
  # Prepare data
  prepared <- prepare_data(dataset)
  if (is.null(prepared)) {
    stop("The dataset is not in a valid format.")
  }
  X <- prepared$X
  y <- prepared$y
  
  # Calculate the distance matrix using cosine distance
  D <- proxy::dist(as.matrix(X), method = "cosine")
  D <- as.matrix(D)
  
  # Define number of clusters from the real cardinality
  E <- target_cardinality
  k <- length(E)
  
  # Obtain initial medoids with PAM
  pam_result <- pam(X, k)
  C <- pam_result$id.med
  
  # Measure exclusively the execution time of the SC_medoids function
  start_SC <- Sys.time()
  result <- SC_medoids(D, k, E, C)
  end_SC <- Sys.time()
  SC_time <- as.numeric(difftime(end_SC, start_SC, units = "secs"))
  
  label_pred <- result$labels
  
  # Calculate the silhouette coefficient
  silhouette_values <- silhouette(x = label_pred, dist = as.dist(D))
  mean_silhouette <- mean(silhouette_values[, "sil_width"])
  
  # Calculate ARI, AMI, and NMI
  ARI_value <- ARI(y, label_pred)
  AMI_value <- AMI(y, label_pred)
  NMI_value <- NMI(y, label_pred)
  
  # Get the predicted cardinality (number of elements per cluster)
  cardinality_pred <- as.integer(table(label_pred))
  
  # Calculate violations in size constraints
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
    Execution_Time = SC_time  # Unified column name
  ))
}

# -----------------------------------------------------------------------------
# Global data frame to store KmedoidsSC results
# -----------------------------------------------------------------------------
global_results_KmedoidsSC <- data.frame(
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
# Run the KmedoidsSC algorithm over the dataset list (odatasets_unique)
# -----------------------------------------------------------------------------
# It is assumed that odatasets_unique is available and contains the following columns:
# - dataset: list of datasets (each element is accessible as odatasets_unique[i]$dataset[[1]])
# - name: dataset name
# - class_distribution_vector: vector with the real cardinality

start_time_total <- Sys.time()

for (i in 1:nrow(odatasets_unique)) {
  cat("\n\n--- Running for dataset at position:", i, "---\n")
  
  tryCatch({
    # Extract dataset, name, and real cardinality
    dataset <- odatasets_unique[i, ]$dataset[[1]]
    dataset_name <- odatasets_unique[i, ]$name
    target_cardinality <- odatasets_unique[i, ]$class_distribution_vector[[1]]
    
    if (is.null(dataset) || is.null(target_cardinality)) {
      cat("Dataset or cardinality not available at position", i, ". Skipping...\n")
      next
    }
    
    # Run KmedoidsSC and obtain the execution time of SC_medoids
    result <- run_KmedoidsSC(dataset, target_cardinality, dataset_name)
    
    # Print size constraint violations to console
    cat("Size constraint violations:", result$Violations, "\n")
    
    # Add results to the global data frame, including the Execution_Time column
    global_results_KmedoidsSC <- rbind(global_results_KmedoidsSC, data.frame(
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
    
  }, error = function(e) {
    cat("Error processing dataset at position", i, ":", e$message, "\n")
  })
}

end_time_total <- Sys.time()
total_execution_time <- as.numeric(difftime(end_time_total, start_time_total, units = "secs"))
cat("\nTotal execution time (including data preparation and iteration over datasets):", total_execution_time, "seconds.\n")

# -----------------------------------------------------------------------------
# Prepare results for visualization and save to CSV
# -----------------------------------------------------------------------------
global_results_KmedoidsSC$cardinality_pred <- sapply(global_results_KmedoidsSC$cardinality_pred, paste, collapse = ", ")
global_results_KmedoidsSC$cardinality_real <- sapply(global_results_KmedoidsSC$cardinality_real, paste, collapse = ", ")
# Write the results to a CSV file
write.csv(global_results_KmedoidsSC, "results_KmedoidsSC.csv", row.names = FALSE)
