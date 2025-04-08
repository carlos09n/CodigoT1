library(proxy)
library(mlr3oml)
library(mlr3)
library(pryr)
library(dplyr) 
# OpenML Functions

# Function to filter unique datasets
filter_unique_datasets <- function(dataset) {
  dataset %>%
    distinct(NumberOfClasses, NumberOfInstances, MajorityClassSize, MinorityClassSize, .keep_all = TRUE) %>%
    mutate(name_lower = tolower(name)) %>%
    distinct(name_lower, .keep_all = TRUE) %>%
    select(-name_lower) %>%
    filter(!is.na(NumberOfSymbolicFeatures), NumberOfSymbolicFeatures <= 1) %>%
    filter(!is.na(NumberOfMissingValues), NumberOfMissingValues == 0)
}

# Function to load datasets with error handling
load_dataset <- function(id) {
  tryCatch({
    odata <- OMLData$new(id = id)
    dataset <- odata$data
    
    if (any(duplicated(names(dataset)))) {
      warning(paste("Dataset ID", id, ": Duplicate columns found. Renaming columns..."))
      names(dataset) <- make.unique(names(dataset))
    }
    
    message(paste("Dataset ID", id, "downloaded successfully."))
    return(dataset)
  }, error = function(e) {
    warning(paste("Error loading dataset ID", id, ":", e$message))
    return(NULL)
  })
}

# Function to get class distributions and validate the number of classes
get_class_distributions <- function(id, expected_classes) {
  dataset <- load_dataset(id)
  
  # If the dataset could not be downloaded, return null values
  if (is.null(dataset)) {
    return(list(text = "Error", vector = NA, data = NULL))
  }
  
  # Determine the dependent variable (last column or "class")
  if ("class" %in% colnames(dataset)) {
    target <- dataset$class
  } else {
    target <- dataset[[ncol(dataset)]]  # Last column
  }
  
  # Get the class distribution
  class_dist <- table(target)
  
  # Validate number of classes
  num_classes_found <- length(class_dist)
  if (num_classes_found != expected_classes) {
    warning(paste("Dataset ID", id, "has a discrepancy in the number of classes:",
                  "expected =", expected_classes, ", found =", num_classes_found))
    return(list(text = "Error", vector = NA, data = NULL))
  }
  
  # Format as text
  class_dist_text <- paste(names(class_dist), as.integer(class_dist), sep = ":", collapse = "; ")
  
  # Format as a numeric vector
  class_dist_vector <- as.integer(class_dist)
  
  # Return a list with the class distribution and the dataset
  return(list(text = class_dist_text, vector = class_dist_vector, data = dataset))
}

# Get the list of datasets
odatasets <- list_oml_data(
  number_features = c(2, 15),
  number_instances = c(100, 5000),
  number_classes = c(2, 15)
)

# Filter unique datasets
odatasets_unique <- filter_unique_datasets(odatasets)

# Measure download time
start_time <- Sys.time()

# Iterate over each data_id and validate against the expected number of classes
class_distributions <- mapply(
  FUN = get_class_distributions,
  id = odatasets_unique$data_id,
  expected_classes = odatasets_unique$NumberOfClasses,
  SIMPLIFY = FALSE
)

end_time <- Sys.time()

execution_time <- end_time - start_time
print(paste("Total execution time:", execution_time))

# Filter valid datasets
valid_datasets <- Filter(function(x) !is.null(x$data), class_distributions)

# Update `odatasets_unique` with valid indices
odatasets_unique <- odatasets_unique[!sapply(class_distributions, function(x) is.null(x$data)), ]
odatasets_unique$class_distribution <- sapply(valid_datasets, `[[`, "text")
odatasets_unique$class_distribution_vector <- lapply(valid_datasets, `[[`, "vector")
odatasets_unique$dataset <- lapply(valid_datasets, `[[`, "data")
