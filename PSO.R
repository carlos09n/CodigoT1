if (!require("cluster")) install.packages("cluster")
library(cluster)
if (!require("pso")) install.packages("pso")
library(pso)  
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
    #=========================================================================================================
    #ESCALAMIENTO MULTIDIMENSIONAL
    fit = cmdscale(distancia,eig=TRUE, k) # k es el numero de dimensiones
    xx = fit$points[,1]
    yy = fit$points[,2]
    plot(xx,yy)
    text(xx, yy, labels = row.names(iris), cex=1)
    #Identificaci�n de las Clases
    plot(xx,yy,col=c("red","green3","blue")[y], main = "Dataset Original")

    #============================================================================================================
    # Gráfico

    k <- length(unique(label_pred))

    # Definir símbolos, uno para cada clúster
    simbolos_cluster <- 1:k  # Asegúrate de que tienes suficientes símbolos para todos los clústeres

    # Generar colores
    colores_cluster <- rainbow(k)

    # Ahora, al graficar, asigna tanto colores como símbolos a los puntos
    plot(1:r, label_pred, pch = simbolos_cluster[label_pred], col = colores_cluster[label_pred], xlab = 'Índice de los Objetos', ylab = 'Número de Clústeres', main = 'Distribución de Objetos en cada Clúster')
    legend("topleft", legend = paste("Clúster", 1:k), col = colores_cluster, pch = simbolos_cluster) # Agregar leyenda (opcional)

    #====================================================================================================================
    #CLUSTERS CON SU OBJETO
    labels <- character(length(label_pred))

    # Crear etiquetas
    for (i in 1:length(label_pred)) {
      labels[i] <- paste(label_pred[i], ",", i)
    }

    # Ordenar las etiquetas
    labels_sorted <- sort(labels)

    # Imprimir las etiquetas ordenadas
    print("Los clusters son (cluster, objeto):")
    print(labels_sorted)

    #================================================================================================================
    # distancia <- mat_dist_cos(X)
    silhouette_values <- silhouette(x = label_pred, dist = as.dist(distancia))
    mean_silhouette <- mean(silhouette_values[, "sil_width"])

    # Personaliza colores
    colores <- rainbow(max(label_pred))

    # Plotear las medidas de silueta
    plot(silhouette_values, col = colores, border = NA, cex.names = 0.7)

    # Agregar la línea vertical para el valor medio de la silueta
    abline(v = mean_silhouette, lty = 2, col = "red")

    # Ajustar la posición del texto "Media"
    text(mean_silhouette, max(silhouette_values[, "sil_width"]),
         labels = paste("Media =", round(mean_silhouette, 3)),
         pos = 2, col = "red", cex = 0.8)

    # Mostrar el promedio del coeficiente de silueta
    print(paste("Promedio del coeficiente de silueta:", mean_silhouette))

    #==================================================================================================================
    #MATRIZ DE DISTANCIAS
    # if (!require("pheatmap")) install.packages("pheatmap")
    # library(pheatmap)
    # 
    # # Convierte la matriz de distancias a una matriz cuadrada
    # D_matrix <- as.matrix(distancia)
    # 
    # # Configura la paleta de colores
    # my_palette <- colorRampPalette(c("blue", "white", "red"))(256)
    # 
    # # Configura las etiquetas de fila y columna
    # rownames(D_matrix) <- colnames(D_matrix) <- as.character(index)
    # 
    # # Crea un gráfico de matriz de distancias con pheatmap
    # pheatmap(
    #   D_matrix,
    #   cluster_cols = FALSE, # No agrupar las columnas
    #   cluster_rows = FALSE, # No agrupar las filas
    #   main = "Matriz de Distancias del Dataset",
    #   fontsize = 8, # Tamaño de letra
    #   fontsize_row = 8,
    #   fontsize_col = 8,
    #   color = my_palette,
    #   labels_row = index,
    #   labels_col = index,
    #   show_colnames = TRUE,
    #   show_rownames = TRUE
    # )
    # #====================================================================================================
    # # Asegúrate de tener instalados estos paquetes
    # if (!require("ggplot2")) install.packages("ggplot2")
    # if (!require("viridis")) install.packages("viridis")
    # if (!require("factoextra")) install.packages("factoextra")
    # 
    # # Cargar las bibliotecas necesarias
    # library(ggplot2)
    # library(factoextra)  # Para PCA
    # library(viridis)
    # 
    # # Realizar PCA sobre tus datos
    # pca_res <- prcomp(X, scale = TRUE)
    # X_pca <- pca_res$x[, 1:2]  # Tomar las dos primeras componentes principales
    # 
    # # Crear un dataframe con los resultados de PCA y las etiquetas de clúster
    # df_pca <- data.frame(Dimension1 = X_pca[, 1], Dimension2 = X_pca[, 2], Cluster = as.factor(label_pred))
    # 
    # # Crear el gráfico
    # ggplot(df_pca, aes(x = Dimension1, y = Dimension2, color = Cluster)) +
    #   geom_point(alpha = 0.7, size = 3) +
    #   scale_color_viridis_d() +
    #   labs(title = paste("Silhouette Analysis with k =", k),
    #        x = "Dimension 1",
    #        y = "Dimension 2",
    #        color = "Cluster") +
    #   theme_minimal() +
    #   theme(plot.title = element_text(size = 15, face = "bold"),
    #         plot.subtitle = element_text(size = 14),
    #         axis.title = element_text(size = 14))

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
    print("str de lo que tu sabes")
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
      cardinality_real = paste(as.vector(size_real), collapse = ", ")
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

    # Imprimir resultados (incluye la escritura de silhouette_results.csv)
   # print_results(results, y, X, D, target_cardinality, dataset_name)

    # Devolver el tiempo de ejecución del algoritmo
    return(algo_time)}
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
  #global_results_PSO$cardinality_pred <- sapply(global_results_PSO$cardinality_pred, paste, collapse = ", ")
  #global_results_PSO$cardinality_real <- sapply(global_results_PSO$cardinality_real, paste, collapse = ", ")

  # Get intersection of names (common datasets)
  #common_names <- intersect(global_results_total$name, global_results_KmedoidsSC$name)

  # Filter global_results_KmedoidsSC to only include datasets with common names
  #global_results_KmedoidsSC <- global_results_KmedoidsSC[global_results_KmedoidsSC$name %in% common_names, ]

