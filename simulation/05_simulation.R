# R code for the simulation

source("02_findtrclust.R")
source("03_idinvariant.R")
source("04_simulate_dag_and_data.R")

library(dosearch)

# Helper function to time operations
time_operation <- function(expr) {
  start <- Sys.time()
  result <- tryCatch(
    expr,
    error = function(e) {
      list(error = TRUE, message = e$message)
    }
  )
  end <- Sys.time()
  time_ms <- as.numeric(difftime(end, start, units = "secs")) * 1000
  list(result = result, time_ms = time_ms)
}

# Runs a simulation to compare identification performance between:
# 1. Raw dosearch on the full (real) DAG with expanded transit clusters
# 2. Clustering approach: finding transit clusters, reducing to clustered DAG, 
#    checking ID invariance, then running dosearch
#
# Inputs:
# n = Number of iterations
# min_n = Min. number of variables in original graph
# max_n = Max. number of variables in original graph
# max_r = Max. number of receivers in transit cluster
# max_e = Max. number of emitters in transit cluster
# max_o = Max. number of other vertices in transit cluster
# min_sources = Min. number of data sources
# max_sources = Max. number of data sources
# prob1, prob2 = Probability parameters related to DAG creation
# prob3 = Probability parameters related to transit cluster creation
# prob_sources = Probability parameters related to data source creation
# timeout = Time limit for dosearch
# indicate = In which indices the progress is printed.
# See more information on probs in their respective functions
# 
# Returns:
# Data frame of simulation results with columns:
#   setting: "id" = identifiable in clustered graph
#            "non_id_idinv" = non-identifiable in clustered, but ID-invariant holds
#            "non_id_non_idinv" = non-identifiable in clustered, and ID-invariant fails
#   raw_timediff: Time (ms) for dosearch on full real DAG
#   clustering_timediff: Total time (ms) for identifying with clustering approach
#   trclust_timediff: Time (ms) to find transit clusters
#   idinv_timediff: Time (ms) to check ID invariance
#   clustered_dag: DAG string with clusters
#   real_dag: DAG string with clusters expanded
#   clustered_data: Data source string for clustered DAG
#   real_data: Data source string for real DAG
#   error: TRUE if iteration encountered an error
simulate_times <- function(n, min_n = 5, max_n = 5, max_r = 2, max_e = 2, max_o = 1,
                           min_sources = 2, max_sources = 3,
                           prob1 = 0.5, prob2 = 0.35, prob3 = 0.5, 
                           prob_sources = c(0.35, 0.15, 0.10, 0.4), timeout = 0.267,
                           indicate = 10) {
  
  # Initialize result data frame
  result <- data.frame(
    setting = character(n),
    raw_timediff = numeric(n),
    clustering_timediff = numeric(n),
    trclust_timediff = numeric(n),
    idinv_timediff = numeric(n),
    clustered_dag = character(n),
    clustered_dag_size = integer(n),
    real_dag = character(n),
    real_dag_size = integer(n),
    clustered_data = character(n),
    real_data = character(n),
    error = logical(n),
    stringsAsFactors = FALSE
  )
  
  # Run n simulation iterations
  for (i in 1:n) {
    tryCatch({
      # Generating random DAG with data sources
      dag <- random_dag(min_n, max_n, prob1 = prob1, prob2 = prob2)
      
      real_dag <- add_transit_cluster(dag$graph, dag$order, max_r = max_r, 
                                      max_e = max_e, max_o = max_o, prob = prob3)
      
      d <- random_data(dag$graph, real_dag$clust, min_sources = min_sources,
                       max_sources = max_sources, prob = prob_sources)
      
      d_real <- original_data(real_dag, d)
      
      do1 <- parse_dosearch(dag$graph, d) 
      do2 <- parse_dosearch(real_dag$graph, d_real)
      
      # Time raw dosearch on the full expanded DAG
      # This is the baseline approach (no clustering)
      raw_timed <- time_operation(
        dosearch::dosearch(do2$data, "p(y|do(x))", do2$graph, control = list(time_limit = timeout))
      )
      if (!is.null(raw_timed$result$error)) {
        stop("Raw dosearch failed: ", raw_timed$result$message)
      }
      raw_timediff <- raw_timed$time_ms
      
      # Discarding all that take more than timeout hours.
      if ((raw_timediff / (1000 * 60 * 60)) > timeout) {
        # Log this as a timeout error and move to next iteration
        result[i, "error"] <- TRUE
        result[i, "setting"] <- "timeout"
        result[i, "raw_timediff"] <- raw_timediff
        result[i, "clustering_timediff"] <- NA
        result[i, "idinv_timediff"] <- NA
        result[i, "trclust_timediff"] <- NA
        result[i, "clustered_dag"] <- do1$graph
        result[i, "clustered_dag_size"] <- length(V(dag$graph)$name)
        result[i, "real_dag"] <- do2$graph
        result[i, "real_dag_size"] <- length(V(real_dag$graph)$name)
        result[i, "clustered_data"] <- do1$data
        result[i, "real_data"] <- do2$data
        
        cat("Iteration:", i, "(timeout)\n")

        next
      }
      
      # Default setting (updated below based on results)
      setting <- "non_id_non_idinv"
      
      # Time finding transit clusters from the expanded DAG
      trclust_timed <- time_operation(
        suppressWarnings(find_transit_components(real_dag$graph, prohibit = c("x", "y")))
      )
      if (!is.null(trclust_timed$result$error)) {
        stop("Transit cluster finding failed: ", trclust_timed$result$message)
      }
      trclust_timediff <- trclust_timed$time_ms
      
      # Time dosearch on the clustered DAG
      reduced_timed <- time_operation(
        dosearch::dosearch(do1$data, "p(y|do(x))", do1$graph)
      )
      if (!is.null(reduced_timed$result$error)) {
        stop("Reduced dosearch failed: ", reduced_timed$result$message)
      }
      d_result <- reduced_timed$result
      reduced_timediff <- reduced_timed$time_ms
      
      # Initialize ID invariance check time
      idinv_timediff <- 0
      
      # Step 9: Determine the setting based on identifiability results
      if (d_result$identifiable == TRUE) {
        # Case 1: Causal effect is identifiable in the clustered DAG
        # No need to check ID invariance or run on full DAG
        setting <- "id"
      } else {
        # Case 2: Causel effect is not identifiable in clustered DAG
        # Check if ID-invariance property holds for the cluster
        idinv_timed <- time_operation(
          checkidinvariant(dag$graph, strsplit(do1$data, "\n")[[1]], real_dag$clust)
        )
        idinv <- idinv_timed$result
        idinv_timediff <- idinv_timed$time_ms
        
        if (idinv == TRUE) {
          # Case 2a: ID-invariance holds
          # Effect is also non-identifiable in the expanded DAG (no need to run dosearch)
          setting <- "non_id_idinv"
        }
        # else: Case 2b remains "non_id_non_idinv"
        # ID-invariance fails, need to run dosearch on full DAG
      }
      
      # Calculate total clustering approach time
      clustering_timediff <- trclust_timediff + reduced_timediff + idinv_timediff
      if (setting == "non_id_non_idinv") {
        # Only add raw dosearch time if we actually need to run it
        # (when ID-invariance check fails)
        clustering_timediff <- clustering_timediff + raw_timediff
      }
      
      # Store results for this iteration
      result[i, "setting"] <- setting
      result[i, "raw_timediff"] <- raw_timediff
      result[i, "clustering_timediff"] <- clustering_timediff
      result[i, "trclust_timediff"] <- trclust_timediff
      result[i, "idinv_timediff"] <- idinv_timediff
      result[i, "clustered_dag"] <- do1$graph
      result[i, "clustered_dag_size"] <- length(V(dag$graph)$name)
      result[i, "real_dag"] <- do2$graph
      result[i, "real_dag_size"] <- length(V(real_dag$graph)$name)
      result[i, "clustered_data"] <- do1$data
      result[i, "real_data"] <- do2$data
      result[i, "error"] <- FALSE
      
    }, error = function(e) {
      # If there is an error, log but continue simulation
      warning(sprintf("Error in iteration %d: %s", i, e$message))
      result[i, "error"] <<- TRUE
      result[i, "setting"] <<- NA
      result[i, "raw_timediff"] <<- NA
      result[i, "clustering_timediff"] <<- NA
      result[i, "idinv_timediff"] <<- NA
      result[i, "trclust_timediff"] <<- NA
      result[i, "clustered_dag"] <<- NA
      result[i, "clustered_dag_size"] <<- NA
      result[i, "real_dag"] <<- NA
      result[i, "real_dag_size"] <<- NA
      result[i, "clustered_data"] <<- NA
      result[i, "real_data"] <<- NA
    })
    
    # Progress indicator every 10 iterations
    if (i %% indicate == 0) {
      cat("Iteration:", i, "\n")
    }
    
    i <- i + 1
  }
  
  return(result)
}
