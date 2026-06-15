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

# Safely checks whether a time_operation() result is an error.
is_timed_error <- function(timed) {
  is.list(timed$result) && isTRUE(timed$result$error)
}

# Runs a simulation to compare identification performance between:
# 1. Raw dosearch on the full (real) DAG with expanded transit clusters
# 2. Clustering approach: finding transit clusters, reducing to clustered DAG,
#    checking ID invariance, then running dosearch
# 3. Pruning approach: pruning irrelevant nodes before running dosearch
# 4. Approaches 2. and 3. applied together
#
# Inputs:
# n = Number of iterations
# clustering = Whether to use clustering (default TRUE)
# prune = Whether to use pruning (default TRUE)
# min_n = Min. number of variables in clustered graph
# max_n = Max. number of variables in clustered graph
# max_r = Max. number of receivers in transit cluster
# max_e = Max. number of emitters in transit cluster
# max_o = Max. number of other vertices in transit cluster
# min_sources = Min. number of data sources
# max_sources = Max. number of data sources
# prob1, prob2 = Probability parameters related to DAG creation
# prob3 = Probability parameters related to transit cluster creation
# prob_sources = Probability parameters related to data source creation
# timeout = Time limit for dosearch (in hours)
# indicate = After how many indices the progress is printed.
# See more information on probs in their respective functions
#
# Returns:
# Data frame of simulation results with columns:
#   setting: "A" = identifiable in clustered graph
#            "B" = non-identifiable in clustered, but ID-invariant holds
#            "C" = pruning reduced graph size
#            "D" = default (no reduction helped; run raw dosearch)
#            "timeout" = raw dosearch exceeded timeout
#   raw_timediff: Time (ms) for dosearch on full real DAG
#   clustering_timediff: Total time (ms) for the clustering approach
#   trclust_timediff: Time (ms) to find transit clusters
#   reduced_timediff: Time (ms) for dosearch on the clustered DAG
#   idinv_timediff: Time (ms) to check ID invariance
#   pruning_timediff: Time (ms) for the pruning() call
#   pruned_timediff: Time (ms) for dosearch on the pruned DAG
#   clustered_dag: DAG string with clusters
#   clustered_dag_size: Number of vertices in clustered DAG
#   real_dag: DAG string with clusters expanded
#   real_dag_size: Number of vertices in real DAG
#   clustered_data: Data source string for clustered DAG
#   real_data: Data source string for real DAG
#   error: TRUE if iteration encountered an error
simulate_times <- function(n, clustering = TRUE, prune = TRUE, 
                           min_n = 5, max_n = 5, max_r = 2, max_e = 2, max_o = 1,
                           min_sources = 2, max_sources = 3,
                           prob1 = 0.5, prob2 = 0.35, prob3 = 0.5,
                           prob_sources = c(0.35, 0.15, 0.10, 0.4), timeout = 0.267,
                           indicate = 10, save_every = 10, 
                           output_file = "/scratch/jkarvane/pruning/simulation_results_%03d.rds") {
  
  # Initialize result data frame
  result <- data.frame(
    setting            = character(n),
    raw_timediff       = numeric(n),
    clustering_timediff = numeric(n),
    trclust_timediff   = numeric(n),
    reduced_timediff   = numeric(n),
    idinv_timediff     = numeric(n),
    pruning_timediff   = numeric(n),
    pruned_timediff    = numeric(n),
    clustered_dag           = character(n),
    clustered_dag_size      = integer(n),
    real_dag                = character(n),
    real_dag_size           = integer(n),
    pruned_dag_size         = integer(n),
    clustered_data          = character(n),
    real_data          = character(n),
    error              = logical(n),
    stringsAsFactors   = FALSE
  )
  
  for (i in 1:n) {
    tryCatch({
      
      # ------------------------------------------------------------------ #
      # 1. Generate random DAG and data sources
      # ------------------------------------------------------------------ #
      dag      <- random_dag(min_n, max_n, prob1 = prob1, prob2 = prob2, pruning = prune)
      real_dag <- add_transit_cluster(dag$graph, dag$order,
                                      max_r = max_r, max_e = max_e, max_o = max_o,
                                      prob = prob3)
      d        <- random_data(dag$graph, real_dag$clust,
                              min_sources = min_sources, max_sources = max_sources,
                              prob = prob_sources)
      d_real   <- original_data(real_dag, d)
      
      dag      <- dag$graph
      real_dag_obj <- real_dag          # keep full object for cluster info
      real_dag <- real_dag$graph
      
      # ------------------------------------------------------------------ #
      # 2. Optionally prune and prepare dosearch inputs
      # ------------------------------------------------------------------ #
      # Initialise timing placeholders
      pruning_timediff <- 0
      pruned_timediff  <- 0
      
      if (prune) {
        # Time the pruning operation itself
        pruning_timed <- time_operation(
          pruning(dag, real_dag, d, d_real, clustering)
        )
        if (is_timed_error(pruning_timed)) {
          stop("Pruning failed: ", pruning_timed$result$message)
        }
        pruning_timediff <- pruning_timed$time_ms
        
        pruned <- pruning_timed$result
        dag <- pruned[[1]]   # pruned clustered DAG
        real_pruned <- pruned[[2]]   # pruned real DAG
        d <- pruned[[3]]   # pruned clustered data
        d_real_pruned <- pruned[[4]]   # pruned real data
        
        do3 <- parse_dosearch(real_pruned, d_real_pruned)
      }
      
      if (clustering) do1 <- parse_dosearch(dag, d)
      do2 <- parse_dosearch(real_dag, d_real)
      
      # ------------------------------------------------------------------ #
      # 3. Baseline: raw dosearch on the full expanded DAG
      # ------------------------------------------------------------------ #
      raw_timed <- time_operation(
        dosearch::dosearch(do2$data, "p(y|do(x))", do2$graph,
                           control = list(time_limit = timeout))
      )
      if (is_timed_error(raw_timed)) {
        stop("Raw dosearch failed: ", raw_timed$result$message)
      }
      raw_timediff <- raw_timed$time_ms
      
      # Discard iterations that exceed the timeout
      if ((raw_timediff / (1000 * 60 * 60)) > timeout) {
        result[i, "error"]              <- TRUE
        result[i, "setting"]            <- "timeout"
        result[i, "raw_timediff"]       <- raw_timediff
        result[i, "clustering_timediff"] <- NA
        result[i, "trclust_timediff"]   <- NA
        result[i, "reduced_timediff"]   <- NA
        result[i, "idinv_timediff"]     <- NA
        result[i, "pruning_timediff"]   <- pruning_timediff
        result[i, "pruned_timediff"]    <- NA
        result[i, "clustered_dag"]           <- if (clustering) do1$graph else NA
        result[i, "clustered_dag_size"]      <- length(igraph::V(dag)$name)
        result[i, "real_dag"]                <- do2$graph
        result[i, "real_dag_size"]           <- length(igraph::V(real_dag)$name)
        result[i, "pruned_dag_size"]         <- if (prune) length(igraph::V(real_pruned)$name)  else NA
        result[i, "clustered_data"]          <- if (clustering) do1$data else NA
        result[i, "real_data"]          <- do2$data
        
        cat("Iteration:", i, "(timeout)\n")
        next
      }
      
      # ------------------------------------------------------------------ #
      # 4. Determine setting and measure reduction approach times
      # ------------------------------------------------------------------ #
      setting            <- "D"   # default: no reduction helped
      clustering_timediff <- 0
      trclust_timediff   <- 0
      reduced_timediff   <- 0
      idinv_timediff     <- 0
      
      # ---- Clustering branch ----
      if (clustering) {
        
        # Time finding transit clusters from the expanded DAG
        trclust_timed <- time_operation(
          suppressWarnings(
            find_transit_components(real_dag, prohibit = c("x", "y"))
          )
        )
        if (is_timed_error(trclust_timed)) {
          stop("Transit cluster finding failed: ", trclust_timed$result$message)
        }
        trclust_timediff <- trclust_timed$time_ms
        
        # Time dosearch on the clustered DAG
        reduced_timed <- time_operation(
          dosearch::dosearch(do1$data, "p(y|do(x))", do1$graph,
                             control = list(time_limit = timeout))
        )
        if (is_timed_error(reduced_timed)) {
          stop("Reduced dosearch failed: ", reduced_timed$result$message)
        }
        reduced_timediff <- reduced_timed$time_ms
        d_result <- reduced_timed$result   # dosearch result object
        
        if (d_result$identifiable) {
          # Case A: identifiable in the clustered DAG — we are done
          setting <- "A"
        } else {
          # Case B/D: not identifiable — check ID invariance
          idinv_timed <- time_operation(
            checkidinvariant(dag, strsplit(do1$data, "\n")[[1]], real_dag_obj$clust)
          )
          if (is_timed_error(idinv_timed)) {
            stop("ID invariance check failed: ", idinv_timed$result$message)
          }
          idinv_timediff <- idinv_timed$time_ms
          idinv          <- idinv_timed$result
          
          if (idinv) {
            # Case B: non-identifiable and ID-invariant — no need to run raw dosearch
            setting <- "B"
          }
          # else remains "D" (or "C" below if pruning helps)
        }
        
        # Total clustering time (add raw only when ID-invariance failed)
        clustering_timediff <- trclust_timediff + reduced_timediff + idinv_timediff
      }
      
      # ---- Pruning branch ----
      # Only check pruning benefit when clustering did not already resolve the query
      if (prune && setting == "D") {
        n_real   <- length(igraph::V(real_dag)$name)
        n_pruned <- length(igraph::V(real_pruned)$name)
        
        if (n_pruned < n_real) {
          # Time dosearch on the pruned DAG
          pruned_timed <- time_operation(
            dosearch::dosearch(do3$data, "p(y|do(x))", do3$graph,
                               control = list(time_limit = timeout))
          )
          if (is_timed_error(pruned_timed)) {
            stop("Pruned dosearch failed: ", pruned_timed$result$message)
          }
          pruned_timediff <- pruned_timed$time_ms
          setting <- "C"
        }
      }
      
      if (setting == "A") reduced_timediff <- pruning_timediff + trclust_timediff + reduced_timediff
      if (setting == "B") reduced_timediff <- pruning_timediff + trclust_timediff + reduced_timediff + idinv_timediff
      if (setting == "C") reduced_timediff <- pruning_timediff + trclust_timediff + reduced_timediff + idinv_timediff + pruned_timediff
      if (setting == "D") reduced_timediff <- pruning_timediff + trclust_timediff + reduced_timediff + idinv_timediff + pruned_timediff + raw_timediff
      
      # ------------------------------------------------------------------ #
      # 5. Store results
      # ------------------------------------------------------------------ #
      result[i, "setting"]            <- setting
      result[i, "raw_timediff"]       <- raw_timediff
      result[i, "clustering_timediff"] <- clustering_timediff
      result[i, "trclust_timediff"]   <- trclust_timediff
      result[i, "reduced_timediff"]   <- reduced_timediff
      result[i, "idinv_timediff"]     <- idinv_timediff
      result[i, "pruning_timediff"]   <- pruning_timediff
      result[i, "pruned_timediff"]    <- pruned_timediff
      result[i, "clustered_dag"]           <- if (clustering) do1$graph else NA
      result[i, "clustered_dag_size"]      <- length(igraph::V(dag)$name)
      result[i, "real_dag"]                <- do2$graph
      result[i, "real_dag_size"]           <- length(igraph::V(real_dag)$name)
      result[i, "pruned_dag_size"]         <- if (prune) length(igraph::V(real_pruned)$name)     else NA
      result[i, "clustered_data"]          <- if (clustering) do1$data else NA
      result[i, "real_data"]          <- do2$data
      result[i, "error"]              <- FALSE
      
    }, error = function(e) {
      warning(sprintf("Error in iteration %d: %s", i, e$message))
      result[i, "error"]              <<- TRUE
      result[i, "setting"]            <<- NA
      result[i, "raw_timediff"]       <<- NA
      result[i, "clustering_timediff"] <<- NA
      result[i, "trclust_timediff"]   <<- NA
      result[i, "reduced_timediff"]   <<- NA
      result[i, "idinv_timediff"]     <<- NA
      result[i, "pruning_timediff"]   <<- NA
      result[i, "pruned_timediff"]    <<- NA
      result[i, "clustered_dag"]           <<- NA
      result[i, "clustered_dag_size"]      <<- NA
      result[i, "real_dag"]                <<- NA
      result[i, "real_dag_size"]           <<- NA
      result[i, "pruned_dag_size"]    <<- NA
      result[i, "real_data"]          <<- NA
    })
    
    if (i %% indicate == 0) {
      cat("Iteration:", i, "\n")
    }
    
    if (i %% save_every == 0) {
      saveRDS(result, file = output_file)
    }
   
  }
  
  return(result)
}