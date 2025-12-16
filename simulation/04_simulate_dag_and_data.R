source("01_utils.R")

# Simulating a random clustered dag, a transit cluster for the dag,
# and corresponding data sources. Parsing them for dosearch

# Generates a random igraph DAG
# Inputs:
# n_min = Minimum number of vertices
# n_max = Maximum number of vertices
# prob1, prob2 = Probabilities that an edge is created between two variables
# in the DAG. In reverse topological order, prob1 is used until variable "x" is reached
# and prob2 afterwards.
# Returns:
# graph = igraph graph
# order = all variables in topological order
random_dag <- function(n_min = 5, n_max = 8, prob1 = 0.5, prob2 = 0.35) {
  
  if (n_min < 3) {
    stop("n_min must be at least 3")
  }
  if (n_min > n_max) {
    stop("n_min cannot be greater than n_max")
  }
  
  # Determine number of vertices (uniformly random between n_min and n_max)
  n <- ifelse(n_min == n_max, n_min, sample(n_min:n_max, 1))
  
  # Create a pool of variable names and randomly sample n of them
  variable_pool <- paste0("z", 1:n)
  vars <- sample(variable_pool, n)
  
  # Force the last variable to be "y" (outcome variable)
  vars[n] <- "y"
  
  # Randomly select position for "x" (treatment variable) between 2 and n-1
  x_ind <- sample(2:(n - 1), 1)
  vars[x_ind] <- "x"
  
  # Initialize edge vectors (v = from, w = to)
  v <- c()
  w <- c()
  
  # Phase 1: Generate edges from x_ind to n with probability prob1
  # Working backwards from y (position n) down to x (position x_ind)
  for (i in (n - 1):x_ind) {
    j <- n
    while(j > i) {
      u <- runif(1)
      if (u < prob1) {
        # Add edge from vars[i] to vars[j]
        v <- c(v, vars[i])
        w <- c(w, vars[j])
      }
      j <- j - 1
    }
  } 
  
  # Create temporary graph to check if x is ancestor of y
  if (length(v) > 0) {
    temp_edges <- cbind(v, w)
    temp_dag <- graph_from_data_frame(data.frame(from = v, to = w), 
                                      directed = TRUE, 
                                      vertices = data.frame(name = vars))
  } else {
    # If no edges yet, create empty graph with all vertices
    temp_dag <- make_empty_graph(n = n, directed = TRUE)
    V(temp_dag)$name <- vars
  }
  
  # Ensure x is an ancestor of y (there's a causal path from x to y)
  if ("y" %in% V(temp_dag)$name && "x" %in% V(temp_dag)$name) {
    if (!"x" %in% ancestors("y", temp_dag)) {
      # If x is not an ancestor of y, add direct edge x -> y
      v <- c(v, "x")
      w <- c(w, "y")
    }
  } else {
    # If y or x doesn't exist in graph yet, add the edge x -> y
    v <- c(v, "x")
    w <- c(w, "y")
  }
  
  # Phase 2: Generate edges from position 1 to x_ind-1 with probability prob2
  # These are variables that come before x in topological order
  for (i in (x_ind - 1):1) {
    j <- n
    while(j > i) {
      u <- runif(1)
      if (u < prob2) {
        # Add edge from vars[i] to vars[j]
        v <- c(v, vars[i])
        w <- c(w, vars[j])
      }
      j <- j - 1
    }
  } 
  
  # Ensure all variables are connected to the graph
  # For any isolated vertex, connect it to the next vertex in order
  for (i in 1:n) {
    if ((!vars[i] %in% v) & (!vars[i] %in% w)) {
      v <- c(v, vars[i])
      w <- c(w, vars[i + 1])
    } 
  }
  
  # Create the DAG from collected edges
  edges <- cbind(v, w)
  dag <- graph_from_edgelist(edges, directed = TRUE)
  
  # Ensure all variables are ancestors of y (connected to outcome)
  y_anc <- ancestors("y", dag)
  for (i in 1:(n - 1)) {
    if (!vars[i] %in% y_anc) {
      # If vars[i] is not an ancestor of y, connect it to a random ancestor of y
      v <- c(v, vars[i])
      poss_w <- vars[(i + 1):n]  # Only connect to later variables (maintain topological order)
      poss_w <- poss_w[poss_w %in% y_anc]  # Only connect to variables that are ancestors of y
      w <- c(w, sample(poss_w, 1))
    }
  }
  
  # Rebuild final DAG with all edges
  edges <- cbind(v, w)
  dag <- graph_from_edgelist(edges, directed = TRUE)
  
  return(list(graph = dag, order = vars))
}

# Adds a transit cluster to a graph
# A transit cluster is a set of variables that replace a single variable in the original DAG.
# The cluster contains:
#   Receivers (r): nodes that receive edges from parents of the original variable
#   Emitters (e): nodes that send edges to children of the original variable
#   Others (o): nodes that are internal to the cluster
# Inputs:
# dag = igraph graph
# vars = Variables of graph in topological order
# max_r = Max number of receivers in the cluster
# max_e = Max number of emitters in the cluster
# max_o = Max number of other vertices in the cluster
# prob = probability that an edge is created between two cluster nodes
# Returns:
# graph = New igraph graph with the cluster expanded
# clust = Name of the variable that was replaced by the cluster
add_transit_cluster <- function(dag, vars, max_r = 3, max_e = 3, max_o = 3, prob = 0.5) {
  n <- length(vars)
  x_ind <- which(vars == "x")
  
  # Get edges from igraph (v = from, w = to)
  el <- as_edgelist(dag)
  v <- el[, 1]
  w <- el[, 2]
  
  # Select a random variable to replace with a cluster (not x or y)
  clust <- sample(vars[-c(n, x_ind)], 1)
  
  # Create pools of variable names for the cluster
  receivers_pool <- paste0("r", 1:max_r)
  emitters_pool <- paste0("e", 1:max_e)
  others_pool <- paste0("o", 1:max_o)
  
  # Flag to ensure emitters are connected to receivers
  emitters_not_connected <- TRUE
  
  infinite_loop <- 1 # security check for infinite loops
  
  # Keep generating clusters until all emitters are connected to receivers
  while(emitters_not_connected) {
    
    # Ensure cluster has at least 2 variables
    l_clust <- 0
    while (l_clust < 2) {
      # Number of receivers: only if clust has parents in the original DAG
      n_receivers <- ifelse(length(parents(clust, dag)) > 0, sample(1:max_r, 1), 0)
      # Number of emitters: only if clust has children in the original DAG
      n_emitters <- ifelse(length(children(clust, dag)) > 0, sample(1:max_e, 1), 0)
      # Number of other internal variables (can be 0)
      n_other <- sample(0:max_o, 1)
      l_clust <- n_receivers + n_emitters + n_other
    }
    
    # Sample specific variables from each pool
    clust_vars <- c(sample(receivers_pool, n_receivers),
                    sample(others_pool, n_other),
                    sample(emitters_pool, n_emitters))
    
    # Initialize edge vectors for cluster internal edges
    v_clust <- c()
    w_clust <- c()
    
    # Generate random edges within the cluster (maintaining topological order)
    for (i in (l_clust - 1):1) {
      j <- l_clust
      while(j > i) {
        u <- runif(1)
        if (u < prob) {
          # Add edge from clust_vars[i] to clust_vars[j]
          v_clust <- c(v_clust, clust_vars[i])
          w_clust <- c(w_clust, clust_vars[j])
        }
        j <- j - 1
      }
      # Ensure non-emitter variables that aren't yet in the graph get connected
      # (connect to next variable in order)
      if ((substr(clust_vars[i], 1, 1) != "e") & (!clust_vars[i] %in% v_clust)) {
        v_clust <- c(v_clust, clust_vars[i])
        w_clust <- c(w_clust, clust_vars[i+1])
      }
    } 
    
    {
      # Identify emitters and receivers in the cluster
      ems <- clust_vars[substr(clust_vars, 1, 1) == "e"]
      recs <- clust_vars[substr(clust_vars, 1, 1) == "r"]
      
      # Get children and parents of the original cluster variable (excluding self-loops)
      chT <- setdiff(children(clust, dag), clust)
      paT <- setdiff(parents(clust, dag), clust)
      
      # Connect all emitters to all children of clust
      v_clust <- c(v_clust, rep(ems, length(chT)))
      w_clust <- c(w_clust, rep(chT, each = length(ems)))
      
      # Connect all parents of clust to all receivers
      v_clust <- c(v_clust, rep(paT, length(recs)))
      w_clust <- c(w_clust, rep(recs, each = length(paT)))
      
      # Combine cluster edges with original graph edges (excluding clust)
      vw_clust <- data.frame(v = v_clust, w = w_clust)
      vw <- data.frame(v, w)
      # Remove all edges involving the original cluster variable
      vw <- vw[vw$v != clust, ]
      vw <- vw[vw$w != clust, ]
      # Add cluster edges
      vw <- rbind(vw, vw_clust)
    }
    
    # Create the new DAG with the expanded cluster
    real_dag <- graph_from_data_frame(vw, directed = TRUE)
    
    # Verify that all emitters are reachable from at least one receiver
    # (ensures the cluster maintains connectivity)
    emitters_not_connected <- FALSE
    for (em in ems) {
      if (!any(recs %in% ancestors(em, real_dag))) emitters_not_connected <- TRUE
    }
    
    infinite_loop <- infinite_loop + 1
    if (infinite_loop > 1000) stop("add_transit_cluster ended up in an infinite loop for unknown reasons.")
  }
  
  return(list(graph = real_dag, clust = clust))
}

# Generates random data sources based on clustered dag
# Input:
# dag = igraph graph
# clust = Cluster vertex in dag
# min_sources = Minimum number of data sources
# max_sources = Maximum number of data sources
# prob = probability for a vertex to belong in class "observed", "intervened",
# "conditioned" or "unobserved", respectively. 
# Returns: list of observed, intervened and conditioned variables for each source 
random_data <- function(dag, clust, min_sources = 2, max_sources = 4, prob = c(0.5, 0.1, 0.1, 0.3)) {
  vars <- V(dag)$name
  vars <- sort(vars) # Ensure vars[1] == "x" and vars[2] == "y"
  # Setting the roles for variables
  roles <- c("obs", "do", "cond", "unobs")
  
  # Number of data sources
  n <- ifelse(min_sources == max_sources, min_sources, sample(min_sources:max_sources, 1))
  obs <- list()
  do <- list()
  cond <- list()
  
  # Flag for whether x appears in at least one data source
  x_in <- FALSE
  # Flag for whether clust appears in at least one data source (compatibility)
  clust_in <- FALSE
  for (i in 1:n) {
    while (TRUE) {
      # Sampling the roles
      s <- sample(roles, length(vars), replace = TRUE, prob = prob)
      # Conditions to check:
      # If nothing is observed sample again
      if (sum(s == "obs") == 0) next
      # Block distributions p(y|do(x))
      if (s[2] == "obs" & s[1] == "do") next
      if (i == 1 & s[1] == "do") next
      # Mark if x or clust is found
      if (s[1] != "unobs") x_in <- TRUE
      if (s[which(vars == clust)] != "unobs") clust_in <- TRUE 
      # Continue generating until x and clust appear
      if (i == n & !x_in & !clust_in) next
      if (i == 1) { # y must always appear in observed for the first input
        obs[[i]] <- unique(c(vars[s == "obs"], "y"))
        do[[i]] <- setdiff(vars[s == "do"], "y")
        cond[[i]] <- setdiff(vars[s == "cond"], "y")
      } else {
        obs[[i]] <- vars[s == "obs"]
        do[[i]] <- vars[s == "do"]
        cond[[i]] <- vars[s == "cond"]
      }
      break
    }
  }
  return(list(obs = obs, do = do, cond = cond))
}

# Function that replaces clust with cluster variables such as the data source
# is compatible (contains all emitters if there are any).
replace_cluster <- function(v, clust, clust_vars) {
  new_v <- setdiff(v, clust)
  # Extract all emitters and include them in the new vector
  ems <- clust_vars[substr(clust_vars, 1, 1) == "e"]
  remaining <- setdiff(clust_vars, ems)
  new_v <- c(new_v, ems)
  # If there are no emitters, ensure that at least one cluster node is in the data.
  # Otherwise there can be 0 non-emitters.
  remaining_n <- ifelse(length(ems) > 0, sample(0:length(remaining), 1), sample(1:length(remaining), 1))
  new_v <- c(new_v, sample(remaining, remaining_n))
  return(new_v)
}

# Creates original data sources based on the clustered data
# Inputs:
# dag_real = list containing:
#               clust = cluster variable
#               graph = containing the original graph
# data = list containing
# obs = List of observed variables
# do = List of intervened variables
# cond = List of conditioned variables
original_data <- function(dag_real, data) {
  o <- data$obs
  d <- data$do
  c <- data$cond
  clust <- dag_real$clust
  vars <- V(dag_real$graph)$name
  clust_vars <- vars[substr(vars, 1, 1) %in% c("r", "e", "o")]
  obs_real <- o
  do_real <- d
  cond_real <- c
  
  # Call replace_cluster if cluster appears in one of the lists.
  for (i in 1:length(o)) {
    if (clust %in% obs_real[[i]]) {
      obs_real[[i]] <- replace_cluster(obs_real[[i]], clust, clust_vars)
      next
    }
    if (clust %in% do_real[[i]]) {
      do_real[[i]] <- replace_cluster(do_real[[i]], clust, clust_vars)
      next
    }
    if (clust %in% cond_real[[i]]) {
      cond_real[[i]] <- replace_cluster(cond_real[[i]], clust, clust_vars)
      next
    }
  }
  return(list(obs_real = obs_real, do_real = do_real, cond_real = cond_real))
}

# Convert igraph to dosearch format
igraph_to_dosearch <- function(g) {
  el <- as_edgelist(g)
  edges_str <- paste(sprintf("%s -> %s", el[, 1], el[, 2]), collapse = "\n ")
  return(edges_str)
}

# Parses a dag and data sources compatible with dosearch
parse_dosearch <- function(dag, data) {
  o <- data$obs
  d <- data$do
  c <- data$cond
  exprs <- mapply(function(o, d, c) {
    o_part <- paste(o, collapse = ", ")
    do_part <- if (length(d) > 0) paste0("do(", paste(d, collapse = ", "), ")") else NULL
    c_part <- if (length(c) > 0) paste(c, collapse = ", ") else NULL
    inside <- paste(c(do_part, c_part), collapse = ", ")
    if (nchar(inside) > 0) {
      paste0("p(", o_part, " | ", inside, ")")
    } else {
      paste0("p(", o_part, ")")
    }
  }, o, d, c, SIMPLIFY = TRUE)
  
  result <- paste(exprs, collapse = "\n")
  
  # Convert igraph to dosearch graph format
  graph_str <- igraph_to_dosearch(dag)
  
  return(list(graph = graph_str, data = result))
}

