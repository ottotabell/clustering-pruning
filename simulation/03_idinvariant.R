# Implementation of CheckIdInvariant algorithm from 
# Clustering and Pruning in Causal Data Fusion (Tabell et al., 2025)

source("01_utils.R")

# Modifies the graph so that all incoming edges in nodes of vector "inc" and
# all outgoing edges in nodes of vector "out" are removed.
modify_dag <- function(graph, inc = NULL, out = NULL) {
  g <- graph
  
  if (!is.null(inc) && length(inc) > 0) {
    # Remove incoming edges to nodes in inc
    inc_existing <- intersect(inc, V(g)$name)
    if (length(inc_existing) > 0) {
      edges_to_remove <- E(g)[.to(inc_existing)]
      if (length(edges_to_remove) > 0) {
        g <- delete_edges(g, edges_to_remove)
      }
    }
  }
  
  if (!is.null(out) && length(out) > 0) {
    # Remove outgoing edges from nodes in out
    out_existing <- intersect(out, V(g)$name)
    if (length(out_existing) > 0) {
      edges_to_remove <- E(g)[.from(out_existing)]
      if (length(edges_to_remove) > 0) {
        g <- delete_edges(g, edges_to_remove)
      }
    }
  }
  
  return(g)
}

# Takes data sources as a vector and returns a list of observed, intervened
# and conditioned variables
# For example: extract_variables_by_source(c("p(a|do(b), c)", "p(d, e, f)")) should
# return a list:
# observed[[1]] = "a"
# observed[[2]] = c("d", "e", "f")
# intervened[[1]] = "b"
# intervened[[2]] = ""
# conditioned[[1]] = "c"
# conditioned[[2]] = ""
extract_variables_by_source <- function(sources) {
  n <- length(sources)
  observed_list <- list()
  intervened_list <- list()
  conditioned_list <- list()
  
  for (i in seq_along(sources)) {
    src <- sources[i]
    
    # Removing "p(" from the start and ")" from the end
    content <- gsub("^p\\(|\\)$", "", src)
    # Removing empty spaces
    content <- gsub(" ", "", content)
    
    obs_vars <- c("")
    int_vars <- c("")
    cond_vars <- c("")
    
    # Splitting variables to A and B&C. [[1]] is needed because strsplit
    # returns a list of one element. [[1]] gives us a vector.
    parts <- strsplit(content, "\\|")[[1]]
    
    # Splitting observed variables by comma
    obs_vars <- strsplit(trimws(parts[1]), ",\\s*")[[1]]
    
    if (length(parts) > 1) {
      cond_part <- trimws(parts[2])
      
      do_match <- regexpr("do\\([^)]*\\)", cond_part)
      if (do_match != -1) {
        do_str <- regmatches(cond_part, do_match)
        int_vars <- strsplit(gsub("do\\(|\\)", "", do_str), ",\\s*")[[1]]
        cond_part <- gsub("do\\([^)]*\\),?", "", cond_part)
        cond_part <- trimws(cond_part)
      }
      
      # Only parse conditioned variables if anything is left
      if (nzchar(cond_part)) {
        cond_vars <- strsplit(cond_part, ",\\s*")[[1]]
      }
    }
    
    # Always assign (even NULL)
    observed_list[[i]]    <- obs_vars
    intervened_list[[i]]  <- int_vars
    conditioned_list[[i]] <- cond_vars
  }
  
  list(
    observed = observed_list,
    intervened = intervened_list,
    conditioned = conditioned_list
  )
}

# Implementation of CheckIdInvariant algorithm from 
# Clustering and Pruning in Causal Data Fusion (Tabell et al., 2025)
# Inputs:
# graph = Clustered causal graph (igraph)
# sources = Data sources (character vector, each element own source)
# clust = Transit cluster(s) 
# If the causal effect is non-identifiable in the clustered graph
# and checkidinvariant returns TRUE, then it is non-identifiable also in the
# original graph.
checkidinvariant <- Vectorize(function(graph, sources, clust) {
  # Line 2: Check if clust has parents
  if (length(parents(clust, graph)) == 1) return(TRUE)
  
  # Parses us the sets of obs, intv and cond variables
  vars <- extract_variables_by_source(sources)
  
  # Line 3
  for (i in 1:length(vars$observed)) {
    A <- vars$observed[[i]]
    B <- vars$intervened[[i]]
    C <- vars$conditioned[[i]]
    
    # Clean empty strings
    list_cleaned <- lapply(list(A, B, C), function(x) x[x != ""])
    A <- list_cleaned[[1]]
    B <- list_cleaned[[2]]
    C <- list_cleaned[[3]]
    
    # Line 4: Check if clust is in the input
    if (clust %in% c(A, B, C)) {
      # Line 5: Check if any descendant of clust (excluding clust itself) is in C
      desc_clust <- setdiff(descendants(clust, graph), clust)
      if (sum(desc_clust %in% C) > 0) return(FALSE)
      
      # Line 6: D is descendants of clust that are in A, excluding clust itself
      D <- setdiff(intersect(descendants(clust, graph), A), clust)
      
      # Line 7: Check if clust is in A and the conditional independence between  A and clust
      if (clust %in% A) {
        if (dSep(modify_dag(graph, B, clust), D, clust, c(B, C, setdiff(A, c(D, clust))))) next
      }
      
      # Line 8: Check if clust is in B
      if (clust %in% B) next
      
      # Line 9: Check if clust is in C and the conditional independence between C and clust
      if (clust %in% C) {
        if (dSep(modify_dag(graph, B, clust), A, clust, c(B, setdiff(C, clust)))) next
      }
    } else { # Line 10: go here if clust not in the input
      # Line 11: Check if there's a marginal distribution over clust (no intervention, no conditioning)
      if (any(mapply(function(obs, intv, cond) {
        (clust %in% obs) & sum(intv == "") > 0 & sum(cond == "") > 0
      }, vars$observed, vars$intervened, vars$conditioned))) {
        
        # Line 12: Check if the three conditional independencies required take place
        cond1 <- dSep(modify_dag(graph, B, clust), A, clust, c(B, C))
        cond2 <- if(length(C) > 0) dSep(modify_dag(graph, B), clust, C, B) else TRUE
        cond3 <- if(length(B) > 0) dSep(modify_dag(graph, B), clust, B) else TRUE
        if (cond1 & cond2 & cond3) next
      }
      
      # Line 13: Check if A and clust are d-separated given B and C when clust is also intervened
      if (dSep(modify_dag(graph, c(B, clust)), A, clust, c(B, C))) next
      
      # Line 14: Check if there's a distribution with A observed, do(B, clust), conditioned on C
      if (any(
        mapply(function(obs, intv, cond) {
          all(A %in% obs) &&
            setequal(intv, c(B, clust)) &&
            setequal(ifelse(length(C) == 0, "", C), cond)
        }, vars$observed, vars$intervened, vars$conditioned)
      )) next
    }
    # Line 15: Return FALSE if none of these conditions were satisfied for the input
    return(FALSE)
  }
  # Line 16: Return TRUE if none of the inputs returned FALSE earlier
  return(TRUE)
}, vectorize.args = "clust")