# Algorithms FindTrComp and FindTrClust to find transit clusters from an
# igraph graph. For more details, see:
# Clustering and Structural Robustness in Causal Diagrams (Tikka et al., 2023)

source("01_utils.R")

# Algorithm 1
find_transit_components <- function(g, prohibit = character(0), singletons = FALSE) {
  nodes <- igraph::V(g)
  n <- length(nodes)
  if (length(prohibit)) {
    restrict <- setdiff(igraph::V(g)$name, prohibit)
  } else {
    restrict <- igraph::V(g)$name
  }
  
  pa <- setNames(lapply(nodes, function(x) parents(x, g, nodes)[-1]), nodes$name)
  ch <- setNames(lapply(nodes, function(x) children(x, g, nodes)[-1]), nodes$name)
  an <- setNames(lapply(nodes, function(x) ancestors(x, g, nodes)), nodes$name)
  de <- setNames(lapply(nodes, function(x) descendants(x, g, nodes)), nodes$name)
  
  # Potential receivers
  C_set <- ch[sapply(ch, length) > 0]
  names(C_set) <- NULL
  
  # Potential emitters
  P_set <- pa[sapply(pa, length) > 0]
  names(P_set) <- NULL
  
  # Add singleton components separately if requested
  if (singletons) {
    tc <- lapply(intersect(nodes$name, restrict), function(x) {
      y <- list(nodes = x, receivers = character(0), emitters = character(0))
      if (length(pa[[x]])) y$receivers <- x
      if (length(ch[[x]])) y$emitters <- x
      y
    })
  } else {
    tc <- list()
  }
  
  # Add the full graph if no restrictions
  if (length(restrict) == n) {
    tc <- c(tc, list(list(nodes = nodes$name, receivers = character(0), emitters = character(0))))
  }
  
  # Explicitly add the empty set
  C_set <- c(C_set, list(character(0)))
  P_set <- c(P_set, list(character(0)))
  
  # Restrict to the set of permitted nodes
  C_set <- lapply(C_set, function(x) intersect(x, restrict))
  P_set <- lapply(P_set, function(x) intersect(x, restrict))
  
  # Remove potential duplicates
  C_set <- C_set[!duplicated(C_set)]
  P_set <- P_set[!duplicated(P_set)]
  
  # Check potential (Re, Em) pairs
  for (i in seq_along(C_set)) {
    X_orig <- C_set[[i]]
    for (j in seq_along(P_set)) {
      Y_orig <- P_set[[j]]
      an_de_XY_orig <- intersect(uu(an[Y_orig]), uu(de[X_orig]))
      if (length(Y_orig)) {
        # Restrict only if the other candidate is not the empty set
        X <- intersect(X_orig, an_de_XY_orig)
      } else {
        X <- X_orig
      }
      if (length(X_orig)) {
        # Restrict only if the other candidate is not the empty set
        Y <- intersect(Y_orig, an_de_XY_orig)
      } else {
        Y <- Y_orig
      }
      XY <- union(X, Y)
      n_X <- length(X)
      n_Y <- length(Y)
      n_XY <- length(XY)
      # Do not group the entire graph
      if (n_XY == n || n_XY == 0) next
      # If there are receiver candidates, they must have parents
      if (n_X && !all(sapply(pa[X], length))) next
      # If there are emitter candidates, they must have children
      if (n_Y && !all(sapply(ch[Y], length))) next
      if (n_XY) {
        # Construct A from vertices connected to X and Y
        # when incoming edges to X and outgoing edges from Y are removed
        g_ne <- edge_subgraph(g, X, Y)
        A <- uu(connected(XY, g_ne))
        if (length(A) > 1 && all(A %in% restrict)) {
          comp <- igraph::components(igraph::induced_subgraph(g, A))
          memb <- comp$membership
          memb_v <- names(memb)
          for (k in seq_along(unique(comp$membership))) {
            A_k <- memb_v[memb == k]
            n_A_k <- length(A_k)
            if (n_A_k > 1 && n_A_k < n) {
              X_k <- intersect(X, A_k)
              Y_k <- intersect(Y, A_k)
              tc <- c(tc, is_transit_component(X_k, Y_k, A_k, g))
            }
          }
        }
      }
    }
  }
  unique(tc)
}

is_transit_component <- function(X, Y, A, g) {
  # All receiver candidates must have the same parents outside A
  pa_X <- lapply(X, function(x) {
    setdiff(parents(x, g), A)
  })
  if (length(unique(pa_X)) > 1) return(NULL)
  # All emitter candidates must have the same children outside A
  ch_Y <- lapply(Y, function(y) {
    setdiff(children(y, g), A)
  })
  if (length(unique(ch_Y)) > 1) return(NULL)
  # Confirm that there are no receivers that should not be emitters
  # and that there are no emitters that should not be receivers
  XY <- intersect(X, Y)
  ex_X <- setdiff(X, XY)
  ex_Y <- setdiff(Y, XY)
  X_leak <- setdiff(uu(children(ex_X, g)), A)
  Y_leak <- setdiff(uu(parents(ex_Y, g)), A)
  if (!length(X_leak) && !length(Y_leak)) {
    list(
      list(
        vertices = A,
        receivers = X,
        emitters = Y
      )
    )
  } else {
    NULL
  }
}

grouped_graph <- function(grouping, g) {
  pa_ex <- setdiff(parents(grouping$receivers, g), grouping$vertices)
  ch_ex <- setdiff(children(grouping$emitters, g), grouping$vertices)
  v <- igraph::V(g)
  keep <- setdiff(v, v[igraph::get.vertex.attribute(g, "name") %in% grouping$vertices])
  grouped <- igraph::induced_subgraph(g, keep)
  representative <- paste0(grouping$vertices, collapse = "")
  grouped <- grouped + vertex(representative, description = "")
  grouped <- grouped + edges(as.vector(t(expand.grid(pa_ex, representative))), description = "")
  grouped <- grouped + edges(as.vector(t(expand.grid(representative, ch_ex))), description = "")
  grouped
}

# Algorithm 2
find_transit_clusters <- function(g, tgr) {
  A <- tgr
  B <- tgr
  for (i in rev(seq_along(tgr))) {
    B[[i]] <- NULL
    A <- c(A, expand_cluster(tgr[[i]], A, B, g))
  }
  unique(A)
}

expand_cluster <- function(t, A, B, g) {
  B_prime <- B
  A_v <- lapply(A, "[[", "vertices")
  for (i in rev(seq_along(B))) {
    B_prime[[i]] <- NULL
    s <- B[[i]]
    st <- union(s$vertices, t$vertices)
    st_in_A <- any(vapply(A_v, function(y) setequal(y, st), logical(1)))
    if (!st_in_A) {
      pa_re_S <- setdiff(parents(s$receivers, g), s$receivers)
      pa_re_T <- setdiff(parents(t$receivers, g), t$receivers)
      ch_em_S <- setdiff(children(s$emitters, g), s$emitters)
      ch_em_T <- setdiff(children(t$emitters, g), t$emitters)
      if (setequal(pa_re_S, pa_re_T) && setequal(ch_em_S, ch_em_T)) {
        st_clust <- list(
          vertices = st,
          receivers = union(s$receivers, t$receivers),
          emitters = union(s$emitters, t$emitters)
        )
        A <- unique(
          c(
            A,
            list(st_clust),
            expand_cluster(st_clust, A, B_prime, g)
          )
        )
        A_v <- lapply(A, "[[", "vertices")
      }
    }
  }
  A
}
