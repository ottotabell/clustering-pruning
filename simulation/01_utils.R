# Useful functions from the private repository related to the article
# Clustering and Structural Robustness in Causal Diagrams (Tikka et al., 2023)
# and R-package causaleffect (Tikka and Karvanen, 2017)
# https://github.com/santikka/causaleffect 

library(igraph)

children <- function(x, g, v = igraph::V(g)) {
  ch_ind <- unlist(igraph::neighborhood(g, order = 1, nodes = x, mode = "out"))
  v[ch_ind]$name
}

parents <- function(x, g, v = igraph::V(g)) {
  pa_ind <- unlist(igraph::neighborhood(g, order = 1, nodes = x, mode = "in"))
  v[pa_ind]$name
}

descendants <- function(x, g, v = igraph::V(g)) {
  de_ind <- unlist(igraph::neighborhood(g, order = length(v), nodes = x, mode = "out"))
  v[de_ind]$name
}

ancestors <- function(x, g, v = igraph::V(g)) {
  an_ind <- unlist(igraph::neighborhood(g, order = length(v), nodes = x, mode = "in"))
  v[an_ind]$name
}

neighbors_ <- function(x, g, v = igraph::V(g)) {
  ne_ind <- unlist(igraph::neighborhood(g, order = 1, nodes = x, mode = "all"))
  v[ne_ind]$name
}

connected <- function(x, g, v = igraph::V(g)) {
  co_ind <- unlist(igraph::neighborhood(g, order = length(v), nodes = x, mode = "all"))
  v[co_ind]$name
}

uu <- function(x) {
  if (length(x)) unique(unlist(x))
  else character(0)
}

edge_subgraph <- function(g, incoming, outgoing) {
  # Setting from and to to NULL to satisfy CRAN if we end up making a package
  # R thinks these are global bindings, but they are igraph-operators for edges
  .to <- .from <- NULL
  e <- igraph::E(g)
  e_inc <- e[.to(incoming)]
  e_out <- e[.from(outgoing)]
  igraph::subgraph.edges(g, e[setdiff(e, union(e_inc, e_out))], delete.vertices = FALSE)
}

# Convert an igraph graph using causaleffect syntax into a dag
# with explicit latent variables
to_dag <- function(g) {
  out <- g
  unobs_edges <- which(igraph::edge.attributes(g)$description == "U")
  if (length(unobs_edges)) {
    e <- igraph::get.edges(g, unobs_edges)
    e <- e[e[ ,1] > e[ ,2], , drop = FALSE]
    e_len <- nrow(e)
    new_nodes <- paste0("U[", 1:e_len, "]")
    g <- igraph::set.vertex.attribute(g, name = "description", value = "")
    g <- g + igraph::vertices(new_nodes, description = rep("U", e_len))
    v <- igraph::get.vertex.attribute(g, "name")
    g <- g + igraph::edges(c(rbind(new_nodes, v[e[ ,1]]), rbind(new_nodes, v[e[ ,2]])))
    obs_edges <- setdiff(igraph::E(g), igraph::E(g)[unobs_edges])
    out <- igraph::subgraph.edges(g, igraph::E(g)[obs_edges], delete.vertices = FALSE)
  }
  out
}

# Convert a dag with explicit latent variables into an 
# acyclic directed mixed graph with causaleffect igraph syntax 
to_admg <- function(g) {
  out <- g
  unobs_vars <- which(igraph::vertex.attributes(g)$description == "U")
  obs_vars <- setdiff(1:length(igraph::V(g)), unobs_vars)
  u <- length(unobs_vars)
  if (u) {
    e <- igraph::E(g)
    g_obs <- igraph::induced_subgraph(g, obs_vars)
    for (i in 1:u) {
      unobs_edges <- igraph::get.edges(g, e[.from(unobs_vars[i])])
      if (nrow(unobs_edges) == 2) {
        g_obs <- g_obs + igraph::edges(c(unobs_edges[1:2,2], unobs_edges[2:1,2]), description = "U")
      }
    }
    out <- g_obs
  }
  out
}

ancestors_unsrt <- function(node, G) {
  an.ind <- unique(unlist(igraph::neighborhood(G, order = igraph::vcount(G), nodes = node, mode = "in")))
  an <- igraph::V(G)[an.ind]$name
  return(an)
}

parents_unsrt <- function(node, G.obs) {
  pa.ind <- unique(unlist(igraph::neighborhood(G.obs, order = 1, nodes = node, mode = "in")))
  pa <- igraph::V(G.obs)[pa.ind]$name
  return(pa)
}

# Implements relevant path separation (rp-separation) for testing d-separation. For details, see:
#
# Relevant Path Separation: A Faster Method for Testing Independencies in Bayesian Networks
# Cory J. Butz, Andre E. dos Santos, Jhonatan S. Oliveira;
# Proceedings of the Eighth International Conference on Probabilistic Graphical Models,
# PMLR 52:74-85, 2016.
#
# Note that the roles of Y and Z have been reversed from the paper, meaning that
# we are testing whether X is separated from Y given Z in G.
dSep <- function(G, x, y, z = NULL) {
  an_z <- ancestors_unsrt(z, G)
  an_xyz <- ancestors_unsrt(union(union(x, y), z), G)
  n <- length(igraph::V(G))
  v <- igraph::V(G)$name
  direction <- NA
  traverse_up <- logical(n)
  visited_up <- logical(n)
  traverse_down <- logical(n)
  visited_down <- logical(n)
  names(traverse_up) <- v
  names(visited_up) <- v
  names(traverse_down) <- v
  names(visited_down) <- v
  traverse_up[x] <- TRUE
  visit <- FALSE
  el_name <- NULL
  while (any(traverse_up) || any(traverse_down)) {
    visit <- FALSE
    for (j in 1:n) {
      if (traverse_up[j]) {
        traverse_up[j] <- FALSE
        if (!visited_up[j]) {
          visit <- TRUE
          direction <- TRUE
          el_name <- v[j]
          break
        }
      }
      if (traverse_down[j]) {
        traverse_down[j] <- FALSE
        if (!visited_down[j]) {
          visit <- TRUE
          direction <- FALSE
          el_name <- v[j]
          break
        }
      }
    }
    if (visit) {
      if (el_name %in% y) return(FALSE)
      if (direction) {
        visited_up[el_name] <- TRUE
      } else {
        visited_down[el_name] <- TRUE
      }
      if (direction && !(el_name %in% z)) {
        visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
        visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
        traverse_up[visitable_parents] <- TRUE
        traverse_down[visitable_children] <- TRUE
      } else if (!direction) {
        if (!(el_name %in% z)) {
          visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
          traverse_down[visitable_children] <- TRUE
        }
        if (el_name %in% an_z) {
          visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
          traverse_up[visitable_parents] <- TRUE
        }
      }
    }
  }
  return(TRUE)
}

children_unsrt <- function(node, G) {
  ch.ind <- unique(unlist(igraph::neighborhood(G, order = 1, nodes = node, mode = "out")))
  ch <- igraph::V(G)[ch.ind]$name
  return(ch)
}
