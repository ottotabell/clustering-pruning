# Testing functions from 02_findtrclust.R, 03_idinvariant.R and 
# 04_simulate_dag_and_data.R

library(dosearch)

source("02_findtrclust.R")
source("03_idinvariant.R")
source("04_simulate_dag_and_data.R")

# The example graphs from the paper.

# Illustrative examples of clustering Fig. 4.
fig_4a <- graph_from_edgelist(rbind(
  c("x","r"), c("r","s1"), c("r","s2"),
  c("s1","s2"), c("s1","e1"), c("s2","e1"), c("s2","e2"),
  c("e1","w1"), c("e2","w1"), c("e1","w2"), c("e2","w2"),
  c("w1","w2"), c("w2","y"),
  c("t1","x"), c("t2","x"), c("t1","w1"), c("t2","w2")
), directed = TRUE)

fig_4b <- graph_from_edgelist(rbind(
  c("t", "x"), c("t", "w1"),
  c("x", "s"), c("s", "w1"), c("s", "w2"),
  c("w1", "w2"), c("w2", "y")
), directed = TRUE)

# ELSA Brazil Fig. 8

fig_8a <- graph_from_edgelist(
  rbind(c("b", "s"), c("b", "m"), c("b", "l"),
  c("l", "h"), c("l", "y"), c("l", "s"),
  c("h", "m"), c("s", "h"), c("s", "m"),
  c("m", "y")
) , directed = TRUE)

fig_8b <- graph_from_edgelist(
  rbind(c("b", "s"), c("b", "m1"), c("b", "l"),
        c("l", "h"), c("l", "y"), c("l", "s"),
        c("h", "m1"), c("s", "h"), c("s", "m1"),
        c("m1", "m2"), c("m2", "y")
  ) , directed = TRUE)

# Causal graphs demonstrating the necessity of lines 7 and 9  Fig. 9.

fig_9a <- graph_from_edgelist(rbind(
  c("w", "x"), c("w", "y"),
  c("x", "z1"), c("z1", "z2"), c("z2", "y")
), directed = TRUE)

fig_9b <- graph_from_edgelist(rbind(
  c("w", "x"), c("w", "y"),
  c("x", "z"), c("z", "y")
), directed = TRUE)

############################
# FINDING TRANSIT CLUSTERS #
############################

fig_4_trcomp <- find_transit_components(fig_4a, prohibit = c("x", "y"))
fig_4_trcomp
find_transit_clusters(fig_4a, fig_4_trcomp)

fig_8_trcomp <- find_transit_components(fig_8b, prohibit = c("l", "y"))
fig_8_trcomp
find_transit_clusters(fig_8b, fig_8_trcomp)

fig_9_trcomp <- find_transit_components(fig_9a, prohibit = c("x", "y"))
fig_9_trcomp
find_transit_clusters(fig_9b, fig_9_trcomp)

####################
# CHECKIDINVARIANT #
####################

checkidinvariant(fig_4b, c("p(x, s, w2)", "p(y, w2 | do(w1))"), c("s", "t"))
checkidinvariant(fig_4b, c("p(x, s)", "p(y | do(t), s)", "p(x, t)"), c("s", "t"))
checkidinvariant(fig_4b, c("p(y, s, t)", "p(x, w1 | do(s))", "p(x, w1)"), c("s", "t"))

checkidinvariant(fig_8a, c("y, l, h, s, m | b"), c("b", "h", "m"))
checkidinvariant(fig_8a, c("y, l, h, s, m , b"), c("b", "h", "m"))
checkidinvariant(fig_8a, c("b, m, s, y", "b, h, m, s"), c("b", "m"))

checkidinvariant(fig_9b, c("p(x, z)", "p(y, z)"), "z")
checkidinvariant(fig_9b, c("p(x, z)", "p(y | z)"), "z")
checkidinvariant(fig_9b, c("p(x, z)", "p(y, z | x)"), "z")
checkidinvariant(fig_9b, c("p(x, z)", "p(y | do(w), z)"), "z")

# Finding transit clusters and checking identifiability properties seems to
# work for all examples cases of the paper

############################
# SIMULATING DAGS AND DATA #
############################

set.seed(20252710)

# Simulating a few different sized DAGs
{
  dag1 <- random_dag(4, 4)
  dag2 <- random_dag(13, 14, prob1 = 0.6, prob2 = 0.6)
  dag3 <- random_dag(5, 9, prob2 = 0.7)
}
  # plot(dag1$graph); plot(dag2$graph); plot(dag3$graph)

# Adding transit clusters to each

{
  real_dag1 <- add_transit_cluster(dag1$graph, dag1$order, max_e = 2, max_r = 2, max_o = 0)
  real_dag2 <- add_transit_cluster(dag2$graph, dag2$order, max_e = 2, max_r = 2, max_o = 2)
  real_dag3 <- add_transit_cluster(dag3$graph, dag3$order, max_e = 3, max_r = 3, max_o = 4, prob = 0.2)
}

# Simulating the data sources

{
  d1 <- random_data(dag1$graph, real_dag1$clust)
  real_d1 <- original_data(real_dag1, d1)
  d2 <- random_data(dag2$graph, real_dag2$clust, min_sources = 4, max_sources = 4,
                    prob = c(0.8, 0.1, 0, 0.1))
  real_d2 <- original_data(real_dag2, d2)
  d3 <- random_data(dag1$graph, real_dag1$clust, prob = c(0.3, 0.1, 0.4, 0.4))
  real_d3 <- original_data(real_dag1, d1)
}

# Parsing to dosearch inputs

{
  do1 <- parse_dosearch(dag1$graph, d1)
  real_do1 <- parse_dosearch(real_dag1$graph, real_d1)
  do2 <- parse_dosearch(dag2$graph, d2)
  real_do2 <- parse_dosearch(real_dag2$graph, real_d2)
  do3 <- parse_dosearch(dag3$graph, d3)
  real_do3 <- parse_dosearch(real_dag3$graph, real_d3)
}

  # do1; real_do1
  # do2; real_do2
  # do3; real_do3

dosearch(do1$data, "p(y|do(x))", do1$graph)
dosearch(do3$data, "p(y|do(x))", do3$graph)
checkidinvariant(dag3$graph,  strsplit(do3$data, "\n")[[1]], real_dag3$clust)
# dosearch(real_do3$data, "p(y|do(x))", real_do3$graph) takes a few minutes

# Let's test out the timeout of dosearch with the huge graph
# Expecting dosearch to stop after 15 minutes. 
# Sys.time()
# dosearch("p(x, y, z1, z2, z3, z4, z6, z7, z8, z9, z10, z11, z13, z14)
#          p(y | x, z1, z2, z3, z4, z6, z7, z8, z9, z10, z11, z13, z14)", "p(y|do(x))", do2$graph, control = list(time_limit = 0.005))
# Sys.time()
# Returns not identifiable, even though it is. Not what we want?

# Let's try R's own time limit

# setTimeLimit(elapsed = 600)
# result <- tryCatch({
#   dosearch("p(x, y, z1, z2, z3, z4, z6, z7, z8, z9, z10, z11, z13, z14)
#          p(y | x, z1, z2, z3, z4, z6, z7, z8, z9, z10, z11, z13, z14)", "p(y|do(x))", do2$graph)
#     }, error = function(e) {
#       message("Stopped after reaching time limit.")
#       NULL
#     })
# setTimeLimit()
