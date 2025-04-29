# R code for Example of Figure 4

library(dosearch)

graph <- "
x -> y
q1 -> r
r -> s1
r -> s2
s1 -> s2
s1 -> e1
s2 -> e1
s2 -> e2
e1 -> x
e1 -> y  
e2 -> x
e2 -> y
t1 -> x
t1 -> y  
t2 -> x
t2 -> y
q1 -> x
q2 -> y
q2 -> t1
q2 -> t2
"

graph_clust <- "
x -> y
q1 -> s
q1 -> x
s -> x
s -> y
t -> x
t -> y
q2 -> y
q2 -> t
"

data <- "
p(y, x, e1, e2, q)
p(y, x|t1, t2)
"

data_clust <-"
p(y, x, s, q)
p(y, x|t)
"

query <- "p(y|do(x))"

dosearch(data, query, graph, control = list(heuristic = FALSE))
dosearch(data_clust, query, graph_clust, control = list(heuristic = FALSE))

####################################################

# Examples showing the necessity of conditions (a) and (b) in Theorem 15
  
# Condition (a)
  
  data <- "
  p(x, z1, z2)
  p(y, z1, z2)
"

# Figure 8a
graph <- "
  w -> x
  w -> y
  x -> z1
  z1 -> z2
  z2 -> y
"

data_clust <- "
  p(x, z)
  p(y, z)
"

# Figure 8b
graph_clust <- "
  w -> x
  w -> y
  x -> z
  z -> y
"

query <- "p(y|do(x))"

# The causal effect is identifiable in the original graph
# but not in the clustered graph.
# Condition a is violated in input distributions.
dosearch(data, query, graph)
dosearch(data_clust, query, graph_clust)

# Condition (b)

data <- "
  p(y|z1,z2)
  p(x,z1,z2)
"

data_clust <- "
  p(y|z)
  p(x,z)
"

# The causal effect is identifiable in the original graph
# but not in the clustered graph.
# Condition b is violated in input distributions.
dosearch(data, query, graph)
dosearch(data_clust, query, graph_clust)