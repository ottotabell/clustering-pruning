# R codes for Section 4

# Example of Figure 4

library(dosearch)

# Original graph

graph <- "
x -> r
r -> s1
r -> s2
s1 -> s2
s1 -> e1
s2 -> e1
s2 -> e2
e1 -> w1  
e2 -> w1
e1 -> w2  
e2 -> w2
w1 -> w2
w2 -> y
t1 -> x
t2 -> x
t1 -> w1
t2 -> w2
"

# Clustered graph

graph_clust <- "
x -> s
s -> w1
w1 -> w2
w2 -> y
s -> w2
t -> x
t -> w1
"

# Case (i)

query <- "p(y|do(x))"

data1 <- "
p(x, e1, e2, r, w2)
p(y, w2| do(w1))
"

data_clust1 <-"
p(x, s, w2)
p(y, w2 | do(w1))
"

dosearch(data1, query, graph, control = list(heuristic = FALSE))
dosearch(data_clust1, query, graph_clust, control = list(heuristic = FALSE))

# Case (ii)

data2 <- "
p(e1, e2, x)
p(y| do(t1, t2), e1, e2, s2)
p(x, t1, t2)
"

data_clust2 <-"
p(s, x)
p(y | do(t), s)
p(x, t)
"

dosearch(data2, query, graph, control = list(heuristic = FALSE))
dosearch(data_clust2, query, graph_clust, control = list(heuristic = FALSE))

# Case (iii)

data3 <- "
p(y, t1, t2, r, e1, e2)
p(x, w1| do(e1, e2))
p(x, w1)
"

data_clust3 <- "
p(y, t, s)
p(x, w1 | do(s))
p(x, w1)
"

dosearch(data3, query, graph, control = list(heuristic = FALSE))
dosearch(data_clust3, query, graph_clust, control = list(heuristic = FALSE))


####################################################

# Examples showing the necessity of conditions (a) and (b) in Theorem 15
  
# Condition (a)
  
  data <- "
  p(x, z1, z2)
  p(y, z1, z2)
"

# Figure 9a
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

# Figure 9b
graph_clust <- "
  w -> x
  w -> y
  x -> z
  z -> y
"

query <- "p(y|do(x))"

# The causal effect is identifiable in the original graph
# but not in the clustered graph.
# Line 7 of the algorithm is violated in input distributions.
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
# Line 9 of the algorithm is violated in input distributions.
dosearch(data, query, graph)
dosearch(data_clust, query, graph_clust)
