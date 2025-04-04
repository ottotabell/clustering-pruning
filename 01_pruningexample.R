# R code for the pruning example in Section 3

# The original graph G

library(dosearch)
graph <- "
z2 -> z3
z2 -> z1
z3 -> z4
z3 -> x1
z1 -> x1
z1 -> x2
x1 -> z4
x1 -> w1
x2 -> w1
u1 -> x1
u2 -> x1
u1 -> w1
u2 -> y
u3 -> x1
u3 -> x2
w1 -> z5
w1 -> y
z5 -> z4
w2 -> x
w2 -> w1
w2 -> y
z6 -> w2
z7 -> z6
"

# The pruned graph G'

graph2 <- "
x1 -> w1
x2 -> w1
u1 -> x1
u1 -> y
u2 -> w1
u2 -> x1
u3 -> x1
u3 -> x2
w1 -> y
w2 -> x1
w2 -> w1
w2 -> y
"

# Original data sources
data <- "
p(w1, z1, z2, z5 | do(x1,x2), w2)
p(y, z3, z4, z5 | do(w1), w2)
p(w2, z6, z7)
"

# The pruned data sources
data2 <- "
p(w1| do(x1,x2), w2)
p(y | do(w1), w2)
p(w2)
"

query <- "p(y|do(x1,x2))"

dosearch(data, query, graph, control = list(heuristic = FALSE))
# Error: The inputs imply a graph with more than 30 nodes.

# With the pruned graph, do-search allows us to derive the identifiability
dosearch(data2, query, graph2, control = list(heuristic = FALSE))

# The query is not identifiable with only two input distributions
data2a <- "
p(y | do(w1), w2)
p(w2)
"
data2b <- "
p(w1| do(x1,x2), w2)
p(w2)
"
data2c <- "
p(w1| do(x1,x2), w2)
p(y | do(w1), w2)
"
dosearch(data2a, query, graph2, control = list(heuristic = FALSE))
dosearch(data2b, query, graph2, control = list(heuristic = FALSE))
dosearch(data2c, query, graph2, control = list(heuristic = FALSE))