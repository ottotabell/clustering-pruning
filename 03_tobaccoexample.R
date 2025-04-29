library(dosearch)

# R Code for the demonstrations in Section 5.1
# Causal Model from an Infant Mortality Study

# 10.1001/jamapediatrics.2017.2536 
# w GDP unemployment
# h health expenditure
# a access to healthcare
# o tobacco control policies
# d smoking during pregnancy
# n congenital anomalies
# r tobacco prices
# c cigarette consumption
# s SHS in pregnancy
# b pre-term birth
# i infant mortality
# e education
# m maternal age
# g small for gestational age
# f social and cultural factors
# q ethnicity
# j SHS in infancy

# Original graph of Figure 5a
graph <- "
w -> h
w -> r
h -> a
a -> i
o -> r
o -> c
o -> s
o -> j
d -> i
d -> n
d -> b
d -> g
n -> i
r -> c
c -> s
c -> j
c -> d
s -> b
s -> g
b -> i
e -> a
e -> c
m -> c
m -> b
m -> n
m -> g
f -> r
f -> e
f -> m
q -> m
q -> e
q -> f
j -> i
g -> i
"

# Assume we have input distributions of p(r, c, o, e, m) and p(g, d, c, s | do(o)).

# (i) Causal effect p(s | do(r))

# Pruned graph for the first causal effect p(s | do(r)), Figure 5b.

query <- "p(s | do(r))"

graph_pruned2_s_do_r <- "
o -> r
o -> c
o -> s
r -> c
c -> s
e -> c
m -> c
f -> r
f -> e
f -> m
q -> e
q -> f
q -> m
"

# pruned distributions
data1 <- "
  p(s, c | do(o))
  p(r,c,o,e,m)
"

res1 <- dosearch(data1, query, graph, control = list(heuristic = FALSE))
# Error: The inputs imply a graph with more than 30 nodes.

res1pruned <- dosearch(data1, query, graph_pruned2_s_do_r, control = list(heuristic = FALSE))
# \sum_{c,e,m,o}\left(p(s|do(o),c)\left(p(e,m,o)p(c|r,e,m,o)\right)\right)

# Pruned graph for the second causal effect p(b | do(r)), Figure 5c.

query2 <- "p(b | do(r))"

graph_pruned_b_do_r <- "
o -> r
o -> c
o -> s
r -> c
c -> s
e -> c
m -> c
f -> r
f -> e
f -> m
q -> e
q -> f
q -> m
c -> d
d -> b
m -> b
s -> b
"

data2 <- "
p(c, o)
p(g, d, c, s, b| do(o))
"

print(res4pruned <- dosearch(data2, query2, graph_pruned_b_do_r, control = list(heuristic = TRUE)))
# Non-identifiable. Takes a few minutes.

# (iii) Causal effect p(g | do(c)), Figure 5d.

query3 <- "p(g | do(c))"

graph_pruned_g_do_c <- "
o -> c
o -> s
o -> r
r -> c
e -> c
m -> c
f -> r
f -> m
f -> e
q -> f
q -> m
q -> e
d -> g
c -> s
c -> d
s -> g
m -> c
m -> g
"

data3 <- "
p(c, o, r, e, m)
p(g, d, c, s | do(o))
"

print(res4pruned <- dosearch(data3, query3, graph_pruned_g_do_c, control = list(heuristic = FALSE)))
# \sum_{s,d,o}\left(\left(p(o)p(s,d|do(o),c)\right)\sum_{c}\left(p(c|do(o))p(g|do(o),c,s,d)\right)\right) 
