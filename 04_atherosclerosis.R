library(dosearch)

# R code for Example of Section 7.2
# 7.2 Causal Model from an Atherosclerosis Study

# The clustered graph of Figure 10a

graph <- "
l -> h
l -> s
l -> y
b -> l
b -> s
b -> m
s -> h
s -> m
h -> m
m -> y
"

# 1) Data sources for the first illustration
data <- "
p(l, y, h, s, m | b)
"

# 2) Data sources for the second illustration (non-id)
data2 <- "
p(l, y, m, h, s, b)
"

query <- "p(y | do(l))"

# 1)
dosearch(data, query, graph, control = list(heuristic = TRUE))
# 2)
dosearch(data2, query, graph, control = list(heuristic = TRUE))

# 3) Not identified by do-calculus. Cluster M does not fulfill the 
# conditions of the transit cluster theorem because
# because H or L is not present in the first input distribution as is
# required to achieve the d-separation.
query3 <- "p(y | do(h))"
data5 <- "
p(b, m, s, y)
p(b, h, m, s)
"
dosearch(data5, query3, graph, control = list(heuristic = FALSE))

# 4) As the plain transit cluster theorem does not apply it is possible 
# that the query is identifiable in a graph where M is not clustered. 
# This indeed is the case. The query is identified in a graph where M = (M1, M2). 
# The graph where cluster M is opened, (Figure 10b).
graph2 <- "
l -> h
l -> s
l -> y
b -> l
b -> s
b -> m1
s -> h
s -> m1
h -> m1
m1 -> m2
m2 -> y
"
data5b <- "
p(b, m1, m2, s, y)
p(b, h, m1, m2, s)
"
dosearch(data5b, query3, graph2, control = list(heuristic = FALSE, draw_derivation = TRUE))

# Here p(y | do(h)) can be identified because M2 serves as a frontdoor.
