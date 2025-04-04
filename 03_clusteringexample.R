# R code for Example 5.2
# 5.2 Causal Model from an Atherosclerosis Study

# The clustered graph of Figure 6a
library(dosearch)
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
p(l, y | b)
p(b)
"

# 2) Data sources for the second illustration (non-id)
 data2 <- "
 p(l, y, m)
"


# 3) Data sources for the second illustration
data3 <- "
p(l, y, m)
p(m | do(l))
p(y | do(m))
"

query <- "p(y | do(l))"

# 1)
dosearch(data, query, graph, control = list(heuristic = TRUE))
# 2)
dosearch(data2, query, graph, control = list(heuristic = TRUE))
# 3)
dosearch(data3, query, graph, control = list(heuristic = TRUE))

# 4) Not identified by do-calculus. The plain transit theorem applies 
# with respect to clusters B, H, M and l. Assuming that 
# the query is not identifiable by other means than do-calculus,
# we conclude that the query is not identifiable regardless of
# the internal structure of B, H, M and l.
query2 <- "p(y | do(s))"
data4 <- "
p(b, l, y)
p(b, h, m, s, l)
"
dosearch(data4, "p(y | do(s))", graph, control = list(heuristic = FALSE))

query3 <- "p(y | do(h))"
# 5) Not identified by do-calculus. Cluster M does not fulfill the 
# conditions of the plain transit cluster theorem because
# because H is not present in the first input distribution as required by
# condition (b).
data5 <- "
p(b, m, s, y)
p(b, h, m, s)
"
dosearch(data5, query3, graph, control = list(heuristic = FALSE))
# The reason why p(y | do(h)) is not identifiable is the backdoor path
# via l that is not present in the input distributions.

# As the plain transit cluster theorem does not apply it is possible 
# that the query is identifiable in a graph where M is not clustered. 
# This indeed is the case. The query is identified in a graph where M = (M1, M2). 
# The graph where cluster M is opened, Figure 6b).
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
out <- dosearch(data5b, query3, graph2, control = list(heuristic = FALSE, draw_derivation = TRUE))
# \sum_{s,m2,b}\left(\sum_{m1}\left(p(s,b)p(m1,m2|h,s,b)\right)\sum_{m1}\left(p(m1|s,b)p(y|s,m1,m2,b)\right)\right) 

# Here p(y | do(h)) can be identified because M2 serves as a frontdoor.

