# Goal: look for some example family network structures to show Jeff.
# Look for ~4 different ones. One or two can be simple ones with, e.g.,
# two parents or a single parent and a child. Two should be more complex.
# Make some slides with one network image per slide with a short caption/
# description; a title slide; and a "next steps" slide (I can fill that one in)

# 1. Load Data and Packages ====================================================

load("~/sdal/projects/vdss/vdss/data/nodes-and-edges.Rdata")
library(tidyverse)
library(network)
# Now the datasets "nodes" and "edges" are available.

# 2. Extract Individual Families ===============================================

# The datasets we have contain information on *all* families. We just want to
# show networks of individual families. The following functions extract node
# and edge data for individual families.

# In the function arguments, "edges" and "nodes" are the edge and node datasets.
# "ID" is the *case* ID.

get_edge = function(edges, ID) {
  edges %>%
    filter(caseID == ID) %>%
    filter(!is.na(relationship)) %>%
    select(c(UID1, UID2, relationship))
}

get_node = function(nodes, ID) {
  nodes %>%
    filter(caseID == ID) %>%
    select(c(UID, NIS_role))
}

# 3. Investigate networks ======================================================

# a) extract individual family -------------------------------------------------
## PARENT-CHILD RELATIONSHIP

test_edges = get_edge(edges, 120)
test_nodes = get_node(nodes, 120)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                   matrix.type = "edgelist", ignore.eval = FALSE,
                   directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)


# Further Notes ================================================================

# More details on netowrk package here:
#  https://cran.r-project.org/web/packages/network/vignettes/networkVignette.pdf

# Feel free to experiment with the arguments to plot() to try to make it look
# nicer. For example, can change edge text size with edge.label.cex; edge
# text color with edge.label.col; node size with vertex.cex. You can read the
# documentation for other options.

# To look for families with different numbers of "nodes" (family members), try:

table(nodes$caseID)

# The number under each case ID is the number of family members. Entering that
# number as the "ID" argument in the functions above will show the network


## NEW RELATIONSHIP: Two Parents, not married, with child ##

# a) extract individual family -------------------------------------------------

test_edges = get_edge(edges, 122)
test_nodes = get_node(nodes, 122)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE,
                       directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)




## NEW RELATIONSHIP: Child with two married caregivers, not parents of child but "Other Family Member/Relative"##
# a) extract individual family -------------------------------------------------

test_edges = get_edge(edges, 123)
test_nodes = get_node(nodes, 123)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE,
                       directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)




## NEW RELATIONSHIP: PA & PB are Parents to Child, but the relationship between PB & PA is"Other non-relative ##
# a) extract individual family -------------------------------------------------

test_edges = get_edge(edges, 124)
test_nodes = get_node(nodes, 124)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE,
                       directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)




## NEW RELATIONSHIP: Child with two parents, married##
# a) extract individual family -------------------------------------------------

test_edges = get_edge(edges, 125)
test_nodes = get_node(nodes, 125)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE,
                       directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)





## NEW RELATIONSHIP: Child with PB as Grandparent, PA as Parent, and PB as parent of PA##
# a) extract individual family -------------------------------------------------

test_edges = get_edge(edges, 59)
test_nodes = get_node(nodes, 59)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE,
                       directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)


## NEW RELATIONSHIP: 4 People: PA: Parent, PB: Step Parent, OIP1: Parent, PA & PB: spouse, Child##
# a) extract individual family -------------------------------------------------

test_edges = get_edge(edges, 207)
test_nodes = get_node(nodes, 207)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE,
                       directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)




## NEW RELATIONSHIP: 4 People: PA: Parent, PB: Step Parent, OIP1: Parent, PA & PB: spouse, Child##
# a) extract individual family -------------------------------------------------

test_edges = get_edge(edges, 53)
test_nodes = get_node(nodes, 53)

# b) make network object -------------------------------------------------------

test_network = network(test_edges[,1:2], vertex.attr = test_nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE,
                       directed = FALSE)

# c) assign some attributres to edges/nodes (so we can see labels)

network.vertex.names(test_network) = test_nodes$NIS_role

# d) plot network --------------------------------------------------------------

plot(test_network, vertex.cex = 3, edge.label = test_edges[,3],
     edge.label.cex = 0.7, edge.label.col = "blue", displaylabels = TRUE)