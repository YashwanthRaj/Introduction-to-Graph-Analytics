#install.packages("igraph")
library(igraph)

# Set working directory to a specific path
setwd("/Users/yashwanth/Documents/GWU/Sem 2/Intro to Big Data/Project/Project 1/R Project")

# Load the data into a data frame
opinions <- read.table("soc-Epinions1_adj(1).tsv")

# Converting the data to matrix
optab <-as.matrix(opinions)

# Selecting the number of elements and storing in a vector v1 and v2
v1 <- optab[1:811480,1]
v2 <-optab[1:811480,2]

# Datafarme with only V1 and V2
relations<- data.frame(from=v1,to=v2)

# Plotting
g<-graph.data.frame(relations,directed=TRUE)  
plot(g,vertex.size=5, edge.arrow.size=0.1)


# SIMPLIFYING THE GRAPH

# Finding the vertex names
MyG <- igraph::V(g)
MyG

# Nodes with degree 1
MyG1 <- igraph::V(g)[igraph::degree(g)<1]
MyG1

# Nodes with degree 2
MyG2 <- igraph::V(g)[igraph::degree(g)<2]
MyG2

# Nodes with degree 10
MyG10 <- igraph::V(g)[igraph::degree(g)<10]
MyG10

# Removing vertices with less than 10 edges
newG <- igraph::delete.vertices(g,MyG10)
newG
plot(newG) # Plotting 

# selecting vertices with 0 degree
newG0 <- igraph::V(newG)[igraph::degree(newG)<1]
newG0
plot(newG0)

# Removing vertices with degree 0
newG2 <- igraph::delete.vertices(newG,newG0)
newG2
plot(newG2)

# This makes no significant difference. So lets try removing 30
# selecting vertices with degree les than 30
newG30 <- igraph::V(newG)[igraph::degree(newG)<30]
newG30

# Removing vertices with degree less than 30
newG30 <- igraph::delete.vertices(newG,newG30)
newG30
plot(newG30)

# selecting vertices with degree les than 70
newG70 <- igraph::V(newG)[igraph::degree(newG)<70]
newG70

# Removing vertices with degree less than 80
newG70 <- igraph::delete.vertices(newG,newG70)
newG70
plot(newG70)

# QUESTION 3

# Determining the. vertices of a graph
V(g)

# Determining the edges of a graph
E(g)

# Adjacency functions
g.adj = igraph::get.adjacency(g)
g.adj

#v Density of graph
g.density <- graph.density(g)
g.density

plot(wc, g, vertex.size=5, vertex.label.cex=0.2, edge.arrow.size=0.1,layout=layout.fruchterman.reingold)

# Question 5

# Alpha Centrality
acsg <- alpha.centrality(newG70)
sort(acsg,decreasing = TRUE)

# Central Node
central_node <- which.max(degree(g, mode = "all"))
central_node

bet <- betweenness(g)
central_node <- which.max(bet)
central_node

# Longest Path
sg = induced.subgraph(newG70, which(components (newG70) $membership == 1))
V(sg)$degree = degree(sg)
result = dfs(sg, root = 1, dist = TRUE)$dist
sort(result, decreasing = TRUE)

# Largest Clique
largest_cliques(g)

# Ego
ego.graph <- igraph::ego(g)
ego.graph

# Power Centrality
pc <- power_centrality(sg, exponent = 0.8)
sort(pc,decreasing = TRUE)

# Shortest Path between two nodes
g.sp = igraph::shortest.paths(newG70) 
g.sp
