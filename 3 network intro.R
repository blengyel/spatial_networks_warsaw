# Community Detection and Network Visualization with R

rm(list=ls())

# 0. Set up working directory

setwd("...")

# 0. Install and call in package

install.packages("igraph") # In case you have not installed the package yet. 
#It is worth to repeat installation on a -say- yearly basis. Igraph R syntax is changing frequently.

install.packages("sand")

library(igraph) # Call this package in case you work with network objects.
library(sand) # Sand is important for teaching purposes only: it contains built-in data.


# 1. NETWORK DATA

# Undirected network
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
                   4-7, 5-6, 6-7) # We manually create a new network object in igraph by adding the edges

V(g) # We call the vertex sequence of g.

E(g) # We call the edge sequence of g.

plot(g) # Simple plot for simple networks.

M=get.adjacency(g) # to see how storing data in matrices would look like

M

# Delete vertex
g = delete.vertices(g, 6)
V(g)[6]

plot(g) # Simple plot for simple networks.

g = delete.vertices(g, 6)
V(g)

#???  g=add_vertices(8)

# Directed network
dg <- graph.formula(1-+2, 1+-3, 2++3) # We create the directed network object dg.
plot(dg)

V(dg)$name<- c("Sam", "Mary", "Tom") # We add name attributes to the vertex sequence.
plot(dg)

dg = delete.vertices(dg, "Sam")

plot(dg)


# Weighted network 
set.seed(42) # Seed is set so that randomization in the next line always gives the same result.
E(g)$weight <- runif(ecount(g)) # runif: we set edge weight to be a random number from the uniform [0,1] distribution. 
# ecount: the number of edges in g

is.weighted(g)
plot(g, edge.width=E(g)$weight*10)

E(g)$weight[3]


# Simplify networks
is.simple(g) # Simple networks do not contain self-loops and redundant edges.
# g is undirected: redundant edge could be 1-2 2-1

mg <- g + edge(2,3) # We create a non-simple network by adding a redundant edge.
E(g)
E(mg)

is.simple(mg)

E(mg)$weight <- 1 # The non-simple network is given a uniform edge weight.
wg2 <- simplify(mg) # unify redundant edges / in this case, summing their weights
is.simple(wg2)

E(wg2)
E(wg2)$weight

?simplify


# 2. Network measures: Density, Transitivity, Average Path Length
data(karate) # very famous data collected from the Zachary karate club
plot(karate)

graph.density(karate) # 0.1390374
transitivity(karate) ## [1] 0.2556818

transitivity(karate, "local", vids=c(1,34))  ## [1] 0.1500000 0.1102941
average.path.length(karate)
diameter(karate)


# 3. Degree distribution
data(karate) # very famous data collected from the Zachary karate club
plot(karate)
is.weighted(karate)
hist(degree(karate), col="lightblue", xlim=c(0,20),
     xlab="Vertex Degree", ylab="Frequency", main="") # Degree distribution with histogram

hist(graph.strength(karate), col="pink",
     xlab="Vertex Strength", ylab="Frequency", main="") # Weighted degree is also called "Strenght" of the vertex


library(igraphdata)
data(yeast)

E(yeast)

ecount(yeast)
vcount(yeast)

is.weighted(yeast)
is.directed(yeast)

is.simple(yeast)

d.yeast <- degree(yeast) # degree gives back a vector of same length as V
hist(d.yeast,col="blue",
     xlab="Degree", ylab="Frequency",
     main="Degree Distribution")

dd.yeast <- degree.distribution(yeast) # degree.distribution calculates relative frequency of a given degree
# result is a numeric vector of the same length as maximum degree plus one (which is zero degree)
d <- 1:max(d.yeast)-1                 # a sequence that goes from 1 to the maximum degree
ind <- (dd.yeast != 0)                # create an index for dropping the zero from degree distribution
plot(d[ind], dd.yeast[ind], log="xy", col="blue",
     xlab=c("Log Degree"), ylab=c("Log Probability"),
     main="Log-Log Degree Distribution") # Plot degree probability on a log-log scale
abline(a=-0.5, b=-1.1)

# 4. Node Centralities
V(karate)$degree=degree(karate) # node characteristics can be added to the nodelist in the network object / useful to store

V(karate)$name
V(karate)["Actor 32"]$degree #WHY NOT WORKING????
V(karate)$degree[32] #WHY NOT WORKING????
V(karate)$degree[which(V(karate)$name=="Actor 32")] #WHY NOT WORKING????

V(karate)$betweenness=betweenness(karate, normalized = T) # normalize when comparing different networks
V(karate)$closeness=closeness(karate, vids = V(karate))

?betweenness
?closeness
?degree


# 5. Edge betweenness and Coreness / See motivation in the Lecture example!

eb <- edge.betweenness(karate)
E(karate)$edge_b= edge.betweenness(karate) # edge characteristics can be added to the edgelist in the network object
E(karate)[order(eb, decreasing=T)[1:3]]

plot(karate, edge.width=edge.betweenness(karate)/15, 
     vertex.size=degree(karate)*3)

cores <- graph.coreness(karate) # calculatees k-coreness. 
#The k-core of graph is a maximal subgraph in which each vertex has at least degree k

set.seed(42)
plot(karate, edge.width=edge.betweenness(karate)/15, vertex.size=graph.coreness(karate)*5)


# 6. Network measures: Density, Transitivity, Average Path Length

graph.density(karate) # observed number of links / potential number of links

transitivity(karate)  # observed number of triangles/ potential number of triangles

transitivity(karate, "local", vids=c(1,34))  ##

?transitivity

degree(karate)

average.path.length(yeast)
diameter(yeast)


# 7. Project and plot bipartite networks

kevin=read.table("kevin_bacon.csv", header=T, sep=",") # read in movie-actor data in a data.frame format

kevin_edgelist=merge(kevin, kevin, by="movie", all=T) # create co-occurrence edgelist by merging data to itself
names(kevin_edgelist)=c("movie", "actor1", "actor2") # give names to the columns

View(kevin_edgelist)

kevin_edgelist=kevin_edgelist[which(kevin_edgelist$actor1!=kevin_edgelist$actor2),c(2:3)] 
# keep only those edges where actors are not identical
# drop the column of movie names

kevin_net=graph_from_data_frame(kevin_edgelist, directed = F, vertices = NULL) # create undirected projection of the network
kevin_net=simplify(kevin_net)  

plot(kevin_net) # simplest plot

# Create a better plot
coords <-layout_on_grid(kevin_net, width = 0, height = 0, dim = 2) # determine the position of nodes
plot(kevin_net, 
     layout=coords)

plot(kevin_net, 
     layout=layout.fruchterman.reingold,
     vertex.label=NA,
     vertex.size=degree(kevin_net)*1.5,
     vertex.color=heat.colors(3, rev=T)[graph.coreness(kevin_net)],
     edge.width=edge.betweenness(kevin_net, directed=F)*0.02)


# 8. Community structure

kc <- fastgreedy.community(karate) # running the fast greedy algorithm to identify communities

# kc is a community structure object containing the following elements:

length(kc) # the number of communities
sizes(kc) # size distribution of community structure
membership(kc) # node characteristics 

modularity(kc) # calculates the Q index of modularity

plot(kc,karate) # simple plot of communities

layout=layout.fruchterman.reingold(karate)

plot(karate, 
     layout=layout,
#     vertex.label=NA,
     vertex.size=20,
     vertex.color=heat.colors(length(kc))[membership(kc)],
     edge.width=2)

mycolors=c("deeppink", # community 1
           "cyan", # community 2
           "magenta") # community 3

plot(karate, 
     layout=layout.fruchterman.reingold,
     #     vertex.label=NA,
     vertex.size=20,
     vertex.color=mycolors[membership(kc)],
     edge.width=2)

# Community finding algorithms and their running times: 
# https://www.r-bloggers.com/summary-of-community-detection-algorithms-in-igraph-0-6/


# 9. Exercise: the global airline network

# Read, clean the data and plot the network
air_nodes=read.table("airlines_nodes.csv", header=T, sep=";")
  air_nodes$lng=as.numeric(air_nodes$lng)
  air_nodes=air_nodes[!is.na(air_nodes$lng),]
air_edges=read.table("airlines_edges.csv", header=T, sep=";")

air_g=graph_from_data_frame(air_edges, vertices=air_nodes, directed=F) # network object from edgelist and nodelist
  air_g=simplify(air_g) # remove self-loops and multiple edges

V(air_g)$lng # vertices of g
E(air_g) # edges of g
  
coordinates<-matrix(c(V(air_g)$lng, V(air_g)$lat),nrow=length(V(air_g)$lat),ncol=2) # coordinate matrix to locate nodes in geographical networks

# plot the network into a PNG file
png(filename="air_net.png", width=6000, height=2000, units="px")
plot.igraph(air_g,
     layout=coordinates, 
     # vertex.color=V(air_g)$col, 
     # vertex.frame.color=V(air_g)$fc, 
     vertex.size=0.5,
     edge.color="gray50", edge.curved=F,
     edge.width=1,
     axes=F, frame=F, rescale=T,
     vertex.label=NA
)
dev.off()

# Color network after community finding algorithm 
#install.packages("viridis")
library(viridis)

set.seed(1024)
air_c=cluster_louvain(air_g) # the Louvain algorithm is a very popular method that is fast on large graphs
  length(air_c)
  length(V(air_g))
  length(air_c[sizes(air_c)>10])

c=as.data.frame(sizes(air_c)) # create community table
  names(c)=c("comm", "size")

v=data.frame(as.integer(V(air_g)$name), as.vector(membership(air_c))) # membership table
  names(v)=c("Id", "comm")

col=c[c$size>10,] # colors into community and membership tables
  #col$col=magma(length(col$comm))
  col$col=c(magma(length(air_c[sizes(air_c)>10])-11),
            "olivedrab1", "navajowhite3", "khaki2", "darkblue",
            "orange","cyan", "deeppink", "red",
            "green4", "brown", "dodgerblue")
  col=col[,c("comm", "col")]
  c=merge(c,col, by="comm", all.x=T)
  v=merge(v,c, by="comm", all.x=T)

air_nodes_new=merge(air_nodes,v, by="Id", all.x=T) # colors into nodes table

# regenerate network and coordinates
air_g=graph_from_data_frame(air_edges, vertices=air_nodes_new, directed=F) # network object from edgelist and nodelist
coordinates<-matrix(c(V(air_g)$lng, V(air_g)$lat),nrow=length(V(air_g)$lat),ncol=2) # coordinate matrix to locate nodes in geographical networks
  
  
# plot the network with the chosen colors of communities
png(filename="air_comm.png", width=6000, height=2000, units="px")
plot(air_g,
     layout=coordinates, 
     vertex.color=V(air_g)$col,
     vertex.frame.color=V(air_g)$col, 
     vertex.size=1,
     edge.color="grey",
     edge.width=0.1,edge.curved=F,
     axes=F, frame=F, rescale=T,
     vertex.label=NA
)
dev.off()

# 10. Evaluate the community finding algorithm

set.seed(24)
c1=cluster_louvain(air_g, resolution = 2)
c2=cluster_louvain(air_g, resolution = 2)

compare(membership(c1), membership(c2), method="nmi")
