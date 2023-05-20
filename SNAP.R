# SNAP Project Code

# -------------------------------------------------------------------------------------------------
# SETUP
# -------------------------------------------------------------------------------------------------

rm(list=ls())

# Install libraries
if (!"statnet" %in% installed.packages()) install.packages("statnet") # For fitting ERGMs
if (!"igraph" %in% installed.packages()) install.packages("igraph") # For network plotting
if (!"tidygraph" %in% installed.packages()) install.packages("tidygraph") # For network plotting

# Set the working directory
# Session > Set Working Directory > To Source File Location
list.files() # List the files in the current working directory to see if you're in the right directory

# -------------------------------------------------------------------------------------------------
# VISUALIZE THE NETWORK
# -------------------------------------------------------------------------------------------------

# Generate airport network
# Directed, but unweighted showing all connected airports
network_ties = read.csv("airportNetwork.csv")

# Visualize network
library('igraph')
library('tidygraph')

# Get the giant component of the graph (there's one isolated dyad)
network_graph <- graph_from_data_frame(network_ties) |>
  as_tbl_graph()
network_comp <- components(network_graph)
network_graph <- network_graph %>% 
  induced.subgraph(., which(network_comp$membership == which.max(network_comp$csize)))
vcount(network_graph) ## the number of nodes/actors/users
ecount(network_graph)

# Set default plot options
igraph_options(vertex.size = 9, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)

# The plot isn't that useful because there are so many vertices...
# I tried playing around with different layouts but they're all kinda just a blob
plot(network_graph, layout = layout_with_lgl(network_graph))


# -------------------------------------------------------------------------------------------------
# LOCAL CENTRALITY MEASURES
# -------------------------------------------------------------------------------------------------

sna <- igraph::get.adjacency(network_graph, sparse=FALSE) %>% network::as.network.matrix()

detach('package:igraph')
library(statnet)

idegScores_network <- degree(sna, cmode = 'indegree')
# Store the information
centralities <- data.frame('node_name' = as.character(network.vertex.names(sna)),
                           # 'airport_code' = igraph::vertex_attr(sna, "code"),
                           'in_degree' = degree(sna, cmode = 'indegree'))
# Calculate out-degree centrality and store it in the data.frame called 'centralities'
centralities$out_degree <- degree(sna, cmode = 'outdegree')

# Calculate betweenness centrality and store it in the data.frame called 'centralities'
centralities$betweenness <- betweenness(sna)

# Calculate closeness centrality and store it in the data.frame called 'centralities'
centralities$incloseness <- igraph::closeness(network_graph, mode = 'in')
centralities$outcloseness <- igraph::closeness(network_graph, mode = 'out')

# Calculate eigenvector centrality and store it in the data.frame called 'centralities'
# using 'igraph' because the code implemented in 'sna' is unreliable
centralities$eigen <- igraph::eigen_centrality(network_graph)$vector

# Calculate Burt's network constraint and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
centralities$netconstraint <- igraph::constraint(network_graph)
help(constraint) # Be careful with the interpretation for constraint:
# High constraint = redundant contacts, low constraint = acting as a broker

# Calculate authority and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
# 'igraph::' allows calling for any igraph function without loading the package
centralities$authority <- igraph::authority_score(network_graph, scale = TRUE)$`vector`

# Calculate hub and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
centralities$hub <- igraph::hub_score(network_graph, scale = TRUE)$`vector`

View(centralities)

save.image("500centralities.RData")

# -------------------------------------------------------------------------------------------------
# GLOBAL PROPERTIES
# -------------------------------------------------------------------------------------------------

detach('package:statnet', unload = TRUE)
library(igraph)

## calculate k-cores
kcore <- network_graph %>% graph.coreness(.); kcore ## show the results of k-core decomposition

## Plot a graph colored by the k-core decomposition results
network_graph %>% 
  plot(.,
       layout = layout_with_gem(.),
       # layout = layout_with_sugiyama(.),
       edge.arrow.size = .3,
       vertex.size = 4,
       vertex.label = NA,
       vertex.color = adjustcolor(graph.coreness(.), alpha.f = .3),
       vertex.label.cex = .5,
       vertex.label.color = 'black',
       mark.groups = by(seq_along(graph.coreness(.)), graph.coreness(.), invisible),
       mark.shape = 1/4,
       mark.col = rainbow(length(unique(graph.coreness(.))),alpha = .1),
       mark.border = NA
  )


# Plot the number of clusters in the graph and their size
# there are also other algorithms for this you may want to explore
# below is using Newman-Girvan Algorithm (2003)
# if communities do not make sense to you, replace with your choice
# e.g., cluster_infomap, cluster_walktrap etc.
cluster <- network_graph %>% cluster_edge_betweenness()
# ## you may see orange warning messages since the edge betweennness algorithm is not designed for a directed graph
# ## but you'll be able to see the results anyway.
# ## if you want to use a more appropriate algorithm for a directed graph, try:
cluster <- network_graph %>% cluster_walktrap()
cluster # idk why this gives an error, but the analysis stuff below works

# modularity measure
modularity(cluster)

# Find the number of clusters
membership(cluster)   # affiliation list
length(cluster) # number of clusters

# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster) 

# Visualize clusters - that puts colored blobs around the nodes in the same community.
# You may want to remove vertex.label=NA to figure out what terms are clustered.
cluster %>% plot(.,network_graph,
                     # layout = layout_with_gem(.),
                     layout = layout_with_fr(network_graph),
                     edge.arrow.size = .3,
                     vertex.size = 4,
                     vertex.label = NA,
                     vertex.color = adjustcolor(membership(.), alpha.f = .3),
                     vertex.label.cex = .5,
                     vertex.label.color = 'black',
                     mark.groups = by(seq_along(membership(.)), membership(.), invisible),
                     mark.shape = 1/4,
                     mark.col = rainbow(length(.),alpha = .1),
                     mark.border = NA
)

# Examine the in-degree distribution
network_graph %>% degree.distribution(.,mode="in") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'In-degree Distribution',
       ylab = 'Density',
       xlab = 'In-degree')

# CCDF - Complementary Cumulative Distribution Function
# Plot a log-log plot of in-degree distribution
network_graph %>% 
  degree.distribution(.,cumulative = TRUE,mode ='in') %>% 
  plot(1:(max(degree(network_graph,mode='in'))+1),., ## since log doesn't take 0, add 1 to every degree
       log='xy', type = 'l',
       main = 'Log-Log Plot of In-degree',
       ylab = 'CCDF',
       xlab = 'In-degree')

# Fit a power law to the degree distribution
# The output of the power.law.fit() function tells us what the exponent of the power law is ($alpha)
# and the log-likelihood of the parameters used to fit the power law distribution ($logLik)
# Also, it performs a Kolmogov-Smirnov test to test whether the given degree distribution could have
# been drawn from the fitted power law distribution.
# The function thus gives us the test statistic ($KS.stat) and p-vaule ($KS.p) for that test
in_power <- network_graph %>% 
  degree.distribution(., mode='in') %>%
  power.law.fit(., xmin=0.00000001)
in_power

# Examine the out-degree distribution
network_graph %>% degree.distribution(.,mode="out") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'Out-degree Distribution',
       ylab = 'Density',
       xlab = 'Out-degree')

# Plot a log-log plot
network_graph %>% 
  degree.distribution(.,cumulative = TRUE,mode ='out') %>% 
  plot(1:(max(degree(network_graph,mode='out'))+1), ## since log doesn't take 0, add 1 to every degree
       ., log='xy', type = 'l',
       main = 'Log-Log Plot of Out-degree',
       ylab = 'CCDF',
       xlab = 'Out-degree')

# Fit a power law to the degree distribution
out_power <- network_graph %>% 
  degree.distribution(., mode='out') %>%
  power.law.fit(.)
out_power

# Small-world Characteristics
ntrials <- 1000 ## set a value for the repetition
cl.rg <- numeric(ntrials) ## create an estimated value holder for clustering coefficient
apl.rg <- numeric(ntrials) ## create an estimated value holder for average path length
for (i in (1:ntrials)) {
  g.rg <- rewire(network_graph, keeping_degseq(niter = 100))
  cl.rg[i] <- transitivity(g.rg, type = 'average')
  apl.rg[i] <- average.path.length(g.rg)
}

# plot a histogram of simulated values for clustering coefficient + the observed value
hist(cl.rg,
     main = 'Histogram of Clustering Coefficient',
     xlab = 'Clustering Coefficient')
par(xpd = FALSE)

# the line indicates the mean value of clustering coefficient for your network
abline(v = network_graph %>% transitivity(., type = 'average'), col = 'red', lty = 2)

# this tests whether the observed value is statistically different from the simulated distribution
t.test(cl.rg, mu=network_graph %>% transitivity(., type = 'average'),
       alternative = 'less') ## pick either 'less' or 'greater' based on your results
## (you want to use the one that generates the smaller p-value)

# plot a histogram of simulated values for average path length + the observed value
hist(apl.rg,
     main = 'Histogram of Average Path Length',
     xlab = 'Average Path Length')
# the line indicates the mean value of average path length for your network
abline(v = network_graph %>% average.path.length(), col = 'red', lty = 2)
# this tests whether the observed value is statistically different from the simulated distribution
t.test(apl.rg, mu=network_graph %>% average.path.length(.),
       alternative = 'less') ## pick either 'less' or 'greater' based on your results
## (you want to use the one that generates the smaller p-value)

# -------------------------------------------------------------------------------------------------
# ERGM STUFF - not working rn
# -------------------------------------------------------------------------------------------------


# library(statnet)
# 
# # Generate network 
# passengersEdgelist <- read.csv("passengersEdgelist.csv")
# head(passengersEdgelist) # Check the first five rows of the edgelist. Third column is the number of passengers
# passengers <- matrix(nrow = 100, ncol = 100) # Number of Direct messages sent from i to j
# for (i in 1:nrow(passengersEdgelist)) { # Read from edgelist
#   passengers[passengersEdgelist$origin[i], passengersEdgelist$destination[i] ] <- as.numeric(passengersEdgelist$passengers[i])
# }
# for (i in 1:100) { # Remove self-ties (flights to self?)
#   passengers[i,i] <- as.numeric(0)
# }
# hundreds_passengers <- passengers / 100 # Change weights to represent hundred of passengers
# # This will make viewing/interpreting ergm coefficients easier

# # Plot network
# passengers_igraph <- graph.adjacency(passengers, weighted = TRUE) ## weighted = TRUE creates edge weight in the igraph object
# ## below because it is added not only edge weight, but also changed the transparency of edges, it will take more time to plot
# ## also, if you use zoom in RStudio, it may take about 1 minute until you see the plot.
# passengers_igraph <- set_vertex_attr(passengers_igraph,"airportCode",value = read.csv("nodeAirportName.csv")$airportCode) #adds airport code labels
# net_layout <- layout_with_fr(passengers_igraph) # Calculates and stores a spring-embedded layout
# # We will re-use this same layout for each plot, so nodes are always in the same place
# 
# plot(passengers_igraph, layout=net_layout, edge.color = adjustcolor('blue',alpha=.2), vertex.label = V(passengers_igraph), edge.width=log(E(passengers_igraph)$weight), edge.arrow.width =.2)
