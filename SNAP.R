# SNAP Project Code

# Install libraries
if (!"statnet" %in% installed.packages()) install.packages("statnet") # For fitting ERGMs
if (!"igraph" %in% installed.packages()) install.packages("igraph") # For network plotting
if (!"texreg" %in% installed.packages()) install.packages("texreg") # For printing "nicer" model output

library(statnet)

# -------------------------------------------------------------------------------------------------
# Set the working directory
# Session > Set Working Directory > To Source File Location
# -------------------------------------------------------------------------------------------------
list.files() # List the files in the current working directory to see if you're in the right directory



# Generate network 
passengersEdgelist <- read.csv("passengersEdgelist.csv")
head(passengersEdgelist) # Check the first five rows of the edgelist. Third column is the number of passengers
passengers <- matrix(nrow = 100, ncol = 100) # Number of Direct messages sent from i to j
for (i in 1:nrow(passengersEdgelist)) { # Read from edgelist
  passengers[passengersEdgelist$origin[i], passengersEdgelist$destination[i] ] <- as.numeric(passengersEdgelist$passengers[i])
}
for (i in 1:100) { # Remove self-ties (flights to self?)
  passengers[i,i] <- as.numeric(0)
}
hundreds_passengers <- passengers / 100 # Change weights to represent hundred of passengers
# This will make viewing/interpreting ergm coefficients easier


# Visualize network
library('igraph') # Ignore messages on any objects that are masked

# Set default plot options
igraph_options(vertex.size = 9, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)

# Plot network
passengers_igraph <- graph.adjacency(passengers, weighted = TRUE) ## weighted = TRUE creates edge weight in the igraph object
## below because it is added not only edge weight, but also changed the transparency of edges, it will take more time to plot
## also, if you use zoom in RStudio, it may take about 1 minute until you see the plot.
passengers_igraph <- set_vertex_attr(passengers_igraph,"airportCode",value = read.csv("nodeAirportName.csv")$airportCode) #adds airport code labels
net_layout <- layout_with_fr(passengers_igraph) # Calculates and stores a spring-embedded layout
# We will re-use this same layout for each plot, so nodes are always in the same place

plot(passengers_igraph, layout=net_layout, edge.color = adjustcolor('blue',alpha=.2), vertex.label = V(passengers_igraph), edge.width=log(E(passengers_igraph)$weight), edge.arrow.width =.2)
