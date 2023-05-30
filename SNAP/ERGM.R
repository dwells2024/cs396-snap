# Exponential Random Graph Models (ERGMs)

# -------------------------------------------------------------------------------------------------
# Setup
# -------------------------------------------------------------------------------------------------

# Install packages below if you do not have them:
# -------------------------------------------------
if (!"statnet" %in% installed.packages()) install.packages("statnet") # For fitting ERGMs
if (!"igraph" %in% installed.packages()) install.packages("igraph") # For network plotting
if (!"texreg" %in% installed.packages()) install.packages("texreg") # For printing "nicer" model output

library(statnet)

# Set the working directory
# Session > Set Working Directory > To Source File Location
list.files() # List the files in the current working directory to see if you're in the right directory

# -------------------------------------------------------------------------------------------------
# Building Networks
# -------------------------------------------------------------------------------------------------

network_ties = read.csv("airportNetworkNumeric.csv")

routes <- as.network.matrix(network_ties, matrix.type = "edgelist")
routes

# Independent variables:
set.vertex.attribute(routes, "population", read.csv("airportPopulation.csv")$population)
routes

get.vertex.attribute(routes, "population")

# passengers between airports
passengersEdgelist <- read.csv("passengersEdgelist.csv")
head(passengersEdgelist) # Check the first five rows of the edgelist. Third column is the number of passengers
passengers <- matrix(nrow = 500, ncol = 500) # Number of Direct messages sent from i to j
for (i in 1:nrow(passengersEdgelist)) { # Read from edgelist
  passengers[passengersEdgelist$origin[i], passengersEdgelist$destination[i] ] <- as.numeric(passengersEdgelist$passengers[i])
}
for (i in 1:500) { # Remove self-ties (flights to self?)
  passengers[i,i] <- as.numeric(0)
}
scaled_passengers <- passengers / 100 # Change weights to represent hundred of passengers
# This will make viewing/interpreting ergm coefficients easier

summary(routes, print.adj = FALSE)

library('igraph') # Ignore messages on any objects that are masked

# Set default plot options
igraph_options(vertex.size = 9, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)                       # vertex.label = NA specifies not to display vertex labels in the plot

# Plot the Advice network
routes_igraph <- graph.adjacency(as.matrix.network(routes)) # make an igraph network object from statnet network object
# routes_igraph <- set_vertex_attr(routes_igraph,"female",value = read.csv("femaleNode.csv")$female)
net_layout <- layout_with_fr(routes_igraph) # Calculates and stores a spring-embedded layout
# We will re-use this same layout for each plot, so nodes are always in the same place
plot(routes_igraph, layout=net_layout, edge.color='black', vertex.label = V(routes_igraph))

# # Plot the Advice network with node coloring based on sex
# V(advice_igraph)$color = ifelse (V(advice_igraph)$female ==1, " orange ", " grey ")
# plot(advice_igraph, layout=net_layout, edge.color='black', vertex.label = V(advice_igraph))
# 
# # Plot the network of who messages whom
# # One unit of weight is 100 messages
# messages_igraph <- graph.adjacency(messages, weighted = TRUE) ## weighted = TRUE creates edge weight in the igraph object
# ## below because it is added not only edge weight, but also changed the transparency of edges, it will take more time to plot
# ## also, if you use zoom in RStudio, it may take about 1 minute until you see the plot.
# plot(messages_igraph, layout=net_layout, edge.color = adjustcolor('blue',alpha=.2), vertex.label = V(advice_igraph), edge.width=log(E(messages_igraph)$weight), edge.arrow.width =.2)

# -------------------------------------------------------------------------------------------------
# ERGM
# -------------------------------------------------------------------------------------------------
detach(package:igraph) # Remove the 'igraph' package from your environment. 
library(statnet)
options(ergm.loglik.warn_dyads=FALSE)

# Look at Endogenous statistics: terms based on only ties in the advice network
summary(routes ~ edges)                     # Number of edges (ties)
summary(routes ~ mutual)                    # Number of pairs of reciprocated ties
summary(routes ~ odegree(0:219))              # Outdegree distribution. (# of nodes with outdegree of 0, # nodes outdegree of 1, etc.)
# Remember, respondents could nominate at most five employees in our survey
summary(routes ~ idegree(0:212))             # Indegree distribution.
summary(routes ~ gwodegree(log(2),fixed=T)) # One parameter summarizing outdegree distribution - tendency against outdegree hubs
summary(routes ~ gwidegree(log(2),fixed=T)) # One parameters summarizing indegree distribution - tendency against indegree hubs
summary(routes ~ desp(1:152))                 # Pairs of nodes with one shared partner, two shared partners, etc.
summary(routes ~ dgwesp(log(2),fixed = T))  # One parameter summarizing 

# Look at Exogenous statistics: terms based on advice ties AND other ties / node attributes
summary(routes ~ nodeicov("population"))
summary(routes ~ nodeocov("population"))
summary(routes ~ diff("population"))
summary(routes ~ edgecov(scaled_passengers))

# The following commands do model estimation for ERGMs.
# This may take a second. Text will print in-console to update you on progress in model estimation.
model1 <- ergm(routes ~ edges                 # This is  a tendency towards a greater number of advice ties existing. Based on a statistic counting the number of ties.
               # Structural patterns
               + mutual                      # This is a tendency towards reciprocity for the advice ties. Based on a statistic counting the number of reciprocated ties.
               # + edgecov(hundreds_messages)  # This is the effect of every 100 messages sent from i->j on likelihood of an advice tie. Based on a weighted sum of advice ties x 100s of messages sent
               + diff("population")
               # Model constraints
               # , constraints =~ bd(maxout=5) # This constraint enforces the maximum outdegree is 5
) 
summary(model1) 

# Diagnostics
pdf('model1diagnostics.pdf')              # Open a pdf file to save to
mcmc.diagnostics(model1) # Run the markov chain monte carlo diagnostics
dev.off()                # Closes and saves the pdf

# This first command simulates 100 networks.
# These networks, if we use sufficient burnin steps in the markov chain used to generate them,
# may be thought of as random samples from the joint probability distribution that is our fitted ERGM.
sim1 <- simulate(model1, burnin=100000, interval=100000, nsim=100, verbose=T)  # Uses the ergm model to simulate a null model
# Plot the first of the simulated networks
sim1_net1 <- igraph::graph.adjacency(as.matrix.network(sim1[[1]]))
igraph::plot.igraph(sim1_net1,layout=net_layout,edge.color="brown",  
                    vertex.color = 'grey',edge.arrow.size=.1)                                                               
# Plot the 10th simulated network
sim1_net10 <- igraph::graph.adjacency(as.matrix.network(sim1[[10]]))
igraph::plot.igraph(sim1_net10,layout=net_layout,edge.color="red",  
                    vertex.color = 'grey',edge.arrow.size=.1)

model1.tridist <- sapply(1:100, function(x) summary(sim1[[x]] ~triangle)) # Extracts the triangle data from the simulated networks
hist(model1.tridist,xlim=c(0,1000),breaks=10)                             # Plots that triangle distribution as a histogram, change xlim to change the x-axis range if necessary
routes.tri <- summary(routes ~ triangle)                                    # Stores the number of observed triangles
routes.tri
arrows(routes.tri,20, routes.tri, 0.5, col="red", lwd=3)                      # Adds an arrow to the plotted histogram
c(obs=routes.tri,mean=mean(model1.tridist),sd=sd(model1.tridist),
  tstat=abs(mean(model1.tridist)-routes.tri)/sd(model1.tridist))

gof1 <- gof(model1, verbose=T, burnin=1e+5, interval=1e+5, control = control.gof.ergm(nsim = 200))
# If you run below and then wouldn't see the plot, trypar(mar=c(2,2,2,2))
dev.off()           # Clear any other plots from the plot window
plot(gof1)          # Plot the goodness of fit
# Note: This should produce five separate plots that you should look through.
#       In RStudio, scroll between the plots using the arrow buttons
gof1  


