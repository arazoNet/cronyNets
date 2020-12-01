#/////////////////////////////////////////////////////////////////////
# Filename:   cronyNets_networkStructures.R
# Purpose:    create two-mode and one-mode network structures
#             for GA, AA, and GG networks
#
#
# ___________________________
# ---- LIST OF FUNCTIONS ----
# ___________________________
#
# 1) matrixGA <-(numA, numG, ptie.max, ptie.mode="rnd")
# 2) networkGA(GA.mat)
# 3) denseGA(GA)
# 4) networkGG(GA.mat)
# 5) networkAA(GA.mat)
# 6) matDensity(mat.net)
# 7) netClustering(net, type=c("overall", "weighted"))
# 8) netStats(net)
# 9) reachedNodes(nodeset, net)
# 10) compileFunction(fun)
#/////////////////////////////////////////////////////////////////////


# BEGIN FUNCTION ....................................................
# function 1: matrixGA <-(numA, numG, ptie.max, ptie.mode="density")
#
#' Derive random GA matrix with density equal to ptie.max
#
#' @description This function creates a rectangular affiliation matrix linking G's (rows) 
#' and A's (columns).  Each cell in this matrix is equal to 1 if the corresponding (row) G
#' protects a given (column) A.  These cell values are determined according to ptie.mode.
#' If ptie.mode="indep", then each GA[i,j] is generated separately with probability ptie.max.
#' If ptie.mode="density", then this function samples from the conditional distribution of GA 
#' random matrices with density equal to ptie.max in one of two ways, depending
#' on the argument ptie.mode.  For example, if numG=10 and numA=10, then ptie.max=0.1 and
#' ptie.mode="density" will sample from GA matrices that have 10 ties, the cells are 
#' therefore dependent in order to ensure that the overall density equals ptie.max.
# ..........
# ARGUMENTS
# ..........
#' @param numA No. of firms
#' @param numG No. of third-party enforcers
#' @param ptie.max (maximum) probability of a tie between G and A
#' @param ptie.mode equal tie probabilities ('indep') or ('density') net formation?
#' 
#' @return A rectangular GA affiliation matrix where GA[i, j]=1 if G_i protects A_j
#' @export

matrixGA <- function(numA, numG, ptie.max, ptie.mode="density"){
  
  numCells <- numA*numG
  GA <- rep(0, numCells)
  
  if (ptie.max > 0) {
    if (ptie.mode == "density") {  ### DEFAULT: density-conditioned sampling
      nTies.required <- round(ptie.max*numCells) # need an integer
      actualTies <- sample(1:numCells,nTies.required, prob=rep(ptie.max,numCells))
      GA[actualTies] <- 1
    } else {                   ### independent draws: all firms face same prob.
      GA <- replicate(numCells, rbinom(1,1,ptie.max))
    }
  }
  GA.mat <- matrix(GA, nrow=numG, ncol=numA, byrow = T)
}

#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 2: networkGA(GA.mat)
#
#' Convert a GA rectangular matrix into a network object
#
#' @description This function takes a rectangular GA matrix and creates a 
#' bipartite network object with the first mode equal to the number of
#' rows in GA.
# ..........
# ARGUMENTS
# ..........
#' @param GA.mat A two-mode affiliation matrix
#' @return A 'network' object
#' @export

networkGA <- function(GA.mat) {
  numG <- dim(GA.mat)[1]
  numA <- dim(GA.mat)[2]
  G.names <- c(paste("G",seq(1:numG),sep=""))
  A.names <- c(paste("A",seq(1:numA),sep=""))
  
  GA.net <- network::network(GA.mat, directed=T, bipartite=numG)
  network::network.vertex.names(GA.net) <- c(G.names, A.names)
  return(GA.net)
}

#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 3: denseGA(GA)
#
#' Calculate density for given affiliation matrix or two-mode network object
#
#' @description This is a generic function that takes either a matrix or network
#' object and calculates the corresponding network density.
#' 
#' Especially when a matrix GA is available, this function is mean to avoid expensive
#' calls to network.density(as.network(GA)).
# ..........
# ARGUMENTS
# ..........
#' @param GA.mat A two-mode affiliation matrix or affiliation network object
#' @return A number between 0 and 1, denoting density of two-mode connections
#' @export

denseGA <- function(GA) {

  if(network::is.network(GA)) {
    GA <- as.matrix(GA)
  }
  GA.density <- sum(GA)/(dim(GA)[1]*dim(GA)[2])
}


#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 4: networkGG(GA.mat)
#
#' Derive G to G projection network from affiliation matrix
#
#' @description This function takes a rectangular GA matrix, and creates a network
#' object out of GA * GA'
# ..........
# ARGUMENTS
# ..........
#' @param GA.mat A two-mode affiliation matrix
#' @return A 'network' object with connections among G's
#' @export


networkGG <- function(GA.mat) {  
  numG <- dim(GA.mat)[1]
  G.names <- c(paste("G",seq(1:numG),sep=""))
  
  GG.net <- network::network(GA.mat %*% t(GA.mat), loops=FALSE, directed=FALSE)
  network::network.vertex.names(GG.net) <- G.names
  return(GG.net)
}

#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 5: networkAA(GA.mat)
#
#' Derive A to A projection network from affiliation matrix
#
#' @description This function takes a rectangular GA matrix, and creates a network
#' object out of GA' * GA
# ..........
# ARGUMENTS
# ..........
#' @param GA.mat A two-mode affiliation matrix
#' @return A 'network' object with connections among A's
#' @export

networkAA <- function(GA.mat) {
  numA <- dim(GA.mat)[2]
  A.names <- c(paste("A",seq(1:numA),sep=""))
  
  AA.net <- network::network(t(GA.mat) %*% GA.mat, loops=FALSE, directed=FALSE)
  network::network.vertex.names(AA.net) <- A.names
  return(AA.net)
}

#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 6: matDensity(mat.net)
#
#' Calculate density for a sociomatrix
#
#' @description This function takes a one-mode (symmetric) matrix representing a network
#' and calculates the density function (ratio of actual ties to max number of possible ties).
#' 
#' This function is meant to quickly calculate density without having to call \code{\link[network]{network.density}}.
# ..........
# ARGUMENTS
# ..........
#' @param mat.net A square matrix representing an undirected one-mode network
#' @return A number between 0 and 1, denoting density of one-mode network
#' @note Added this function to speed up netClustering function, which had made expensive
#' calls to as.network of extracted ego networks
#' @export

matDensity <- function(mat.net) {
  n <- dim(mat.net)[1]
  if(n == 1) {
    return (0)
  }
  total.ties <- n*(n-1)/2
  
  sum(sna::lower.tri.remove(mat.net,0))/total.ties
}


# BEGIN FUNCTION ....................................................
# function 7: netClustering(net, type=c("overall", "weighted"))
#
#' Calculate clustering coefficient for one-mode network
#
#' @description This is an own function to calculate the clustering coefficient 
#' as the "...the tendency towards dense local neighborhoods..." as explained by
#' Hanneman in \url{http://www.faculty.ucr.edu/~hanneman/nettext/C8_Embedding.html#transitivity}.
#' See notes.
# ..........
# ARGUMENTS
# ..........
#' @param net a network object
#' @param type calculation option in c("overall", "weighted")
#' @return A clustering coefficient between 0 and 1
#' @note 
#' Based on average density of ego neighborhoods
#' Source:  \url{http://www.faculty.ucr.edu/~hanneman/nettext/C8_Embedding.html#transitivity}
#'  "...the tendency towards dense local neighborhoods, 
#' or what is now thought of as "clustering.
#'
#' One common way of measuring the extent to which a graph displays clustering
#' is to examine the local neighborhood of an actor (that is, all the actors 
#' who are directly connected to ego), and to calculate the density in this neighborhood 
#' (leaving out ego).  After doing this for all actors in the whole network, we can 
#' characterize the degree of clustering as an average of all the neighborhoods."
#' @export

netClustering <- function(net, type="overall") {
  n <- network::network.size(net)
  
  if (network::network.density(net) == 0) {
    return (0)
  } else {
    ego.stats <- matrix(0,nrow=n, ncol=2)  # initialize vector for egonet densities
    
    ego.nets <- sna::ego.extract(net)
    for (i in 1:n) {
      net.tmp <- ego.nets[[i]]
      ego.stats[i,1] <- net.alters <- dim(net.tmp)[1] -1
      ego.stats[i,2] <- matDensity(net.tmp)
    }
    rm(ego.nets)
    
    # determine output type ----
    if (type=="overall") {
      return (sum(ego.stats[,2])/n)
    } else {
      weightedAlters <- ego.stats[,1]/sum(ego.stats[,1])
      return (sum(weightedAlters*ego.stats[,2]))
    }
    
  }
} 
#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 8: netStats(net)
#
#' Calculates various network-centric statistics
#
#' @description This function calculates 10 network-analytic variables for given network.
#' 
#' Among these variables, the function delivers density, centralization, number of 
#' components, etc.
# ..........
# ARGUMENTS
#' @param net: a network object
#' @return A vector with selected network-centric statistics
#' 
#'   (1) n: network size
#'   (2) d: density
#'   (3) c.deg:  sna::centralization using degree
#'   (4) c.bet: sna::centralization using betweenness
#'   (5) connected: Dummy variable for connected networks
#'   (6) comp: Number of components 
#'   (7) comp.large: Size of largest component
#'   (8) clustering: clustering coefficient
#'   (9) trans: Transitivity score
#'   (10) avgReach: average of node-level reachability
#'   
#' @family Simulation summary statistics
#' @export


netStats <- function(net) {

  if (!network::is.network(net)) {
    cat("Input is not a network\n")
    return(rep(NA,10))
  }
  
  n <- network::network.size(net)
  d <- network::network.density(net)
  c.deg <- sna::centralization(net, degree, mode="graph") 
  c.bet <- sna::centralization(net, betweenness, mode="graph")
  # not using c.eig due to computational problems with sparse networks
  #c.eig <- sna::centralization(net, evcent, mode="graph",use.eigen=TRUE) # using eigenvector centrality
  capture.output(components <- cmp_component.dist(net), file='netStats_discard.tmp')
  capture.output(connected <- ifelse(cmp_is.connected(net, comp.dist.precomp = components),1,0), file='netStats_discard.tmp')
  comp <-  length(components$csize)
  comp.large <-  max(components$csize)
  clustering <- netClustering(net)
  trans <- sna::gtrans(net, mode="graph")
  capture.output(reach.mat <- cmp_reachability(net), file='netStats_discard.tmp')
  avgReach <- mean(apply(reach.mat,2,sum)-1)
  # not using avgGeoD due to pervasive NA's with sparse networks
  #avgGeoD <- sum(geodist(net, inf.replace=NA)$gdist)/(n*(n-1)) # use NA with isolates => Infinite geodesics
  
  return(c(n, d,c.deg, c.bet, connected, comp, comp.large, clustering, trans, avgReach))
}

#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 9: reachedNodes(nodeset, net)
#
#' Count nodes reached from nodeset by traversing net
#
#' @description This function is used by \code{\link{a0.Stats}} to take a given
#' set of nodes and calculate their /strong{collective} reach to other nodes in
#' a given network /emph{net}.
# ..........
# ARGUMENTS
# ..........
#' @param nodeset a vector of nodes
#' @param net a square network matrix
#'
#' @return An integer that counts reached nodes
#' @export

reachedNodes <- function(nodeset, net) {
  # get reachability matrix
  capture.output( reach.mat <- cmp_reachability(net), file="netStats_discard.tmp")
  N <- network::network.size(net)
  reached <- c()
  
  for (i in nodeset) {
    for (j in 1:N) {
      if (reach.mat[i,j]==1) {
        reached <- union(reached, j)
      }
    }
  }  
  reached <- setdiff(reached, nodeset)  
  return(length(reached))
}
#....................................................... END FUNCTION



# BEGIN FUNCTION ....................................................
# function 10: compileFunction(function)
#
#' Creates byte-compiled version of a function
#
#' @description This function is used to create compiled versions of R functions,
#' for possible substitution if they can run faster than original functions.
# ..........
# ARGUMENTS
#' @param fun: a function
#' @return A byte compiled version of 'fun' function
#' @note Used to bytecompile some sna functionst that take a lot of time
#' @export

compileFunction <- function(fun) {
  compiler::cmpfun(fun)
}

# byte-compiled versions of selected 'sna' recursive functions

#' Find the Centralization of a Given Network, for Some Measure of Centrality
#' @export
#' @inheritParams sna::centralization
#' @seealso \code{\link[sna]{centralization}}
cmp_centralization <- compileFunction(sna::centralization)

#' Find the Reachability Matrix of a Graph
#' @export
#' @inheritParams sna::reachability
#' @seealso \code{\link[sna]{reachability}}
cmp_reachability <- compileFunction(sna::reachability)

#' Is a Given Graph Connected?
#' @export
#' @inheritParams sna::is.connected
#' @seealso \code{\link[sna]{sna::is.connected}}
cmp_is.connected <- compileFunction(sna::is.connected)

#' Calculate the Component Size Distribution of a Graph
#' @export
#' @inheritParams sna::component.dist
#' @seealso \code{\link[sna]{component.dist}}
cmp_component.dist <- compileFunction(sna::component.dist)
#....................................................... END FUNCTION


