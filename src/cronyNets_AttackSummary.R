#/////////////////////////////////////////////////////////////////////
# Filename:   cronyNets_AttackSummary.R
# Purpose:    calculate aggregate summaries of predation attacks
#
# ___________________________
# ---- LIST OF FUNCTIONS ----
# ___________________________
#
# 1) attackSummary(society, attack.dat)
# 2) a0_Stats(victim, GA.net, AA, A.dat, GG, G.dat)
# 3) sessionVars()
#
#/////////////////////////////////////////////////////////////////////

# BEGIN FUNCTION ....................................................
# function 1: attackSummary(society, attack.dat)
#
#' Summarize output from a given predation attack
#
#' @description This function combines parameter information from a
#' society object and information from a particular attack.
#' 
#' Some of the variable names are repeated so the attackNet function can
#' be tested separately without calling the whole society object
# ..........
# ARGUMENTS
# ..........
#' @param society output of SocietySetup
#' @param attack.dat  output of attackNet
#'
#' @return Summarize output from a given attack
#' 
#' @family Simulation summary statistics
#' @export

attackSummary <- function(society, attack.dat) {

  ## Create matrix/data frame for data collection ----
  numVAR <- length(sessionVars())
  output.dat <- matrix(NA, nrow=1, ncol=numVAR)
  output.dat <- data.frame(output.dat)
  names(output.dat) <- sessionVars()   # for testing purposes
  
  A.dat <- society$A
  numA <- dim(A.dat)[1]
  G.dat <- society$G
  numG <- dim(G.dat)[1]
  D.dat <- society$D
  
  # Populate first nineteen entries with actor variables ----
  
  output.dat[1,1:19] <- c(numA,
                          min(A.dat$R),mean(A.dat$R),max(A.dat$R),sum(A.dat$R), # 2-5
                          min(A.dat$t),mean(A.dat$t),max(A.dat$t),D.dat$tR,  # 6-9
                          min(A.dat$b),mean(A.dat$b),max(A.dat$b),sum(G.dat$bR), #10-13
                          numG,
                          min(G.dat$rho),mean(G.dat$rho),max(G.dat$rho),sum(G.dat$rho), #15-18
                          D.dat$Cd)  #19           
  output.dat[1,20] <- society$attack.mode
  output.dat[1,21] <- society$attack.N
  output.dat[1,22] <- society$ptie.mode

  ## Add NETWORK stats ----

  # For GA:  calculate density
# output.dat[1,23] <- denseGA(society$GA)
  output.dat[1,23] <- network::network.density(society$GA)  # faster than denseGA
  # For GG and AA (10 properties each--see NetworkStructures functions)
  A.range <- 24:33  # these could be altered later if new stats are added
  G.range <- 34:43
  
  output.dat[1,A.range] <- netStats(society$AA)
  output.dat[1,G.range] <- netStats(society$GG)
  ## Add PREDATION stats ----

  # 1st victim stats: in case initial conditions (i.e., choice of victim) matters
  
  #a0 <- attack.dat$victims[1]
  a0 <- attack.dat$a0
  a0.range <- 44:55   # a0 is first victim; range can be altered if new stats are added
  # currently: 16 variables (see AttackSetup)
  # A.dat and G.dat have already been extracted from 'society'

  output.dat[1,a0.range] <- a0.Stats(a0,society$GA, society$AA, A.dat, society$GG, G.dat)
  
  # other attack variables (56-62) ----
  output.dat[1,56] <- attack.dat$victims # No. of victims
  output.dat[1,57] <- attack.dat$protectors  # No. of activated protectors
  output.dat[1,58] <- attack.dat$s  # Step at which predation stops
  output.dat[1,59] <- attack.dat$prey.gain  # Predation gains
  output.dat[1,60] <- attack.dat$penalty  # Predation losses
  
  # Add ptie.max and exog dummy variable ----
  output.dat[1,61] <- society$ptie 
  output.dat[1,62] <- society$exog
  
  ## RETURN collection of variables ----
  return(output.dat)
}

#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 10: a0_Stats(victim, GA.net, AA, A.dat, GG, G.dat)
#
#' Summary statistics corresponding to an attacked firm
#
#' @description This function calculates various summary statistics for the
#' first A attacked by D.  This output can be useful to assess whether there
#' are idiosyncratic factors that affect the incidence and extent of predation.
#' For example, a firm with multiple G's will enable the spread of predation to
#' multiple firms.  
# ..........
# ARGUMENTS
# ..........
#' @param victim a firm that was initially targeted for predation
#' @param GA.net affiliation network matrix of G's and A's
#' @param AA a network of A's with common protectors
#' @param A.dat Firm-specific data
#' @param GG a network of G's with common protection duties
#' @param G.dat Third-party enforcer data
#' 
#' @return a vector with this information:
#' 
#' (1) identify FIRST victim a0
#' (2) a0's Rents
#' (3) degree centrality
#' (4) betweenness centrality
#' (5) how many other notes are reachable?
#' (6) No.of G protectors
#' (7) how many other firms do related G's protect?
#' (8) avg RHO of related G's
#' (9) cumulative RHO of related G's
#' (10) avg degree centrality of related G's
#' (11) avg betweenness centrality of related G's
#' (12) average reach of related G's
#' 
#' @family Simulation summary statistics
#' @export


a0.Stats <- function(victim, GA.net, AA, A.dat, GG, G.dat) {
  
  num.Params <- 12
  n.A <- dim(A.dat)[1]
  a0.out <- rep(NA, num.Params)  # initialize output vector
  
  a0.out[1] <- a0 <- victim  # identify FIRST victim
  a0.out[2] <- A.dat$R[a0]   # a0's Rents
  a0.out[3] <-  a.d <- sna::degree(AA, gmode="graph")[a0] # degree centrality
  a0.out[4] <-  sna::betweenness(AA, gmode="graph")[a0] # betweenness centrality
  #  a0.out[5] <-  evcent(AA, gmode="graph",use.eigen=TRUE)[a0] # eigenvector centrality
  #  a0.out[5] <- ifelse(a0.out[5]<0,-1*a0.out[5],a0.out[5]) # need to change signs for use.eigen=T cases
  a0.out[5] <- ifelse(a.d == 0, 0, reachedNodes(a0,AA))
  #  a0.out[7] <-  sum(geodist(AA, inf.replace=NA)$gdist[a0,])/(n.A-1)# average geodesic distance to other A's (may be NA)
  a0.out[6] <-  n.G <- A.dat$Gs[a0] # No.of G protectors

  ## NOTE:  n.G=0 is a problematic case because the network properties below such as reachability cannot be calculated
  ## SOLUTION:  Check for n.G=0, in which case the network properties are set to ZERO.
  
  if (n.G == 0) {
    a0.out[7:12] <- 0
  }
  else {
    protectors <- activateG(a0, GA.net)
    a0.out[7] <-  length(protectedFirms(protectors,GA.net))-1 # how many other firms do related G's protect?
    a0.out[8] <-  mean(G.dat$rho[protectors]) # average RHO of related G's
    a0.out[9] <-  sum(G.dat$rho[protectors]) # cumulative RHO of related G's
    a0.out[10] <- mean(sna::degree(GG, gmode="graph")[protectors]) # avg degree centrality of related G's
    a0.out[11] <- mean(sna::betweenness(GG, gmode="graph")[protectors]) # avg betweenness centrality of related G's
    a0.out[12] <- reachedNodes(protectors, GG)
  }  
  #  a0.out[14] <- mean(evcent(GG, gmode="graph",use.eigen=TRUE)[protectors]) # avg eigenvector centrality of related G's
  #  a0.out[14] <- ifelse(a0.out[14]<0,-1*a0.out[14],a0.out[14]) # need to change signs for use.eigen=T cases
  #  a0.out[16] <- (sum(geodist(GG, inf.replace=NA)$gdist[protectors,])-n.G)/n.G # avg geodesic distance of related G's
  
  return(a0.out)   
}

#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 3: sessionVars()
#
#' Wrapper function to define variable names for a simulation run
#
#' @description This function provides a one-stop collection of all variables
#' that are collected from a single attack through \code{\link{attackNet}}
# ..........
# ARGUMENTS: None
# ..........
#
#' @return a vector of variable names to match output from \code{\link{attackSummary}}.
#' @family Simulation summary statistics
#' @export

sessionVars <- function() {
  session.VARS <- c(    ##### START NAMES VECTOR
    ## Add SOCIETY stats ----
    ## adding suffix ".1" for variable names that are already supplied with paramsGRID
    ## This redundancy is useful to test isolated attack functions that don't have paramsGRID
    "numA.1", "R.min", "R.avg", "R.max", "R.sum", # 1-5
    "t.min", "t.avg", "t.max", "tR.sum",  # 6-9
    "b.min", "b.avg", "b.max", "bR.sum",  # 10-13
    "numG.1", "rho.min", "rho.avg", "rho.max", "rho.sum", # 14-18
    "Cd.1", # 19
    "attack.mode.1",  # 20, TEXT:  attack mode
    "attack.N.1",  # 21, TEXT:  attack mode
    "ptie.mode.1",   # 22, TEXT:  ptie mode
    ## Add NETWORK stats ----
    "GA.den",  # 23
    # AA net, 24-33
    "AA.n",  # network size, same as numA
    "AA.den", #density
    "AA.deg", # centralization using degree
    "AA.bet",   # using betweenness
    "AA.connect", # Dummy for connected networks
    "AA.comp",  # Number of components 
    "AA.comp.max",  # Size of largest component
    "AA.clust",   # Using own algorithm of overall clustering
    "AA.trans",  # Transitivity score
    "AA.avgReach",  # average of node-level reachability

    # GG net, 34-43
    "GG.n",  # network size, same as numA
    "GG.den", #density
    "GG.deg", # centralization using degree
    "GG.bet",   # using betweenness
    "GG.connect", # Dummy for connected networks
    "GG.comp",  # Number of components 
    "GG.comp.max",  # Size of largest component
    "GG.clust",   # Using own algorithm of overall clustering
    "GG.trans",  # Transitivity score
    "GG.avgReach",  # average of node-level reachability

    ## Add PREDATION stats ----
    
    # first victim stats, 44-55
    "a0", "a0.R", "a0.deg", "a0.bet", "a0.reach", "a0.G",
    "a0.GA", "a0.rho.avg", "a0.rho.sum", "a0.G.deg", "a0.G.bet", "a0.G.reach",
    
    # other attack variables (56-60 ) ----
    "victims", "protectors", "s.stop", "prey.gain", "prey.loss",
    # tie probability for GA matrix generation (61)
    "ptie",
    # Exogenous rent, rho distributions? (62)
    "exog.1"
  )  ## END OF 'session.VARS' vector
  
}
#................................................... END session.VARS
