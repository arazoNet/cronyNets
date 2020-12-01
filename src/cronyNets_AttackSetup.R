#/////////////////////////////////////////////////////////////////////
# Filename: cronyNets_AttackSetup.R
# Purpose:  Run network simulations for crony capitalism paper
#
# ___________________________
# ---- LIST OF FUNCTIONS ----
# ___________________________
#
# 1) societyAttack(society, attack.list)
# 2) societyAttackMC(society, attack.list)
# 3) attackList(biasAttr, attack.mode)
# 4) pickFirm(biasAttr, attack.mode)
# 5) attackNet(AA.net, GA.net, A.dat, G.dat, D.dat, pick.A0)
# 6) nextVictims(victim.0, step, AA.net)
# 7) newRents(newVictims, A.dat)
# 8) activateG(A.victims, GA.net, protectors)
# 9) newPenalty(activatedG, G.dat)
# 10) protectedFirms(protectors, GA.net)
#
#
#/////////////////////////////////////////////////////////////////////


# BEGIN FUNCTION ....................................................
# function 1: societyAttack(society)
#
#' Simulates the impact of a predation attack
#
#' @description Simulates the impact of a predation attack on selected firms, which are
#' chosen according to the attack.N attribute of society, which can be either "single"
#' (for a random attack on a single A) or "all" (to attack all firms), which averages the
#' results of multiple attacks.
#' 
#' This function is the main output for each simulation run created by \code{\link{cronyNets}} or
#' \code{\link{cronysimple}}.
# ..........
# ARGUMENTS
# ..........
#' @param society A society (list) object, which includes an attack list of size numA
#' @return An attackSummary for a given list of firms to attack (averages if  attack.N="all")
#' @export

societyAttack <- function(society) {
  
  numA <- dim(society$A)[1]
  attack.list <- attackList(numA, society$attack.mode, society$attack.N, 
                            society$A$R)  # got rid of attack.mode to use "all" default
  numVar <- length(sessionVars())
  
  #society.attacks.list <- list()   # Initialize list to collect firm-specific results
  
  society.attacks.list <- foreach::foreach(i = attack.list) %do% attackSummary(society, attackNet(society$AA, society$GA, society$A, society$G, society$D, i))
  
  
  # collect simulation results into a rectangular frame
  society.attacks <- do.call(rbind, society.attacks.list)
  
  # prepare results
  society.avg <- data.frame(matrix(NA, nrow=1,ncol=numVar))
  names(society.avg) <- sessionVars()
  vars.avoid <- c(20,21,22)  ## these are text variables
  society.avg[1,20] <- society$attack.mode
  society.avg[1,21] <- society$attack.N
  society.avg[1,22] <- society$ptie.mode
  society.avg[1,-vars.avoid] <- apply(society.attacks[,-vars.avoid],2,mean)
  return(society.avg)
}
#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 2: societyAttackMC(society)
#
#' Simulates the impact of a predation attack using DoMC parallel backend
#
#' @description Simulates the impact of a predation attack on selected firms, which are
#' chosen according to the attack.N attribute of society, which can be either "single"
#' (for a random attack on a single A) or "all" (to attack all firms), which averages the
#' results of multiple attacks.
# ..........
# ARGUMENTS
# ..........
#' @param society A society (list) object, which includes an attack list of size numA
#' @return An "average" of attackSummary for a given list of firms to attack
#' @note  Adds a %dopar% to SocietyAttack, which already uses foreach::foreach
#' @export


societyAttackMC <- function(society) {
  
  mc.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores=mc.cores)
  
  numA <- dim(society$A)[1]
  attack.list <- attackList(numA, society$attack.mode, society$attack.N, 
                            society$A$R)  # got rid of attack.mode to use "all" default
  numVar <- length(sessionVars())
  
  #society.attacks.list <- list()   # Initialize list to collect firm-specific results
  
  society.attacks.list <- foreach::foreach(i = attack.list) %dopar% attackSummary(society, attackNet(society$AA, society$GA, society$A, society$G, society$D, i))
  
  # collect simulation results into a rectangular frame
  society.attacks <- do.call(rbind, society.attacks.list)
  
  # prepare results
  society.avg <- data.frame(matrix(NA, nrow=1,ncol=numVar))
  names(society.avg) <- sessionVars()
  vars.avoid <- c(20,21)  ## these are text variables
  society.avg[1,20] <- society$attack.mode
  society.avg[1,21] <- society$ptie.mode
  society.avg[1,-vars.avoid] <- apply(society.attacks[,-vars.avoid],2,mean)
  return(society.avg)
}

# BEGIN FUNCTION ....................................................
# function 3: attackList(attack.mode, attack.N, biasAttr, attack.mode="all")
#
#' Identify victims of predation attacks (as many as the number of firms)
#
#' @description This function delivers either a single firm ID is attack.mode equals "single"
#' or all firms ID's if attack.mode equals "all". 
# ..........
# ARGUMENTS
# ..........
##
#' @param attack.mode "rnd"=choose victims at random;"biased"=choose victims proportional to their rents; "all": all firms will be attacked
#' @param attack.N 1 for as single random attack, society$numA to attack all firms
#' @param numA No. of firms
#' @param biasAttr A firm attribute \(e.g., rents\) for biased selection
##
#' @return a vector of firm ID's of length equal to 1 if single="TRUE" or no. of firms \(in effect, numA separate attacks\)
##
#' @note This function replaces pickFirm, but keep pickFirm to have the ability to select just one firm.
#
# attackList() serves to populate an attack list that gets created by setupSociety(...)
#
#' @export

attackList <- function(numA, attack.mode="rnd", attack.N = "single", biasAttr) {
  
  if (attack.N == "single") {
    return(pickFirm(numA, biasAttr, attack.mode))
  } else {
    return(1:numA)
  }
}
#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 4: pickFirm(numA, attack.mode, biasAttr)
#
#' Pick a firm to be attacked by D
#
# @description This function identifies a firm to be targeted for a predation attack.
# ..........
# ARGUMENTS
# ..........
#' @param numA No. of firms
#' @param biasAttr Firm-level attribute to determine proportational selection probabilities
#' @param attack.mode "rnd": pick at random // "biased": pick proportional to biasAttr
#' @export

pickFirm <- function(numA, biasAttr, attack.mode="rnd") {

  if (attack.mode == "biased") {
    try(
      if (length(biasAttr) == 0) 
      stop("You need an attribute to calculate biased selection probabilities")
    )
    p.R <- biasAttr/sum(biasAttr)
    return (sample(1:numA,1,prob=p.R))
  } else {
    return(sample(1:numA,1))
  }
}
#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 5: attackNet(AA.net, GA.net, A.dat, G.dat, D.dat, victim.0)
#
#' Simulates an isolated attack on a firm A, which spreads to its neighbors
#
#' @description This is the main function to simulate an attack on one firm.
#' 
#' This function can be called multiple times within a single society attack if
#' \code{\link{societyAttack}} is using the attack.N="all" option.
# ..........
# ARGUMENTS
# ..........
#' @param AA.net Network of A's that share G's
#' @param GA.net Affiliation Network of G's and A's
#' @param A.dat Network of A's
#' @param G.dat Network of A's
#' @param D.dat Network of A's
#' @param victim.0 number that identifies targeted A
#'
#' @return A list with these items:
#'
#' a0: initial victim's ID
#' victims: a vector of all affected firms
#' protectors: a vector of activated G's
#' s: the last predation step
#' prey.gain: cumulative predation gains
#' penalty: cumulative penalty
#'
#' @export

attackNet <- function(AA.net, GA.net, A.dat, G.dat, D.dat, victim.0) {
  
  numA <- dim(A.dat)[1]  # number of firms
  net.diameter <- numA -1        # MAX possible network diameter in a connected graph
  s <- 0                 # step number
  
  # initialize relevant actors ----
  A.next <- victims <- victim.0   # first victim is given A
  protectors <- c()
  
  # initialize relevant payoffs ----
  prey.gain <- D.dat$tR - D.dat$Cd   # Commitment payoffs
  prey.gain.old <- prey.gain # use to avoid extra gains when attack is deterred within WHILE loop
  #  cat("prey.gain BEFORE loop=",prey.gain,"\n")
  prey.loss <- 0
  prey.loss.old <- prey.loss # use to avoid extra loss when attack is deterred within WHILE loop
  prey.stop <- FALSE   # used to flag end of attack
  
  # predation loop ----
  while ((prey.stop == FALSE) & (s <= net.diameter)) {
    
    prey.gain <- prey.gain.old + newRents(A.next, A.dat)
    #identify protectors
    activeG <- activateG(A.next, GA.net, protectors)
    prey.loss <- prey.loss.old + newPenalty(activeG, G.dat)
    
    # Should D continue to prey? ----
    if (prey.gain <= prey.loss) {  # STOP if net gains are negative
      prey.stop <- TRUE
      ifelse(s >0, s-1, 0)        # Don't count round if predation is deterred
      prey.gain <- prey.gain.old  # Don't count attempted predation gains in this round
      prey.loss <- prey.loss.old  # Don't count this round's loss (avoided)
      victims <- setdiff(victims, A.next) # don't count last set of potential victims to avoid loops
    } else {
      # If so, add new victims of successful predation ----
      victims <- sort(union(victims, A.next))
      protectors <- sort(union(protectors, activeG))  # keep track of ALL activated G's
      
      # check whether A can reach more more neighbors -----
      A.next <- setdiff(nextVictims(victim.0,s+1,AA.net),victims)
      if (length(A.next) == 0) { # EMPTY A.next?
        prey.stop <- TRUE
      }
      
      # update prey.gain, prey.loss, and s for next loop
      if (prey.stop==FALSE) {
        prey.gain.old <- prey.gain
        prey.loss.old <- prey.loss
        s <- s + 1
      }
    }
  }
  
  # attackNet return information ----
  # returning number of victims and protectors, rather than their collection
  return(list(a0=victim.0,victims=length(victims), protectors=length(protectors), s=s,
              prey.gain=prey.gain, penalty=prey.loss))
  
}

#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 6: nextVictims(victim.0, step, AA.net)
#
#' Identifies set of new firms to attack
#
#' @description This function takes a given firm as a starting point to traverse
#' the network and identify all connected firms within a certain neighborhood of
#' size \emph{step}.
# ..........
# ARGUMENTS
# ..........
#' @param victim.0 is the numerical ID of first attacked firm
#' @param step An integer > 1, the neighborhood size for subsequent predation
#' @param AA.net is the one-mode network of related firms
#
#
# RETURN VALUE:  a vector of firm ID's
#' @export

nextVictims <- function(victim.0, step, AA.net) {

  numA <- network::network.size(AA.net)   # MAX iterations
  checkA <- (1:numA)[-victim.0]  # Don't need to check victim.0, already identified
  nextA <- c()                  # Initialize vector of firms to attack next
  
  neighbors <- sna::neighborhood(AA.net, step, neighborhood.type="in",mode="graph",return.all=FALSE, partial=TRUE)
  
  for (j in checkA) {
    if (neighbors[victim.0,j]==1) { # if there's a related firm
      nextA <- c(nextA, j)        # add it to vector of new victims
    }
  }
  return(nextA)                  # Return cumulative set of new victims
}
#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 7: newRents(newVictims, A.dat)
#
#' Calculate rents from the next set of (networked) victims
#
#' @description This function takes a set of firm ID's and calculates their
#' collective rents.
# ..........
# ARGUMENTS
# ..........
#' @param victims a (possibly empty) set of preyed upon firms
#' @param A.dat a matrix with firm-level data, including rents
#'
#' @return The sum of victims' available rents
#' @export

newRents <- function(newVictims, A.dat) {
  
  # Add ability to handle empty new victim sets to avoid warnings in subsequent calculations
  rents <- ifelse(length(newVictims) == 0,0,
                  sum((1-A.dat$t[newVictims])*A.dat$R[newVictims]))  ## Add up rents from 'victims' set
}

#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 8: activateG(A.victims, GA.net, protectors=c())
#
#' Identifies a new set of (activated) G's that can impose additional penalties
#
#' @description This function traverses the GG network to identify G's that can
#' assist the firms in a given vector A.victims. 
# ..........
# ARGUMENTS
# ..........
#' @param A.victims is a vector of firms to be attacked
#' @param GA.net the affiliation network of private protection
#' @param protectors a vector (possibly empty) of previously activated G's
#'
#' @return a vector with new G id numbers
#' @export

activateG <- function(A.victims, GA.net, protectors=c()) {
  
  if (!is.matrix(GA.net)) {
    GA.net <- as.matrix(GA.net)
  }
  numG <- dim(GA.net)[1]
  activeG <- c()          # initialize vector of new active G's
  
  for (j in A.victims) {
    for (k in 1:numG) {
      if (GA.net[k, j] > 0) {
        activeG <- union(activeG, k)
      }
    }
  }
  
  # # Delete G's that have been activated before
  # commonG <- intersect(activeG, protectors) # overlap b/w new and old protector sets
  # if (length(commonG)>0) {
  #   sameG <- setequal(activeG, commonG)  # is activeG a subset of old protectors?
  #   if (sameG) {
  #     activeG <- c()  # No new protectors
  #   } else {
  #     activeG <- sort(setdiff(activeG, commonG))
  #   }
  # }
  return(activeG)
}


#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 9: newPenalty(activatedG, G.dat)
#
#' Calculate cumulative penalty from activated G's
#
#' @description This function calcualtes the collective penalty that can be imposed
#' by a set of activated G's.
# ..........
# ARGUMENTS
# ..........
#' @param activatedG a vector of G id numbers
#' @param G.dat a data frame with information on G's
#' @return a non-negative cumulative penalty
#' @export

newPenalty <- function(activatedG, G.dat) {
  
  ## Allow empty G sets, in which case the penalty is nil
  newRho <- ifelse(length(activatedG) == 0,0,sum(G.dat$rho[activatedG]))
}


#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 10: protectedFirms(protectors, GA.net)
#
#' Identify firms protected by a set of G protectors
#
#' @description  This function traverses the network to find all firms that are protected
#' by a set of G protectors.
# ..........
# ARGUMENTS
# ..........
#' @param protectors A vector of IDs for active G's
#' @param GA.net An affiliated network bewteen G's and A's
#' @return a vector with related firm IDs
#' @export

protectedFirms <- function(protectors, GA.net) {
  A.set <- c()
  
  # check that the protectors vector is not empty, in which case return an empty vector
  if (length(protectors) == 0) {
    return(A.set)
  }
  
  # if protectors is not empty, then check whether they protect other firms
  if(!is.matrix(GA.net)) {
    GA.net <- as.matrix(GA.net)
  }
  n.A <- dim(GA.net)[2]
  
  for (k in protectors) {
    for (i in 1:n.A) {
      if (GA.net[k, i] > 0) {
        A.set <- union(A.set, i)
      }
    }
  }
  
  # It's still posible that a non-empty protectors vector produces a null outcome
  # If that's the case, there is no need to sort id's
  if (length(A.set) == 0) {
    return(A.set)
  } else {
    return(sort(A.set))
  }
}
