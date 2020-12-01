#/////////////////////////////////////////////////////////////////////
# Filename:   cronyNets_SocietySetup.R 
# Purpose:    create a Society object to simulate predation attacks
#
# ___________________________
# ---- LIST OF FUNCTIONS ----
# ___________________________
#
# 1) newSociety(numA, numG, ptie.max, Cd, rho, attack.mode, attack.N, ptie.mode, exog)
# 2) societalContraints(Cd, numG)
# 3) createNetworks(numA, numG, ptie.max, ptie.mode)
# 4) A_Data(Cd, exog, params, GA.mat)
# 5) G_Data(rho, exog, params, GA.mat, A.dat)
# 6) D_Data(Cd, A.dat)
#
#/////////////////////////////////////////////////////////////////////



# BEGIN FUNCTION ....................................................
# function 1: newSociety(numA, numG, ptie.max, Cd, rho, attack.mode, attack.N, ptie.mode, exog)
#
#' Create a society object to simulate predation attacks
#
#' @description This function creates an S3 'society' class, a list with various societal-level
#' components such as the associated GA network, the derived AA network that ties A's with common
#' protectors, the derived overlapping protection GG network that links G's.  This class also
#' contains node-specific attributes such as a firm's rents and a protector's rho (penalty) capacity.
# ..........
# ARGUMENTS
# ..........
#' @param numA No. of firms
#' @param numG No. of third-party enforcers
#' @param ptie.max (maximum) probability of a tie between G and A
#' @param Cd Dictator's incumbency cost
#' @param rho baseline strength for a given G (penalty per firm)
#' @param attack.mode in c("rnd", "biased") specifies how to select potential predation victims
#' @param attack.N in c("single", "all") specifies how many firms to use for attack summary
#' @param ptie.mode equal tie probabilities ('rnd') or ('biased') net formation?
#' @param exog dummy variable to account for exogenous or endogenous rents
#'
#' @return A society list, which includes: 
#' (i) three network structures for GA affiliation network, related A's (AA), related G's (GG)
#' (ii) three data tables for actors A, G, and D
#' (iii) four original parameters about tie probabilities and attack type
#' 
#' @examples 
#' # set up a society with 20 asset holders and 10 private enforcers with prob of GA ties to 0.01
#' #' mySociety <- newSociety(numA=20, numG=10, ptie.max=0.01)
#' # set up a society with 20 asset holders and 10 private enforcers with prob of GA ties to 0.01
#' # plus other specified arguments
#' \dontrun{
#' mySociety <- newSociety(numA=20, numG=10, ptie.max=0.01, Cd=100,rho=1,
#'                         attack.mode="rnd",attack.n="single", ptie.mode="indep",exog=0)
#' }                         
#' @export

newSociety <- function(numA, numG, ptie.max, Cd=100,rho=1,
                  attack.mode="rnd", attack.N="single", ptie.mode="density",exog=1) {

## Derive parameters, network structures, and data frames ----
society.constraints <- societalConstraints(Cd, numG)  # derives tau.AVG, b.fee, R.star, rho.star
society.networks <- createNetworks(numA, numG, ptie.max, ptie.mode)
GA.mat <- as.matrix(society.networks$GA)
A.dat <- A_Data(Cd, exog, society.constraints, GA.mat)
G.dat <- G_Data(rho, exog, society.constraints, GA.mat, A.dat)
D.dat <- D_Data(Cd, A.dat)


return(list(GA=society.networks$GA.net,
            GG=society.networks$GG.net,
            AA=society.networks$AA.net,
              A=A.dat, G=G.dat, D=D.dat,
              attack.mode=attack.mode,
              attack.N=attack.N,
              ptie.mode=ptie.mode,
              ptie=ptie.max,
              exog=exog, 
              Cd=Cd,
              b.fee=society.constraints$b.fee,
              R.star=society.constraints$R.star,
              rho.star=society.constraints$rho.star,
              t=society.constraints$tau.AVG))
}
#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 2: societalConstraints(Cd, numG)
#
#' Establish societal parameters (taxes, etc.) to induce players to participate
#
#' @description This function calculates values that ensure that this society has the right parameters
#' for all actors to participate, and thus run a simulation.
# ..........
# ARGUMENTS
# ..........
#' @param Cd D's incumbency cost
#' @param numG No. of third-party enforcers
#' @return four societal parameters that ensure participation constraints
#' @family participant data creation functions
#' 
#' @export


societalConstraints <- function(Cd, numG) {
    
#   b.fee <- 1/(2*numG - 1)  
#   R.star <- 2*Cd/(1-b.fee)
#   rho.star <- ((1+b.fee)/(1-b.fee))*Cd
tau.AVG <- 0.1
b.fee <- (1-tau.AVG)/numG
#b.fee <- 1/(2*numG-1)
R.star <- Cd/tau.AVG
rho.star <- (1-tau.AVG)*R.star  # from 10/19 new RHO calculations
#rho.star <- (2*numG/(2*numG-2))*Cd

return(list(tau.AVG=tau.AVG, b.fee=b.fee, R.star=R.star, rho.star=rho.star))
}
#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 3: createNetworks(numA, numG, ptie.max, ptie.mode)
#
#' Create affiliation (two-mode) and one-mode networks to link A's and G's
#
#' @description This is a wrapper function that creates all the embedded networks
#' in a society object GA, AA, and GG) by passing required inputs to the functions
#' \code{link{networkAA}}, \code{link{networkAA}}, and \code{link{networkAA}}, respectively.
# ..........
# ARGUMENTS (passed from setupSociety function)
# ..........
#' @param numA No. of firms
#' @param numG No. of third-party enforcers
#' @param ptie.max (maximum) probability of a tie between G and A
#' @param ptie.mode equal tie probabilities ('rnd') or 'biased' net formation?
#'
#' @return A list with three network structures and a dummy variable
#' (i) An affiliation matrix linking G's to A's
#' (ii) A one-mode network of A's
#' (iii) A one-mode network of G's
#' (iv) empty.net=1 if there are no links between G's and A's
#' @export


createNetworks <- function(numA, numG, ptie.max, ptie.mode) {

  GA <- matrixGA(numA, numG, ptie.max, ptie.mode)
  empty.net <- (sum(GA)==0)  # use to track empty random networks
  GG <- networkGG(GA)
  AA <- networkAA(GA)
  GA <- networkGA(GA)


	return(list(GA.net=GA, empty.net=empty.net, GG.net=GG, AA.net=AA))
}
#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 4: A_Data(Cd, exog, society.constraints, exog, GA.mat)
#
#' Collect data to describe all A's
#
#' @description This function creates a data frame with a variety of firm-level
#' attributes such as rent and the number of associated G's.
# ..........
# ARGUMENTS
# ..........
#' @param Cd D's incumbency cost
#' @param exog exogenous rent generation flag
#' @param society.constraints societal parameters to induce participation
#' @param GA.mat affiliation matrix (Gs: rows, As: columns)
#' 
#' @return  A data frame with NINE columns
#' (1) id:  a firm number
#' (2) R:  rents
#' (3) R.base: minimum individual rent to induce G, D to participate
#' (4) v: reservation value (as % of rents)
#' (5) t: tax rate (paid to Cd)
#' (6) b: total private protection fees (paid to linked G's)
#' (7) I: equals 1 if firm wants to invest
#' (8) Gs: Number of (hired) private enforcers
#' (9) prey: equals 1 if D prays on firm "id"
#' 
#' @family participant data creation functions
#' @export


A_Data <- function(Cd, exog, society.constraints, GA.mat) {

  numA <- dim(GA.mat)[2]
  A.dat <- data.frame(matrix(NA, nrow=numA, ncol=9))
  names(A.dat) <- c("id", "R", "R.base", "v", "t", "b", "I", "Gs", "prey")
  empty.net <- (sum(GA.mat)==0)  # use to track empty random networks
  b.fee <- society.constraints$b.fee
  tau.AVG <- society.constraints$tau.AVG
  R.star <- society.constraints$R.star

  A.dat$id <- 1:numA
  
  A.dat$v <- 0 # ((1-b.fee)/2)# just capture fraction here; technically: it's vR (*A.dat$R)
 
  A.dat$t <- tau.AVG # Same for ALL in this simulation, but could vary by firm in general
  A.dat$Gs <- apply(GA.mat,2, sum)

  numProtectors <- sum(A.dat$Gs)  # Used to avoid division by zero in calculation of R.base

#R.base DEPRECATED
  A.dat$R.base <- ifelse(numProtectors==0,0,Cd/sum(A.dat$t*A.dat$Gs)) # No rents <=> empty GA networks

#Exogenous or Endogenous rents ----

  if (exog==0) {  # ENDOGENOUS Networks:  Rents are related to connections
    if (empty.net==TRUE) {
      R.wt <- rep(0,numA)
    } else {
      R.wt <- A.dat$Gs/sum(A.dat$Gs) # a type of normalized "degree" centrality
    }
    A.dat$R <-  R.star*R.wt  
  } 
  else {
    A.dat$R <- R.star/numA  # R.star divided equally among all firms
  }
  A.dat$b <- b.fee*A.dat$Gs

# if participation constraint satisfied: I(invest)=1 ----
# By construction, A_i should always invest
  A.dat$I <- ifelse((1-b.fee-A.dat$t-A.dat$v)*A.dat$R>=0,1,0)
  A.dat$prey <- 0  # Dummy=1 for attacked firm; initialized at '0'

  return(A.dat)
}
#....................................................... END FUNCTION

# BEGIN FUNCTION ....................................................
# function 5: G_Data(rho, exog, society.constraints, GA.mat, A.dat)
#
#' Collect data to describe all G's
#
#' @description This function creates a data frame with various attributes
#' such as a protector's own capacity to punish the dictator, the number of
#' protected A's, etc.
# ..........
# ARGUMENTS
# ..........
#' @param rho:  individual punishment capacity
#' @param exog: exogenous rent generation
#' @param society.constraints societal parameters to induce participation
#' @param GA.mat: affiliation matrix (Gs: rows, As: columns)
#' @param A.dat: firm-level data
#'
#' @return a data frame with FIVE columns
#' (1) id:  G's "name"
#' (2) rho: private punishment capacity
#' (3) As: number of protected firms
#' (4) bR: private protection payoffs
#' (5) enforce: equals 1 if G is willing to enforce
#' 
#' @family participant data creation functions
#' 
#' @export

G_Data <- function(rho, exog, society.constraints, GA.mat, A.dat) {

  numG <- dim(GA.mat)[1]
  G.dat <- data.frame(matrix(NA, nrow=numG, ncol=5))
  names(G.dat) <- c("id", "rho", "As", "bR", "enforce")
  G.dat$id <- 1:numG
  G.dat$As <- apply(GA.mat,1,sum)
  empty.net <- (sum(GA.mat)==0)  # use to track empty random networks
  rho.star <- society.constraints$rho.star
  b.fee <- society.constraints$b.fee
  
## Exogenous/Endogenous case ------

  if (exog==0) {  # Rho's are related to connections
    if (empty.net==TRUE) {
      rho.wt <- rep(0,numG)
    } else {
      rho.wt <- G.dat$As/sum(G.dat$As)
    }
    G.dat$rho <- rho*rho.star*rho.wt
  } 
  else {
    G.dat$rho <- rho*rho.star/numG  # rho.star divided equally among all G's
  }
#   G.dat$rho <- (rho*Cd)/numG   # rho is collective capacity, divide by numG
  R.0 <- A.dat$R.base[1] # base rent, same for all firms with protectors
  G.dat$bR <- b.fee*G.dat$As#*R.0
  G.dat$enforce <- 1  # Dummy=1 for G that is a willing enforcer (for future development)

  return(G.dat)
}
#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 6: D_Data(Cd, A.dat)
#
#' Collect data to describe D
#
#' @description This function creates a list with government-specific attributes.
#' 
#' At this point, these variables are immutable, but in future development, these
#' variables could be updated (e.g., prey.gain)
# ..........
# ARGUMENTS
# ..........
#' @param Cd:  D's incumbency cost
#' @param A.dat:  a data frame with firm-level information,
#'         esp. paid tax rates and individual rents
#' @return  a data frame with SIX columns
#' (1) Cd:  D's imcumbency cost
#' (2) t: average "tax rate" charged to A's
#' (3) tR: total revenue from all firms
#' (4) promise: equals 1 if D promises protection (Cd is covered)
#' (5) prey.gain: initialized at zero (before game begins)
#' (6) prey.loss: initialized at zero (before game begins)
#' 
#' @family participant data creation functions
#' @export

D_Data <- function(Cd, A.dat) {

# calculate total "tax" revenue from all firms ----
tR <- round(sum(A.dat$t*A.dat$R))

# Promise protection only if budget constraint is met ----
promise <- ifelse(tR>=Cd,1,0)  

return(list(Cd=Cd, t=mean(A.dat$t),tR=tR, promise=promise,
  				prey.gain=0,prey.loss=0))

}
#....................................................... END FUNCTION