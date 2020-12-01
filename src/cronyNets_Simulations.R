#/////////////////////////////////////////////////////////////////////
# Filename:   cronyNets_Simulations.R
# Purpose:    Functions to seed and run simulated attacks
#
#
# ___________________________
# ---- LIST OF FUNCTIONS ----
# ___________________________
#
# 1) cronyNets(params.GRID, reps)
# 2) cronysimple(numA, numG, ptie.max)
# 3) societyParamsGrid(numA, numG, ptie.max, Cd, rho.base, attack.mode, attack.N, ptie.mode, exog, saveFile="")


# BEGIN FUNCTION ....................................................
# function 1: cronyNets(params.GRID, REPS)
#
#' Simulate predation impact of GA network structure(s)
#' 
#' @description This is the main cronyNets function to simulate a predation attack and return simulation results.
#' 
#' The end user does not need to use any other functions, except to generate a parameter GRID using
#' 
#' \code{\link{societyParamsGrid}}.
# ..........
# ARGUMENTS
# ..........
#' @param params.GRID Data frame with parametric configurations for societySetup
#' 
#' @param REPS number of repetitions per societal configuration
#' 
#' @return A data frame with simulation results for REPS repetitions for each params.GRID configuration
#' @examples
#' \dontrun{
#' sim1 <- cronyNets(params.GRID, reps = 50)
#' }
#' @note A wrapper function to run various configurations
#' @export

cronyNets <- function(params.GRID, REPS = 1) {
  
  mc.cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores=mc.cores)
  
  #nSocieties <- dim(params.GRID)[1]
  nSocieties <- dim(params.GRID)[1]
  
  # # Collect configuration and run information
  configData <- function(s, r) {
    data.frame(s, r, params.GRID[s,])
  }
  
  simID <- foreach::foreach(r = 1:REPS, .combine=rbind) %:% foreach::foreach(s = 1:nSocieties) %dopar% configData(s, r)
  simID <- data.frame(do.call(rbind, simID))
  names(simID)[1:2] <- c("s", "r")
#  simID.config <- do.call(rbind, replicate(REPS, params.GRID, simplify=FALSE))
#  simID <- cbind(simID, simID.config)

  # simulated societies
  # paramLists <- iter(params.GRID, by = "row")
  # paramLists <- BBmisc::convertRowsToList(params.GRID)
  aSociety <- function(s) {
    do.call(newSociety, params.GRID[s,])
  }
  allSocieties <- foreach::foreach(r = 1:REPS, .combine = c) %:% foreach::foreach(s = 1:nSocieties) %dopar% aSociety(s)
  iterSocieties <- iter(allSocieties)

  # simulated attacks
  newAttack <- function(s) {
    data.frame(societyAttack(s))
  }
  attackSummaries <- foreach::foreach(s = iterSocieties, .combine=rbind) %dopar% newAttack(s)
  cbind(simID, attackSummaries)
}
#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 2: cronysimple(numA, numG, ptie.max)
#
#' A simpler version of cronyNets that takes three basic inputs
#' 
#' @description This is a simpler version of \code{\link{cronyNets}}, which does not require a parameter vector.
#' The user only needs to provide numbers for A's and G's and a probability of G<->A ties.
#' 
#' Missing societal parameters are supplied automatically using default options in \code{\link{societyParamsGrid}}
# ..........
# ARGUMENTS
# ..........
#' @param numA  No. of firms
#' @param numG  No. of third-party enforcers
#' @param ptie.max Probability of a tie between G's and A's
#'
#' @return a data matrix with simulation results
#' @export
#'
#' @examples
#' \dontrun{
#' sim1 <- cronysimple(100,50,0.05)  # 100 A's, 50 G's, Pro(G<->A) = 0.05
#' }

cronysimple <- function(numA, numG, ptie.max) {
  params <- societyParamsGrid(numA, numG, ptie.max)
  cronyNets(params)
}
#....................................................... END FUNCTION


# BEGIN FUNCTION ....................................................
# function 3: societyParamsGrid(numA, numG, ptie.max, Cd, rho.base, attack.mode, ptie.mode, exog, saveFile="")
#
#' Create multiple societal configurations for predation simulations
#' 
#' @description This function takes possible values for 9 configuration variable values
#' to construct a data frame with all value combinations.
# ..........
# ARGUMENTS
# ..........
#' @param numA num of A's (asset holders), either a single number or a vector of numbers
#' 
#' @param numG num of G's (private enforcers), either a single number or a vector of numbers
#' 
#' @param ptie.max probability of a tie between one A & one G, either a single number or a vector of numbers
#' 
#' @param rho.base baseline (per firm) private penalty imposed by each G, either a single number or a vector of numbers
#'
#' @param Cd D's cost of staying in office, either a single number or a vector of numbers
#' 
#' @param attack.mode a value in c("rnd", "biased"), used to specify how predation victims will be selected
#' 
#' @param attack.N in c("single", "all") to denote how many firms to attack for simulated society
#' 
#' @param ptie.mode any or all values c("density", "indep"), used to determine sampling scheme for GA network formation process 
#' 
#' @param exog any or all values in c(0,1): exog=1 if rent (and rho distributions) are exogenous,
#'         used as a flag variable to trigger different society setups
#'         
#' @param saveFile a string "fileName" to save results as "fileName.RData"         
#' 
#' @return A data frame with all combinations of given configuration variables
#' 
#' @examples
#' \dontrun{
#' grid <- societyParamsGrid(numA=c(25,50), # will try societies with 25 and 50 asset holders
#'               numG=10,                       # but a fixed number of G's: 50
#'               ptie.max=seq(0.01,0.25,0.01),  # variable probabilities of ties between A's & G's
#'               Cd=100,                        # a fixed cost of running the government
#'               rho.base=0.1,                  # a fixed rho
#'               attack.mode=c("rnd", "biased"),# D will attack both in random and biased fashion
#'               attack.N=c("single", "all"),   # D will attack single and all firms
#'               ptie.mode=c("indep"),          # independent GA[i,j] probabilities
#'               exog=c(0,1),                   # will try both exogenous and endogenous rent setup
#'               saveFile="simGrid")            # save grid as "simGrid.RData" for future retrieval
#' }
#' @export

societyParamsGrid <- function(numA, numG, ptie.max, 
                              Cd = 100, rho.base = 1, 
                              attack.mode = "rnd", attack.N = "single", 
                              ptie.mode = "rnd", exog = 1, saveFile="simParams") {
  params.GRID <- expand.grid(numA=numA, 
                             numG=numG,
                             ptie.max=ptie.max,
                             Cd=Cd,
                             rho=rho.base,
                             attack.mode=attack.mode, 
                             attack.N=attack.N, 
                             ptie.mode=ptie.mode,
                             exog=exog, 
                             stringsAsFactors=F)
  if (saveFile != "") {
    filename=paste(saveFile,".RData",sep="")
    save(params.GRID, file=filename)  
  }
  return(params.GRID)
}
#....................................................... END FUNCTION


