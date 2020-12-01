# Filename:  predationSimulations.R
# Date: January 13, 2020
# Purpose:  Simulate predation attacks on societies defined in paramsGRID

# Load societal parameters
load("paramsGrid.RData")   # params has 4K configurations

# Run simulations

library(cronyNets)
set.seed(0117)

# ------------------
# Break up large simulation into smaller ranges

start <- seq(1,1600,05)
end <- seq(05,1600, 05)
#end <- seq(2000,160000,2000)
simRanges <- cbind(start, end)
nRanges <- nrow(simRanges)

nReps <- 25
# Run batch simulations and store in a scratch folder
# merge smaller datasets into a single file for statistical analysis
simData005 <- netsim(params.GRID[simRanges[1,1]:simRanges[1,2],],nReps)
save(simData005, file="scratch/simData005.RData")
simData <- simData005
save(simData, file="scratch/simData.RData", safe=T)


for (s in 2:nRanges) {
  if (s < 20) {
    batchName <- paste("scratch/simData00",simRanges[s,2], ".RData",sep="")
  } else if (s < 200) {
    batchName <- paste("scratch/simData0",simRanges[s,2], ".RData",sep="")
  } else {
    batchName <- paste("scratch/simData",simRanges[s,2], ".RData",sep="")
  }
  batchSims <- netsim(params.GRID[simRanges[s,1]:simRanges[s,2],],nReps)
  save(batchSims,file = batchName)
  simData <- rbind(simData, batchSims)
  save(simData, file="scratch/simData.RData", safe=T)
}

