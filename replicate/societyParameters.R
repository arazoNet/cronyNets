# Filename:  societyParameters.R
# Date: January 13, 2020
# Purpose:  generate a grid of parameter configurations for input into netsim

library(cronyNets)
# Create Grid
params.GRID <- societyParamsGrid(numA=100,                      # 100 Firms (idea: each contributes 1 unit to pay for Cd)
                                 numG=100,                      # 100 Protectors (if rho.base=1, then a firm needs 9 protectors
                                 # thwart an attack; the number increases when rho.base < 1
                                 ptie.max=seq(0.005,0.20,0.005),  # variable probabilities of ties between A's & G's
                                 Cd=100,                        # a fixed cost of running the government
                                 rho.base=seq(.1,1,.1)  ,  # a variable individual penalty (enforcement effectiveness)
                                 attack.mode=c("rnd","biased"),   # D will attack both at random 
                                 attack.N=c("single"),     # D will attack a single firm at random (per paper's language)
                                 ptie.mode=c("indep"),# density-conditioned and independent ties
                                 exog=c(1,0))                   # random and biased rent distribution
save(params.GRID, file="data/paramsGrid.RData")        
