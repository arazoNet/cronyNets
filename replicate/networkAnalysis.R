# Filename:  network_level_analysis.R
# date:  Jan 13, 2020

# load libraries
library(MASS)
library(ggplot2)
library(gridExtra)
library(pscl)
library(stargazer)
library(foreign)


# load simulation data

#load data
load("data/simData.RData")
n <- dim(simData)[1]



cat("...tidying up dataset...\n")

#rename ptie.max
names(simData)[5] <- "GA_prob"
#rename exog
names(simData)[11] <- "privileged"
# swap periods for underscores
names(simData)[27] <- "rho_avg"
names(simData)[34] <- "GA_d"

names(simData)[36] <- "AA_d"
names(simData)[44] <- "AA_reach"
names(simData)[40] <- "AA_comp"
names(simData)[41] <- "AA_comp_max"
names(simData)[56] <- "a0_R"
names(simData)[62] <- "a0_rho_avg"
names(simData)[63] <- "a0_rho_sum"
names(simData)[59] <- "a0_reach"
names(simData)[57] <- "a0_deg"

names(simData)[46] <- "GG_d"
names(simData)[54] <- "GG_reach"
names(simData)[50] <- "GG_comp"
names(simData)[51] <- "GG_comp_max"
names(simData)[60] <- "a0_G"
names(simData)[64] <- "a0_G_deg"
names(simData)[66] <- "a0_G_reach"


#biased attacks dummy
simData$bigTarget <- ifelse(simData$attack.mode=="biased",1,0)

# variables to keep for statistical analysis
keepVars <- c("victims",  # dependent variable
              "protectors", # activated protectors
              "protectProb","privileged","bigTarget", # controls
              "rho_avg","GA_d",
              "AA_d","AA_reach","AA_comp","AA_comp_max",   # AA propagation
              "a0_R","a0_rho_avg","a0_rho_sum","a0_reach","a0_deg", # a0 propagation
              "GG_d","GG_reach","GG_comp","GG_comp_max",   # GG retaliation
              "a0_G","a0_G_deg","a0_G_reach")       # a0 retaliation



#--- FIGURES


fig.ga.smooth <- qplot(GA_prob, victims, data= simData,geom = "smooth") 
fig.aa.smooth <- qplot(AA_d, victims, data= simData,geom = "smooth") 
fig.gg.smooth <- qplot(GG_d, victims, data= simData,geom = "smooth") 
fig.ga.raw <- qplot(GA_prob, victims, data= simData,geom = "jitter") 
fig.aa.raw <- qplot(AA_d, victims, data= simData,geom = "jitter") 
fig.gg.raw <- qplot(GG_d, victims, data= simData,geom = "jitter") 

fig.grid1 <- grid.arrange(h1,fig.ga.smooth,fig.aa.smooth,fig.gg.smooth)
ggsave("predation_summary.png",plot=netsim_fig.grid1)


# STATISTICAL ANALYSIS
# ZINB model

crony.net.A <- zeroinfl(victims ~ GA_prob + rho_avg 
                        + privileged + bigTarget + privileged*bigTarget
                  | AA_comp,
                   data = simData, dist = "negbin", EM = TRUE)

crony.net.B <- zeroinfl(victims ~ GA_prob + rho_avg 
                        + privileged + bigTarget + privileged*bigTarget
                        + AA_reach + GG_reach + rho_avg*GG_reach
                        | AA_comp,
                        data = simData, dist = "negbin", EM = TRUE)

crony.net.C <- zeroinfl(victims ~ GA_prob + rho_avg 
                        + privileged + bigTarget + privileged*bigTarget
                        + AA_comp_max + GG_comp_max + rho_avg*GG_comp_max
                        | AA_comp,
                        data = simData, dist = "negbin", EM = TRUE)

# Save models
save(crony.net.A,crony.net.B,crony.net.C, file = "analysis/crony_net.RData")

