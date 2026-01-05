
###########################################################################
#
# Project: How to handle data gaps
#
# Purpose: Simulate data
#
# Author: Jacqueline Rudolph
#
# Last Update: 18 Sep 2025
#
###########################################################################


packages <- c("tidyverse", "msm", "tableone", "gmodels")
for (package in packages){
  suppressPackageStartupMessages(library(package, character.only=T, quietly=T))
}

expit <- function(x) {1/(1+exp(-x))}


for (model in c("dag1")) {
                #"dag2.1", "dag2.2", "dag2.3", "dag2.4" # Baseline common cause
                #"dag3", "dag3.1", "dag3.2",            # Time-varying common cause
                #"dag4", "dag4.1)) {                    # Outcome affects missingness
for (outcome in c("transient", "permanent", "repeated")) {
    
set.seed(123)
  
  
# Define simulation parameters --------------------------------------------

reps <- 1000     # Number of simulation iterations
n <- 1000        # Number of participants
followup <- 10   # Maximum followup (visits)

# Y = outcome
  # Reference probability of Y
  p.Y <- case_when(model %in% c("dag2.2", "dag2.4") ~ -(1/0.275 - 1), 
                   T ~ -(1/0.28 - 1))
  # Effect of B (standardized) on Y
  beta_B_Y <- case_when(model %in% c("dag2.2", "dag2.4") ~ log(2.0),
                        T ~ -log(2.0))  
  # Effect of Z on Y
  beta_Z_Y <- case_when(model=="dag3.1" ~ log(4.0),
                        T ~ log(2.0))

# M = miss next visit 
  # Reference probability of M
  p.M <- -(1/0.4 - 1) 
  # Effect of M(t-1) on M
  beta_M_M <- log(3.0) 
  # Effect of B (standardized) on M
  beta_B_M <- case_when(model=="dag1" ~ 0, 
                        model %in% c("dag2.3", "dag2.4") ~ log(3.0),
                        T ~ -log(3.0))
  # Effect of Z on M
  beta_Z_M <- case_when(model %in% c("dag3", "dag3.2", "dag3.3",
                                     "dag4", "dag4.1") ~ log(2.0),
                        model=="dag3.1" ~ log(4.0),
                        T ~ 0)
  # Effect of Y on M
  beta_Y_M <- case_when(model=="dag4" ~ log(4.0),
                        model=="dag4.1" ~ log(8.0),
                        T ~ 0)
  
# R = among those who miss next visit (M=1), drop out vs. stay in study
  # Reference probability of R
  p.R <- case_when(model=="dag3.2" ~ -(1/0.25 - 1),
                   model=="dag3.3" ~ -(1/0.55 - 1),
                   T ~ -(1/0.35 - 1))  
  # Effect of M(t-1) on R
  beta_M_R <- log(2.0) 
  # Effect of B (standardized) on R
  beta_B_R <- case_when(model=="dag1" ~ 0, 
                        T ~ -log(2.0))
  # Effect of Z on R
  beta_Z_R <- case_when(model %in% c("dag3", "dag3.2", "dag3.3",
                                     "dag4", "dag4.1") ~ log(2.0),
                        model=="dag3.1" ~ log(4.0),
                        T ~ 0)
  
# Z = time-varying variable also measured at visits (like depressive symptoms) 
  # Reference probability of Z
  p.Z <- -(1/0.45 - 1)   
  # Effect of Z(t-1) on Z
  beta_Z_Z <- log(3.0)
  
  
# Generate baseline variables ---------------------------------------------

sim.rep <- function(sim) {
  
  id <- c(1:n)
  B <- rtnorm(n, 50, 10, lower=18, upper=80)   # Baseline age
  Bs <- (B - mean(B))/sd(B)
  base <- tibble(id = paste0(id, "_", sim),
                 B = B)


# Generate time-dependent variables ---------------------------------------

Z <- rep(NA, n*(followup + 1))
Y <- rep(NA, n*(followup + 1))
M <- rep(NA, n*(followup + 1))
R <- rep(NA, n*(followup + 1))

for (i in 1:n) {
  for (j in 1:(followup+1)) { # j=1 corresponds to t=0
    
    # Generate variables at baseline, t=0
    if (j==1) {
      # Indicator for being unobserved (everyone observed at t=0)
      M[(i-1)*(followup + 1) + j] <- 0
      
      # Indicator for having dropped out (no one dropped out at t=0)
      R[(i-1)*(followup + 1) + j] <- 0
      
      # Indicator for time-varying covariate
      Z[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Z))
      
      # Outcome (no one has outcome at baseline)
      Y[(i-1)*(followup + 1) + j] <- 0
      everY <- 0
      
      # Determine if miss next visit 
      m <- rbinom(1, 1, expit(p.M + 
                              beta_B_M*Bs[i] + 
                              beta_M_M*M[(i-1)*(followup + 1) + j] +
                              beta_Z_M*Z[(i-1)*(followup + 1) + j] +
                              beta_Y_M*Y[(i-1)*(followup + 1) + j]))
      
      # Among those who will miss next visit, determine if they will drop out
      r <- ifelse(m==0, NA, rbinom(1, 1, expit(p.R + 
                                               beta_B_R*Bs[i] + 
                                               beta_M_R*M[(i-1)*(followup + 1) + j] +
                                               beta_Z_R*Z[(i-1)*(followup + 1) + j])))

    } else {
      M[(i-1)*(followup + 1) + j] <- m
      R[(i-1)*(followup + 1) + j] <- r
      
      # Indicator for time-varying covariate
      Z[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Z + 
                                                        beta_Z_Z*Z[(i-1)*(followup + 1) + j - 1]))
      
      # Outcome
      if (outcome=="repeated") {
        
        Y[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Y + 
                                                          beta_B_Y*Bs[i] +
                                                          beta_Z_Y*Z[(i-1)*(followup + 1) + j] +
                                                          beta_Z_Y*Z[(i-1)*(followup + 1) + j - 1]))
        
      } else if (outcome=="permanent") {
        
        if (everY==0) {
          Y[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Y + 
                                                            beta_B_Y*Bs[i] +
                                                            beta_Z_Y*Z[(i-1)*(followup + 1) + j] +
                                                            beta_Z_Y*Z[(i-1)*(followup + 1) + j - 1]))
          
          everY <- ifelse(Y[(i-1)*(followup + 1) + j]==1, 1, everY)
          
        } else { # Once you have Y==1, always Y==1
          Y[(i-1)*(followup + 1) + j] <- 1
          
        }
        
      } else if (outcome=="transient") {
        
        if (everY==0) {
          
          Y[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Y + 
                                                            beta_B_Y*Bs[i] +
                                                            beta_Z_Y*Z[(i-1)*(followup + 1) + j] +
                                                            beta_Z_Y*Z[(i-1)*(followup + 1) + j - 1]))
          
          everY <- ifelse(Y[(i-1)*(followup + 1) + j]==1, 1, everY)
          
        } else { # Outcome only detected at the time it happened
          
          Y[(i-1)*(followup + 1) + j] <- 0
          
        }
      }
      
      # Determine if miss next visit (M=1)
        # Once r=1, all future values of m=0
      m <- case_when(r==1 & !is.na(r) ~ 1,
                     T ~ rbinom(1, 1, expit(p.M + 
                                            beta_B_M*Bs[i] + 
                                            beta_M_M*M[(i-1)*(followup + 1) + j] +
                                            beta_Z_M*Z[(i-1)*(followup + 1) + j] +
                                            beta_Z_M*Z[(i-1)*(followup + 1) + j - 1] +
                                            beta_Y_M*Y[(i-1)*(followup + 1) + j])))
      
      # Among those who will miss next visit, determine if they will drop out
        # once r=1, all future values of r=1
      lastr <- r
      r <- case_when(lastr==1 ~ 1,
                     m==0 ~ NA_real_, 
                     T ~ rbinom(1, 1, expit(p.R + 
                                            beta_B_R*Bs[i] + 
                                            beta_M_R*M[(i-1)*(followup + 1) + j] +
                                            beta_Z_R*Z[(i-1)*(followup + 1) + j] +
                                            beta_Z_R*Z[(i-1)*(followup + 1) + j - 1])))

    }
  }
}

sim.dat <- tibble(sim_rep = sim,
                  id = paste0(rep(id, followup + 1)[order(rep(id, followup + 1))], "_", sim_rep),
                  t = rep(c(0:followup), n),
                  Z = Z,
                  Y = Y,
                  M = M,
                  R = R) %>% 
  left_join(base, by="id")

return(sim.dat)

}

sim.dat <- lapply(1:reps, function(x){sim.rep(x)})
sim.dat <- bind_rows(sim.dat)

write_csv(sim.dat, paste0("../data/", model, "_", outcome, ".csv"))

}}
