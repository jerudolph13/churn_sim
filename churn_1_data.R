
###########################################################################
#
# Project: How to handle churn
#
# Purpose: Simulate observed data
#
# Author: Jacqueline Rudolph
#
# Last Update: 23 Jul 2024
#
###########################################################################


#lib <- "~/R/4.3"
packages <- c("tidyverse", "msm", "tableone", "gmodels")
for (package in packages){
  #if (!(package %in% installed.packages(lib=lib))) {install.packages(package, lib=lib)}
  suppressPackageStartupMessages(library(package, character.only=T, quietly=T)) #lib.loc=lib
}

expit <- function(x) {1/(1+exp(-x))}

set.seed(123)

for (model in c("dag2.1", "dag2.2", "dag2.3", "dag2.4")) { #"dag1", 
for (outcome in c("transient", "permanent", "repeated")) {
    

# Define simulation parameters --------------------------------------------

reps <- 500     # Number of simulation iterations
n <- 1000        # Number of participants
followup <- 10   # Maximum followup (visits)

# Y = outcome
  # Reference probability of Y
  p.Y <- case_when(model %in% c("dag1", "dag2.1", "dag2.3") ~ -(1/0.28 - 1),
                   T ~ -(1/0.275 - 1))
  # Effect of B (standardized) on Y
  beta_B_Y <- case_when(model %in% c("dag1", "dag2.1", "dag2.3") ~ -log(2.0), 
                        model %in% c("dag2.2", "dag2.4") ~ log(2.0))    

# M = miss next visit 
  # Reference probability of M
  p.M <- -(1/0.4 - 1) 
  # Effect of M(t-1) on M
  beta_M_M <- log(3.0) 
  # Effect of B (standardized) on M
  beta_B_M <- case_when(model=="dag1" ~ 0, 
                        model %in% c("dag2.1", "dag2.2") ~ -log(3.0),
                        model %in% c("dag2.3", "dag2.4") ~ log(3.0))
  
# R = among those who miss next visit (M=1), drop out vs. stay in study
  # Reference probability of R
  p.R <- -(1/0.35 - 1)   
  # Effect of M(t-1) on R
  beta_M_R <- log(2.0) 
  # Effect of B (standardized) on R
  beta_B_R <- case_when(model=="dag1" ~ 0, 
                        T ~ -log(2.0))

  
# Generate baseline variables ---------------------------------------------

sim.rep <- function(rep) {
  
  id <- c(1:n)
  B <- rtnorm(n, 50, 10, lower=18, upper=80)   # Baseline age
  Bs <- (B - mean(B))/sd(B)
  base <- tibble(id = id,
                 B = B)


# Generate time-dependent variables ---------------------------------------

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
      
      # Outcome (no one has outcome at baseline)
      Y[(i-1)*(followup + 1) + j] <- 0
      everY <- 0
      
      # Determine if miss next visit 
      m <- rbinom(1, 1, expit(p.M + beta_B_M*Bs[i] + beta_M_M*M[(i-1)*(followup + 1) + j]))
      
      # Among those who will miss next visit, determine if they will drop out
      r <- ifelse(m==0, NA, rbinom(1, 1, expit(p.R + beta_B_R*Bs[i] + beta_M_R*M[(i-1)*(followup + 1) + j])))

    } else {
      M[(i-1)*(followup + 1) + j] <- m
      R[(i-1)*(followup + 1) + j] <- r
      
      # Outcome
      if (outcome=="repeated") {
        
        Y[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Y + beta_B_Y*Bs[i]))
        
      } else if (outcome=="permanent") {
        
        if (everY==0) {
          Y[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Y + beta_B_Y*Bs[i]))
          everY <- ifelse(Y[(i-1)*(followup + 1) + j]==1, 1, everY)
        } else {
          Y[(i-1)*(followup + 1) + j] <- 1
        }
        
      } else if (outcome=="transient") {
        if (everY==0) {
          
          Y[(i-1)*(followup + 1) + j] <- rbinom(1, 1, expit(p.Y + beta_B_Y*Bs[i]))
          everY <- ifelse(Y[(i-1)*(followup + 1) + j]==1, 1, everY)
          
        } else {
          
          Y[(i-1)*(followup + 1) + j] <- 0
          
        }
      }
      
      # Determine if miss next visit (M=1)
        # Once r=1, all future values of m=0
      m <- ifelse(r==1 & !is.na(r), 1,
                  rbinom(1, 1, expit(p.M + beta_B_M*Bs[i] + beta_M_M*M[(i-1)*(followup + 1) + j])))
      
      # Among those who will miss next visit, determine if they will drop out
        # once r=1, all future values of r=1
      lastr <- r
      r <- case_when(lastr==1 ~ 1,
                    m==0 ~ NA_real_, 
                    T ~ rbinom(1, 1, expit(p.R + beta_B_R*Bs[i] + beta_M_R*M[(i-1)*(followup + 1) + j])))

    }
  }
}

sim.dat <- tibble(id = rep(id, followup + 1)[order(rep(id, followup + 1))],
                  t = rep(c(0:followup), n),
                  Y = Y,
                  M = M,
                  R = R,
                  rep = rep) %>% 
  left_join(base, by="id")

return(sim.dat)

}


sim.dat <- lapply(1:reps, function(x){sim.rep(x)})
sim.dat <- bind_rows(sim.dat)

write_csv(sim.dat, paste0("../data/", model, "_", outcome, ".csv"))


# When using test simulation ----------------------------------------------

# Visualize visit pattern
pattern <- sim.dat %>%
  group_by(id) %>%
  summarize(cumM = sum(M==0)) %>%
  filter(!duplicated(id, fromLast=T))
last <- sim.dat %>%
  filter(M==0) %>%
  filter(!duplicated(id, fromLast=T))
fig <- sim.dat %>%
  left_join(pattern, by="id") %>%
  mutate(pattern_id = paste0(cumM, "_", id))
ggplot(data=fig, aes(x=t, y=pattern_id, fill=as.factor(M))) +
 geom_tile() +
 scale_fill_manual(values=c("black", "white"))

# Simulation characteristics

#CreateTableOne(data=sim.dat, vars=c("Z", "Y"), strata="M")
#CreateTableOne(data=sim.dat, vars=c("B1", "B2", "Z", "M"), strata="Y")

# obs <- filter(sim.dat, M==0) %>%
#   group_by(id) %>%
#   mutate(m_bw = t - lag(t))
# summary(obs$m_bw)
# mean(obs$G)
# CreateTableOne(data=obs, vars=c("B1", "B2", "Z", "Y"), strata="G")

}}
