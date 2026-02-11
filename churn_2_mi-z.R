
###########################################################################
#
# Project: How to handle gaps
#
# Purpose: Run multiple imputation (Z included)
#
# Author: Jacqueline Rudolph
#
# Last Update: 08 Jan 2026
#
###########################################################################

  # NOTE: Slight tweaks have been made to this code since publication. Numerical results 
  #       differ but findings are consistent.

packages <- c("tidyverse", "survival", "broom", "zoo", "splines", "mice", "parallel")
for (package in packages){
  suppressPackageStartupMessages(library(package, character.only=T, quietly=T))
}

cores <- 10


# Set-up for analysis -----------------------------------------------------

nsim <- 1000    # Number of simulations
M <- 50       # Number of imputations

set.seed(123)

model <- "dag4.1" # Options: "dag1", 
                  #          "dag2.1", "dag2.2", "dag2.3", "dag2.4", 
                  #          "dag3", "dag3.1", "dag3.2", "dag3.3",
                  #          "dag4", "dag4.1"

for (outcome in c("transient", "permanent", "repeated")) { 
  
  
# Read in data ------------------------------------------------------------

dat <- read_csv(paste0("../data/", model, "_", outcome, ".csv"))
  
rep.res <- function(r) {
  dat.r <- filter(dat, sim_rep==r)
  
# True natural course -----------------------------------------------------
  
  true.dat <- dat.r %>% 
    group_by(id) %>% 
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = lag(t)) %>% 
    filter(cumY<=1) %>% 
    filter(t>0) %>% 
    ungroup()
  
  true.risk <- tidy(survfit(Surv(last_t, t, Y) ~ 1, id=id, data=true.dat)) %>% 
    mutate(t = time,
           truth = 1 - estimate) %>% 
    select(t, truth) 
  
  true.haz <- true.dat %>% 
    group_by(t) %>% 
    summarize(truth = sum(Y) / n()) 
  
  true.rate <- true.dat %>% 
    summarize(truth = sum(Y) / sum((t - last_t))) 
  

# Multiple imputation -----------------------------------------------------
  
  gap.dat <- dat.r %>%
    mutate(Y_obs = ifelse(M==1, NA, Y), # Y only observed when M==0
           Z_obs = ifelse(M==1, NA, Z)) # Z is missing at missed visits

  wide <- gap.dat %>%
    pivot_wider(id_cols=c(id, B), names_from=t, values_from=c(Y_obs, Z_obs))
  
  # Fit MI
  wide.imp <- mice(select(wide, -id), m=M, maxit=50, print=F)
  
  # Summarize across imputations 
  imp.rep <- function(m) {
    imp.dat <- bind_cols(id=wide$id, mice::complete(wide.imp, m)) %>%
      pivot_longer(!c(id, B),
                   names_to = c(".value", "t"),
                   names_pattern = "(.*)_(.*)") %>%
      rename(Y = Y_obs,
             Z = Z_obs) %>% 
      group_by(id) %>%
      mutate(t = as.numeric(t),
             last_t = lag(t),
             cumY = cumsum(cumsum(Y))) %>%
      filter(cumY<=1) %>% 
      filter(t>0) %>% 
      ungroup()
    
    # Incidence of Y
    mi.risk <- tidy(survfit(Surv(last_t, t, Y) ~ 1, id=id, data=imp.dat)) %>%
      mutate(t = time,
             mi = 1 - estimate) %>%
      select(t, mi) %>% 
      mutate(estimand="risk")
    
    mi.haz <- imp.dat %>% 
      group_by(t) %>% 
      summarize(mi = sum(Y) / n()) %>% 
      mutate(estimand="hazard")
    
    mi.rate <- imp.dat %>% 
      summarize(mi = sum(Y) / sum((t - last_t))) %>% 
      mutate(estimand = "rate",
             t = 10)
    
    # Combine results in one dataset
    res <- bind_rows(mi.risk, mi.haz, mi.rate)
    
    return(res)
  }
  
  imp.res <- lapply(1:M, function(x){imp.rep(x)})
  imp.res <- bind_rows(imp.res) %>% 
    group_by(estimand, t) %>%
    summarize(mi = mean(mi)) %>% 
    mutate(rep = r)
  
  res.risk <- filter(imp.res, estimand=="risk") %>% 
    left_join(true.risk, by="t") 
  
  res.haz <- filter(imp.res, estimand=="hazard") %>% 
    left_join(true.haz, by="t")
  
  res.rate <- filter(imp.res, estimand=="rate") %>% 
    bind_cols(true.rate)
  
  res <- bind_rows(res.risk, res.haz, res.rate)

  return(res)
}

sim.res <- mclapply(1:nsim, function(x){rep.res(x)}, mc.cores=cores, mc.set.seed=F)
sim.res <- bind_rows(sim.res)

write_csv(sim.res, paste0("../results/", model, "/", model, "_", outcome, "_mi-z_all.csv"))

summ.res <- sim.res %>% 
  group_by(estimand, t) %>% 
  summarize(across(!rep, list(avg = ~mean(.x, na.rm=T), sd = ~sd(.x, na.rm=T)))) %>% 
  mutate(bias_truth = truth_avg - truth_avg,
         bias_mi = mi_avg - truth_avg) %>% 
  ungroup()

write_csv(summ.res, paste0("../results/", model, "/", model, "_", outcome, "_mi-z_summ.csv"))

}
