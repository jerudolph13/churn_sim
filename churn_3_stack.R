
###########################################################################
#
# Project: How to handle churn
#
# Purpose: Stack results
#
# Author: Jacqueline Rudolph
#
# Last Update: 29 Sep 2025
#
###########################################################################

library("tidyverse")

risk_all <- NULL
rate_all <- NULL

for (model in c("dag1", "dag2.1", "dag2.2", "dag2.3", "dag2.4",
                "dag3", "dag3.1", "dag3.2", "dag3.3",
                "dag4", "dag4.1")) {
  for (outcome in c("transient", "permanent", "repeated")) {

    # Risk
    risk1 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_risk_all.csv")) %>% 
      rename(t = time) %>% 
      select(rep, t, truth, censor, gap_allt, gap_1t)
    risk2 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_wt_all.csv")) %>% 
      filter(estimand=="risk") %>% 
      select(rep, t, censor_us, censor_s, obs_wt)
    risk3 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_mi_all.csv")) %>% 
      filter(estimand=="risk") %>% 
      select(rep, t, mi)
    
    risk <- left_join(risk1, risk2, by=c("rep", "t")) %>% 
      left_join(risk3, by=c("rep", "t"))
    
    # Rate
    rate1 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_rate_all.csv")) %>% 
      rename(truth = rate_truth,
             censor = rate_censor,
             gap_allt = rate_gap1,
             gap_1t = rate_gap2) %>% 
      filter(t==10) %>% 
      select(rep, truth, censor, gap_allt, gap_1t)
    rate2 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_wt_all.csv")) %>% 
      filter(estimand=="rate") %>% 
      select(rep, censor_us, censor_s, obs_wt)
    rate3 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_mi_all.csv")) %>% 
      filter(estimand=="rate") %>% 
      select(rep, mi)
    
    rate <- left_join(rate1, rate2, by="rep") %>% 
      left_join(rate3, by="rep")
    
    if (model %in% c("dag3", "dag3.1", "dag3.2", "dag3.3",
                   "dag4", "dag4.1")) {
      risk4 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_wt-z_all.csv")) %>% 
        filter(estimand=="risk") %>% 
        rename(censor_us_z = censor_us,
               censor_s_z = censor_s,
               obs_wt_z = obs_wt) %>% 
        select(rep, t, censor_us_z, censor_s_z, obs_wt_z)
      risk5 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_mi-z_all.csv")) %>% 
        filter(estimand=="risk") %>% 
        rename(mi_z = mi) %>% 
        select(rep, t, mi_z)
      
      risk <- left_join(risk, risk4, by=c("rep", "t")) %>% 
        left_join(risk5, by=c("rep", "t"))
      
      rate4 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_wt-z_all.csv")) %>% 
        filter(estimand=="rate") %>% 
        rename(censor_us_z = censor_us,
               censor_s_z = censor_s,
               obs_wt_z = obs_wt) %>% 
        select(rep, censor_us_z, censor_s_z, obs_wt_z)
      rate5 <- read_csv(paste0("../results/", model, "/", model, "_", outcome, "_mi-z_all.csv")) %>% 
        filter(estimand=="rate") %>% 
        rename(mi_z = mi) %>% 
        select(rep, mi_z)
      
      rate <- left_join(rate, rate4, by="rep") %>% 
        left_join(rate5, by="rep")
      
    } 
    
    risk <- risk %>% 
      mutate(dag = model,
             outcome = outcome)
    
    risk_all <- bind_rows(risk_all, risk)
      
    rate <- rate %>% 
      mutate(dag = model,
             outcome = outcome)  
    
    rate_all <- bind_rows(rate_all, rate)
    
  }
}

write_csv(risk_all, "./results/risk.csv")
write_csv(rate_all, "./results/rate.csv")
