

###########################################################################
#
# Project: How to handle churn
#
# Purpose: Run weighted analyses (Z included)
#
# Author: Jacqueline Rudolph
#
# Last Update: 08 Sep 2025
#
###########################################################################


packages <- c("tidyverse", "survival", "broom", "splines")
for (package in packages){
  suppressPackageStartupMessages(library(package, character.only=T, quietly=T)) 
}

nsim <- 1000

model <- "dag4.1" #"dag1", "dag2.1", "dag2.2", "dag2.3", "dag2.4", "dag3", "dag3.1", "dag4", "dag4.1"
for (outcome in c("transient", "permanent", "repeated")) {


# Read in results ---------------------------------------------------------

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
    mutate(truth = 1 - estimate) %>% 
    select(time, truth) 
  
  true.haz <- true.dat %>% 
    group_by(t) %>% 
    summarize(truth = sum(Y) / n()) 
  
  true.rate <- true.dat %>% 
    summarize(truth = sum(Y) / sum((t - last_t))) 
            
  
# IPCW --------------------------------------------------------------------

  obs.dat <- dat.r %>%
    filter(M==0)
  
  censor.dat <- obs.dat %>%
    group_by(id) %>%
    mutate(last_t = lag(t, default=0),
           diff_t = t - last_t,
           gap = diff_t>1,
           cum_gap = cumsum(gap),
           Z_1 = lag(Z, default=0),
           Z_2 = lag(Z, n=2, default=0)) %>%
    filter(cum_gap<1)

  censor.dat <- censor.dat %>%
    mutate(cumY = cumsum(cumsum(Y))) %>%
    filter(cumY<=1) 
  
  censor.dat <- censor.dat %>% 
    mutate(drop = as.numeric(!duplicated(id, fromLast=T) & Y==0 & t<10))
           
  censor.dat$num <- glm(I(drop==0) ~ as.factor(t), 
                        family=binomial(link="logit"), data=censor.dat)$fitted.values 
  censor.dat$den <- glm(I(drop==0) ~  bs(B, df=3)*as.factor(t) + Z + Z_1 + Z_2, 
                        family=binomial(link="logit"), data=censor.dat)$fitted.values 

  censor.dat <- censor.dat %>% 
    group_by(sim_rep, id) %>% 
    mutate(wt_us = 1/den,
           cum_wt_us = cumprod(wt_us),
           wt_s = num/den,
           cum_wt_s = cumprod(wt_s)) %>% 
    ungroup()
  
  censor.dat <- censor.dat %>% 
    filter(t>0)
  
  censor.risk1 <- tidy(survfit(Surv(last_t, t, Y) ~ 1, id=id, weights=cum_wt_us, data=censor.dat)) %>%
    mutate(censor_us = 1 - estimate) %>%
    select(time, censor_us)
  
  censor.risk2 <- tidy(survfit(Surv(last_t, t, Y) ~ 1, id=id, weights=cum_wt_s, data=censor.dat)) %>%
    mutate(censor_s = 1 - estimate) %>%
    select(time, censor_s)
  
  censor.haz <- censor.dat %>% 
    group_by(t) %>% 
    summarize(censor_us = sum(Y*cum_wt_us) / sum(cum_wt_us),
              censor_s = sum(Y*cum_wt_s) / sum(cum_wt_s)) 
  
  censor.rate <- censor.dat %>% 
    summarize(censor_us = sum(Y*cum_wt_us) / sum((t - last_t)*cum_wt_us),
              censor_s = sum(Y*cum_wt_s) / sum((t - last_t)*cum_wt_s)) 


# IPOW --------------------------------------------------------------------

  gap.dat <- dat.r %>%
    group_by(id) %>%
    mutate(Y_obs = ifelse(M==1, 0, Y), # Only count observed Y
           cumY = cumsum(cumsum(Y_obs)),
           last_t = lag(t),
           Z_1 = lag(Z, default=0),
           Z_2 = lag(Z, n=2, default=0)) %>%
    filter(cumY<=1) 
  
  gap.dat <- gap.dat %>% 
    mutate(obs = as.numeric(M==0),
           lag_obs = lag(obs, default=0)) 
  
  num.mod <- glm(I(obs==1) ~ lag_obs + as.factor(t), data=gap.dat,
                 family=binomial(link="logit"))$fitted.values
  num <- gap.dat$obs*num.mod + (1-gap.dat$obs)*(1-num.mod)
  den.mod <- glm(I(obs==1) ~ bs(B, df=3) + lag_obs + as.factor(t) + Z + Z_1 + Z_2, data=gap.dat,
                 family=binomial(link="logit"))$fitted.values
  den <- gap.dat$obs*den.mod + (1-gap.dat$obs)*(1-den.mod)
  gap.dat$wt <- num/den
  
  gap.dat <- gap.dat %>% 
    mutate(cum_wt = cumprod(wt)) %>% 
    filter(obs==1) %>% 
    ungroup()
  
  gap.dat <- gap.dat %>% 
    filter(t>0)

  gap.risk <- tidy(survfit(Surv(last_t, t, Y) ~ 1, id=id, weights=cum_wt, data=gap.dat)) %>%
    mutate(obs_wt = 1 - estimate) %>%
    select(time, obs_wt)
  
  gap.haz <- gap.dat %>% 
    group_by(t) %>% 
    summarize(obs_wt = sum(Y*cum_wt) / sum(cum_wt)) 
  
  gap.rate <- gap.dat %>% 
    summarize(obs_wt = sum(Y*cum_wt) / sum((t - last_t)*cum_wt)) 
  

# Combine results ---------------------------------------------------------

  res.risk <- data.frame(estimand="risk", 
                    rep = r, 
                    time = seq(1, 10, 1)) %>% 
    left_join(true.risk, by="time") %>% 
    left_join(censor.risk1, by="time") %>% 
    left_join(censor.risk2, by="time") %>% 
    left_join(gap.risk, by="time") %>% 
    rename(t = time)
  
  res.haz <- data.frame(estimand="hazard", 
                         rep = r, 
                         t = seq(1, 10, 1)) %>% 
    left_join(true.haz, by="t") %>% 
    left_join(censor.haz, by="t") %>% 
    left_join(gap.haz, by="t") 
  
  res.rate <- data.frame(estimand="rate", 
                        rep = r, 
                        t = 10) %>% 
    bind_cols(true.rate) %>% 
    bind_cols(censor.rate) %>% 
    bind_cols(gap.rate) 
  
  res <- bind_rows(res.risk, res.haz, res.rate)
  
  return(res)

}

all.res <- lapply(1:nsim, function(x){rep.res(x)})
all.res <- bind_rows(all.res)

write_csv(all.res, paste0("../results/", model, "_", outcome, "_wt-z_all.csv"))

summ.res <- all.res %>% 
  group_by(estimand, t) %>% 
  summarize(across(!rep, list(avg = ~mean(.x, na.rm=T), sd = ~sd(.x, na.rm=T)))) %>% 
  mutate(bias_truth = truth_avg - truth_avg,
         bias_censor_us = censor_us_avg - truth_avg,
         bias_censor_s = censor_s_avg - truth_avg,
         bias_obs_wt = obs_wt_avg - truth_avg) %>% 
  ungroup()

write_csv(summ.res, paste0("../results/", model, "_", outcome, "_wt-z_summ.csv"))


}
