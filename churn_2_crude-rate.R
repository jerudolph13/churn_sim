
packages <- c("tidyverse", "tableone", "survival", "broom")
for (package in packages){
  suppressPackageStartupMessages(library(package, character.only=T, quietly=T)) 
}

nsim <- 1000

for (model in c("dag1", "dag2.1", "dag2.2", "dag2.3", "dag2.4",
                "dag3", "dag3.1", "dag3.2", "dag3.3", "dag4", "dag4.1")) { 
for (outcome in c("transient", "permanent", "repeated")) {


# Read in results ---------------------------------------------------------

dat <- read_csv(paste0("../data/", model, "_", outcome, ".csv"))


# Look at data ------------------------------------------------------------

#CreateTableOne(data=dat, vars=c("B1", "B2", "Z", "Y"), strata="M")


# Estimate rate -----------------------------------------------------------

rep.res <- function(r) {

  dat.r <- filter(dat, sim_rep==r)
  
  # True natural course
  true.dat <- dat.r %>% 
    group_by(id) %>% 
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = lag(t)) %>% 
    filter(cumY<=1) %>% 
    filter(t>0)
  
  true.haz <- true.dat %>% 
    group_by(t) %>% 
    summarize(d = sum(Y),
              n = n()) %>% 
    mutate(haz_truth = d/n)%>% 
    select(t, haz_truth)
  true.rate <- sum(true.dat$Y)/sum(true.dat$t - true.dat$last_t)
  
  # Censor at missed visit
  obs.dat <- dat.r %>%
    filter(M==0)

  censor.dat <- obs.dat %>%
    group_by(id) %>%
    mutate(last_t = lag(t, default=0),
           diff_t = t - last_t,
           gap = diff_t>1,
           cum_gap = cumsum(gap)) %>%
    filter(cum_gap<1)

  censor.dat <- censor.dat %>%
    mutate(cumY = cumsum(cumsum(Y))) %>%
    filter(cumY<=1) %>%
    filter(t>0)
  
  censor.haz <- censor.dat %>% 
    group_by(t) %>% 
    summarize(d = sum(Y),
              n = n()) %>% 
    mutate(haz_censor = d/n)%>% 
    select(t, haz_censor)
  censor.rate <- sum(censor.dat$Y)/sum(censor.dat$t - censor.dat$last_t)

  # Allow participants to return after missing visit (use all time)
  gap.dat <- obs.dat %>%
    group_by(id) %>%
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = lag(t)) %>%
    filter(cumY<=1) %>%
    filter(t>0)
  
  gap.haz1 <- gap.dat %>% 
    group_by(t) %>% 
    summarize(d = sum(Y),
              n = n()) %>% 
    mutate(haz_gap1 = d/n)%>% 
    select(t, haz_gap1)
  gap.rate1 <- sum(gap.dat$Y)/sum(gap.dat$t - gap.dat$last_t)

  # Allow participants to return after missing visit (exclude missed time)
  gap.dat <- obs.dat %>%
    group_by(id) %>%
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = t-1) %>%
    filter(cumY<=1) %>%
    filter(t>0)
  
  gap.haz2 <- gap.dat %>% 
    group_by(t) %>% 
    summarize(d = sum(Y),
              n = n()) %>% 
    mutate(haz_gap2 = d/n) %>% 
    select(t, haz_gap2)
  gap.rate2 <- sum(gap.dat$Y)/sum(gap.dat$t - gap.dat$last_t)
  
  # Combine results
  res.haz <- data.frame(rep = r, 
                    t = seq(1, 10, 1)) %>% 
    left_join(true.haz, by="t") %>% 
    left_join(censor.haz, by="t") %>% 
    left_join(gap.haz1, by="t") %>% 
    left_join(gap.haz2, by="t") 
  
  res.rate <- bind_cols(rate_truth = true.rate, 
                        rate_censor = censor.rate, 
                        rate_gap1 = gap.rate1, 
                        rate_gap2 = gap.rate2)
  
  res <- bind_cols(res.haz, res.rate)
  
  return(res)

}

all.res <- lapply(1:nsim, function(x){rep.res(x)})
all.res <- bind_rows(all.res)

write.csv(all.res, paste0("../results/", model, "_", outcome, "_rate_all.csv"))

summ.res <- all.res %>% 
  group_by(t) %>% 
  summarize(across(!rep, list(avg = ~mean(.x, na.rm=T), sd = ~sd(.x, na.rm=T)))) %>% 
  mutate(bias_haz_truth = haz_truth_avg - haz_truth_avg,
         bias_haz_censor = haz_censor_avg - haz_truth_avg,
         bias_haz_gap_allt = haz_gap1_avg - haz_truth_avg,
         bias_haz_gap_1t = haz_gap2_avg - haz_truth_avg,
         
         bias_rate_truth = rate_truth_avg - rate_truth_avg,
         bias_rate_censor = rate_censor_avg - rate_truth_avg,
         bias_rate_gap_allt = rate_gap1_avg - rate_truth_avg,
         bias_rate_gap_1t = rate_gap2_avg - rate_truth_avg) %>% 
  ungroup()

write.csv(summ.res, paste0("../results/", model, "_", outcome, "_rate_summ.csv"))


}}
