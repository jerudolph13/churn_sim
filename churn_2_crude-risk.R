
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


# Estimate risk -----------------------------------------------------------

rep.res <- function(r) {

  dat.r <- filter(dat, sim_rep==r)
  
  # True natural course
  true.dat <- dat.r %>% 
    group_by(id) %>% 
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = lag(t)) %>% 
    filter(cumY<=1) %>% 
    filter(t>0)
  
  true.risk <- tidy(survfit(Surv(last_t, t, Y) ~ 1, data=true.dat)) %>% 
    mutate(truth = 1 - estimate) %>% 
    select(time, truth) 
  
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

  censor.risk <- tidy(survfit(Surv(last_t, t, Y) ~ 1, data=censor.dat)) %>%
    mutate(censor = 1 - estimate) %>%
    select(time, censor)

  # Allow participants to return after missing visit (use all time)
  gap.dat <- obs.dat %>%
    group_by(id) %>%
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = lag(t)) %>%
    filter(cumY<=1) %>%
    filter(t>0)

  gap.risk1 <- tidy(survfit(Surv(last_t, t, Y) ~ 1, data=gap.dat)) %>%
    mutate(gap_allt = 1 - estimate) %>%
    select(time, gap_allt)

  # Allow participants to return after missing visit (exclude missed time)
  gap.dat <- obs.dat %>%
    group_by(id) %>%
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = t-1) %>%
    filter(cumY<=1) %>%
    filter(t>0)

  gap.risk2 <- tidy(survfit(Surv(last_t, t, Y) ~ 1, data=gap.dat)) %>%
    mutate(gap_1t = 1 - estimate) %>%
    select(time, gap_1t)
  
  res <- data.frame(rep = r, 
                    time = seq(1, 10, 1)) %>% 
    left_join(true.risk, by="time") %>% 
    left_join(censor.risk, by="time") %>% 
    left_join(gap.risk1, by="time") %>% 
    left_join(gap.risk2, by="time") 
  
  return(res)

}

all.res <- lapply(1:nsim, function(x){rep.res(x)})
all.res <- bind_rows(all.res)

write.csv(all.res, paste0("../results/", model, "_", outcome, "_risk_all.csv"))

summ.res <- all.res %>% 
  group_by(time) %>% 
  summarize(across(!rep, list(avg = ~mean(.x, na.rm=T), sd = ~sd(.x, na.rm=T)))) %>% 
  mutate(bias_truth = truth_avg - truth_avg,
         bias_censor = censor_avg - truth_avg,
         bias_gap_allt = gap_allt_avg - truth_avg,
         bias_gap_1t = gap_1t_avg - truth_avg) %>% 
  ungroup()

print (paste("Summary results for crude risk for", model, "with", outcome, "outcome"))
print(summ.res)

write.csv(summ.res, paste0("../results/", model, "_", outcome, "_risk_summ.csv"))


}}



