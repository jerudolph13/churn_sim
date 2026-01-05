
packages <- c("tidyverse", "tableone", "survival", "broom")
for (package in packages){
  suppressPackageStartupMessages(library(package, character.only=T, quietly=T)) 
}

nsim <- 1000

model <- "dag1"
outcome <- "permanent"
dat <- read_csv(paste0("../data/", model, "_", outcome, ".csv"))

rep.res <- function(r) {
  
  dat.r <- filter(dat, sim_rep==r)
  
  # Censor at missed visit
  obs.dat <- dat.r %>%
    filter(M==0)
  
  ic.dat <- obs.dat %>%
    group_by(id) %>%
    mutate(cumY = cumsum(cumsum(Y)),
           last_t = lag(t)) %>%
    filter(cumY<=1) %>%
    filter(t>0)
  
  ic.dat <- ic.dat %>%
    mutate(status = case_when(Y==0 ~ 0,
                              (t - last_t)>1 ~ 3,
                              T ~ 1),
           t1 = ifelse(status!=3, t, last_t),
           t2 = t)
  
  # In this set-up, answer will be wrong unless only 1 record per person
  last <- filter(ic.dat, !duplicated(id, fromLast=T))
  
  ic.risk <- tidy(survfit(Surv(t1, t2, status, type="interval") ~ 1, id=id, data=last)) %>%
    mutate(interval = 1 - estimate) %>%
    select(time, interval)
  
}

all.res <- lapply(1:nsim, function(x){rep.res(x)})
all.res <- bind_rows(all.res)

write_csv(all.res, paste0("../results/", model, "/", model, "_", outcome, "_ic_all.csv"))

summ.res <- all.res %>%
  group_by(time) %>%
  summarize(across(everything(), list(avg = ~mean(.x, na.rm=T), sd = ~sd(.x, na.rm=T)))) %>%
  ungroup()

write_csv(summ.res, paste0("../results/", model, "/", model, "_", outcome, "_ic_summ.csv"))
