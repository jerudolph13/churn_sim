
###########################################################################
#
# Project: How to handle churn
#
# Purpose: Compare approaches in ALIVE
#
# Author: Jacqueline Rudolph
#
# Last Update: 17 Jan 2025
#
###########################################################################


packages <- c("tidyverse", "survival", "broom", "zoo", "splines", "mice")
for (package in packages) {
  if (!(package %in% installed.packages())) {install.packages(package)}
  library(package, character.only=T)
}

set.seed(123)

# Prepare data ------------------------------------------------------------

dat <- read_csv("../data/alive.csv") %>% 
  mutate(visdate = mdy(visdate))

length(unique(dat$id))
  # 1703 participants
  # 13174 visits

min(dat$visdate)

# Pre-pandemic
dat2 <- dat %>% 
  filter(year(visdate)<=2019) %>% 
  select(id, visdate, odivnoniv, m0f1, age, 
         cesd23, curuser, work, auditgrp, anntivwomj)
summary(dat2)

# Must have visit in 2014
# Must report no OD in past 6 months at baseline
include <- dat2 %>% 
  filter(!duplicated(id)) %>% 
  filter(year(visdate)==2014) %>% 
  filter(odivnoniv==0 | is.na(odivnoniv))

dat3 <- dat2 %>% 
  filter(id %in% include$id)
  
# Set up variables for analysis
dat4 <- dat3 %>% 
  group_by(id) %>% 
  mutate(visnum = row_number(),
         last_vis = lag(visdate),
         tbtw = (last_vis %--% visdate)/months(1),
         tbtw = ifelse(is.na(tbtw), 0, tbtw),
         t = cumsum(tbtw),
         last_t = lag(t, default=0),
         odivnoniv = ifelse(t==0, 0, odivnoniv)) %>% 
          # Don't consider events prior to baseline
  rename(Y = odivnoniv) %>% 
  filter(floor(t)<=60) %>%  
  # Admin censor at 60 months
  select(-c(visdate, last_vis))
  # 914 participants
  # 7315 visits

# Make age baseline age
first <- dat4 %>% 
  filter(!duplicated(id)) %>% 
  mutate(age_b = age) %>% 
  select(id, age_b)

dat5 <- dat4 %>% 
  left_join(first, by="id") %>% 
  select(-age)

# Need to deal with the small amount of missingness in the data
n_y <- as.numeric(sum(is.na(dat5$Y))) # 30 people missing OD
p_y <- mean(dat5$Y, na.rm=T) # P(Y=1) = 0.02
dat5$Y[is.na(dat5$Y)] <- rbinom(n_y, 1, p_y)

# LOCF/NOCB for covariates
dat6 <- dat5 %>%
  group_by(id) %>%
  mutate_at(vars(-group_cols()), ~na.locf(., na.rm=F)) %>% 
  mutate_at(vars(-group_cols()), ~na.locf(., na.rm=F, fromLast=T))

# Distribution of time between visits
summary(dat6$tbtw[dat6$t!=0])
  # Min: 4.7, Q1: 5.97; Med: 6.00; Q3: 6.19; Max: 45.0


# Crude censoring ---------------------------------------------------------

# Incidence of first
censor <- dat6 %>%
  group_by(id) %>%
  mutate(gap_9mo = as.numeric(tbtw>9),
         gap_9mo = ifelse(is.na(gap_9mo), 0, gap_9mo),
         cum_gap = cumsum(gap_9mo)) %>%
  filter(cum_gap<1)

censor2 <- censor %>%
  group_by(id) %>%
  mutate(cum_Y = cumsum(cumsum(Y))) %>%
  filter(cum_Y<=1) %>%
  filter(t>0) %>% 
  filter(!duplicated(id, fromLast=T))
  
censor.risk1 <- tidy(survfit(Surv(t, Y) ~ 1, data=censor2))

censor.rate1 <- (sum(censor2$Y)/sum(censor2$t))*12


# IPCW --------------------------------------------------------------------

# Incidence of first
censor3 <- censor %>%
  group_by(id) %>%
  mutate(cum_Y = cumsum(cumsum(Y)),
         cesd_1 = lag(cesd23),
         curuser_1 = lag(curuser),
         work_1 = lag(work),
         auditgrp_1 = lag(auditgrp),
         anntivwomj_1 = lag(anntivwomj))

# fill in missing lagged values using NOCB
censor3 <- censor3 %>% 
  group_by(id) %>% 
  mutate_at(vars(-group_cols()), ~na.locf(., na.rm=F, fromLast=T)) %>% 
  mutate_at(vars(-group_cols()), ~ifelse(is.na(.), 0, .)) %>% 
  ungroup() %>% 
  filter(cum_Y<=1)
  
censor3 <- censor3 %>%
  mutate(last = as.numeric(!duplicated(id, fromLast=T)),
         drop = as.numeric(last==1 & Y==0 & t<52)) # Count as admin censor if last visits occurs [52, 60]
  
num.mod <- glm(I(drop==0) ~ bs(t, df=3), data=censor3,
               family=binomial(link="logit"))$fitted.values
den.mod <- glm(I(drop==0) ~ bs(t, df=3) + bs(age_b, df=3) + m0f1 + cesd23 + cesd_1 + 
                 curuser + curuser_1 + work + work_1 + auditgrp + auditgrp_1 +
                 anntivwomj + anntivwomj_1, data=censor3,
               family=binomial(link="logit"))$fitted.values
censor3$wt <- num.mod/den.mod
  
censor3 <- censor3 %>%
    group_by(id) %>%
    mutate(cum_wt = cumprod(wt)) %>%
    filter(t>0)
  
censor.risk2 <- tidy(survfit(Surv(last_t, t, Y) ~ 1, data=censor3, id=id, weights=cum_wt))
  
rate <- censor3 %>%
  mutate(Y_wt = Y*cum_wt,
         t_delta = (t - last_t)*cum_wt)
censor.rate2 <- (sum(rate$Y_wt)/sum(rate$t_delta))*12


# All-time approach -------------------------------------------------------

# Incidence of first
gap <- dat6 %>%
  group_by(id) %>%
  mutate(cum_Y = cumsum(cumsum(Y))) %>%
  filter(cum_Y<=1) %>%
  filter(t>0)
  
gap.risk1 <- tidy(survfit(Surv(last_t, t, Y) ~ 1, data=gap, id=id))

gap.rate1 <- (sum(gap$Y)/sum(gap$t - gap$last_t))*12


# Look-back period approach -----------------------------------------------

gap2 <- gap %>%
  mutate(last_t = ifelse((t - last_t)>6, t - 6, last_t))
  
gap.risk2 <- tidy(survfit(Surv(last_t, t, Y) ~ 1, data=gap2, id=id))

gap.rate2 <- (sum(gap2$Y)/sum(gap2$t - gap2$last_t))*12


# IPOW --------------------------------------------------------------------

obs.structure <- bind_rows(lapply(tibble(id = unique(dat6$id)), rep, 11)) %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(n = 6,
         t = cumsum(n)) %>%
  select(-n)

set.seed(123)
gap3 <- bind_rows(dat6, obs.structure) %>%
  arrange(id, t) %>%
  group_by(id) %>%
  mutate(cum_Y = cumsum(cumsum(ifelse(is.na(Y), 0, Y))),
         new_t = case_when(t%%6!=0 & (t - lag(t))<(lead(t) - t) ~ lag(t),
                           t%%6!=0 & (t - lag(t))==(lead(t) - t) & runif(1)<=0.5 ~ lag(t),
                           t%%6!=0 & (t - lag(t))==(lead(t) - t) ~ lead(t),
                           t%%6!=0 & (t - lag(t))>(lead(t) - t) ~ lead(t),
                           T ~ t),
         obs = as.numeric(!is.na(age_b))) %>%
  filter(cum_Y<=1) %>%
  filter(new_t<66) %>% 
  select(-c(t, cum_Y))
  
gap3 <- gap3 %>%
  group_by(id, new_t) %>%
  summarize(across(!obs, ~ mean(.x, na.rm=T)),
            obs = max(obs)) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x))) %>%
  ungroup(new_t)
  
gap3 <- gap3 %>%
  mutate_at(vars(-group_cols()), ~ na.locf(., na.rm = FALSE)) %>%
  mutate(obs_1 = lag(obs, default=0),
         obs_2 = lag(obs, n=2, default=0),
         cesd_1 = lag(cesd23),
         curuser_1 = lag(curuser),
         work_1 = lag(work),
         auditgrp_1 = lag(auditgrp),
         anntivwomj_1 = lag(anntivwomj))

gap3 <- gap3 %>% 
  group_by(id) %>% 
  mutate_at(vars(-group_cols()), ~na.locf(., na.rm=F, fromLast=T)) %>% 
  ungroup()
  
num.mod <- glm(I(obs==1) ~ bs(new_t, df=3), data=gap3,
               family=binomial(link="logit"))$fitted.values
num <- gap3$obs*num.mod + (1-gap3$obs)*(1-num.mod)
den.mod <- glm(I(obs==1) ~ bs(new_t, df=3) + bs(age_b, df=3) + m0f1 + cesd23 + cesd_1 +
                 curuser + curuser_1 + work + work_1 + auditgrp + auditgrp_1 +
                 anntivwomj + anntivwomj_1 + obs_1 + obs_2, 
               data=gap3,
               family=binomial(link="logit"))$fitted.values
den <- gap3$obs*den.mod + (1-gap3$obs)*(1-den.mod)
gap3$wt <- num/den
  
gap3 <- gap3 %>%
  group_by(id) %>%
  mutate(cum_wt = cumprod(wt),
         new_last_t = lag(new_t)) %>%
  filter(new_t>0)
  
gap.risk3 <- tidy(survfit(Surv(new_last_t, new_t, Y) ~ 1, data=gap3, id=id, weights=cum_wt))
  
rate <- gap3 %>%
  mutate(Y_wt = Y*cum_wt,
         t_delta = (new_t - new_last_t)*cum_wt)
gap.rate3 <- (sum(rate$Y_wt)/sum(rate$t_delta))*12


# Multiple imputation -----------------------------------------------------

# Identify missed visits
set.seed(123)
obs2 <- bind_rows(dat6, obs.structure) %>%
  arrange(id, t) %>%
  group_by(id) %>%
  mutate(new_t = case_when(t%%6!=0 & (t - lag(t))<(lead(t) - t) ~ lag(t),
                           t%%6!=0 & (t - lag(t))==(lead(t) - t) & runif(1)<=0.5 ~ lag(t),
                           t%%6!=0 & (t - lag(t))==(lead(t) - t) ~ lead(t),
                           t%%6!=0 & (t - lag(t))>(lead(t) - t) ~ lead(t),
                           T ~ t),
         obs = as.numeric(!is.na(age_b))) %>%
  filter(new_t<66) %>% 
  select(-t)

obs3 <- obs2 %>%
  group_by(id, new_t) %>%
  summarize(across(!obs, ~ mean(.x, na.rm=T)),
            obs = max(obs)) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x))) %>%
  ungroup(new_t) %>%
  mutate_at(vars(c(age_b, m0f1)), ~ na.locf(., na.rm = FALSE)) %>%
  ungroup()

wide <- obs3 %>%
  pivot_wider(id_cols=c(id, age_b, m0f1), 
              names_from=new_t, 
              values_from=!c(id, age_b, m0f1, new_t, visnum, tbtw, last_t, obs))

M <- 100
wide.imp <- mice(select(wide, -id), m=M, maxit=50, print=F)

imp.rep <- function(m) {
  imp.dat <- bind_cols(id=wide$id, mice::complete(wide.imp, m)) %>%
    pivot_longer(!c(id, age_b, m0f1),
                 names_to = c(".value", "t"),
                 names_pattern = "(.*)_(.*)") %>%
    group_by(id) %>%
    mutate(t = as.numeric(t),
           last_t = lag(t)) %>%
    filter(!is.na(last_t))
  
  # Risk
  risk.dat <- imp.dat %>%
    group_by(id) %>%
    mutate(cum_Y = cumsum(cumsum(Y))) %>%
    filter(cum_Y<=1) %>%
    filter(!duplicated(id, fromLast=T))
  
  risk <- tidy(survfit(Surv(t, Y) ~ 1, data=risk.dat))
  
  # Rate of first outcome
  rate.first <- (sum(risk.dat$Y)/sum(risk.dat$t))*12
  
  # Combine results in one dataset
  res <- bind_cols(risk, rate.first=rate.first)
  return(res)
}

imp.res <- lapply(1:M, function(x){imp.rep(x)})
imp.res <- bind_rows(imp.res) %>%
  group_by(time) %>%
  summarize(estimate = mean(estimate),
            rate.first = mean(rate.first))

gap.rate4 <- imp.res$rate.first[1]



# Compare risk ------------------------------------------------------------

jpeg("../figures/alive_risk_compare.jpeg", height=5, width=7, units="in", res=300)
ggplot() +
  theme_classic() +
  labs(x="Months since baseline", y="Survival", color="Approach:") +
  geom_point(aes(x=censor.risk1$time, y=censor.risk1$estimate, color="Crude censor")) +
  geom_point(aes(x=censor.risk2$time, y=censor.risk2$estimate, color="IPCW")) +
  geom_point(aes(x=gap.risk1$time, y=gap.risk1$estimate, color="All-time")) +
  geom_point(aes(x=gap.risk2$time, y=gap.risk2$estimate, color="Look-back period")) +
  geom_point(aes(x=gap.risk3$time, y=gap.risk3$estimate, color="IPOW")) +
  geom_point(aes(x=imp.res$time, y=imp.res$estimate, color="MI")) +
  scale_x_continuous(expand=c(0, 0))
dev.off()  
  

fig <- NULL
for (t in seq(6, 60, 6)) {
  
  temp1 <- censor.risk1 %>% 
    filter(time<=t) %>% 
    filter(time==max(time)) %>% 
    mutate(time = t,
           censor_unwt = 1 - estimate) %>% 
    select(censor_unwt)
  temp2 <- censor.risk2 %>% 
    filter(time<=t) %>% 
    filter(time==max(time)) %>% 
    mutate(time = t,
           censor_wt = 1 - estimate) %>% 
    select(censor_wt)
  temp3 <- gap.risk1 %>% 
    filter(time<=t) %>% 
    filter(time==max(time)) %>% 
    mutate(time = t,
           all_time = 1 - estimate) %>% 
    select(all_time)
  temp4 <- gap.risk2 %>% 
    filter(time<=t) %>% 
    filter(time==max(time)) %>% 
    mutate(time = t,
           look_back = 1 - estimate) %>% 
    select(look_back)
  temp5 <- gap.risk3 %>% 
    filter(time<=t) %>% 
    filter(time==max(time)) %>% 
    mutate(time = t,
           ipow = 1 - estimate) %>% 
    select(ipow)
  temp6 <- imp.res %>% 
    filter(time<=t) %>% 
    filter(time==max(time)) %>% 
    mutate(time = t,
           mi = 1 - estimate) %>% 
    select(mi)
  
  temp <- bind_cols(time = t, temp1, temp2, temp3, temp4, temp5, temp6)
  fig <- bind_rows(fig, temp)
  
}

fig <- fig %>% 
  pivot_longer(!time, names_to="analysis", values_to="risk") %>% 
  mutate(analysis = factor(analysis, 
                           levels=c("censor_unwt", "censor_wt",
                                    "all_time", "look_back", 
                                    "ipow", "mi"),
                           labels=c("Crude censor", "IPCW",
                                    "All-time", "Look-back period",
                                    "IPOW", "MI")))

jpeg("../figures/alive_risk_compare.jpeg", height=5, width=7, units="in", res=300)
ggplot(data=fig, aes(x=time, y=risk, color=analysis)) +
  labs(x="Months since baseline", y="Risk", color="Approach:") +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(6, 60, 6)) +
  theme_bw() +
  theme(axis.text = element_text(color="black", size=12),
        axis.title = element_text(color="black", size=12),
        legend.text = element_text(color="black", size=12),
        legend.title = element_text(color="black", size=12))
dev.off()  

fig.summ <- fig %>% 
  group_by(time) %>% 
  summarize(range = max(risk) - min(risk))
