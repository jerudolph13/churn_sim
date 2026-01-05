
###########################################################################
#
# Project: How to handle data gaps
#
# Purpose: Create Table 1
#
# Author: Jacqueline Rudolph
#
# Last Update: 03 Oct 2025
#
###########################################################################

library("tidyverse")


# Read in results ---------------------------------------------------------

risk <- read_csv("../results/risk.csv") %>% 
  mutate(outcome = factor(outcome, levels=c("transient", "repeated", "permanent")))
rate <- read_csv("../results/rate.csv") %>% 
  mutate(outcome = factor(outcome, levels=c("transient", "repeated", "permanent")))


# Format risk results -----------------------------------------------------

risk.bias <- risk %>% 
  filter(t %in% c(3, 6, 9)) %>% 
  filter(dag %in% c("dag1", "dag2.1", "dag3")) %>% 
  group_by(dag, outcome, t) %>% 
  summarize(across(c("truth", "censor", "gap_allt", "gap_1t", 
                     "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                     "obs_wt", "obs_wt_z", "mi", "mi_z"), ~mean(.x, na.rm=T))) %>% 
  mutate(across(c("censor", "gap_allt", "gap_1t", 
                  "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                  "obs_wt", "obs_wt_z", "mi", "mi_z"), ~.x - truth)) %>% 
  select(-truth) %>% 
  pivot_longer(!c(dag, outcome, t), names_to="approach", values_to="Bias") %>% 
  pivot_wider(id_cols=c(dag, outcome, approach), names_from="t", values_from="Bias", names_prefix="Bias")

risk.ese <- risk %>% 
  filter(t %in% c(3, 6, 9)) %>% 
  filter(dag %in% c("dag1", "dag2.1", "dag3")) %>% 
  group_by(dag, outcome, t) %>% 
  summarize(across(c("censor", "gap_allt", "gap_1t", 
                     "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                     "obs_wt", "obs_wt_z", "mi", "mi_z"), ~sd(.x, na.rm=T))) %>% 
  pivot_longer(!c(dag, outcome, t), names_to="approach", values_to="ESE") %>% 
  pivot_wider(id_cols=c(dag, outcome, approach), names_from="t", values_from="ESE", names_prefix="ESE")

risk.mse <- risk %>% 
  filter(t %in% c(3, 6, 9)) %>% 
  filter(dag %in% c("dag1", "dag2.1", "dag3")) %>% 
  mutate(across(c("censor", "gap_allt", "gap_1t", 
                  "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                  "obs_wt", "obs_wt_z", "mi", "mi_z"), ~(.x - truth)^2)) %>% 
  group_by(dag, outcome, t) %>% 
  summarize(across(c("censor", "gap_allt", "gap_1t", 
                     "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                     "obs_wt", "obs_wt_z", "mi", "mi_z"), ~mean(.x))) %>% 
  pivot_longer(!c(dag, outcome, t), names_to="approach", values_to="MSE") %>% 
  pivot_wider(id_cols=c(dag, outcome, approach), names_from="t", values_from="MSE", names_prefix="MSE")

risk.summ <- left_join(risk.bias, risk.ese, by=c("dag", "outcome", "approach")) %>% 
  left_join(risk.mse, by=c("dag", "outcome", "approach")) %>% 
  select(dag, outcome, approach, Bias3, ESE3, MSE3, Bias6, ESE6, MSE6, Bias9, ESE9, MSE9)


# Format rate results -----------------------------------------------------

rate.bias <- rate %>% 
  filter(dag %in% c("dag1", "dag2.1", "dag3")) %>% 
  group_by(dag, outcome) %>% 
  summarize(across(c("truth", "censor", "gap_allt", "gap_1t", 
                     "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                     "obs_wt", "obs_wt_z", "mi", "mi_z"), ~mean(.x, na.rm=T))) %>% 
  mutate(across(c("censor", "gap_allt", "gap_1t", 
                  "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                  "obs_wt", "obs_wt_z", "mi", "mi_z"), ~.x - truth)) %>% 
  select(-truth) %>% 
  pivot_longer(!c(dag, outcome), names_to="approach", values_to="Rate_Bias") 

rate.ese <- rate %>% 
  filter(dag %in% c("dag1", "dag2.1", "dag3")) %>% 
  group_by(dag, outcome) %>% 
  summarize(across(c("censor", "gap_allt", "gap_1t", 
                     "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                     "obs_wt", "obs_wt_z", "mi", "mi_z"), ~sd(.x, na.rm=T))) %>% 
  pivot_longer(!c(dag, outcome), names_to="approach", values_to="Rate_ESE") 

rate.mse <- rate %>% 
  filter(dag %in% c("dag1", "dag2.1", "dag3")) %>% 
  mutate(across(c("censor", "gap_allt", "gap_1t", 
                  "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                  "obs_wt", "obs_wt_z", "mi", "mi_z"), ~(.x - truth)^2)) %>% 
  group_by(dag, outcome) %>% 
  summarize(across(c("censor", "gap_allt", "gap_1t", 
                     "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                     "obs_wt", "obs_wt_z", "mi", "mi_z"), ~mean(.x))) %>% 
  pivot_longer(!c(dag, outcome), names_to="approach", values_to="Rate_MSE") 

rate.summ <- left_join(rate.bias, rate.ese, by=c("dag", "outcome", "approach")) %>% 
  left_join(rate.mse, by=c("dag", "outcome", "approach")) 


# Output table ------------------------------------------------------------

table <- left_join(risk.summ, rate.summ, by=c("dag", "outcome", "approach")) %>% 
  filter(!(approach %in% c("censor_us", "censor_us_z"))) %>% 
  mutate(across(c("Bias3", "ESE3", "MSE3", "Bias6", "ESE6", "MSE6", "Bias9", "ESE9", "MSE9", 
                  "Rate_Bias", "Rate_ESE", "Rate_MSE"), ~round(.x, 3)),
         approach = factor(approach, levels=c("censor", "gap_1t", "gap_allt",
                                              "censor_s", "censor_s_z", "obs_wt", "obs_wt_z", "mi", "mi_z"))) %>% 
  arrange(dag, outcome, approach)

write_csv(table, "../results/table1.csv")


# Simulation characteristics ----------------------------------------------

model <- "dag4.1"
outcome <- "repeated"
sim.dat <- read_csv(paste0("./data/", model, "_", outcome, ".csv"))

sim.dat2 <- sim.dat %>% 
  group_by(id) %>% 
  mutate(cum_Y = cumsum(Y),
         R2 = ifelse(is.na(R), 0, 1))

last <- sim.dat2 %>% 
  filter(!duplicated(id, fromLast=T))

# Dropout before outcome
last2 <- filter(sim.dat2, cum_Y==0) %>% 
  filter(!duplicated(id, fromLast=T))

# Length of gaps
gaps <- sim.dat %>%
  filter(M==0) %>%
  group_by(id) %>%
  mutate(last_t = lag(t),
         gap = t - last_t) %>%
  filter(!is.na(gap) & gap>1)

if (outcome=="repeated") {
  print(paste("Outcome frequency:", mean(sim.dat$Y)))
  
} else if (outcome=="permanent") {
  print(paste("Outcome frequency:", mean(last$Y)))
  
} else {
  print(paste("Outcome frequency:", mean(last$cum_Y)))
  
}

print(paste("Missingness frequency:", mean(sim.dat$M)))

print(paste("Drop out frequency:", mean(last$R2)))

print(paste("Pre-outcome Drop out frequency:", mean(last2$R2)))

summary(gaps$gap)