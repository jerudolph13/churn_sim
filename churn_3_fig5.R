
library("tidyverse")
library("scales")
library("patchwork")

thm <- theme_bw() + 
  theme(title = element_text(color="black", size=10),
        axis.text = element_text(color="black", size=10),
        axis.title = element_text(color="black", size=10),
        legend.title = element_text(color="black", size=8),
        legend.text = element_text(color="black", size=8),
        legend.key.height = unit(0.5, "in"))


# Format results ----------------------------------------------------------

combine_risk <- function(model, y) {
  
name <- case_when(y=="transient" ~ "Transient, absorbing",
                  y=="permanent" ~ "Permanent",
                  y=="repeated" ~ "Transient, repeated")

# Crude results
crude.risk <- read_csv(paste0("../results/", model, "_", y, ".csv")) %>% 
  select(time, bias_censor, bias_gap_allt, bias_gap_1t,
               censor_sd, gap_allt_sd, gap_1t_sd) %>%
  summarize(across(!time, ~ mean(.x)))

# Adjusted results
res1 <- read_csv(paste0("../results/", model, "_", y, "_wt.csv"))
res2 <- read_csv(paste0("../results/", model, "_", y, "_mi.csv")) %>% 
  mutate(bias_mi = mi_avg - truth_avg) %>%  
  select(-bias_truth)
adj.risk <- filter(res1, estimand=="risk") %>% 
  left_join(filter(res2, estimand=="risk"), by="t") %>% 
  select(t, bias_censor_s, bias_obs_wt, bias_mi,
         censor_s_sd, obs_wt_sd, mi_sd) %>% 
  summarize(across(!t, ~ mean(.x)))

# Concatenate and transpose
bias <- bind_cols(crude.risk, adj.risk) %>%
  select(bias_censor, bias_gap_allt, bias_gap_1t, bias_censor_s, bias_obs_wt, bias_mi) %>% 
  pivot_longer(everything(), names_to="Approach", values_to="Bias") %>% 
  mutate(Approach = factor(Approach, 
                           levels=c("bias_censor", "bias_gap_1t", "bias_gap_allt", 
                                    "bias_censor_s", "bias_obs_wt", "bias_mi"),
                           labels=c("Crude censored", "Look-back period",
                                    "All-time", "IPCW", "IPOW", "MI")))

ese <- bind_cols(crude.risk, adj.risk) %>%
  select(censor_sd, gap_allt_sd, gap_1t_sd, censor_s_sd, obs_wt_sd, mi_sd) %>% 
  pivot_longer(everything(), names_to="Approach", values_to="ESE") %>% 
  mutate(Approach = factor(Approach, 
                           levels=c("censor_sd", "gap_1t_sd", "gap_allt_sd", 
                                    "censor_s_sd", "obs_wt_sd", "mi_sd"),
                           labels=c("Crude censored", "Look-back period",
                                    "All-time", "IPCW", "IPOW", "MI")))

res <- left_join(bias, ese, by="Approach") %>%
  mutate(Outcome = name)

return(res)
}

combine_rate <- function(model, y) {
  
  name <- case_when(y=="transient" ~ "Transient, absorbing",
                    y=="permanent" ~ "Permanent",
                    y=="repeated" ~ "Transient, repeated")
  
  # Crude results
  crude.rate <- read_csv(paste0("../results/", model, "_", y, "_rate.csv")) %>% 
    select(bias_rate_censor, bias_rate_gap_allt, bias_rate_gap_1t,
           rate_censor_sd, rate_gap1_sd, rate_gap2_sd) %>%
    summarize(across(everything(), ~ mean(.x)))
  
  # Adjusted results
  res1 <- read_csv(paste0("../results/", model, "_", y, "_wt.csv"))
  res2 <- read_csv(paste0("../results/", model, "_", y, "_mi.csv")) %>% 
    mutate(bias_mi = mi_avg - truth_avg) %>%  
    select(-bias_truth)
  adj.rate <- filter(res1, estimand=="rate") %>% 
    left_join(filter(res2, estimand=="rate"), by="t") %>% 
    select(bias_censor_s, bias_obs_wt, bias_mi,
           censor_s_sd, obs_wt_sd, mi_sd) %>% 
    summarize(across(everything(), ~ mean(.x)))
  
  # Concatenate and transpose
  bias <- bind_cols(crude.rate, adj.rate) %>%
    select(bias_rate_censor, bias_rate_gap_allt, bias_rate_gap_1t, bias_censor_s, bias_obs_wt, bias_mi) %>% 
    pivot_longer(everything(), names_to="Approach", values_to="Bias") %>% 
    mutate(Approach = factor(Approach, 
                             levels=c("bias_rate_censor", "bias_rate_gap_1t", "bias_rate_gap_allt", 
                                      "bias_censor_s", "bias_obs_wt", "bias_mi"),
                             labels=c("Crude censored", "Look-back period",
                                      "All-time", "IPCW", "IPOW", "MI")))
  
  ese <- bind_cols(crude.rate, adj.rate) %>%
    select(rate_censor_sd, rate_gap1_sd, rate_gap2_sd, censor_s_sd, obs_wt_sd, mi_sd) %>% 
    pivot_longer(everything(), names_to="Approach", values_to="ESE") %>% 
    mutate(Approach = factor(Approach, 
                             levels=c("rate_censor_sd", "rate_gap2_sd", "rate_gap1_sd", 
                                      "censor_s_sd", "obs_wt_sd", "mi_sd"),
                             labels=c("Crude censored", "Look-back period",
                                      "All-time", "IPCW", "IPOW", "MI")))
  
  res <- left_join(bias, ese, by="Approach") %>%
    mutate(Outcome = name)
  
  return(res)
}


res1.risk <- lapply(c("transient", "permanent", "repeated"), function(tt) {combine_risk("dag1", tt)})
res1.risk <- bind_rows(res1.risk) %>% 
  mutate(Outcome = factor(Outcome, levels=c("Transient, absorbing",
                                            "Transient, repeated",
                                            "Permanent"))) 
res1.rate <- lapply(c("transient", "permanent", "repeated"), function(tt) {combine_rate("dag1", tt)})
res1.rate <- bind_rows(res1.rate) %>% 
  mutate(Outcome = factor(Outcome, levels=c("Transient, absorbing",
                                            "Transient, repeated",
                                            "Permanent"))) 

res2.risk <- lapply(c("transient", "permanent", "repeated"), function(tt) {combine_risk("dag2.1", tt)})
res2.risk <- bind_rows(res2.risk) %>% 
  mutate(Outcome = factor(Outcome, levels=c("Transient, absorbing",
                                             "Transient, repeated",
                                             "Permanent"))) 
res2.rate <- lapply(c("transient", "permanent", "repeated"), function(tt) {combine_rate("dag2.1", tt)})
res2.rate <- bind_rows(res2.rate) %>% 
  mutate(Outcome = factor(Outcome, levels=c("Transient, absorbing",
                                            "Transient, repeated",
                                            "Permanent"))) 


# Visualize bias ----------------------------------------------------------

# Scenario 1: Risk
p1 <- ggplot(res1.risk, aes(x=Outcome, y=Approach, fill=Bias)) +
  labs(tag="A)", title="Scenario 1: Risk", x="", y="", fill="Average Bias:", size="") + 
  geom_tile() +
  #geom_point(shape=21, color="black") +
  scale_y_discrete(expand=c(0, 0)) +
  scale_x_discrete(expand=c(0, 0), labels=label_wrap(10)) +
  scale_fill_distiller(limits=c(-0.1, 0.1), breaks=seq(-0.09, 0.09, 0.03),
                       palette="RdBu")
  #guides(size="none") 

p2 <- ggplot(res2.risk, aes(x=Outcome, y=Approach, fill=Bias)) +
  labs(tag="C)", title="Scenario 2: Risk", x="", y="", fill="Average Bias:", size="") + 
  geom_tile() +
  #geom_point(shape=21, color="black") +
  scale_y_discrete(expand=c(0, 0)) +
  scale_x_discrete(expand=c(0, 0), labels=label_wrap(10)) +
  scale_fill_distiller(limits=c(-0.1, 0.1), breaks=seq(-0.09, 0.09, 0.03),
                        palette="RdBu")
  #guides(size="none") 

p3 <- ggplot(res1.rate, aes(x=Outcome, y=Approach, fill=Bias)) +
  labs(tag="B)", title="Scenario 1: Rate", x="", y="", fill="Bias:", size="") + 
  geom_tile() +
  #geom_point(shape=21, color="black") +
  scale_y_discrete(expand=c(0, 0)) +
  scale_x_discrete(expand=c(0, 0), labels=label_wrap(10)) +
  scale_fill_distiller(limits=c(-0.027, 0.027), breaks=seq(-0.025, 0.025, 0.005),
                       palette="RdBu")

p4 <- ggplot(res2.rate, aes(x=Outcome, y=Approach, fill=Bias)) +
  labs(tag="D)", title="Scenario 2: Rate", x="", y="", fill="Bias:", size="") + 
  geom_tile() +
  #geom_point(shape=21, color="black") +
  scale_y_discrete(expand=c(0, 0)) +
  scale_x_discrete(expand=c(0, 0), labels=label_wrap(10)) +
  scale_fill_distiller(limits=c(-0.027, 0.027), breaks=seq(-0.025, 0.025, 0.005),
                       palette="RdBu")

p12 <- p1 / p2 + plot_layout(guides="collect", ncol=1) & thm
p34 <- p3 / p4 + plot_layout(guides="collect", ncol=1) & thm
jpeg("../figures/heatmap.jpeg", width=10, height=8, units="in", res=300)
p12 | p34 & thm
dev.off()
