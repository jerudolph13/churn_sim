
###########################################################################
#
# Project: How to handle churn
#
# Purpose: Visualize unadjusted results
#
# Author: Jacqueline Rudolph
#
# Last Update: 01 Oct 2025
#
###########################################################################


library("tidyverse")
library("patchwork")

thm <- theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=8, color="black"),
        legend.title = element_text(size=8, color="black"),
        axis.text = element_text(size=7, color="black"),
        axis.title = element_text(size=8, color="black"),
        plot.tag = element_text(size=8, color="black"))

palette <- c("Truth"="black", "Censor"="#1b9e77", 
             "All-time"="#a6761d", "Look-back"="#7570b3")
palette.bw <- c("Truth"="white", "Censor"="#bbbbbb", 
                "All-time"="#777777", "Look-back"="#333333")


# Read results ------------------------------------------------------------

risk <- read_csv("../results/risk.csv")
rate <- read_csv("../results/rate.csv")


for (model in c("dag1", "dag2.1", "dag2.2", "dag2.3", "dag2.4",
                "dag3", "dag3.1", "dag3.2", "dag3.3",
                "dag4", "dag4.1")) {

  if (!dir.exists(paste0("../figures/", model))) {
    dir.create(paste0("../figures/", model))
  }
  
list <- c("Truth", "Censor", "All-time", "Look-back")

  
# Transient outcome -------------------------------------------------------

# Risk
res.risk.t <- risk %>% 
  filter(outcome=="transient") %>% 
  filter(dag==model) %>% 
  select(-c(outcome, dag)) %>% 
  pivot_longer(cols=!c(rep, t), names_to="approach", values_to="risk") %>% 
  mutate(approach = factor(approach, 
                           levels=c("truth", "censor", "gap_allt", "gap_1t",
                                    "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                                    "obs_wt", "obs_wt_z", "mi", "mi_z"),
                           labels=c("Truth", "Censor", "All-time", "Look-back", 
                                    "IPCW2", "IPCW", "IPCW2 (Z)", "IPCW (Z)", 
                                    "IPOW", "IPOW (Z)", "MI", "MI (Z)"))) %>% 
  filter(approach %in% list)

# Rate
res.rate.t <- rate %>% 
  filter(outcome=="transient") %>% 
  filter(dag==model) %>% 
  select(-c(outcome, dag)) %>% 
  pivot_longer(cols=!rep, names_to="approach", values_to="rate") %>% 
  mutate(approach = factor(approach, 
                           levels=c("truth", "censor", "gap_allt", "gap_1t",
                                    "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                                    "obs_wt", "obs_wt_z", "mi", "mi_z"),
                           labels=c("Truth", "Censor", "All-time", "Look-back", 
                                    "IPCW2", "IPCW", "IPCW2 (Z)", "IPCW (Z)", 
                                    "IPOW", "IPOW (Z)", "MI", "MI (Z)"))) %>% 
  filter(approach %in% list)

# Plot without outliers (color)
p1 <- ggplot(res.risk.t, aes(x=as.factor(t), y=risk, color=approach)) +
  thm +
  labs(x="Visit", y="Risk of Transient Outcome", color="Analysis", tag="A)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
  scale_color_manual(values=palette)

p2 <- ggplot(res.rate.t, aes(x=approach, y=rate, color=approach)) +
  thm +
  labs(x="Approach", y="Rate of Transient Outcome", color="Analysis", tag="B)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  scale_y_continuous(limits=c(0, 0.2), expand=c(0, 0)) +
  scale_color_manual(values=palette)

# Plot without outliers (black & white)
p1_2 <- ggplot(res.risk.t, aes(x=as.factor(t), y=risk, fill=approach)) +
  thm +
  labs(x="Visit", y="Risk of Transient Outcome", fill="Analysis", tag="A)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
  scale_fill_manual(values=palette.bw)

p2_2 <- ggplot(res.rate.t, aes(x=approach, y=rate, fill=approach)) +
  thm +
  labs(x="Approach", y="Rate of Transient Outcome", fill="Analysis", tag="B)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  scale_y_continuous(limits=c(0, 0.2), expand=c(0, 0)) +
  scale_fill_manual(values=palette.bw)


# Repeated outcome --------------------------------------------------------

# Risk
res.risk.r <- risk %>% 
  filter(outcome=="repeated") %>% 
  filter(dag==model) %>% 
  select(-c(outcome, dag)) %>% 
  pivot_longer(cols=!c(rep, t), names_to="approach", values_to="risk") %>% 
  mutate(approach = factor(approach, 
                           levels=c("truth", "censor", "gap_allt", "gap_1t",
                                    "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                                    "obs_wt", "obs_wt_z", "mi", "mi_z"),
                           labels=c("Truth", "Censor", "All-time", "Look-back", 
                                    "IPCW2", "IPCW", "IPCW2 (Z)", "IPCW (Z)", 
                                    "IPOW", "IPOW (Z)", "MI", "MI (Z)"))) %>% 
  filter(approach %in% list)

# Rate
res.rate.r <- rate %>% 
  filter(outcome=="repeated") %>% 
  filter(dag==model) %>% 
  select(-c(outcome, dag)) %>% 
  pivot_longer(cols=!rep, names_to="approach", values_to="rate") %>% 
  mutate(approach = factor(approach, 
                           levels=c("truth", "censor", "gap_allt", "gap_1t",
                                    "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                                    "obs_wt", "obs_wt_z", "mi", "mi_z"),
                           labels=c("Truth", "Censor", "All-time", "Look-back", 
                                    "IPCW2", "IPCW", "IPCW2 (Z)", "IPCW (Z)", 
                                    "IPOW", "IPOW (Z)", "MI", "MI (Z)"))) %>% 
  filter(approach %in% list)

# Plot without outliers (color)
p3 <- ggplot(res.risk.r, aes(x=as.factor(t), y=risk, color=approach)) +
  thm +
  labs(x="Visit", y="Risk of Repeated Outcome", color="Analysis", tag="C)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
  scale_color_manual(values=palette)

p4 <- ggplot(res.rate.r, aes(x=approach, y=rate, color=approach)) +
  thm +
  labs(x="Approach", y="Rate of Repeated Outcome", color="Analysis", tag="D)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  scale_y_continuous(limits=c(0, 0.2), expand=c(0, 0)) +
  scale_color_manual(values=palette)

# Plot without outliers (black & white)
p3_2 <- ggplot(res.risk.r, aes(x=as.factor(t), y=risk, fill=approach)) +
  thm +
  labs(x="Visit", y="Risk of Repeated Outcome", fill="Analysis", tag="C)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
  scale_fill_manual(values=palette.bw)

p4_2 <- ggplot(res.rate.r, aes(x=approach, y=rate, fill=approach)) +
  thm +
  labs(x="Approach", y="Rate of Repeated Outcome", fill="Analysis", tag="D)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  scale_y_continuous(limits=c(0, 0.2), expand=c(0, 0)) +
  scale_fill_manual(values=palette.bw)


# Permanent outcome -------------------------------------------------------

# Risk
res.risk.p <- risk %>% 
  filter(outcome=="permanent") %>% 
  filter(dag==model) %>% 
  select(-c(outcome, dag)) %>% 
  pivot_longer(cols=!c(rep, t), names_to="approach", values_to="risk") %>% 
  mutate(approach = factor(approach, 
                           levels=c("truth", "censor", "gap_allt", "gap_1t",
                                    "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                                    "obs_wt", "obs_wt_z", "mi", "mi_z"),
                           labels=c("Truth", "Censor", "All-time", "Look-back", 
                                    "IPCW2", "IPCW", "IPCW2 (Z)", "IPCW (Z)", 
                                    "IPOW", "IPOW (Z)", "MI", "MI (Z)"))) %>% 
  filter(approach %in% list)

# Rate
res.rate.p <- rate %>% 
  filter(outcome=="permanent") %>% 
  filter(dag==model) %>% 
  select(-c(outcome, dag)) %>% 
  pivot_longer(cols=!rep, names_to="approach", values_to="rate") %>% 
  mutate(approach = factor(approach, 
                           levels=c("truth", "censor", "gap_allt", "gap_1t",
                                    "censor_us", "censor_s", "censor_us_z", "censor_s_z", 
                                    "obs_wt", "obs_wt_z", "mi", "mi_z"),
                           labels=c("Truth", "Censor", "All-time", "Look-back", 
                                    "IPCW2", "IPCW", "IPCW2 (Z)", "IPCW (Z)", 
                                    "IPOW", "IPOW (Z)", "MI", "MI (Z)"))) %>% 
  filter(approach %in% list)

# Plot without outliers (color)
p5 <- ggplot(res.risk.p, aes(x=as.factor(t), y=risk, color=approach)) +
  thm +
  labs(x="Visit", y="Risk of Permanent Outcome", color="Analysis", tag="E)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
  scale_color_manual(values=palette)

p6 <- ggplot(res.rate.p, aes(x=approach, y=rate, color=approach)) +
  thm +
  labs(x="Approach", y="Rate of Permanent Outcome", color="Analysis", tag="F)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  scale_y_continuous(limits=c(0, 0.2), expand=c(0, 0)) +
  scale_color_manual(values=palette)

# Plot without outliers (black & white)
p5_2 <- ggplot(res.risk.p, aes(x=as.factor(t), y=risk, fill=approach)) +
  thm +
  labs(x="Visit", y="Risk of Permanent Outcome", fill="Analysis", tag="E)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
  scale_fill_manual(values=palette.bw)

p6_2 <- ggplot(res.rate.p, aes(x=approach, y=rate, fill=approach)) +
  thm +
  labs(x="Approach", y="Rate of Permanent Outcome", fill="Analysis", tag="F)") +
  geom_boxplot(outliers=F, linewidth=0.4, fatten=1) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  scale_y_continuous(limits=c(0, 0.2), expand=c(0, 0)) +
  scale_fill_manual(values=palette.bw)


# Combine plots -----------------------------------------------------------

panel <- (p1 + p2 + p3 + p4 + p5 + p6) + 
  plot_layout(nrow=3, ncol=2, guides="collect", widths=c(2, 1, 2, 1, 2, 1)) &
  thm

jpeg(paste0("../figures/", model, "/", model, "_crude.jpeg"), height=17, width=17, units="cm", res=300)
  print(panel)
dev.off()

panel2 <- (p1_2 + p2_2 + p3_2 + p4_2 + p5_2 + p6_2) +
  plot_layout(nrow=3, ncol=2, guides="collect", widths=c(2, 1, 2, 1, 2, 1)) &
  thm

jpeg(paste0("../figures/", model, "/", model, "_crude_bw.jpeg"), height=17, width=17, units="cm", res=300)
  print(panel2)
dev.off()

}


