
library("tidyverse")
library("patchwork")

thm <- theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12, color="black"))

model <- "dag2.2"


# Transient outcome -------------------------------------------------------

res.t1 <- read_csv(paste0("../results/", model, "_transient_wt.csv"))
res.t2 <- read_csv(paste0("../results/", model, "_transient_mi.csv")) %>% 
  mutate(bias_mi = mi_avg - truth_avg) %>%  
  select(-bias_truth)
res.risk.t <- filter(res.t1, estimand=="risk") %>% 
  left_join(filter(res.t2, estimand=="risk"), by="t") %>% 
  select(t, bias_truth, bias_censor_s, bias_obs_wt, bias_mi)

res.rate.t <- filter(res.t1, estimand=="rate") %>% 
  left_join(filter(res.t2, estimand=="rate"), by="t") %>% 
  select(bias_truth, bias_censor_s, bias_obs_wt, bias_mi) %>% 
  pivot_longer(cols=everything(), names_to="approach", values_to="bias") %>% 
  mutate(approach = factor(approach, 
                           levels=c("bias_truth", "bias_censor_s", "bias_obs_wt", "bias_mi"),
                           labels=c("Truth", "IPCW", "IPOW", "MI")))

p1 <- ggplot(res.risk.t, aes(x=t)) +
  thm +
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="A)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_censor_s, color="IPCW"), linewidth=0.75) +
  geom_line(aes(y=bias_obs_wt, color="IPOW"), linewidth=0.75) +
  geom_line(aes(y=bias_mi, color="MI"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "IPCW"="#1f78b4",
                              "IPOW"="#33a02c", "MI"="#b2df8a"))

p2 <- ggplot(res.rate.t, aes(x=approach, color=approach)) +
  thm +
  labs(x="Approach", y="Bias in the incidence rate", tag="B)") +
  geom_hline(aes(yintercept=0), linewidth=0.75, linetype="dashed") +
  geom_segment(aes(y=0, yend=bias), linewidth=0.75) +
  geom_point(aes(y=bias), size=2) +
  scale_color_manual(values=c("Truth"="black", "IPCW"="#1f78b4",
                              "IPOW"="#33a02c", "MI"="#b2df8a")) +
  guides(color="none") +
  scale_y_continuous(limits=c(-0.03, 0.03), breaks=seq(-0.03, 0.03, 0.01)) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  coord_flip()

plot1 <- p1 + p2 + 
  #plot_layout(nrow=1, guides="collect") +
  plot_annotation(title=paste0("Transient outcome")) & 
  thm

# jpeg(paste0("../figures/", model, "_transient.jpeg"), height=5, width=8, units="in", res=300)
# print(plot)
# dev.off()


# Repeated outcome --------------------------------------------------------

res.r1 <- read_csv(paste0("../results/", model, "_repeated_wt.csv"))
res.r2 <- read_csv(paste0("../results/", model, "_repeated_mi.csv")) %>% 
  mutate(bias_mi = mi_avg - truth_avg) %>%  # re-run code to fix this
  select(-bias_truth)
res.risk.r <- filter(res.r1, estimand=="risk") %>% 
  left_join(filter(res.r2, estimand=="risk"), by="t") %>% 
  select(t, bias_truth, bias_censor_s, bias_obs_wt, bias_mi)

res.rate.r <- filter(res.r1, estimand=="rate") %>% 
  left_join(filter(res.r2, estimand=="rate"), by="t") %>% 
  select(bias_truth, bias_censor_s, bias_obs_wt, bias_mi) %>% 
  pivot_longer(cols=everything(), names_to="approach", values_to="bias") %>% 
  mutate(approach = factor(approach, 
                           levels=c("bias_truth", "bias_censor_s", "bias_obs_wt", "bias_mi"),
                           labels=c("Truth", "IPCW", "IPOW", "MI")))

p3 <- ggplot(res.risk.r, aes(x=t)) +
  thm + 
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="C)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_censor_s, color="IPCW"), linewidth=0.75) +
  geom_line(aes(y=bias_obs_wt, color="IPOW"), linewidth=0.75) +
  geom_line(aes(y=bias_mi, color="MI"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "IPCW"="#1f78b4", 
                              "IPOW"="#33a02c", "MI"="#b2df8a"))

p4 <- ggplot(res.rate.r, aes(x=approach, color=approach)) +
  thm +
  labs(x="Approach", y="Bias in the incidence rate", tag="D)") +
  geom_hline(aes(yintercept=0), linewidth=0.75, linetype="dashed") +
  geom_segment(aes(y=0, yend=bias), linewidth=0.75) +
  geom_point(aes(y=bias), size=2) +
  scale_color_manual(values=c("Truth"="black", "IPCW"="#1f78b4", 
                              "IPOW"="#33a02c", "MI"="#b2df8a")) +
  guides(color="none") +
  scale_y_continuous(limits=c(-0.03, 0.03), breaks=seq(-0.03, 0.03, 0.01)) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  coord_flip()

plot2 <- p3 + p4 + 
  #plot_layout(nrow=1, guides="collect") +
  plot_annotation(title=paste0("Repeated outcome")) & 
  thm


# Permanent outcome -------------------------------------------------------

res.p1 <- read_csv(paste0("../results/", model, "_permanent_wt.csv"))
res.p2 <- read_csv(paste0("../results/", model, "_permanent_mi.csv")) %>% 
  mutate(bias_mi = mi_avg - truth_avg) %>%  # re-run code to fix this
  select(-bias_truth)
res.risk.p <- filter(res.p1, estimand=="risk") %>% 
  left_join(filter(res.p2, estimand=="risk"), by="t") %>% 
  select(t, bias_truth, bias_censor_s, bias_obs_wt, bias_mi)

res.rate.p <- filter(res.p1, estimand=="rate") %>% 
  left_join(filter(res.p2, estimand=="rate"), by="t") %>% 
  select(bias_truth, bias_censor_s, bias_obs_wt, bias_mi) %>% 
  pivot_longer(cols=everything(), names_to="approach", values_to="bias") %>% 
  mutate(approach = factor(approach, 
                           levels=c("bias_truth", "bias_censor_s", "bias_obs_wt", "bias_mi"),
                           labels=c("Truth", "IPCW", "IPOW", "MI")))

p5 <- ggplot(res.risk.p, aes(x=t)) +
  thm +
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="E)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_censor_s, color="IPCW"), linewidth=0.75) +
  geom_line(aes(y=bias_obs_wt, color="IPOW"), linewidth=0.75) +
  geom_line(aes(y=bias_mi, color="MI"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "IPCW"="#1f78b4", 
                              "IPOW"="#33a02c", "MI"="#b2df8a"))

p6 <- ggplot(res.rate.p, aes(x=approach, color=approach)) +
  thm +
  labs(x="Approach", y="Bias in the incidence rate", tag="F)") +
  geom_hline(aes(yintercept=0), linewidth=0.75, linetype="dashed") +
  geom_segment(aes(y=0, yend=bias), linewidth=0.75) +
  geom_point(aes(y=bias), size=2) +
  scale_color_manual(values=c("Truth"="black", "IPCW"="#1f78b4", 
                              "IPOW"="#33a02c", "MI"="#b2df8a")) +
  guides(color="none") +
  scale_y_continuous(limits=c(-0.03, 0.03), breaks=seq(-0.03, 0.03, 0.01)) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  coord_flip()

plot3 <- p5 + p6 + 
  #plot_layout(nrow=1, guides="collect") +
  plot_annotation(title=paste0("Permanent outcome")) & 
  thm

# jpeg(paste0("../figures/", model, "_permanent.jpeg"), height=5, width=8, units="in", res=300)
# print(plot)
# dev.off()


# Combine plots -----------------------------------------------------------

panel <- (plot1 | plot2 | plot3) + 
  plot_layout(nrow=3, guides="collect") &
  thm

jpeg(paste0("../figures/", model, "_adj.jpeg"), height=12, width=10, units="in", res=300)
print(panel)
dev.off()

