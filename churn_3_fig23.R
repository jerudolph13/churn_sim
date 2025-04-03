
library("tidyverse")
library("patchwork")

thm <- theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12, color="black"))

model <- "dag1"


# Follow-up figure --------------------------------------------------------

outcome <- "transient"
dat <- read_csv(paste0("../data/", model, "_", outcome, ".csv")) %>% 
  mutate(last_t = t-1) %>% 
  filter(rep==1) %>% 
  filter(M==0) %>% 
  filter(t>0)

p <- ggplot(data=dat, aes(y=id)) +
  thm + theme(axis.text.y = element_blank()) +
  labs(x="Visit", y="Individual") +
  geom_segment(aes(x=last_t, xend=t, y=id, yend=id)) +
  scale_x_continuous(limits=c(0, 10), breaks=seq(1, 10, 1), expand=c(0,0)) +
  scale_y_continuous(expand=c(0, 0))
  
jpeg("../figures/example_followup.jpeg", height=8, width=4, units="in", res=300)
print(p)
dev.off()

# Transient outcome -------------------------------------------------------

for (model in c("dag1", "dag2.1", "dag2.2", "dag2.3", "dag2.4")) {
  
res.risk.t <- read_csv(paste0("../results/", model, "_transient.csv"))
res.rate.t <- read_csv(paste0("../results/", model, "_transient_rate.csv")) %>% 
  filter(t==1) %>% 
  select(bias_rate_truth, bias_rate_censor, bias_rate_gap_allt, bias_rate_gap_1t) %>% 
  pivot_longer(cols=everything(), names_to="approach", values_to="bias") %>% 
  mutate(approach = factor(approach, 
                           levels=c("bias_rate_truth", "bias_rate_censor", "bias_rate_gap_allt", "bias_rate_gap_1t"),
                           labels=c("Truth", "Censor", "All-time", "Look-back")))

# p1 <- ggplot(res, aes(x=time)) +
#   labs(x="Visit", y="Risk", color="Analysis") +
#   geom_step(aes(y=truth, color="Truth"), linewidth=0.75) +
#   geom_step(aes(y=censor, color="Censor"), linewidth=0.75) +
#   geom_step(aes(y=gap_allt, color="All-time"), linewidth=0.75) +
#   geom_step(aes(y=gap_1t, color="Look-back"), linewidth=0.75) +
#   scale_x_continuous(breaks=seq(1, 10, 1)) +
#   scale_y_continuous(limits=c(0.0, 0.7), breaks=seq(0, 0.7, 0.1)) +
#   scale_color_manual(values=c("Truth"="black", "Censor"="#66c2a5", "All-time"="#fc8d62",
#                      "Look-back"="#8da0cb"))

p1 <- ggplot(res.risk.t, aes(x=time)) +
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="A)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_censor, color="Censor"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_allt, color="All-time"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_1t, color="Look-back"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "Censor"="#66c2a5", "All-time"="#fc8d62",
                              "Look-back"="#8da0cb"))

p2 <- ggplot(res.rate.t, aes(x=approach, color=approach)) +
  thm +
  labs(x="Approach", y="Bias in the incidence rate", tag="B)") +
  geom_hline(aes(yintercept=0), linewidth=0.75, linetype="dashed") +
  geom_segment(aes(y=0, yend=bias), linewidth=0.75) +
  geom_point(aes(y=bias), size=2) +
  scale_color_manual(values=c("Truth"="black", "Censor"="#66c2a5", "All-time"="#fc8d62",
                              "Look-back"="#8da0cb")) +
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


# Permanent outcome -------------------------------------------------------

res.risk.p <- read_csv(paste0("../results/", model, "_permanent.csv"))
res.rate.p <- read_csv(paste0("../results/", model, "_permanent_rate.csv")) %>% 
  filter(t==1) %>% 
  select(bias_rate_truth, bias_rate_censor, bias_rate_gap_allt, bias_rate_gap_1t) %>% 
  pivot_longer(cols=everything(), names_to="approach", values_to="bias") %>% 
  mutate(approach = factor(approach, 
                           levels=c("bias_rate_truth", "bias_rate_censor", "bias_rate_gap_allt", "bias_rate_gap_1t"),
                           labels=c("Truth", "Censor", "All-time", "Look-back")))

p3 <- ggplot(res.risk.p, aes(x=time)) +
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="E)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_censor, color="Censor"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_allt, color="All-time"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_1t, color="Look-back"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "Censor"="#66c2a5", "All-time"="#fc8d62",
                              "Look-back"="#8da0cb"))

p4 <- ggplot(res.rate.p, aes(x=approach, color=approach)) +
  thm +
  labs(x="Approach", y="Bias in the incidence rate", tag="F)") +
  geom_hline(aes(yintercept=0), linewidth=0.75, linetype="dashed") +
  geom_segment(aes(y=0, yend=bias), linewidth=0.75) +
  geom_point(aes(y=bias), size=2) +
  scale_color_manual(values=c("Truth"="black", "Censor"="#66c2a5", "All-time"="#fc8d62",
                              "Look-back"="#8da0cb")) +
  guides(color="none") +
  scale_y_continuous(limits=c(-0.03, 0.03), breaks=seq(-0.03, 0.03, 0.01)) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  coord_flip()

plot2 <- p3 + p4 + 
  #plot_layout(nrow=1, guides="collect") +
  plot_annotation(title=paste0("Permanent outcome")) & 
  thm

# jpeg(paste0("../figures/", model, "_permanent.jpeg"), height=5, width=8, units="in", res=300)
# print(plot)
# dev.off()


# Repeated outcome --------------------------------------------------------

res.risk.r <- read_csv(paste0("../results/", model, "_repeated.csv"))
res.rate.r <- read_csv(paste0("../results/", model, "_repeated_rate.csv")) %>% 
  filter(t==1) %>% 
  select(bias_rate_truth, bias_rate_censor, bias_rate_gap_allt, bias_rate_gap_1t) %>% 
  pivot_longer(cols=everything(), names_to="approach", values_to="bias") %>% 
  mutate(approach = factor(approach, 
                           levels=c("bias_rate_truth", "bias_rate_censor", "bias_rate_gap_allt", "bias_rate_gap_1t"),
                           labels=c("Truth", "Censor", "All-time", "Look-back")))

p5 <- ggplot(res.risk.r, aes(x=time)) +
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="C)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_censor, color="Censor"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_allt, color="All-time"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_1t, color="Look-back"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "Censor"="#66c2a5", "All-time"="#fc8d62",
                              "Look-back"="#8da0cb"))

p6 <- ggplot(res.rate.r, aes(x=approach, color=approach)) +
  thm +
  labs(x="Approach", y="Bias in the incidence rate", tag="D)") +
  geom_hline(aes(yintercept=0), linewidth=0.75, linetype="dashed") +
  geom_segment(aes(y=0, yend=bias), linewidth=0.75) +
  geom_point(aes(y=bias), size=2) +
  scale_color_manual(values=c("Truth"="black", "Censor"="#66c2a5", "All-time"="#fc8d62",
                              "Look-back"="#8da0cb")) +
  guides(color="none") +
  scale_y_continuous(limits=c(-0.03, 0.03), breaks=seq(-0.03, 0.03, 0.01)) +
  scale_x_discrete(labels=function(x){str_wrap(x, width=15)}) +
  coord_flip()


plot3 <- p5 + p6 + 
  #plot_layout(nrow=1, guides="collect") +
  plot_annotation(title=paste0("Repeated outcome")) & 
  thm

panel <- (plot1 | plot3 | plot2) + 
  plot_layout(nrow=3, guides="collect") &
  thm

jpeg(paste0("../figures/", model, ".jpeg"), height=12, width=10, units="in", res=300)
print(panel)
dev.off()

}


# Interval censoring ------------------------------------------------------

model <- "dag1"
outcome <- "permanent"

res1 <- read_csv(paste0("../results/", model, "_permanent.csv"))
res2 <- read_csv(paste0("../results/", model, "_permanent_ic.csv"))

res.ic <- left_join(res1, res2, by="time") %>% 
  mutate(bias_interval = interval_avg - truth)

p1 <- ggplot(res.ic, aes(x=time)) +
  thm +
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="A)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_interval, color="Interval censored"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_allt, color="All-time"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "Interval censored"="#A63A50", "All-time"="#fc8d62"))

model <- "dag2.1"
outcome <- "permanent"

res1 <- read_csv(paste0("../results/", model, "_permanent.csv"))
res2 <- read_csv(paste0("../results/", model, "_permanent_ic.csv"))

res.ic <- left_join(res1, res2, by="time") %>% 
  mutate(bias_interval = interval_avg - truth)

p2 <- ggplot(res.ic, aes(x=time)) +
  labs(x="Visit", y="Bias in the risk", color="Analysis", tag="B)") +
  geom_step(aes(y=bias_truth, color="Truth"), linewidth=0.75) +
  geom_line(aes(y=bias_interval, color="Interval censored"), linewidth=0.75) +
  geom_line(aes(y=bias_gap_allt, color="All-time"), linewidth=0.75) +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  scale_y_continuous(limits=c(-0.15, 0.15), breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  scale_color_manual(values=c("Truth"="black", "Interval censored"="#A63A50", "All-time"="#fc8d62"))

plot <- p1 + p2 + 
  plot_layout(nrow=1, guides="collect") & 
  thm


jpeg(paste0("../figures/interval_censor.jpeg"), height=5, width=8, units="in", res=300)
print(plot)
dev.off()