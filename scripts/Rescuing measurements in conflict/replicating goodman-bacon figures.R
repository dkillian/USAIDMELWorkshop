# MENA MELS workshop
# Rescuing impact measurements
# Replicating Goodman-Bacon figures

set.seed(123)  

d <- data.frame(Time = -10:20,
                early = c(rep(12, 11), rep(18, 20)) + rnorm(31, sd = .3),  # Early group
                late = c(rep(8, 21), rep(14, 10)) + rnorm(31, sd=.3),  # Late group
                untreated = c(rep(3, 31)) + rnorm(31, sd = .3))  # Untreated group

dL <- pivot_longer(d,
                   cols=2:4,
                   names_to="Group",
                   values_to="Y")

head(dL)
psych::describe(dL)

# panel a ---- 

ggplot(filter(dL, Group!="late"), aes(x = Time, y = Y, color = Group)) +
  geom_vline(xintercept = 0, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_line() +
  #  stat_smooth(span=.2) +
  scale_color_manual(values = c(usaid_red, dark_grey), 
                     labels=c("Early treated", "Never treated")) +
#  scale_x_continuous(labels=paste(10:-20, sep="")) +
  scale_y_continuous(limits=c(0,20),
                     breaks=seq(0,20,5)) +
  labs(x = "Time", 
       y = "Outcome",
       title = "A. <span style='color:#BA0C2F;'>Early treated</span> vs. <span style='color:#6C6463;'>Never treated</span>") +
  theme(legend.title = element_blank(),
        axis.title.y=element_text(angle=0, vjust=.45),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=element_markdown())

ggsave("scripts/Rescuing measurements in conflict/Baker panel a replication.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# panel b ---- 

ggplot(filter(dL, Group!="early"), aes(x = Time, y = Y, color = Group)) +
  geom_vline(xintercept = 10, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_line() +
  #  stat_smooth(span=.2) +
  scale_color_manual(values = c(usaid_blue, dark_grey), 
                     labels=c("Late treated", "Never treated")) +
  #  scale_x_continuous(labels=paste(10:-20, sep="")) +
  scale_y_continuous(limits=c(0,20),
                     breaks=seq(0,20,5)) +
  labs(x = "Time", 
       y = "Outcome",
       title = "B. <span style = 'color:#002F6C;'>Late treated</span> vs. <span style = 'color:#6C6463;'>Never treated<span>") +
  #theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.y=element_text(angle=0, vjust=.45),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=element_markdown())

ggsave("scripts/Rescuing measurements in conflict/Baker panel b replication.png",
       device="png",
       type="cairo",
       height=4,
       width=7)



# panel c ---- 

head(dL)

panc <- dL %>%
  filter(Group!="untreated") %>%
  mutate(Y=ifelse(Time>10, NA, Y))  
#         Time<11)

head(panc)

ggplot(panc, aes(x = Time, y = Y, color = Group)) +
  geom_vline(xintercept = 0, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_vline(xintercept = 10, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_line(data=filter(panc, Time<11)) +
  #  stat_smooth(span=.2) +
  scale_color_manual(values = c(usaid_red, usaid_blue), 
                     labels=c("Early treated", "Late treated")) +
  scale_x_continuous(limits=c(-10,20)) +
  scale_y_continuous(limits=c(0,20),
                     breaks=seq(0,20,5)) +
  labs(x = "Time", 
       y = "Outcome",
       title = "C. <span style = 'color:#BA0C2F;'>Early treated</span> vs. <span style = 'color:#002F6C;'>Late treated</span>") +
  #theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.y=element_text(angle=0, vjust=.45),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=element_markdown())

ggsave("scripts/Rescuing measurements in conflict/Baker panel c replication.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# panel d ---- 

ggplot(dL, aes(x = Time, y = Y, color = Group)) +
  geom_vline(xintercept = 0, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_vline(xintercept = 10, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_line() +
  #  stat_smooth(span=.2) +
  scale_color_manual(values = c(usaid_red, usaid_blue, dark_grey), 
                     labels=c("Early treated", "Late treated", "Untreated")) +
  #scale_x_continuous(limits=c(-10,20)) +
  scale_y_continuous(limits=c(0,20),
                     breaks=seq(0,20,5)) +
  labs(x = "Time", 
       y = "Outcome",
       title = "C. <span style = 'color:#002F6C;'>Late treated</span> vs. <span style='color:#BA0C2F;'>Early treated </span>") +

  theme(legend.title = element_blank(),
        axis.title.y=element_text(angle=0, vjust=.45),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=element_markdown())

ggsave("scripts/Rescuing measurements in conflict/Baker panel d replication.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# three timings ---- 

ggplot(dL, aes(x = Time, y = Y, color = Group)) +
  geom_vline(xintercept = 0, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_vline(xintercept = 10, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_line() +
  #  stat_smooth(span=.2) +
  scale_color_manual(values = c(usaid_red, usaid_blue, dark_grey), 
                     labels=c("Early treated", "Late treated", "Untreated")) +
  #scale_x_continuous(limits=c(-10,20)) +
  scale_y_continuous(limits=c(0,20),
                     breaks=seq(0,20,5)) +
  labs(x = "Time", 
       y = "Outcome",
       title="Three timings, nine groups") +
#       title = "C. <span style = 'color:#002F6C;'>Late treated</span> vs. <span style='color:#BA0C2F;'>Early treated </span>") +
  
  theme(legend.title = element_blank(),
        axis.title.y=element_text(angle=0, vjust=.45),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=element_markdown())

ggsave("scripts/Rescuing measurements in conflict/Baker three timings replication.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# dynamic treatment effects ---- 

d2 <- data.frame(Time = -10:20,
                early = c(rep(12, 11), rep(18, 20)) + rnorm(31, sd = .3),  # Early group
                early_dynamic = c(rep(12,11), rep(19, 4), rep(18, 4), rep(17,4), rep(16,4), rep(15,4)) + rnorm(31, sd=.2), # Early dynamic
                late = c(rep(8, 21), rep(14, 10)) + rnorm(31, sd=.3),  # Late group
                untreated = c(rep(3, 31)) + rnorm(31, sd = .3))  # Untreated group

head(d2)

ggplot(d2, aes(x = Time)) +
  geom_vline(xintercept = 0, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_vline(xintercept = 10, linetype="dotdash", color = "grey60", linewidth = 1, alpha=.8) +
  geom_line(aes(y=early_dynamic),
            color=usaid_red) +
  geom_line(aes(y=late),
            color=usaid_blue) +
  geom_line(aes(y=untreated),
            color=dark_grey) +
  scale_color_manual(values = c(usaid_red, usaid_blue, dark_grey), 
                     labels=c("Early treated", "Late treated", "Untreated")) +
  #scale_x_continuous(limits=c(-10,20)) +
  scale_y_continuous(limits=c(0,20),
                     breaks=seq(0,20,5)) +
  labs(x = "Time", 
       y = "Outcome",
       title="Dynamic treatment effect in early group") +
  #       title = "C. <span style = 'color:#002F6C;'>Late treated</span> vs. <span style='color:#BA0C2F;'>Early treated </span>") +
  
  theme(legend.title = element_blank(),
        axis.title.y=element_text(angle=0, vjust=.45),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title=element_markdown())

ggsave("scripts/Rescuing measurements in conflict/dynamic treatment effects biases TWFE.png",
       device="png",
       type="cairo",
       height=4,
       width=7)




