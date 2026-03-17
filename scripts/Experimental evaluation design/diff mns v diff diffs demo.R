# Field Experiments
# diff in means v diff in diffs

getwd()
d <- read_csv("Field Experiments/tables/table 4.1.csv") %>%
  mutate(diff=y1-y0)
d

lm(y1 ~ D, d) %>%
  summary()

lm(y0~D, d) %>%
  summary()

lm(diff ~ D, d) %>%
  summary()

psych::describe(d)

set.seed(32)

demo <- data.frame(dm = rnorm(1e4, 4, 4.8),
                   dd = rnorm(1e4, 4, 1.5))
head(demo)

ggplot(demo) + 
  geom_vline(xintercept=4, color="darkgoldenrod2", size=1) +
#  geom_histogram()
  geom_density(aes(dm),
               color=usaid_red,
               fill=usaid_red,
               alpha=.3) + 
  geom_density(aes(dd),
               fill=usaid_blue,
               color=usaid_blue,
               alpha=.5) +
  scale_x_continuous(limits=c(-12,22),
                     breaks=seq(-10, 20, 5)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x="Treatment effect",
       y="",
       title="Difference in means vs difference in differences estimation")

ggsave("scripts/Evaluation designs/Evaluation design - RCT/dm vs dd.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

