# machine learning data

# non-overlapping clusters ---- 

# Set seed for reproducibility
set.seed(42)

# Number of samples per cluster
n_samples <- 100

# Generate Cluster 1 (centered around (2, 2))
cluster1 <- data.frame(
  x = rnorm(n_samples, mean = 2, sd = 0.5),
  y = rnorm(n_samples, mean = 2, sd = 0.5)
)

# Generate Cluster 2 (centered around (8, 8))
cluster2 <- data.frame(
  x = rnorm(n_samples, mean = 8, sd = 0.5),
  y = rnorm(n_samples, mean = 8, sd = 0.5)
)

# Generate Cluster 3 (centered around (5, 15))
cluster3 <- data.frame(
  x = rnorm(n_samples, mean = 5, sd = 0.5),
  y = rnorm(n_samples, mean = 15, sd = 0.5)
)

# Combine clusters to form the dataset
d <- rbind(cluster1, cluster2, cluster3)
head(d)

nonov <- ggplot(d, aes(x,y)) +
  geom_point(color="dodgerblue2",
             size=.6,
             alpha=.6) + 
  labs(x="\nFeature 1",
       y="Feature 2",
       title="Easily clusterable data") +
  scale_x_continuous(limits=c(0,10)) +
  scale_y_continuous(limits=c(0,20)) +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_text(angle=0, 
                                  vjust=.5))

ggsave("scripts/Machine learning/easily clusterable.png",
       device="png",
       type="cairo",
       height=4,
       width=6)

nonov

# overlapping clusters ---- 

set.seed(432)

#y1=rnorm(100, 2*seq(-4,4, length.out=100) + 2, 2),
#y2=rnorm(100, 2*x + 18, 2),
#y3=rnorm(100, 2*x + 8, 2),
#y4=rnorm(100, 2*x + 12, 2),



# Increase standard deviation to 2
cluster1 <- data.frame(
  x = rnorm(n_samples, mean = 5, sd = 1.5),
  y = rnorm(n_samples, mean = 5, sd = 1.5)
)

psych::describe(cluster1)

cluster2 <- data.frame(
  x = rnorm(n_samples, mean = 10, sd = 1.5),
  y = rnorm(n_samples, mean = 10, sd = 1.5)
)

cluster3 <- data.frame(
  x = rnorm(n_samples, mean = 15, sd = 1.5),
  y = rnorm(n_samples, mean = 15, sd = 1.5)
)

ov_clust <- rbind(cluster1, cluster2, cluster3)

head(ov_clust)

ov <- ggplot(ov_clust, aes(x,y)) +
  geom_point(color="dodgerblue2",
             size=.6,
             alpha=.6) + 
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(limits=c(0, 20)) +
  labs(x="",
       y="",
       title="Overlapping clusters?") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y=element_text(angle=0,
                                  vjust=.55))

ggsave("scripts/Machine learning/overlapping clusters.png",
       device="png",
       type="cairo",
       height=4,
       width=6)

nonov + ov

ggsave("scripts/Machine learning/overlapping and non-overlapping clusters.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


plot(ov_clust$x, data$y, col = 'gray', pch = 16,
     main = 'Dataset with Increased Overlapping',
     xlab = 'Feature 1', ylab = 'Feature 2')



