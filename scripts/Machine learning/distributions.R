
set.seed(32)

x <- seq(-4,4, length.out=100)

d <- data.frame(x=x,
                y1=rnorm(100, 2*x + 2, 2),
                y2=rnorm(100, 2*x + 18, 2),
                y3=rnorm(100, 2*x + 8, 2),
                y4=rnorm(100, 2*x + 12, 2),
                index=seq(0,20, length.out=100))

head(d)

nonov <- ggplot(d, aes(x)) + 
  geom_point(aes(y=y1),
             size=.5,,
             alpha=.6,
             color=usaid_red) +
  geom_point(aes(y=y2),
             size=.5,
             alpha=.6,
             color=usaid_blue) +
  geom_line(aes(y=index),
            color="grey60",
            linewidth=1,
            alpha=.5) + 
  labs(x="Feature 1",
       y="Feature 2",
       title="Linear, non-overlapping groups") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

ggsave("linear, non-overlapping groups.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

ov <- ggplot(d, aes(x)) + 
  geom_point(aes(y=y3),
             size=.5,
             alpha=.6,
             color=usaid_red) +
  geom_point(aes(y=y4),
             size=.5,
             alpha=.6,
             color=usaid_blue) +
  geom_line(aes(y=index),
            color="grey60",
            linewidth=1,
            alpha=.5) + 
  labs(x="",
       y="",
       title="Linear, overlapping groups") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

ggsave("linear, overlapping groups.png",
       device="png",
       type="cairo",
       height=4,
       width=7)







library(ggplot2)
library(dplyr)

# Fibonacci spiral generation
fibonacci_spiral <- function(n = 10) {
  golden_ratio <- (1 + sqrt(5)) / 2
  spiral_data <- data.frame(x = 0, y = 0, angle = 0, width = 1, height = 1)
  
  for (i in 1:n) {
    width <- spiral_data$width[i]
    height <- width * golden_ratio
    angle <- spiral_data$angle[i] + pi / 2
    x <- spiral_data$x[i] + width * cos(angle)
    y <- spiral_data$y[i] + width * sin(angle)
    spiral_data <- rbind(
      spiral_data,
      data.frame(x = x, y = y, angle = angle, width = width, height = height)
    )
  }
  return(spiral_data)
}

# Generate Fibonacci spiral data
spiral_data <- fibonacci_spiral(10)
spiral_data

# Plot Fibonacci spiral
ggplot(spiral_data, aes(x, y)) +
  geom_path(aes(x = x, y = y), color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Fibonacci Spiral Approximating the Golden Ratio")


# Define a function to generate Fibonacci sequence using a loop

fibonacci_loop <- function(n) {
  # Create a vector to store Fibonacci numbers
  fib <- numeric(n)
  fib[1] <- 0
  fib[2] <- 1
  
  # Loop to generate the sequence
  for (i in 3:n) {
    fib[i] <- fib[i - 1] + fib[i - 2]
  }
  
  return(fib)
}

# Generate the first 10 Fibonacci numbers

fibonacci_loop(10)




library(ggplot2)
library(dplyr)
library(plotrix)

# Function to generate Fibonacci sequence
fibonacci_sequence <- function(n) {
  fib <- numeric(n)
  fib[1] <- 1
  fib[2] <- 1
  for (i in 3:n) {
    fib[i] <- fib[i - 1] + fib[i - 2]
  }
  return(fib)
}

# Function to create quarter circles
quarter_circle <- function(x, y, r, start_angle) {
  t <- seq(0, pi / 2, length.out = 100)
  data.frame(
    x = x + r * cos(t + start_angle),
    y = y + r * sin(t + start_angle)
  )
}

# Generate Fibonacci spiral data
fibonacci_spiral <- function(n_squares) {
  fib <- fibonacci_sequence(n_squares)
  
  # Initialize position and angle for each square
  x <- numeric(n_squares)
  y <- numeric(n_squares)
  angle <- numeric(n_squares)
  angle[1] <- 0  # Start by moving to the right
  
  for (i in 2:n_squares) {
    if ((i - 1) %% 4 == 0) {
      # Move up
      x[i] <- x[i - 1]
      y[i] <- y[i - 1] + fib[i - 1]
      angle[i] <- pi / 2
    } else if ((i - 1) %% 4 == 1) {
      # Move left
      x[i] <- x[i - 1] - fib[i - 1]
      y[i] <- y[i - 1]
      angle[i] <- pi
    } else if ((i - 1) %% 4 == 2) {
      # Move down
      x[i] <- x[i - 1]
      y[i] <- y[i - 1] - fib[i - 1]
      angle[i] <- 3 * pi / 2
    } else {
      # Move right
      x[i] <- x[i - 1] + fib[i - 1]
      y[i] <- y[i - 1]
      angle[i] <- 0
    }
  }
  
  # Generate quarter circle data for the spiral
  spiral_data <- do.call(rbind, lapply(1:n_squares, function(i) {
    quarter_circle(x[i], y[i], fib[i], angle[i])
  }))
  
  return(list(spiral_data = spiral_data, rect_data = data.frame(x = x, y = y, size = fib)))
}

# Plot the Fibonacci spiral
n_squares <- 10
spiral_data <- fibonacci_spiral(n_squares)

# Plot Fibonacci squares and the spiral
ggplot() +
  # Draw squares
  geom_rect(data = spiral_data$rect_data, aes(xmin = x, xmax = x + size, ymin = y, ymax = y + size), 
            fill = NA, color = "blue") +
  # Draw the spiral
  geom_path(data = spiral_data$spiral_data, aes(x = x, y = y), color = "red", size = 1) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Fibonacci Spiral with Golden Ratio")








polar_golden_spiral <- function(theta) exp(0.30635*theta)

seq_theta <- seq(0,4*pi,by=0.05)
dist_from_origin <- sapply(seq_theta,polar_golden_spiral)

ggplot(data.frame(x = seq_theta, y = dist_from_origin), aes(x,y)) +
  geom_point() +
  coord_polar(theta="x")

plotrix::radial.plot(dist_from_origin, seq_theta,rp.type="s", point.col = "blue")


cartesian_golden_spiral <- function(theta) {
  a <- polar_golden_spiral(theta)*cos(theta)
  b <- polar_golden_spiral(theta)*sin(theta)
  c(a,b)
}

serie <- sapply(seq_theta,cartesian_golden_spiral)

set.seed(32)

df <- data.frame(t(serie)) %>%
  mutate(x1=X1 + rnorm(252, 0,.4),
         x2=X2 + rnorm(252, 0, .6))

head(df)
psych::describe(df)

df2 <- df %>%
  select(x=x1,
         y=x2) %>%
  filter(x<20,
         y>-10,
         y<14) %>%
  mutate(group=sample(c("a","b"), size=199, replace=T))

head(df2)

ggplot(df, aes(x=x1,y=x2)) +
  geom_point()

fib <- ggplot(df2, aes(x,y, color=group)) + 
  geom_point(size=.4, 
             alpha=.6) +
  scale_color_manual(values=c(usaid_red, usaid_blue)) + 
  theme(legend.position="none") + 
  labs(x="",
       y="",
       title="What the heck?!?") + 
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())
  
ggsave("golden spiral.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

nonov + ov + fib 

#plot_annotation(x="Feature 1",
#                  y="Feature 2")

?plot_annotation

ggsave("scripts/Machine learning/three distributions.png",
       device="png",
       type="cairo",
       height=3,
       width=8)



geom_path(color="blue") +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(-20,20,by=10)) +
  scale_x_continuous(breaks = seq(-20,50,by=10)) +
  coord_fixed() +
  labs(title = "Golden spiral",
       subtitle = "Another view on the Fibonacci sequence",
       caption = "www.linkedin.com/in/abhirup-moitra-7a66731a6
                https://github.com/ABHI20-STAT.",
       x = "",
       y = "")


