# This problem is more open-ended than the others. Feel free to collabo-
# rate on the solution. Suppose you want to estimate the Earth’s proportion of
# water very precisely. Specifically, you want the 99% percentile interval of the
# posterior distribution of p to be only 0.05 wide. This means the distance be-
# tween the upper and lower bound of the interval should be 0.05. How many
# times will you have to toss the globe to do this? I won’t require a precise
# answer. I’m honestly more interested in your approach.
library(rethinking)
p_grid <- seq(from=0, to=1, length.out = 1000)
likelihood <- dbinom(1, size = 10, prob=p_grid)
diff <- 1
size <- 2
while(diff>0.05){
  size <- size + 2
  likelihood <- dbinom(size, size = size, prob=p_grid)
  interval <- quantile(likelihood, c(0.005, 0.995))
  diff <- interval[2] - interval[1]
  # PI99 <- PI( likelihood, 0.99 )
  # diff = as.numeric( PI99[2] - PI99[1] )
}
(size)
(diff)

# A numerical calculation suggest it should take at least 250 draws before our 99% 
# percentile interval, when the actual probability is around the center.

# When the actual probability is heavily skewed toward the left or right, it takes
# significantly more draws, from 600 to 700 to almost 1000, to reduce the range of
# uncertainty.

# The above is a less accurate description because it is deterministic. The proportions
# were strictly calculated from binomial distribution which ignores the uncertainty in
# inference making from the globe tossing process. That is, the binomial distribution
# directly always give the best case scenario.

f <- function(N){
  p_true <- 0.01
  W <- rbinom(1, size=N, prob=p_true)
  p_grid <- seq(from = 0, to = 1, length.out = 1000)
  prior <- rep(1, 1e3)
  prob_data <- dbinom(W, size = N, prob = p_grid)
  posterior <- prior * prob_data
  posterior <- posterior / sum(posterior)
  samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)
  PI99 <- PI(samples, 0.99)
  as.numeric(PI99[2]-PI99[1])
}

Nlist <- c(20, 50, 100, 200, 400, 800, 1600, 2000, 3000)
Nlist <- rep(Nlist, each=100)
width <- sapply(Nlist, f)
plot(Nlist, width)
abline(h=0.05, col='red')

