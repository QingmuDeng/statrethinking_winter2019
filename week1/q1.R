# Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
library(rethinking)
p_grid <- seq(from=0, to=1, length.out = 100)
prior <- rep(1, length.out=100)
dist <- dbinom(8, 15, prob=p_grid)
posterior <- prior * dist
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
# plot(p_grid, posterior, type="b-", xlab="probability of water", ylab="posterior probability")
dens(samples, xlim=c(0,1), ylim=c(0,6))
mtext("Posterior Globe Tossing 8W7L")

