# Start over in 1, but now use a prior that is zero below p = 0.5 and a con-
# start above p = 0.5. This corresponds to prior information that a majority
# of the Earth’s surface is water. What difference does the better prior make?
# If it helps, compare posterior distributions (using both priors) to the true
# value p = 0.7.
p_grid <- seq(from=0, to=1, length.out = 100)
prior1 <- rep(0, length.out=50)
prior2 <- rep(1, length.out=50)
prior <- c(prior1, prior2)
dist <- dbinom(8, 15, prob=p_grid)
posterior <- prior * dist
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
# plot(p_grid, posterior, type="b-", xlab="probability of water", ylab="posterior probability")
dens(samples, add = TRUE, lty=2)
mtext("Posterior Globe Tossing 8W7L")

# Compared to a uniform prior, the step function prior increased the density 
# of the most probable value from ~3 to >5.