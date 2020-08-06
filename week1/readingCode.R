# Normalizing case counts

ways <- c(0,3, 8, 9, 0)
ways / sum(ways)

# tossing globe: binomial distribution
dbinom(6, size=9, prob=0.5)

############### Grid Approximation ############
# globe toss example
# p the proportion of water on Earth's surface
# define grid: p value from 0 to 1 with length 20
p_grid <- seq( from=0, to=1, length.out=20)
# define prior
prior <- rep(1, 20)
# compute the likelihood for each p value
likelihood <- dbinom(6, size=9, prob=p_grid)
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("20 points")

############# Quadratic Approximation ############
library(rethinking)
globe.qa <- quap(alist(W ~ dbinom(W+L, p), p~ dunif(0, 1)),
                 data=list(W=6, L=3))
precis(globe.qa)
# analytical calculation
W <- 6
L <- 3
curve(dbeta(x, W+1, L+1), from=0, to=1)
# quadratic approximation
curve(dnorm(x,0.67, 0.16), lty=2, add=TRUE)

############# Monte Carlo Globe Tossing ############
n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
  p_new <- rnorm( 1 , p[i-1] , 0.1 )
  if ( p_new < 0 ) p_new <- abs( p_new )
  if ( p_new > 1 ) p_new <- 2 - p_new
  q0 <- dbinom( W , W+L , p[i-1] )
  q1 <- dbinom( W , W+L , p_new )
  p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
}
dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE )

############# Vampire ############
Pr_Positive_Vampire <- 0.95
Pr_Positive_Mortal <- 0.01
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire + Pr_Positive_Mortal*(1 - Pr_Vampire)
Pr_Vampire_Positive <- Pr_Positive_Vampire*Pr_Vampire/Pr_Positive
# parenthesis print in R

############# Sampling from grid-approximation ############
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, 9, prob = p_grid)
posterior <- prob_p * prob_data
posterior <- posterior/sum(posterior)

# draw samples from well mixed buckets
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
plot(samples)
library(rethinking)
dens(samples)
############# Interval of defined Boundaries ############
# add up posterior prob where p<0.5
sum(posterior[p_grid<0.5])
# use samples to calculate p<0.5
sum(samples<0.5)/1e4
# how much in between p=0.5 and p=0.75
sum(samples>0.5 & samples<0.75)/1e4
# ask for where 80 percent is
quantile(samples, 0.8)
# ask for 10 and 90 percent
quantile(samples, c(0.1, 0.9))

############# Interval of Defined Mass ############
### percentile intervals' drawback with skew data
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size=3, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)
PI(samples, prob=0.5) # exclude the most probable, p=1

# highest posterior density interval fixes this issue
HPDI(samples, prob=0.5)

############# Point Estimates ############

# highest posterior probability, a Maximum a Posteriori estimate
p_grid[which.max(posterior)]
# sample approach
chainmode(samples, adj=0.01)
# mean
mean(samples)
# median
median(samples)

## calculation on loss
sum(posterior*abs(0.5-p_grid))
# direct loss corresponds to median
loss <- sapply(p_grid, function(d) sum(posterior*abs(d-p_grid)))
p_grid[which.min(loss)]
# quadratic loss corresponds to mean
loss_quad <- sapply(p_grid, function(d) sum(posterior*(d-p_grid)^2))
p_grid[which.min(loss_quad)]
(mean(samples))

############# Sampling to simulate Prediction ############
dbinom(0:2, size=2, prob=0.7)
# simulate according to binomial distribution
rbinom(10, size=2, prob=0.7)
# simulate samples according to binomial distribution to 
# check whether the dbinom proportion is right
dummy_w <- rbinom(1e5, size=2, prob=0.7)
table(dummy_w)/1e5
# do nine tosses
dummy_w <- rbinom(1e5, size=9, prob=0.7)
simplehist(dummy_w, xlab="dummy water count")

# Model Checking
# simulate predicted observation for p=0.6
w <- rbinom(1e4, size=9, prob=0.6)
# or, you can propagate the uncertainty
w <- rbinom(1e4, size=9, prob = samples)




