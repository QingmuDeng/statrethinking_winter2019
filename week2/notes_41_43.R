library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >=18,]
# precis(d)
xbar <- mean(d2$weight)

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_mu)
# h <- dnorm(sample_mu, sample_sigma)
dens(h, prior_h) # a T-dsitribution with fat tails

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)
m4.1 <- quap(flist, data=d2)
precis(m4.1)


m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), 
  data=d2 )


########## Reading ###########
#### WHY Normal ####
# Coin toss on soccer field; normal by addition
pos <- replicate(1000, sum(runif(16, -1, 1)))
# hist(pos)
plot(density(pos))
# normal by multiplication
prod(1 + runif(12, 0, 0.1))

growth <- replicate(1e4, prod(1+runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

big <- replicate(1e4, prod(1+runif(12, 0, 0.5))) 
small <- replicate(1e4, prod(1+runif(12, 0, 0.1))) # small variance more closely
                                                  # mimic additions and thus appears more normal
# normal by log multiplication
big <- replicate(1e4, log(prod(1+runif(12, 0, 0.5)))) # large variances produce gausian in log scale!!!
dens(big, norm.comp=TRUE)


#### Gaussian Model of Height ####
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

curve(dnorm(x, 178, 20), from=100, to=250)
curve(dunif(x, 0, 50), from=-10, to=60)
# prior predictive simulation can inform us about the quality of our priors
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, mean = sample_mu, sd = sample_sigma)
dens(prior_h)
# a relatively poor prior can be identified through graphs and common sense
sample_mu <- rnorm(1e4, 178, 100)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

# Grid Approximation of the Posterior Distribution
mu.list <- seq(from=150, to= 160, length.out=100)
sigma.list <- seq(from=7, to=9, length.out=100)
post <- expand.grid(mu=mu.list, sigma=sigma.list)
post$LL <- sapply(1:nrow(post), function(i)sum(dnorm(d2$height, post$mu[i], post$sigma[i], log=TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod-max(post$prod))
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)
# Sampling from the posterior 
samples.rows <- sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
samples.mu <- post$mu[samples.rows]
samples.sigma <- post$sigma[samples.rows]
plot(samples.mu, samples.sigma, cex=0.75, pch=16, col=col.alpha(rangi2, 0.05))

dens(samples.mu)
dens(samples.sigma)

PI(samples.mu)
PI(samples.sigma)

## overthinking: normality of sigma
d3 <- sample(d2$height, size=20)
mu.list <- seq(from=150, to= 170, length.out=200)
sigma.list <- seq(from=4, to=9, length.out=200)
post2 <- expand.grid(mu=mu.list, sigma=sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i)sum(
  dnorm(d3, post2$mu[i], post2$sigma[i], log=TRUE)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod-max(post2$prod))
samples2.rows <- sample(1:nrow(post2), size=1e4, replace=TRUE, prob=post2$prob)
samples2.mu <- post2$mu[samples2.rows]
samples2.sigma <- post2$sigma[samples2.rows]
plot(samples2.mu, samples2.sigma, cex=0.75, pch=16, col=col.alpha(rangi2, 0.05),
     xlab="mu", ylab="sigma")
dens(samples2.sigma)

### finding the posterior distribution with quap
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- quap(flist, data=d2)
precis(m4.1)

# overthinking: start values for quap
start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)
m4.1 <- quap(flist, data=d2, start=start)

m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~dunif(0, 50)
  ), data=d2)
precis(m4.2)

# sampling from quap
vcov(m4.1)
diag(vcov(m4.1))
cov2cor(vcov(m4.1))
library(MASS)
post <- mvrnorm(n=1e4, mu=coef(m4.1), Sigma=vcov(m4.1))
head(post)
plot(post)
