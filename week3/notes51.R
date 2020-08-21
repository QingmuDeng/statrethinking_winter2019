# load data and copy
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

#### Spurious Association ####
d$M <- scale(d$Marriage)
d$A <- scale(d$MedianAgeMarriage)
d$D <- scale(d$Divorce)
m5.1 <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- a + bA*A,
  a ~ dnorm(0, 0.2), # thanks to normalization, the average should be zero
  bA ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data=d)
precis(m5.1)

m5.2 <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- a + bM*M,
  a ~ dnorm(0, 0.2), # thanks to normalization, the average should be zero
  bM ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data=d)
precis(m5.2)


post <- extract.samples(m5.1)
mu_link <- function(A) post$a + post$b * A
# mu <- link(m5.1)
Asam <- seq(from=-2.5, to=3.0, length.out = 100)
mu <- sapply(Asam, mu_link) 
mu_mu <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI, .89)
plot(d$A, d$D, col=col.alpha(4, alpha=0.5))
lines(Asam, mu_mu) 
shade(mu_PI, Asam)

### simulate some prior
set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post=prior, data=list(A=c(-2,2)))
plot(NULL, xlim=c(-2, 2), ylim=c(-2, 2))
for (i in 1:50) lines( c(-2, 2), mu$mu[i,], col=col.alpha("black", 0.4) )

#### Directed Acyclic Graph ####
library(dagitty)
dag5.1 <- dagitty(
  "dag{
  A -> D
  A -> M
  M -> D
}")
coordinates(dag5.1) <- list(x=c(A=0, M=2, D=1), y=c(A=0,D=1,M=0))
plot(graphLayout(dag5.1))
drawdag(dag5.1)
dag5.2 <- dagitty("dag{ M <- A -> D}")
coordinates(dag5.2) <- list(x=c(A=0, M=2, D=1), y=c(A=0,D=1,M=0))
drawdag(dag5.2)
impliedConditionalIndependencies(dag5.2) # D _||_ M | A
impliedConditionalIndependencies(dag5.1) # None, everything should be associated with each other

#### Multiple Regression ####
m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma), 
    mu <- a + bM * M + bA * A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
precis(m5.3)
plot(coeftab(m5.1, m5.2, m5.3), par=c('bA', 'bM'))
# there are almost no additional predictive help from marriage rate once we included 
# the information on median marriage age

#### Plotting Multivariate Posteriors ####
## Predictor Residual Plot
m5.4 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bAM * A,
    a ~ dnorm(0, 0.2),
    bAM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
# calculate residual by subtracting the observed marriage rate from the predicted rate
mu <- link(m5.4)
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$M - mu_mean
plot(mu_resid, d$D)
identify(x=mu_resid, y=d$D, labels=d$Loc)
m5.4b <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a + bMA * M,
    a ~ dnorm(0, 0.2),
    bMA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
mu <- link(m5.4b)
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$A - mu_mean
plot(mu_resid, d$D)
identify(x=mu_resid, y=d$D, labels=d$Loc)
## Posterior Prediction Plot
mu <- link(m5.3)
# summarize samples across cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
# simulate observation
D_sim <- sim(m5.3, n=1e4)
D_PI <- apply(D_sim, 2, PI)
plot(mu_mean~d$D, col=rangi2, ylim=range(mu_PI),
     xlab="Observed divorce", ylab="Predicted divorce")
abline(a=0, b=1, lty=2)
for (i in 1:nrow(d)) lines(rep(d$D[i], 2), mu_PI[,i], col=rangi2)
# counterfactual plots
m5.3_A <- quap(
  alist(
    ## A -> D <- M
    D ~ dnorm(mu, sigma), 
    mu <- a + bM * M + bA * A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1),
    ## A -> M
    M ~ dnorm(mu_M, sigma_M),
    mu_M <- aM + bAM*A,
    aM ~ dnorm(0, 0.2),
    bAM ~ dnorm(0, 0.5),
    sigma_M ~ dexp(1)
  ), data=d
)
precis(m5.3_A)
A_seq <- seq(from=-2, to=2, length.out = 30)
# prep data
sim_dat <- data.frame(A=A_seq)

# simulate M and ten D, using A_seq
s <- sim(m5.3_A, data=sim_dat, vars=c("M","D"))
plot(sim_dat$A, colMeans(s$D), ylim=c(-2, 2), type="l",
     xlab="manipulated A", ylab="counterfactual D")
shade(apply(s$D, 2, PI), sim_dat$A)
mtext("Total counterfactual effect of A on D")

sim_dat <- data.frame(M=seq(from=-2, to=2, length.out=30), A=0)
s <- sim(m5.3_A, data=sim_dat, vars=c("D"))
plot(sim_dat$M, colMeans(s), ylim=c(-2, 2), type="l",
     xlab="manipulated M", ylab="counterfactual D")
shade(apply(s, 2, PI), sim_dat$M)
mtext("Total counterfactual effect of M on D") # the little variation of D with respect
                                               # to M suggests the minimum relationship between M and D

