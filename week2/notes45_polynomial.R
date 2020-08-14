library(rethinking)
data(Howell1); d <- Howell1;
d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2

m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*d$weight_s + b2*d$weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=d
)
precis(m4.5)
plot(d$weight_s, d$height, col=col.alpha(rangi2,0.5))

# define the weight sequence over which to regress
weight.seq <- seq(from=-2.2, to=2, length.out=30)
pred_dat <- data.frame(weight_s=weight.seq, weight_s2=weight.seq^2)
# generate mu heights based on regression
post <- extract.samples(m4.5, n=50)
# define a function that link weights to mu
mu.link <- function(weight) post$a + post$b1 * weight + post$b2 * weight^2
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.97)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq, col=col.alpha(2,0.5))
# 89% of the prediction uncertainty through simulation
post <- extract.samples(m4.5)
sim.height <- sapply(weight.seq,
                     function(weight)
                       rnorm(n=nrow(post), mean=post$a + post$b1 * weight + post$b2 * weight^2,
                             sd=post$sigma)
)
heights.PI <- apply(sim.height, 2, PI, 0.89)
shade(heights.PI, weight.seq, col=col.alpha(1,0.5))

### Third degrees
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*d$weight_s + b2*d$weight_s2 + b3*d$weight_s3,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=d
)
precis(m4.6)

# define the weight sequence over which to regress
weight.seq <- seq(from=-2.2, to=2, length.out=30)
# pred_dat <- data.frame(weight_s=weight.seq, weight_s2=weight.seq^2, weight_s2=weight.seq^2)
# generate mu heights based on regression
post <- extract.samples(m4.6, n=50)
# define a function that link weights to mu
mu.link <- function(weight) post$a + post$b1*weight + post$b2*weight^2 + post$b3*weight^3
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq, col=col.alpha(3,0.5))
# 89% of the prediction uncertainty through simulation
post <- extract.samples(m4.6)
sim.height <- sapply(weight.seq,
                     function(weight)
                       rnorm(n=nrow(post), mean=post$a + post$b1*weight + post$b2*weight^2 + post$b3*weight^3,
                             sd=post$sigma)
)
heights.PI <- apply(sim.height, 2, PI, 0.89)
shade(heights.PI, weight.seq, col=col.alpha(3,0.5))
points(d$weight_s, d$height, col=col.alpha(1,0.5))
