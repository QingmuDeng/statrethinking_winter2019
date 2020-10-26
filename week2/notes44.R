library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[d$age >= 18,]
plot(d2$height ~ d2$weight)

#### Linear Prediction ####
set.seed(2971)
N <- 100 
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0,10)

plot(NULL, xlim=range(d2$weight), ylim=c(-100, 400),
     xlab="weight", ylab="height")
abline(h=0, lty=2) # line style 2
abline(h=272, lty=1, lwd=0.5) # line style 1, line width 0.5
mtext("b ~ dnorm(0, 10)")
xbar <- mean(d2$weight)
for (i in  1:N)
  curve(a[i]+b[i]*(x-xbar),
        from=min(d2$weight), to=max(d2$weight), add=TRUE,
        col=col.alpha("black", 0.2))
# log normal to restrict to positive slopes
b <- rlnorm(1e4, 0,1)
dens(b, xlim=c(0,5), adj=0.1)

set.seed(2971)
N <- 100 
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0,1)

plot(NULL, xlim=range(d2$weight), ylim=c(-100, 400),
     xlab="weight", ylab="height")
abline(h=0, lty=2) # line style 2
abline(h=272, lty=1, lwd=0.5) # line style 1, line width 0.5
mtext("b ~ dnorm(0, 10)")
xbar <- mean(d2$weight)
for (i in  1:N)
  curve(a[i]+b[i]*(x-xbar),
        from=min(d2$weight), to=max(d2$weight), add=TRUE,
        col=col.alpha("black", 0.2))

## Find Posterior Distirbution
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[d$age >= 18,]
# define averge weight
xbar <- mean(d2$weight)
# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(height-xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data=d2
)
precis(m4.3)
round(vcov(m4.3), 3)
pairs(m4.3)

## Plotting inference against the data
plot(height ~ weight, data=d2, col=rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x-xbar), add=TRUE)

## Adding uncertainty around the mean
N <- 10
dN <- d2[1:N, ]
mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data=dN
)
# extract 20 samples
post <- extract.samples(mN, n=20)
plot(dN$weight, dN$height, xlim=range(d2$weight), ylim=range(d2$height),
     col=rangi2, xlab="weight", ylab="height")
mtext(concat("N=",N))
# plot the line with transparency
for (i in 1:20)
  curve(post$a[i] + post$b[i]*(x-mean(dN$weight)),
        col=col.alpha("black",0.3), add=TRUE)

plot_uncer <- function(N){
  dN <- d2[1:N, ]
  mN <- quap(
    alist(
      height ~ dnorm(mu, sigma),
      mu <- a + b*(weight - mean(weight)),
      a ~ dnorm(178, 20),
      b ~ dlnorm(0, 1),
      sigma ~ dunif(0, 50)
    ), data=dN
  )
  # extract 20 samples
  post <- extract.samples(mN, n=20)
  plot(dN$weight, dN$height, xlim=range(d2$weight), ylim=range(d2$height),
       col=rangi2, xlab="weight", ylab="height")
  mtext(concat("N=",N))
  # plot the line with transparency
  for (i in 1:20)
    curve(post$a[i] + post$b[i]*(x-mean(dN$weight)),
          col=col.alpha("black",0.3), add=TRUE)
}
plot_uncer(150)

## Plotting regression intervals and contours
post <- extract.samples(m4.3)
mu_at_50 <-post$a + post$b * (50 - xbar)
dens(mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50")
PI(mu_at_50, prob=0.89)

mu <- link(m4.3)
str(mu)
# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq(from=25, to=70, by=1)
# use link to comput mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link(m4.3, data=data.frame(weight=weight.seq))
str(mu)
# use type="n" to hide raw data
plot(height~weight, d2, type="n")
# loop over samples and plot each mu value
for (i in 1:100)
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2,0.1))
# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
# plot raw data
# fade out points to make line and interval more visible
plot(height ~ weight, data=d2, col=col.alpha(rangi2,0.5))
# plot MAP line
lines(weight.seq, mu.mean)
# plot 89% PI
shade(mu.PI, weight.seq)

### Prediction Intervals
sim.height <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob=.89) #along dimension 2, columns
height.PI67 <- apply(sim.height, 2, PI, prob=.67) #along dimension 2, columns
height.PI97 <- apply(sim.height, 2, PI, prob=.97) #along dimension 2, columns
# plot raw data
# fade out points to make line and interval more visible
plot(height ~ weight, data=d2, col=col.alpha(rangi2,0.8))
# plot 89% PI for height predictions
# shade(height.PI97, weight.seq, col=col.alpha(2,0.5))
shade(height.PI, weight.seq, col=col.alpha(2,0.5))
# shade(height.PI67, weight.seq, col=col.alpha(4,0.5))
# plot MAP line
lines(weight.seq, mu.mean)
# plot 89% PI for mu
shade(mu.PI, weight.seq)
points(height ~ weight, data=d2, col=col.alpha(rangi2,0.8))

