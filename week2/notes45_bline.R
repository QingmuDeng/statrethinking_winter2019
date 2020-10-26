library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
precis(d)

# complete cases on doy
d2 <- d[complete.cases(d$doy),]
num_knots <- 20
knot_list <- quantile(d2$year, probs=seq(0, 1,length.out = num_knots))
plot(d2$year, d2$doy)

library(splines)
B <- bs(d2$year, knots=knot_list[-c(1, num_knots)], degree=3, intercept=TRUE)

plot(NULL, xlim=range(d2$year), ylim=c(0,1), xlab="year", ylab="basis")
for (i in 1:ncol(B)) lines(d2$year, B[,i])

# Quadratic approximation to sum up all the 
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w, # matrix B of 827x22 times column vector w of 22x1, results in 827x1
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data=list(D=d2$doy, B=B),
  start=list(w=rep(0, ncol(B)))
)

# visualization of weighted basis
post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d2$year), ylim=c(-6,6), xlab="year", ylab="basis*weight")
for (i in 1:ncol(B)) lines(d2$year, w[i]*B[,i])

# plot wiggles 
mu <- link(m4.7) # randomly sample mu values based on calculated regression in m4.7
mu_PI <- apply(mu, 2, PI, 0.97)
plot(d2$year, d2$doy, col=col.alpha(rangi2, 0.5), pch=16)
shade(mu_PI, d2$year, col=col.alpha("black", 0.5))

