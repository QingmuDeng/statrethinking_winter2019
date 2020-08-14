##########                  HW1                #############
# The weights listed below were recorded in the !Kung census, but heights
# were not recorded for these individuals. Provide predicted heights and 89%
# compatibility intervals for each of these individuals. That is, fill in the table
# below, using model-based predictions.
#                        Mean
# [1] 154.5820 150.7262 179.3215 141.2915 161.2785
#                       89% PI
# 5%  146.6908 143.3683 171.0462 133.9998 153.8453
# 94% 162.4444 159.0973 187.3319 148.7311 169.1090
library(rethinking)
data(Howell1)
d <- Howell1;
# standardize weight distribution
d$weight_s <- (d$weight-mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3

# Use third degree polynomial fit
modlist <- alist(height ~ dnorm(mu, sigma),
                 mu <- a + b1*d$weight_s + b2*d$weight_s2 + b3*d$weight_s3,
                 a ~ dnorm(178, 20),
                 b1 ~ dlnorm(0, 1),
                 b2 ~ dnorm(0, 10),
                 b3 ~ dnorm(0, 10),
                 sigma ~ dunif(0, 50)
                 )
m4.5 <- quap(modlist, data = d)
precis(m4.5)

# display the scatterplot of known data
plot(d$weight_s, d$height)
mtext("Height versus Weight standar deviation")

# simulate mus
post <- extract.samples(m4.5)
mu_link <- function(weight) post$a + post$b1*weight + post$b2*weight^2 + post$b3*weight^3

weights <- c(45, 40, 65, 31, 53)
weights_s <- (weights-mean(d$weight))/sd(d$weight)

mu.height <- sapply(weights_s, mu_link)
mu.mu <- apply(mu.height, 2, mean)
(mu.mu)

sim.height <- sapply(weights_s, function(weights) rnorm(n=1e3, mean=mu_link(weights), sd=post$sigma))

str(sim.height)

sim.PI89 <- apply(sim.height, 2, FUN = PI, .89)

##########                  HW2                #############
# Model the relationship between height (cm) and the natural logarithm of
# weight (log-kg): log(weight) . Use the entire Howell1 data frame, all 544
# rows, adults and non-adults. Use any model type from Chapter 4 that you
# think useful: an ordinary linear regression, a polynomial or a spline. Plot
# the posterior predictions against the raw data.
d$weight_l <- log(d$weight)
d$weight_l2 <- d$weight_l^2
d$weight_l3 <- d$weight_l^3

# Use third degree polynomial fit
modlist <- alist(height ~ dnorm(mu, sigma),
                 mu <- a + b1*d$weight_l + b2*d$weight_l2 + b3*d$weight_l3,
                 a ~ dnorm(178, 20),
                 b1 ~ dlnorm(0, 1),
                 b2 ~ dnorm(0, 20),
                 b3 ~ dnorm(0, 20),
                 sigma ~ dunif(0, 50)
)
m4.6 <- quap(modlist, data = d)
precis(m4.6)

# display the scatterplot of known data
plot(d$weight_l, d$height)
mtext("Height versus Log Weight")

# simulate mus
post <- extract.samples(m4.6)
mu_link <- function(weight) post$a + post$b1*weight + post$b2*weight^2 + post$b3*weight^3

weights_l <- seq(from=1.5, to=4.3, length.out=1e3)

mu.height <- sapply(weights_l, mu_link)
mu.mu <- apply(mu.height, 2, mean)
# (mu.mu)
lines(weights_l, mu.mu)

sim.height <- sapply(weights_l, function(weights) rnorm(n=1e3, mean=mu_link(weights), sd=post$sigma))

str(sim.height)

sim.PI89 <- apply(sim.height, 2, FUN = PI, .89)
shade(sim.PI89, weights_l)

##########                  HW3                #############
# Plot the prior predictive distribution for the polynomial regression model
# in Chapter 4. You can modify the the code that plots the linear regression
# prior predictive distribution. 20 or 30 parabolas from the prior should 
# suffice to show where the prior probability resides. Can you modify the prior
# distributions of α, β1 , and β2 so that the prior predictions stay within the
# biologically reasonable outcome space? That is to say: Do not try to fit the
# data by hand. But do try to keep the curves consistent with what you know
# about height and weight, before seeing these exact data.
set.seed(2971)
n <- 1e2
a <- rnorm(n, 160, 20)
b1 <- rlnorm(n, 0, 1)
b2 <- rnorm(n, 0, 4)
b3 <- rnorm(n, 0, 4)
sigma <- runif(n, 0, 50)
weight_seq = seq(from=-2.2, to=2.2, length.out = n)
mu <- a + b1*weight_seq + b2*weight_seq^2 + b3*weight_seq^3
rheight <- rnorm(n, mean=mu, sd=sigma)
rheight <- na.omit(rheight)
# dens(rheight)

plot(NULL, xlim=range(weight_seq), ylim=c(-100, 400),
     xlab="weight", ylab="height")
abline(h=0, lty=2) # line style 2
abline(h=272, lty=1, lwd=0.5) # line style 1, line width 0.5
for (i in  1:n)
  curve(a[i]+b1[i]*x+b2[i]*x^2+b3[i]*x^3,
        from=min(weight_seq), to=max(weight_seq), add=TRUE,
        col=col.alpha("black", 0.2))

##########                  4H5                #############
# Return to data("cherry_blossoms") and model the association between blossom date
# and March temperature. Note that there are many missing values in both variables.
# You may consider a linear model, a polynomial, or a spline on temperature. How
# well does the temperature trend predict the blossom trend?
library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
d2 <- d[complete.cases(d$doy),]
d2 <- d2[complete.cases(d2$temp),]
plot(d2$temp, d2$doy)
plot(d2$year, d2$temp)

#### Let's fit the temperature by B-spline first
num_knots <- 30
knot_list <- quantile(d2$year, probs=seq(0, 1, length.out = num_knots))
library(splines)
B <- bs(d2$year, knots=knot_list[-c(1,num_knots)], degree=3, intercept = TRUE) #minus sign takes out the indexed elements

# list model design
mod_list = alist(
  temp ~ dnorm(mu, sigma),
  mu <- a + B %*% w,
  a ~ dnorm(6, 3),
  w ~ dnorm(0, 10),
  sigma ~ dexp(1)
)

m.temp <- quap(mod_list, data=list(temp=d2$temp, B=B), start = list(w=rep(0, ncol(B))))

post <- extract.samples(m.temp)
mu.link <- matrix(0, nrow = 787, ncol = 1e4)
for (i in 1:length(post$a))
  mu.link[,i] <- post$a[i] + B %*% post$w[i,]

yr_seq <- seq(from=min(d2$year), to=max(d2$year), length.out=1e5)
# mu <- link(m.temp)#sapply(m.temp, mu.link)#link(m.temp)#
mu_PI <- apply(mu.link, 1, PI, .89)
plot(d2$year, d2$temp)
shade(mu_PI, d2$year, col=col.alpha(3,.5))
# tp <- rnorm(n=1e3, mean=mu.link, sd=post$sigma)
# tp_PI <- sapply(tp, PI, .89)
# shade(tp_PI, yr_seq, col=col.alpha(4,.3))

### Regress temperature and day of 
plot(d2$temp, d2$doy)
num_knots <- 7
knot_list <- quantile(d2$temp, probs=seq(0, 1, length.out = num_knots))
library(splines)
B <- bs(d2$temp, knots=knot_list[-c(1,num_knots)], degree=3, intercept = TRUE) 
#minus sign takes out the indexed elements

# list model design
mod_list = alist(
  doy ~ dnorm(mu, sigma),
  mu <- a + B %*% w,
  a ~ dnorm(105, 15),
  w ~ dnorm(0, 10),
  sigma ~ dexp(1)
)

m.doy <- quap(mod_list, data=list(doy=d2$doy, B=B), start = list(w=rep(0, ncol(B))))

post <- extract.samples(m.doy)
mu.link <- matrix(0, nrow = 787, ncol = 1e4)
for (i in 1:length(post$a))
  mu.link[,i] <- post$a[i] + B %*% post$w[i,]

# mu <- link(m.temp)#sapply(m.temp, mu.link)#link(m.temp)#
mu_mu <- apply(mu.link, 1, mean)
mu_PI <- apply(mu.link, 1, PI, .89)
plot(d2$temp, d2$doy)
shade(mu_PI, d2$temp, col=col.alpha(3,.5))
lines(d2$temp, mu_mu, col=col.alpha(1,.5))

plot(NULL, xlim=range(d2$temp), ylim=c(-5,5), xlab="temp", ylab="weighted basis")
alpha <- apply(post$a, 2, mean)
w <- apply(post$w, 2, mean)
zero_seq <- rep(0, 787)
for (i in 1:ncol(B)) {
  points(d2$temp, w[i] *B[,i])
  zero_seq <- w[i]*B[,i] + zero_seq
}
  
i <- 2
plot(d2$temp, d2$doy, cex=0.75, pch=16, col=col.alpha(rangi2, 0.4))
points(d2$temp, zero_seq+mean(post$a))
