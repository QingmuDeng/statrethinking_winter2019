library(rethinking)
######### Sec 5.3 Categorical Variables ##########
data("Howell1")
d <- Howell1
str(d)
d$sex <- ifelse(d$male==1, 2, 1)

m5.8 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a[sex],
    a[sex] ~ dnorm(160, 20),
    sigma ~ dunif(0, 50)
  ), data=d
)
precis(m5.8,depth=2)


post <- extract.samples(m5.8,n=1e4)
post$diff <- post$a[,2] - post$a[,1]
precis(post,depth=2)

####### Milk Example ########
data(milk)
d <- milk
levels(d$clade)

d$clade_id <- as.integer(d$clade)
d$K <- scale(d$kcal.per.g)

m5.9 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
labels<- paste("a[",1:4,"]:", levels(d$clade), sep="")
plot(precis(m5.9, depth=2, pars="a"), label=labels,xlab="expected kcal[std]")

##### Making Random categoies ####
set.seed(63)
d$house <- sample(rep(1:4,each=8), size=nrow(d))
m5.10 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a[clade_id]+h[house],
    a[clade_id] ~ dnorm(0, 0.5),
    h[house] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
precis(m5.10, depth=2)
labels<- paste("a[",1:4,"]:", levels(d$house), sep="")
plot(precis(m5.10, depth=2, pars="h"), label=labels,xlab="expected kcal[std]")

