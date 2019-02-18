library(rethinking)
##4.1##
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos)

##4.2
prod(1 + runif(12,0,0.1))

##4.3##
growth <- replicate(10000,prod(1 + runif(12,0,0.1)))
dens(growth, norm.comp = T)

##4.4##
big <- replicate(10000, prod(1 + runif(12,0,0.5)))
small <- replicate(10000, prod(1 + runif(12,0,0.01)))
dens(big, norm.comp = T)
dens(small, norm.comp = T)

##4.5##
log.big <- replicate(10000, log(prod(1 + runif(12,0,0.5))))
dens(log.big,norm.comp = T)

##4.6##
w <- 6; n <- 9;
p_grid <- seq(from = 0, to = 1, length.out = 100)
posterior <- dbinom(w,n,p_grid) * dunif(p_grid,0,1)
posterior <- posterior/sum(posterior)

##4.7##
data(Howell1)
d <- Howell1

##4.8##
str(d)

##4.10##
d2 <- d[d$age >= 18,]

dens(d2$height)

##4.11##
curve( dnorm(x,178,20), from = 100, to = 250)

##4.12##
curve(dunif(x, 0, 50), from = -10, to = 60)

##4.13##
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

##4.14##
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

##4.15##
contour_xyz(post$mu, post$sigma, post$prob)

##4.16##
image_xyz(post$mu, post$sigma, post$prob)

##4.17##
sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE,
                             prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

##4.18##
plot(sample.mu, sample.sigma, cex = 1, pch = 16, col = col.alpha(rangi2,0.1))

##4.19##
dens(sample.mu)
dens(sample.sigma)

#4.20
HPDI(sample.mu)
HPDI(sample.sigma)


##4.21##
d3 <- sample(d2$height, size = 20)

##4.22##
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )

##4.23##
dens(sample2.sigma, norm.comp = T)

##4.24##
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]

##4.25##
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

##4.26##
m4.1 <- map(flist, data = d2)

##4.27##
precis(m4.1)

##4.28##
start <- list(
  mu = mean(d2$height),
  sigma = sd(d2$height)
)

##4.29##
m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ), 
  data = d2)
precis(m4.2)

##4.30##
vcov(m4.1)

##4.31##
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

##4.32##
post <- extract.samples(m4.1, n = 1e4)
head(post)

##4.33##
precis(post)

##4.34##
library(MASS)
post <- mvrnorm(n = 1e4, mu = coef(m4.1), Sigma = vcov(m4.1))

##4.35##
m4.1_logsigma <-map(
  alist(
    height ~ dnorm(mu, exp(log_sigma)),
    mu ~ dnorm(178, 20),
    log_sigma ~ dnorm(2, 10)
  ), 
  data = d2
)

##4.36##
post <- extract.samples(m4.1_logsigma)
sigma <- exp(post$log_sigma)

##4.37##
plot(d2$height ~ d2$weight)

##4.38
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]

#fit model
m4.3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

##4.40##
precis(m4.3)

##4.41##
precis(m4.3, corr = T)

##4.42##
d2$weight.c <- d2$weight - mean(d2$weight)

##4.43##
m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c , 
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ) ,
data = d2)

##4.44##
precis(m4.4, corr = T)

##4.45##
plot(height ~ weight, data = d2)
abline(a=coef(m4.3)["a"], b = coef(m4.3)["b"])

##4.46##
post <- extract.samples(m4.3)
post[1:5,]

##4.48##
N <- 10
dN <- d2[1:N,]
mN <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = dN
)

##4.49##
post <- extract.samples(mN, n = 20)

plot(dN$weight, dN$height,
     xlim = range(d2$weight), ylim = range(dN$height),
     col = rangi2, xlab = "weight", ylab= "height")
mtext(concat("N = ", N))

for(i in 1:20)
  abline( a = post$a[i], b = post$b[i], col = col.alpha("black",0.3))

##4.50
post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * 50

##4.51##
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu|weight = 50")

##4.52##
HPDI(mu_at_50, prob = 0.89)

##4.53##
mu <- link(m4.3)
str(mu)

##4.54##
weight.seq <- seq(from = 25, to = 70, by = 1)
mu <- link(m4.3, data = data.frame(weight = weight.seq))

##4.55##
plot(height ~ weight, d2, type = "n")
for(i in 1:1000)
  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))

##4.56##
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)

##4.57##
plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))

lines(weight.seq, mu.mean)

shade(mu.HPDI, weight.seq)
