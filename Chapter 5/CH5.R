library(rethinking)

##5.1##
data(WaffleDivorce)
d <- WaffleDivorce
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)

m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma) ,
    mu <- a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10,10) ,
    bA ~ dnorm(0,1) ,
    sigma ~ dunif(0,10)
  ) , data = d)

##5.2
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

##5.3##
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma) , 
    mu <- a + bR * Marriage.s,
    a ~ dnorm(10, 10) ,
    bR ~ dnorm(0,1) ,
    sigma ~ dunif(0,10)
  ), data = d)
summary(m5.2)

##5.4##
m5.3<- map(
  alist(
    Divorce ~ dnorm(mu, sigma) ,
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bR ~ dnorm(0,1),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d)
precis(m5.3)

##5.5##
plot(precis(m5.3))

##5.6##
m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s ,
    a~dnorm(0,10), 
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)

##5.7##
mu <- coef(m5.4)['a'] + coef(m5.4)['b'] * d$MedianAgeMarriage.s
m.resid <- d$Marriage.s - mu

##5.8##
plot(Marriage.s ~ MedianAgeMarriage.s, d, col = rangi2)
abline(m5.4)
for(i in 1:length(m.resid)){
  x <- d$MedianAgeMarriage.s[i]
  y <- d$Marriage.s[i]
  lines(c(x,x), c(mu[i],y), lwd = 0.5, col = col.alpha("black", 0.7))
}
##5.9##
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from=-3, to =3, length.out = 30)
pred.data <- data.frame(
  Marriage.s = R.seq,
  MedianAgeMarriage.s = A.avg)

mu <- link(m5.3, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

R.sim <- sim(m5.3, data = pred.data, n = 1e4)
R.PI <- apply(R.sim, 2, PI)

plot(Divorce ~ Marriage.s, data = d, tpe = "n")
mtext("MedianMarriage.s=0")
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)

##5.10##
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from = -3, to = 3.5, length.out = 30)
pred.data2 <- data.frame(
  Marriage.s = R.avg,
  MedianAgeMarriage.s= A.seq)
mu <- link(m5.3, data = pred.data2)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu,2,PI)

A.sim <- sim(m5.3, data=pred.data2, n=1e4)
A.PI <- apply(A.sim, 2, PI)

plot(Divorce~MedianAgeMarriage.s, data=d, type = "n")
mtext("Marriage.s=0")
lines(A.seq,mu.mean)
shade(mu.PI,A.seq)
shade(A.PI, A.seq)

##5.11##
mu <- link(m5.3)
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI)

divorce.sim <- sim(m5.3, n=1e4)
divorce.PI <- apply(divorce.sim,2,PI)

##5.12##
plot(mu.mean ~ d$Divorce,col=rangi2,ylim = range(mu.PI), xlab = "Observed divorce", ylab = "Predicted divorce")
abline(a=0,b=1,lty = 2)
for(i in 1:nrow(d)){
  lines(rep(d$Divorce[i],2),c(mu.PI[1,i],mu.PI[2,i]), col=rangi2)
}

##5.13
identify(x = d$Divorce, y = mu.mean, labels = d$Loc, cex = 0.8)


##5.14
#Compute residuals
divorce.resid <- d$Divorce - mu.mean
o <- order(divorce.resid)
dotchart(divorce.resid[o], labels = d$Loc[o], xlim = c(-6,5), cex = 0.6)
abline(v = 0, col = col.alpha("black", 0.2))
for(i in 1:nrow(d)){
  j <- o[i]
  lines(d$Divorce[j] - c(mu.PI[1,j], mu.PI[2,j]), rep(i, 2))
  points(d$Divorce[j] - c(divorce.PI[1,j], divorce.PI[2,j]), rep(i,2),
         pch = 3, cex = 0.6, col = "gray")
}  

##5.15
N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <- rnorm(N, x_real)
d <- data.frame(y, x_real, x_spur)
pairs(d)
summary(lm(y ~x_real + x_spur,data=d))

##5.16
library(rethinking)
data(milk)
d <- milk
str(d)

##5.17
m5.5 <- map(
  alist(
   kcal.per.g ~ dnorm(mu, sigma),
   mu <- a + bn*neocortex.perc,
   a ~ dnorm(0 , 100), 
   bn ~ dnorm(0,1),
   sigma ~ dunif(0,1)
  ),
  data = d
)

#5.18
d$neocortex.perc

#5.19
dcc <- d[complete.cases(d),]

##5.20 
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0 , 100), 
    bn ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ),
  data = dcc
)

##5.21
precis(m5.5, digits = 3)

##5.22
coef(m5.5)["bn"] * (76-55)

##5.23
np.seq <- 1:100
pred.data <- data.frame(neocortex.perc = np.seq)

mu <- link(m5.5, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, col = rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

##5.24
dcc$log.mass <- log(dcc$mass)

##5.25
m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm * log.mass, 
    a ~ dnorm(0,1),
    bm ~ dnorm(0,1),
    sigma ~ dunif(0, 1)
  ),
  data = dcc
)
precis(m5.6)

np.seq <- -10:10
pred.data <- data.frame(log.mass = np.seq)

mu <- link(m5.6, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, col = rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

##5.26
m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc + bm * log.mass,
    a ~ dnorm(0,100),
    bn ~ dnorm(0,1),
    bm ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ),
  data = dcc
)
precis(m5.7)

##5.27
mean.log.mass <- mean( log(dcc$mass))
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc = np.seq,
  log.mass = mean.log.mass)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq,mu.PI[2,], lty = 2)

mean.neocortex <- mean(dcc$neocortex.perc)
np.seq <- -10:10
pred.data <- data.frame(
  neocortex.perc <- mean.neocortex, 
  log.mass <- np.seq)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean = apply(mu,2,mean)
mu.PI <- apply(mu,2,PI)

plot(kcal.per.g ~ log.mass, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq,mu.PI[2,], lty = 2)


##5.28
N <- 100 #sample size
rho = 0.7 #corr between covariates
x_pos <- rnorm(N)
x_neg  <- rnorm(N, rho * x_pos, sqrt(1-rho^2))
y <- rnorm(N, x_pos - x_neg)
d <- data.frame(y,  x_pos, x_neg)

pairs(d)

##5.29
N <- 100 #no. of individuals
height <- rnorm(N, 10, 2) # simulate total height of each
leg_prop <- runif(N, 0.4, 0.5) # leg as proportion of height
leg_left <- leg_prop * height + rnorm(N, 0, 0.02) 
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)
d <- data.frame(height, leg_left, leg_right)


m5.8 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + bl * leg_left + br * leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.8)

post <- extract.samples(m5.8)
plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)

#5.33
sum_blbr <- post$bl + post$br
dens(sum_blbr, col = rangi2, lwd = 2, xlab = "sum of bl and br")

##5.34 
m5.9 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    sigma ~ dunif(0,10)
  ),
  data = d
)
precis(m5.9)

##5.35
data(milk)
d <- milk

##5.36
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat, 
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0,10)
  ),
  data = d
)

m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl * perc.lactose, 
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0,10)
  ),
  data = d
)

precis(m5.10, digits = 3)
precis(m5.11, digits = 3)

##5.37
m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat + bl * perc.lactose,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.12, digits = 3)

pairs( ~kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)

##5.39
cor(d$perc.fat, d$perc.lactose)

##5.41
#number of plants
N <- 100
h0 <- rnorm(N, 10, 2)

treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, 5 - 3 * fungus)

d <- data.frame(h = h0, h1 = h1, treatmment = treatment, fungus = fungus)


##5.42

m5.13 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh * h0 + bt*treatment + bf * fungus,
    a ~ dnorm(0,100),
    c(bh, bt, bf) ~ dnorm(0,10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.13)

##5.43
m5.14 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh * h0 + bt*treatment ,
    a ~ dnorm(0,100),
    c(bh, bt) ~ dnorm(0,10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.14)


##5.44
data(Howell1)
d <- Howell1
str(d)

##5.45
m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm * male,
    a ~ dnorm(178,100),
    bm ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ), 
  data = d
  )
)
precis(m5.15)

##5.46
post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)
mu.male2 <- rnorm(1000,post$a + post$bm, post$sigma)

#5.47
m5.15b <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- af * (1-male) + am * male,
    af ~ dnorm(178, 100),
    am ~ dnorm(178, 100),
    sigma ~ dunif(0,50)
  ),
  data = d
)
precis(m5.15b)


##5.48
data(milk)
d <- milk
unique(d$clade)

##5.49
(d$clade.NWM <- ifelse(d$clade == "New World Monkey", 1, 0))

##5.50
(d$clade.OWM <- ifelse(d$clade == "Old World Monkey", 1, 0))
(d$clade.S <- ifelse(d$clade == "Strepsirrhine", 1, 0))

##5.51
m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
     mu <- a + b.NWM * clade.NWM + b.OWM * clade.OWM + b.S * clade.S,
    a ~ dnorm(0.6, 10),
    b.NWM ~ dnorm(0,1),
    b.OWM ~ dnorm(0,1),
    b.S ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data = d
)
precis(m5.16)

##5.52 
post <- extract.samples(m5.16)
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S))

##5.53
diff.NWM.OWM <- mu.NWM - mu.OWM
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975))

##
(d$clade_id <- coerce_index(d$clade))

##
m5.16_alt <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0,10)
  ),
  data=d
)
precis(m5.16_alt,depth = 2)
str(d)

##5.62
data(cars)
glimmer(dist ~ speed, data = cars)
