 ##7.1
library(rethinking)
data(rugged)
d <- rugged

d$log_gdp <- log(d$rgdppc_2000)

dd <- d[complete.cases(d$rgdppc_2000),]

d.A1 <- dd[dd$cont_africa == 1,]
d.A0 <- dd[dd$cont_africa == 0,]

##7.2
m7.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d.A1
)
m7.2 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a~dnorm(8,100),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d.A0
)

##7.3
m7.3 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8,100),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = dd
)

##7.4
m7.4 <- map(alist(
  log_gdp ~ dnorm(mu, sigma),
  mu <- a + bR * rugged + bA * cont_africa,
  a~dnorm(8,100),
  bR ~ dnorm(0,1),
  bA ~ dnorm(0, 1),
  sigma ~ dunif(0,10)
),data = dd
)

##7.5
compare(m7.3, m7.4)

##7.6
rugged.seq <- seq(from = -1, to = 8, by = 0.25)

mu.NotAfrica <- link(m7.4, data = data.frame(cont_africa = 0, rugged = rugged.seq))
mu.Africa <- link(m7.4, data = data.frame(cont_africa = 1, rugged = rugged.seq))

mu.NotAfrica.mean <- apply(mu.NotAfrica,2,mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica,2,PI, prob = 0.97)
mu.Africa.mean <- apply(mu.Africa,2,mean)
mu.Africa.PI <- apply(mu.Africa,2,PI, prob = 0.97)

plot(log_gdp~rugged, data = dd)
lines(rugged.seq,mu.NotAfrica.mean)
shade( mu.NotAfrica.PI,rugged.seq)
lines(rugged.seq,mu.Africa.mean)
shade(mu.Africa.PI, rugged.seq)

##7.7
m7.5 <- map(alist(
  log_gdp ~ dnorm(mu, sigma),
  mu <- a + gamma * rugged + bA * cont_africa,
  gamma <- bR + bAR * cont_africa,
  a~dnorm(8,100),
  bA ~ dnorm(0,1),
  bR ~ dnorm(0,1),
  bAR ~ dnorm(0,1),
  sigma ~ dunif(0,10)
), data = dd
)

##7.8
compare(m7.3,m7.4,m7.5)

##7.10
rugged.seq <- seq(from = -1, to = 8, by = 0.25)

mu.Africa <- link(m7.5, data = data.frame(cont_africa = 1, rugged = rugged.seq))
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)

mu.NotAfrica <- link(m7.5, data = data.frame(cont_africa=0,rugged = rugged.seq))
mu.NotAfrica.mean <- apply(mu.NotAfrica,2,mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica,2,PI,prob = 0.97)

##7.11
d.A1 <- dd[dd$cont_africa == 1,]
plot(log(rgdppc_2000) ~ rugged, data = d.A1,
col = rangi2, ylab = "log GDP year 2000",
xlab = "Terrain Ruggedness index")
mtext("African Nations", 3)
lines(rugged.seq, mu.Africa.mean, col = rangi2)
shade(mu.Africa.PI, rugged.seq, col = col.alpha(rangi2,0.3))

d.A0 <- dd[dd$cont_africa == 0,]
plot(log(rgdppc_2000) ~ rugged, data = d.A0,
     col = "black", ylab = "log GDP year 2000", 
     xlab = "Terrain ruggedness index")
mtext("Non african nations", 3)
lines(rugged.seq, mu.NotAfrica.mean)
shade(mu.NotAfrica.PI, rugged.seq)


##7.12
precis(m7.5)

##7.13
post <- extract.samples(m7.5)
gamma.Africa <- post$bR + post$bAR*1
gamma.notAfrica <- post$bR + post$bAR*0

##7.14
mean(gamma.Africa)
mean(gamma.notAfrica)

##7.15
dens(gamma.Africa, xlim = c(-0.5,0.6), ylim = c(0,5.5),
     xlab = "gamma", col = rangi2)
dens(gamma.notAfrica,add=T)

##7.16
diff <- gamma.Africa - gamma.notAfrica
sum(diff<0)/length(diff)

##7.17
q.rugged <- range(dd$rugged)

mu.ruggedlo <- link(m7.5,
                    data = data.frame(rugged = q.rugged[1], cont_africa = 0:1))
mu.ruggedlo.mean <- apply(mu.ruggedlo,2,mean)
mu.ruggedlo.PI <- apply(mu.ruggedlo, 2, PI)

mu.ruggedhi <- link(m7.5,
                    data = data.frame(rugged = q.rugged[2], cont_africa = 0:1))
mu.ruggedhi.mean <- apply(mu.ruggedhi,2,mean)
mu.ruggedhi.PI <- apply(mu.ruggedhi, 2, PI)

med.r <- median(dd$rugged)
ox <- ifelse(dd$rugged > med.r, 0.05, -0.05)
plot(dd$cont_africa + ox, log(dd$rgdppc_2000),
     col = ifelse(dd$rugged > med.r, rangi2,"black"),
                  xlim = c(-0.25,1.25), xaxt = "n", ylab = "log GDP year 2000",
                  xlab = "Continent")
axis(1, at = c(0,1), labels = c("other", "Africa"))
lines(0:1, mu.ruggedlo.mean,lty=2)     
shade(mu.ruggedlo.PI, 0:1)
lines(0:1,mu.ruggedhi.mean,lty=2)
shade(mu.ruggedhi.PI,0:1)

##7.18
library(rethinking)
data(tulips)
d <- tulips
str(d)

##7.19
m7.6 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water + bS * shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0,100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0,100)
  ), data=d
)

m7.7 <- map(
  alist(
  blooms ~ dnorm(mu, sigma),
  mu <- a + bW * water + bS * shade + bWS * water * shade,
  a ~ dnorm(0, 100),
  bW ~ dnorm(0,100),
  bS ~ dnorm(0,100),
  bWS ~ dnorm(0,100),
  sigma ~ dunif(0, 100)
), data = d
)

##7.20
m7.6 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water + bS * shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0,100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0,100)
  ), data=d,
  method = "Nelder-Mead",
  control = list(maxit=1e4)
)

m7.7 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water + bS * shade + bWS * water * shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0,100),
    bS ~ dnorm(0,100),
    bWS ~ dnorm(0,100),
    sigma ~ dunif(0, 100)
  ), data = d,
  method = "Nelder-Mead",
  control = list(maxit = 1e4)
)

##7.21
coeftab(m7.6, m7.7)

##7.22
compare(m7.6,m7.7)

##7.23
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)

##7.24
m7.8 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water.c + bS*shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ),
  data = d,
  start = list(a = mean(d$blooms), bW = 0, bS = 0, sigma = sd(d$blooms))
)

m7.9 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water.c + bS*shade.c + bWS * water.c * shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ),
  data = d,
  start = list(a = mean(d$blooms), bW = 0, bS = 0, bWS = 0, sigma = sd(d$blooms))
)

coeftab(m7.8,m7.9)

##7.25
k <- coef(m7.7)
k[1] + k[2] * 2 + k[3] * 2 + k[4] * 2 * 2

##7.26
k <- coef(m7.9)
k[1] + k[2]*0 + k[3] * 0 + k[4] *0*0

##7.27
precis(m7.9)

##7.28
par(mfrow = c(1,3))

shade.seq <- -1:1
for ( w in -1:1 ) {
  dt <- d[d$water.c==w,]
  plot( blooms ~ shade.c , data=dt , col=rangi2 ,
        main=paste("water.c =",w) , xaxp=c(-1,1,2) , ylim=c(0,362) ,
        xlab="shade (centered)" )
  mu <- link( m7.9 , data=data.frame(water.c=w,shade.c=shade.seq) )
  mu.mean <- apply( mu , 2 , mean )
  mu.PI <- apply( mu , 2 , PI , prob=0.97 )
  lines( shade.seq , mu.mean )
  lines( shade.seq , mu.PI[1,] , lty=2 )
  lines( shade.seq , mu.PI[2,] , lty=2 )
}

##7.33
x <- z <- w <- 1
colnames(model.matrix(~x*w*z))
