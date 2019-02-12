##3.1##

PrPV <- 0.95
PrPM <- 0.01
PrV <- 0.001
PrP <- PrPV * PrV + PrPM * (1 - PrV)
(PrVP <- (PrPV * PrV)/PrP)

##3.2##
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior = rep(1,1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

##3.3##
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)
 ##3.4##
plot(samples)

##3.5##
library(rethinking)
dens(samples)

##3.6##
sum( posterior[p_grid<0.5])


##3.7##
sum(samples < 0.5)/1e4

##3.8##
sum(samples > 0.5 & samples < 0.75)/1e4

##3.9##
quantile(samples, 0.8)

##3.10##
quantile(samples, c(0.1, 0.9))

##3.11##
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1,1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

##3.12##
PI(samples, prob = 0.5)

##3.13##
HPDI(samples, prob = 0.5)


##3.14##
p_grid[which.max(posterior)]

##3.15##
chainmode(samples, adj = 0.01)

##3.16##
mean(samples)
median(samples)

##3.17##
sum(posterior * abs(0.5 - p_grid))

##3.18##
loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
loss

##3.19##
p_grid[which.min(loss)]


#3.20##
dbinom(0:2, size = 2, prob = 0.7)

##3.21##
rbinom(1, size = 2, prob = 0.7)

##3.22##
rbinom(10, size = 2, prob = 0.7)

##3.23##
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w)/1e5

##3.24##
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

##3.25##
w <- rbinom(1e4,size = 9, prob = 0.6)
simplehist(w)

##3.26##
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
