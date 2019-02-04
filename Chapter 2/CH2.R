## 2.3 ##
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior = rep(1, 20)

#compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

## 2.4 ##
plot( p_grid, posterior, type = "b",
      xlab = "probability of water", ylab  = "posterior probability")
mtext("20 points")

## 2.5 ##
prior2 <- ifelse(p_grid < 0.5, 0, 1)

unstd.posterior2 <- likelihood * prior2

posterior2 <- unstd.posterior2/sum(unstd.posterior2)

plot(p_grid, posterior2, type = "b", 
     xlab = "probability of water")
mtext("20 points")

prior3 <- exp( -5 * abs(p_grid - 0.5))

unstd.posterior3 <- likelihood * prior3

posterior3 <- unstd.posterior3/sum(unstd.posterior3)

plot(p_grid, posterior3, type = "b",
     xlab = "probability of water")
mtext("20 points")

## 2.6 ##
library(rethinking)
global.qa <- map(
  alist(
    w ~ dbinom(9, p),  #binomial likelihood
    p ~ dunif(0, 1)
  ),
  data = list(w = 6))
  )
)
#display summary of quadratic approximation
precis(global.qa)

## 2.7 ##
#analytical calculation
w <- 6
n <- 9
curve( dbeta(x, w+1, n-w+1), from = 0, to = 1)
#quadratic approximation
curve(dnorm(x, 0.67,0.16), lty = 2, add = TRUE)
