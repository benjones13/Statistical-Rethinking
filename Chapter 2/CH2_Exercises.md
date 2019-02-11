CH2\_Practice
================
Ben Jones
4 February 2019

R Markdown
----------

*2E1:* (2) & (4)

*2E2:* (3)

*2E3:* (1), by definition. Also (4), by Bayes theorem

*2E4:* What de finetti meant was likely that probability is simply a metric for expressing uncertainty regarding an outcome given the available information, and that this probability can change in light of new information. With regard to the statement "the probability of water is 0.7", what we are saying is that given our observations with the globe tossing exercise, alongside any prior information we have, our best guess of the proportion of the earth's surface covered in water is 0.7, although if we were to obtain more information this estimate could change.

*2M1:* i)

``` r
p_grid = seq(0,1,length.out = 20)
prior = rep(1,20)
likelihood = dbinom(3,size = 3, prob = p_grid)
unstd.posterior = likelihood * prior
posterior = unstd.posterior/sum(unstd.posterior)
plot(p_grid, posterior, type = "b", xlab = "Probability of Water", ylab = "posterior probability")
```

![](CH2_Exercises_files/figure-markdown_github/cars-1.png) ii)

``` r
p_grid2 = seq(0,1,length.out = 20)
prior2 = rep(1,20)
likelihood2 = dbinom(3,size = 4, prob = p_grid)
unstd.posterior2 = likelihood2 * prior2
posterior2 = unstd.posterior2/sum(unstd.posterior2)
plot(p_grid2, posterior2, type = "b", xlab = "Probability of Water", ylab = "posterior probability")
```

![](CH2_Exercises_files/figure-markdown_github/unnamed-chunk-1-1.png) iii)

``` r
p_grid3 = seq(0,1,length.out = 20)
prior3 = rep(1,20)
likelihood3 = dbinom(5,size = 7, prob = p_grid)
unstd.posterior3 = likelihood3 * prior3
posterior3 = unstd.posterior3/sum(unstd.posterior3)
plot(p_grid3, posterior3, type = "b", xlab = "Probability of Water", ylab = "posterior probability")
```

![](CH2_Exercises_files/figure-markdown_github/unnamed-chunk-2-1.png)

*2M2:* i)

``` r
p_grid4 = seq(0,1,length.out = 20)
prior4 = ifelse(p_grid4< 0.5,0,1)
likelihood4 = dbinom(3,size = 3, prob = p_grid)
unstd.posterior4 = likelihood4 * prior4
posterior4 = unstd.posterior4/sum(unstd.posterior4)
plot(p_grid4, posterior4, type = "b", xlab = "Probability of Water", ylab = "posterior probability")
```

![](CH2_Exercises_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  

``` r
p_grid5 = seq(0,1,length.out = 20)
prior5 = ifelse(p_grid4< 0.5,0,1)
likelihood5 = dbinom(3,size = 4, prob = p_grid)
unstd.posterior5 = likelihood5 * prior5
posterior5 = unstd.posterior5/sum(unstd.posterior5)
plot(p_grid5, posterior5, type = "b", xlab = "Probability of Water", ylab = "posterior probability")
```

![](CH2_Exercises_files/figure-markdown_github/unnamed-chunk-4-1.png)

1.  

``` r
p_grid6 = seq(0,1,length.out = 20)
prior6 = ifelse(p_grid4< 0.5,0,1)
likelihood6 = dbinom(5,size = 7, prob = p_grid)
unstd.posterior6 = likelihood6 * prior6
posterior6 = unstd.posterior6/sum(unstd.posterior6)
plot(p_grid6, posterior6, type = "b", xlab = "Probability of Water", ylab = "posterior probability")
```

![](CH2_Exercises_files/figure-markdown_github/unnamed-chunk-5-1.png)

*2M3:*
*P*(*L**a**n**d*|*E**a**r**t**h*)=0.3
 and
*P*(*E**a**r**t**h*)=0.5
, as given. Then as Mars is 100 land and Earth is 30 land, *P*(*L**a**n**d*)=0.15. The by Bayes' Theorem,

``` math
$$(Earth|Land) = \frac{P(Land|Earth)P(Earth)}{P(Land)} = \frac{0.3 \times 0.5}{0.15} = 0.23$$
```
