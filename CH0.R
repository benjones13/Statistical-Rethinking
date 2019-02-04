## Rcode 0.4 ##
# Load the data:
# Car braking distances in feet paired with speeds in km/h
# see ?cars for details
data(cars)
# fit a linear regression of distance on speed
m <- lm( dist ~ speed,  data = cars)

# estimated coefficients from the model
coef(m)

# plot the residuals against speed
plot( resid(m) ~ speed, data = cars)

## Rcode 0.5 ## 
install.packages(c("coda", "mvtnorm", "devtools"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
