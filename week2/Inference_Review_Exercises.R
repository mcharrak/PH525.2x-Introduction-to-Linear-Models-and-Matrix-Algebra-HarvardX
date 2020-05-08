## week 2: Inference Review Exercises

g = 9.8 ## meters per second^2 (earth graviation)
h0 = 56.67 ## height at tt=0
v0 = 0 ## speed at tt=0
n = 25 ## number of observations
tt = seq(0,3.4,len=n) ## time in secs, t is a base function (i.e. transpose)
y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1) #the estimate for the covariate tt^2 will be beta2 == g*0.5, thus estimate of g == 2*beta2

# now we act as if we didn't know h0, v0 and -0.5*g and use regression to estimate these
# we can rewrite the model as y = b0 + b1*t + b2*t^2 + e and obtain the LSE we have used in this class. Note that g = -2*b2.

# To obtain the LSE in R we could write:
X <- cbind(1,tt,tt^2)
A <- solve(crossprod(X)) %*% t(X) #this corresponds to betahat without y because: betahat = (X'X)^-1 * X'*y
betahat  <- A %*% y
LSE_g <- 2*betahat[3]
cat(LSE_g)

# answer: 
# -2 * (A %*% y) [3]

# explanation: 
# 9.8 is not the answer because the LSE is a random variable. The A%*%y gives us the LSE for all three coefficients. 
# The third entry gives us the coefficient for the quantradic term which is -0.5 * g. We multiply by -2 to get the estimate of g.

## exercise 2

set.seed(1)
B <- 100000 # number of MC simulated datasets
LSE_gs <- replicate(n = B, expr = {
  y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)
  A <- solve(crossprod(X)) %*% t(X) #this corresponds to betahat without y because: betahat = (X'X)^-1 * X'*y
  betahat  <- A %*% y
  LSE_g <- 2*betahat[3]
})

SD_LSE_g <- sd(LSE_gs)
cat(SD_LSE_g)
# alternatively
#VAR_LSE_g_alt <- mean( ( LSE_gs - mean(LSE_gs) )^2 )
#SD_LSE_g_alt <- sqrt(VAR_LSE_g_alt)

## exercise 3

library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)

# Now let's run a Monte Carlo simulation in which we take a sample of size 50 over and over again
N = 50
index = sample(x = 1:n, size = N)
sampledat = father.son[index, ]

x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef

# Use the function replicate() to take 10,000 samples.
B = 10000
set.seed(1)
betahat_mat <- replicate(B, {
  index <- sample(x = 1:n, size = N)
  sampledat = father.son[index, ]
  
  x = sampledat$fheight
  y = sampledat$sheight
  betahat = lm(y~x)$coef
  return(betahat)
})
betahat_mat <- t(betahat_mat) #transpose b/c only each row will be a new sample

# compute SE (i.e. SD of the slope esimates)
slope_estimates <- betahat_mat[,2]
SE_slope_estimates <- sd(slope_estimates)
print(SE_slope_estimates)

## exercise 4

x <- father.son$fheight
y <- father.son$sheight

# compute covariacne between father heights (x) and son heights (y)
mu_x <- mean(x)
mu_y <- mean(y)

# numerator
num <- crossprod(x = (y - mu_y), y = (x - mu_x))
# denominator
denom <- length(x)

cov <- num/denom
print(cov[1,1])
# NOTE: only if x and y are independent, then cov(y,x) can be 0