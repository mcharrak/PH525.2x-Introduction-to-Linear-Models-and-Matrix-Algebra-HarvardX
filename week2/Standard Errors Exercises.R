## week 2: Standard Errors Exercises

library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef

fit = lm(y ~ x)
y_hat <- fit$fitted.values
res <- y - y_hat
SSR <- sum(res^2)
SSR

p <- length(betahat)
df <- N - p
sigma2 <- SSR/df

## exercise 2

X = cbind(rep(1,N), x)
inv_X_prime_X <- solve(t(X)%*%X)
cat(inv_X_prime_X[1,1])

## exercise 3

diagonal <- diag(inv_X_prime_X)
var_estimates <- sigma2*diagonal
se_estimates <- sqrt(var_estimates)
cat('Standard error of the slope coefficient:\n')
cat(se_estimates[2])

# compare to lm output:
summary(fit)