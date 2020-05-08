## exercise: Linear Models in Practice Exercises

# in the previous lecture we saw, that the ttest and lm give us the same t-statistic. 
# we know that the numerator of the t-value in both cases is the difference between the average of the groups.
# In the t-test, the denominator of the t-value is the standard error of the difference:
# SE <- sqrt(var(diff)) with var(diff) being:
# var(diff) <- (1/nx + 1/ny) (sum {(x_i - mu_x)^2} + sum{(y_i - mu_y)^2}) / (nx + ny - 2)
# where nx is the number of samples in the first group and ny is the number of samples in the second group.

# If we look carefully, the second part of this equation is the sum of squared residuals, divided by (N - 2).
# So all that is left to show is that:
# ( (X^T X)^-1 )[2,2] = (1/nx + 1/ny)
# where [2,2] indicates the 2nd row, 2nd column, with X as the design matrix of a linear model of two groups.

## exercise 1

nx <- 5
ny <- 7
N <- nx+ny

col1 <- rep(1, N)
col2 <- rep(x = c(0,1), time = c(nx, ny))
X <- cbind(rep(1,nx + ny), rep(c(0,1), c(nx, ny)))
head(X)

X_prime_X <- t(X) %*% X
print(X_prime_X[1,1])

## exercise 2

print(X_prime_X)