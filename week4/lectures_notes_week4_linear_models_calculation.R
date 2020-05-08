# week 4 lecture notes: Calculation of Linear Models"

# lecture notes: Collinearity

# rank of a matrix (e.g. design matrix)
set.seed(1)
Sex <- c(0,0,0,0,1,1,1,1)
A <- c(1,1,0,0,0,0,0,0)
B <- c(0,0,1,1,0,0,0,0)
C <- c(0,0,0,0,1,1,0,0)
D <- c(0,0,0,0,0,0,1,1)
X <- model.matrix(~ Sex + A + B + C + D - 1)
cat("ncol=",ncol(X), "rank=", qr(X)$rank, "\n")


# solve such collinearity issue with correct experiment design: 
# balance sex and treatmetn by assigning 1 female and 1 male to each of the four treatments
Sex <- c(0,1,0,1,0,1,0,1)
A <- c(1,1,0,0,0,0,0,0)
B <- c(0,0,1,1,0,0,0,0)
C <- c(0,0,0,0,1,1,0,0)
D <- c(0,0,0,0,0,0,1,1)
X <- model.matrix(~ Sex + A + B + C + D - 1)
cat("ncol=",ncol(X), "rank=", qr(X)$rank, "\n")

# lecture notes: QR decomposition

set.seed(1)
n <- 50; M <- 500
x <- seq(from = 1,to = M, length.out = n)
X <- cbind(1,x,x^2,x^3)
beta <- matrix(data = c(1,1,1,1), nrow = 4, ncol = 1)
# compute outcome y using x times beta plus random normal noise epsilon
y <- X %*% beta + rnorm(n = n, sd = 1)
# plot data
plot(x,y)

# solve X'X (X' denoting X transposed)
solve(crossprod(X))
# we get an error. becuase below we see, the values in crossprod(X) have very different numbers (in terms of magnitude)
head(crossprod(X)) 
log10(crossprod(X))

# solution that permits to compute the inverse of X'X

# compute QR matrices using qr()
QR <- qr(X)
Q <- qr.Q(QR)
R <- qr.R(QR)
# compute beta using backsolve function
betahat <- backsolve(r = R, x = crossprod(Q,y))
# alternatively 
betahat_alt1 <- solve(R) %*% t(Q) %*% y
betahat_alt2 <- solve(R) %*% crossprod(Q,y)

# use R built-in function to get beta estimates directly
betahat <- solve.qr(QR, y)

#fitted <- Q %*% t(X) %*% y
# alternatively
fitted <- tcrossprod(Q) %*% y

# plot fitted and original data
plot(x,y)
lines(x,fitted,col=2)

# now let us compute the Standard Error

df <- length(y) - QR$rank # compute degrees of freedom: sample size - rank of design matrix used for fitting
residuals <- y - fitted
squared_residuals <- residuals^2
sigma2 <- sum(squared_residuals)/df
var <- sigma2*chol2inv(QR$qr) # here: chol2inv(QR$qr) gives use the value of (X'X)^(-1)
# the diagonal contains the variance of each of the betas
SE <- sqrt(diag(var))
cbind(betahat, SE)
# all these computations can be done directly with lm() R function
summary(lm(y ~ X-1)) # remove 1 because the design matrix X already includes the intercept
# OBSERVATION: SAME RESULT FOR MANUAL AND AUTOMATED METHOD USING lm() R function