## week2 notes lectures

## lecture: Examples

## how to perform the scalar product (i.e. crossproduct or dot-product)

u <- matrix(data = c(1,2,3,4,5,6,7,8,9,10),ncol = 1)
v <- matrix(data = rep(1, 10), ncol = 1)

# dim(u) == dim(v) == (10,1)

# compute crossprodcut
# method 1:
res_method1 <- t(u) %*% v
res_method2 <- crossprod(u,v)

# we know that the dot product <u,v> should give the sum of values in u. let's test:
all.equal(as.matrix(sum(u)), res_method2)

# crossproduct with a vector itself e.g. <u,u> can be done as follows
auto_dotproduct_method1 <- crossprod(u)
auto_dotproduct_method2 <- t(u) %*% u
all.equal(auto_dotproduct_method1, auto_dotproduct_method2)

## lecture: Matrix Algebra in Practice I

g <- 9.8 #acceleration constant
n <- 25 #number of observations
tt <- seq(0,3.4,length.out = n) #times of observations => we use tt instead of t because t is the built-in transpose function t() of R
f <- 56.67 + 0*tt - 0.5*g*tt^2 
y <- f + rnorm(n,sd = 1) #add observational error term epsilon with SD = 1
# y(t) + v0*t + y0 - 0.5*g*t^2 -> where v0 is initial velocity, y0 is initial altitude
# formula for "free fall"/ vertical motion of an object falling a small distnace close to the ground (i.e. it is a good approximation when the object's initial velocity is always much less than the terminal velocity)
# => from here https://en.wikipedia.org/wiki/Free_fall
plot(x = tt, y = y, xlab = "Time in secs", ylab = 'Distance in meters') # path of the data that we observe (with error)
lines(x = tt, y = f, col = 2) # actual path (without error)

# define a function to calculate the residual sum of squares (rss)
rss <- function(Beta0, Beta1, Beta2){
  # create residuals
  r <- y - (Beta0 + Beta1*tt + Beta2*tt^2)
  rss <- sum(r^2)
  return(rss)
}

Beta2s <- seq(-10,0,length.out = 100)
RSS <- sapply(Beta2s, rss, Beta0 = 55, Beta1 = 0) # Beta0 and Beta1 are just "good guesses"
plot(Beta2s, RSS, type = "l")
RSS <- sapply(Beta2s, rss, Beta0 = 65, Beta1 = 0) # Beta0 and Beta1 are just "good guesses"
lines(Beta2s, RSS, type = "l", col = 3) #add new line to previous plot

# obtain coefficients with linear model regression fitting
tt2 <- tt^2
fit <- lm(y~tt+tt2)
# show summary of linear regression
summary(fit)
# observation: we see that the p-value for tt is not significant, which indicates that the esimated formula for y is y = 56.77 + 0*tt - 5.76*tt2

## lecture: Matrix Algebra in Practice II
X <- cbind(rep(1, length(tt)), tt, tt^2)
head(X)
#X <- cbind(1,tt,tt^2) #works as well b/c R replicates 1 in first column to fit the other arguments in cbind()

Beta <- matrix(c(55,0,5),3,1)

# compute residuals
r <- y - X%*%Beta
# compute rss
RSS <- t(r) %*% r # equivalent to crossprod() function which is faster and more stable
RSS <- crossprod(r)

# now use the least squares approach to compute the beta estimates (betahat)
betahat <- solve(crossprod(X, X)) %*% crossprod(X, y) # by default R transposes the first argument: crossprod(x,x) == t(x)%*%x
# alternatively we could do as below:
#betahat <- solve(crossprod(X)) %*% crossprod(X, y)  #this works because by default crossprod() uses X as 2nd argument when no 2nd argument given
# or a bit slower:
#betahat <- solve ( t(X) %*%  X) %*% t(X) %*% y 
cat(fit$coefficients)
cat(t(betahat))
# we see that the results are similar (i.e. same)

# FINALLY THE REMAINING ISSUE IS THAT SOLVE() IS SOMEWHAT UNSTABLE => IT IS BETTER TO USE A QR DECOMPOSITION TO RETRIEVE THE INVERSE OF A MATRIX
QR <- qr(X)  #which decomposes the matrix X into two new matrices Q and R
# Reconstruct the Q, R, or X Matrices from a QR Object (here: QR)
Q <- qr.Q(QR)
R <- qr.R(QR)
# now use backsolve() more stable version than solve which requires a triangular system of linear equations and a transformed right hand side (RHS) using t(Q)%*%y 
betahat_QR <- backsolve(r = R, x = crossprod(Q,y))
cat(betahat_QR)
# we see that the results we get by using {QR decomposition + backsolve()} are similar to the results of the approaches above (i.e. same)

## lecture note slide: Technical note on variance

# common assumption that X is fixed. Thus, X*beta has no variance (b/c X are fixed). Therefore, given the fact Y = X*beta + eps, we must
# conclude that Var(Y) = Var(eps) = sigma^2

# for example, compute the following:

x =  father.son$fheight
beta =  c(34,0.5)
var(beta[1]+beta[2]*x)

# we can see that this variance (1.88) is nothing near 0 (as we would expect from Var(eps))
# The function var() is simply computing the variance of the list we feed it - 
# while the mathematical use of variance is considering only quantities that are random variables.

# another example with the free fall object formula (e.g. falling from pisa tower)
n = length(tt)
y = h0 + v0*tt  - 0.5*g*tt^2 + rnorm(n, sd=1) #should have a variance of 1 because the eps term has SD=1
var(y)
# observation: variance of 326 instead of 1, because tt is not fixed

## lecture notes: Inference for LSE

# CLT applies in linear models. 

# 1)
# If N (sample size) is large enough, then the LSE will be normally distributed with 
# mean beta and standard errors as computed in previous lectures and exercises of week 2

# 2)
# For small samples sizes N, if the error term is normally distributed, then the betahat-betas follow a t-distribution