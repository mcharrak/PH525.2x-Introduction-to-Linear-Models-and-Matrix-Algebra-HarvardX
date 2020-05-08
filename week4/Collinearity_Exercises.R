# Collinearity Exercises

# ex1

# deisgn matrix without collinerity problem: E
# check the rank using qr()$rank base fct.
m <- matrix(c(1,0,0,0, 1,0,1,0, 1,1,0,0, 1,1,1,1), nrow = 4, ncol = 4)
cat("ncol=",ncol(m), "rank=", qr(m)$rank, "\n")

# ex2

# Let's use the example from the lecture to visualize how there is not a single best beta-hat, 
# when the design matrix has collinearity of columns.

sex <- factor(rep(c("female", "male"), each = 4))
trt <- factor(c("A","A","B","B","C","C","D","D"))

X <- model.matrix(~ sex + trt)
qr(X)$rank

# generate some outcome Y
Y <- 1:8
makeYstar <- function(a,b) {
  Y - X[,2]*a - X[,5]*b
}

fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}

# ex2 solution
cat(fitTheRest(a = 1, b = 2))

# ex3

betas <- expand.grid(-2:8,-2:8)
rss <- apply(X = betas, MARGIN = 1, FUN = function(x) {fitTheRest(x[1], x[2])})
min_idxs <-  which(rss == min(rss))
betas[min_idxs,]
# visualise the sum of squares
library(rafalib)
plot(betas[min_idxs,])
# OBSERVATION: There is clearly not a single beta which optimizes the least squares equation, 
# due to collinearity, but an infinite line of solutions which produce an identical sum of squares values.