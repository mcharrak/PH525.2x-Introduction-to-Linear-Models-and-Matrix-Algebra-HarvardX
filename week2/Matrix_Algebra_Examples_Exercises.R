## week 2: Matrix Algebra Examples Exercises

## set of 4 samples where the first 2 samples are from a treatment group A & the second 2 samples from a treatment group B
# => This design can be represented with a model matrix like so:
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
beta <- c(5,2)

# HINT: matrix multiplication operator in R is: %*%

## exercise 1

yhat <- X %*% beta
cat(yhat["a",])

## exercise 2

cat(yhat["b",])

# Suppose now we are comparing two treatments B and C to a control group A, each with 2 samples. 
# this design can be represented with a model matrix like so:
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
beta <- c(10,3,-3)

## exercise 3

yhat <- X %*% beta
cat(yhat["b",])

## exercise 4

cat(yhat["c",])