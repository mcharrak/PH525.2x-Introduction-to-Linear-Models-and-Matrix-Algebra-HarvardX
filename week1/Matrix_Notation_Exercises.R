## week 1: Matrix Notation Exercises

################################################################################################################

## exercise 1
            
X_ex1 = matrix(1:1000,100,10)
X_ex1[25,3]

## exercise 2

x <- 1:10
X_ex2 <- cbind(1*x,
           2*x,
           3*x,
           4*x,
           5*x)
sum(X_ex2[7,])

## exercise 3

# matrix with multiples of 3 in the 3rd column
x <- 11:20
X_ex3 <- matrix(data = 1:60, nrow = 20, ncol = 3, byrow = TRUE) # default is byrow = FALSE, indicating numbers populated by columns (downwards)
#to confirm run below
#all(X_ex3[,3]%%3 == 0)