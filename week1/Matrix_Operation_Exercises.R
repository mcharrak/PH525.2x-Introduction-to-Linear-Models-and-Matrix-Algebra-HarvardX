## week 1: Matrix Operation Exercises

## exercise 1

# answer: X%*% matrix(1,ncol(X)) is not equivalent to X
# reason: because all others answers are equivalent to X and the answer above does not even guarantee to have the same dimensions as X

## exercise 2

y <- matrix(data = c(10,
                     5,
                     7,
                     4), nrow = 4, ncol = 1)
X <- matrix(data = c( 3,  2,  1,  5,  #a
                      4,  2, -1,  0,  #b
                     -5,  2,  5,  0,  #c
                      1, -1, -5,  1  #d
                      ), nrow = 4,ncol = 4, byrow = FALSE)

sol_ex1 <- solve(X)%*%y
cat("The solution for c is:\n")
cat(sol_ex1[3])

# alternative appraoch:
sol_ex1_alternative <- solve(a = X, b = y) #generic function 'solve()' solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
cat(sol_ex1_alternative[3])


# Load the following two matrices into R:
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)

## exercise 3

matrix_ex3 <- a%*%b
cat(matrix_ex3[3,2])

## exercise 4

row_a <- a[3,]
col_b <- b[,2]
matrix_ex4 <- row_a * col_b
cat(sum(matrix_ex4))
# observation: This is equivalent to the 3rd row, 2nd column element of the product of the two matrices.