## week1 notes lectures:

# lecture: matrix multiplication

# matrix multiplication can be achieve with the opterator: %*%.

X <- matrix(data = c(1,3,2,1,-2,1,1,1,-1), 3, 3)
beta <- c(3,2,1)

X%*%beta

# inverse of a matrix to solve a set of linear equations

# (I)  a +  b + c = 6
# (2) 3a - 2b + c = 2
# (3) 2a +  b - c = 1

# from above LEs we get X and y as follows

X <- matrix(data = c(1,3,2,1,-2,1,1,1,-1), 3,3)
y <- matrix(c(6,2,1), 3, 1)
beta_opt <- solve(X)%*%y
beta_opt

# lets make a test to check if X*beta_opt equals y
X%*%beta_opt
y
# HOWEVER, solve only gives approximate values as we can see below, X*inv(X) only gives a fair approximation of a perfect identity matrix
X%*%solve(X)