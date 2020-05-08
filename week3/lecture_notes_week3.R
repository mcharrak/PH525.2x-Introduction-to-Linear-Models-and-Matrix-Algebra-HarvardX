setwd("~/Desktop/ML_classes/Linear_Models_and_Matrix_Algebra_harvard/week3")
## lecture notes week 3

## lecture: Linear Models as Matrix Multiplication I

# perform matrix multiplication instead of loop
y <- rnorm(1e6)
x <- cbind(rep(1,1e6), rep(0:1, each = 5e5)) # concatenate 2 vectors of same length (because 1e6 == 2*5e5)
beta <- c(1,1)

# measure time to execute matrix multiplication
system.time({
  res <- sum((y - x %*% beta)^2)
  cat(res)
  })

# compared to loop
system.time( {
  res <- sum(sapply(seq_along(y), function(i) (y[i] - x[i,1] * beta[1] - x[i,2] * beta[2])^2))
  cat(res)
})

## lecture: Expressing Experimental Designs

# in this lecture we will learn two base R functions: {formula(), model.matrix()}
# model.matrix can be used to create design matrices for linear models

x <- c(1,1,2,2)
f <- formula(~ x)
model.matrix(f)

class(x)
# x is of type nuermic, however if x were of type factor then the model matrix will look different:

x <- factor(c(1,1,2,2))
class(x)
f <- formula(~ x)
model.matrix(~ x)
# we can see from the column x2, that it became an indicator variable s.t. x2==1 iff x==2

# now we have a factor x with 3 levels
x <- factor(c(1,1,2,2,3,3))
f <- formula(~ x)
model.matrix(f)
model.matrix(f, contrasts.arg = list(x  = 'contr.sum'))

x <- factor(c(1,1,1,1,2,2,2,2))
y <- factor(c('a', 'a', 'b', 'b', 'a', 'a', 'b', 'b'))
model.matrix(~ x + y)
# or with interaction term using ':' notation
model.matrix(~ x + y + x:y) # adds an interaction variable x*y as 3rd column
# we see that the interation term is only 1 iff both x and y are on i.e. x == 2, y =='b' (see rows 7 and 8)

# or equivalently we can just use the asterisk symbol * to achieve the same design matrix as x + y + x*y
model.matrix(~ x*y)
# OBSERVATION: 1) model.matrix(~ x + y + x:y) and 2) model.matrix(~ x*y) --> same resulting design matrix

x <- factor(c(1,1,2,2))
model.matrix(~ x) # first level receives the x1 values of 0, which is when x == 1
x <- relevel(x, "2") 
model.matrix(~ x) # first level receives the x1 values of 0, which is now when x == 2 (because of the re-leveled x factor)
# levels of a factor are re-ordered s.t. the level specified by ref is 1st and the others are moved down
# this does not change the order of the values in the vector x!

# now lets change to the old factor level order
x <- factor(x, levels = c("1", "2"))

z <- 1:4
model.matrix(~ z)
model.matrix(~ 0 + z) # with 0, we can specify, that the model matrix has no intercept column -> intercept column is dropped
model.matrix(~ z + I(z^2)) # if we want to include a numeric transformation of z, we have to wrap the trafo in the I() function

## lecture : Linear Models in Practice I

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
dat <- read.csv(filename)

# create 1D scatter plots (or dot plots)
# => good alternative to boxplot when sample size is small
stripchart(dat$Bodyweight ~ dat$Diet,
           vertical=T, method = "jitter",
           main = "Bodyweight over Diet")

## create linear model to analyze the difference between the 2 groups
levels(dat$Diet) # order of the levels is taken alphabetically for the function model.matrix
X <- model.matrix(~ Diet, data = dat)
head(X)
# OBSERVATION: 1st colum: intercept & 2nd column: indicator variable
colnames(X)
# so let us revel to use hf as reference level
dat$Diet <- relevel(dat$Diet, ref = "hf")
levels(dat$Diet)
X <- model.matrix(~ Diet, data = dat)
head(X)
# and reverse levels back to original order
dat$Diet <- relevel(dat$Diet, ref = "chow")
levels(dat$Diet)

## lecture : Linear Models in Practice II

fit <- lm(Bodyweight ~ Diet, data = dat)
summary(fit)
coefs <- coef(fit)
# diff. between control and high fat mice is roughly 3.021  (beta1)#

## The mathematics behind lm() is the solution to betaHat being

# $$ \hat{\beta} = (X^t X)^{-1} X^t y  $$

y <- dat$Bodyweight
x <- model.matrix(object = ~ Diet, data = dat)
beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
cat(beta_hat)

s <- split(x = dat$Bodyweight, f = dat$Diet, )
# intercept (beta0) is just the average Bodyweight for the control samples
mean(s[["chow"]]) #double brackets needed to retrieve the numeric vector
# slope (beta1) is just the difference between average Bodyweight of high fat to control samples
mean(s[["hf"]]) - mean(s[["chow"]])


# make graph from the lecture
stripchart(dat$Bodyweight ~ dat$Diet,
           vertical=T, method = "jitter",
           main = "Bodyweight over Diet",
           ylim = c(0,40), xlim = c(0,3))
a <- -0.25 
lgth <- .1 #length of the eges of the arrow head
library(RColorBrewer)
cols <- brewer.pal(n = 3, name = "Dark2") # get 3 different colors
abline(h=0)
arrows(x0 = 1+a, y0 = 0, x1 = 1+a, y1 = coefs[1],
       col = cols[1], lwd = 3, length = lgth)
# add horizontal line at the intercept value
abline(h = coefs[1], col = cols[1])
arrows(x0 = 2+a, y0 = coefs[1], x1 = 2+a, y1 = coefs[1] + coefs[2],
       col = cols[2], lwd = 3, length = lgth)
# add horizontal line for the average of the hf group
abline(h = coefs[1] + coefs[2], col = cols[2])
legend("right", names(coefs), fill = cols, cex = 0.75, bg = "white")

# now: let's compare lm results with results from a simple 2 group t-test
summary(fit)$coefficients
ttest <- t.test(x = s[["chow"]], y = s[["hf"]], var.equal = T)
summary(fit)$coefficients[2,3]
ttest$statistic
# OBSERVATION: both t-values are same despite the sign is different: -2.05 vs 2.05
# to get the same sign we just have to flip the order or arguments in ttest()
ttest <- t.test(x = s[["hf"]], y = s[["chow"]], var.equal = T)
ttest$statistic