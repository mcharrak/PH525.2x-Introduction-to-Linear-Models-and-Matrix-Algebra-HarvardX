# Interactions Exercises

# load data
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

# add new transformed variable to spider data
spider$log2friction <- log2(spider$friction)
# visualize this transformation effect
boxplot(log2friction ~ type*leg, data = spider)
mypar(2,1)
boxplot(formula = log2friction ~ type*leg, data = spider, main = "With log transformation")
boxplot(formula = friction ~ type*leg, data = spider, main = "Without log transformation")

# exercise 1
fit1 <- lm(data = spider, formula = log2friction ~ type + leg + type:leg)
summary(fit1)

# exercise 2
anova(fit1) # we reject the null hypothesis, that the push vs pull effect on log2(friction) in the same for all leg-type pairs

# exercise 3
library(contrast)
contrast1 <- contrast(fit = fit1, 
                      list(type="pull", leg="L2"), 
                      list(type="pull", leg="L1"))
contrast1$Contrast
# alternatively as L1 is the reference group in fit1, we can simply use the coefficient of legL2, 
# because push is by default set to pull and remains constant
coefs <- coef(fit1)
# here the reference is pull therefore the length of such an arrow would be 0
# coefs['legL2]    - ( 0 )
cat(coefs["legL2"] -   0 )

# exercise 4
contrast2 <- contrast(fit = fit1, 
                      list(type="push", leg="L2"), 
                      list(type="push", leg="L1"))
contrast2$Contrast
# alternatively using arrows graphic from lecture_notes_week4.Rmd
# (  purple   +       orange       +         yellow )        -    ( orange )
coefs['legL2'] + coefs['typepush'] + coefs['typepush:legL2'] - coefs['typepush']

# exercise pre-experiments 5

# example calculating first F-value in anova()
ss.mean <- sum((spider$friction - mean(spider$friction))^2)
ss.type <- 0
frictions.push <- spider[spider$type=="push",c("friction")]
frictions.pull <- spider[spider$type=="pull",c("friction")]
ss.frictions.push <- sum( ( frictions.push - mean(frictions.push))^2)
ss.frictions.pull <- sum( ( frictions.pull - mean(frictions.pull))^2)
ss.type <- ss.frictions.pull + ss.frictions.push
F.val.type <- ss.mean - ss.type
F.val.type
# compare to the following equivalent result: sum of squared residuals differences between fitted values for the models ~type and ~1
fit.mean <- lm(friction ~ 1, data = spider)
fit.type <- lm(friction ~ type, data = spider)
sum(fit.mean$residuals^2) - sum(fit.type$residuals^2)

# exercise 5

N <- 40
p <- 4
group <- factor(rep(x = 1:p, each = N/p))
X <- model.matrix(~ group)

# first generate some random, null data, where the mean is the same for all groups:
Y <- rnorm(N,mean=42,7)

# the base model we will compare against is simply Y-hat = mean(Y), which we will call mu0, 
# and the initial sum of squares is the Y values minus mu0
m0 <- mean(Y)
initial.ss <- sum( (Y-m0)^2)

# compute fitted values for each group
s <- split(x = Y, f = group)

# define function to cmpute the Sum of Squared Residuals (SSR)
compute_SSR <- function(x) {
  R <- (x - mean(x)) # residuals
  SR  <- R^2 # squared residuals
  SSR <- sum(SR) # sum of squared residuals
  return(SSR)
}

after.group.ss <- sum(sapply(X = s, FUN = compute_SSR)) # use sapply to perform same function on each element of input X

# NOW the explanatory power of the 'group' variable is the initial sum of squares minus the residual sum of squares:
group.ss <- initial.ss - after.group.ss

# calculate means of groups.ss and after.group.ss
group.ms <- group.ss / (p - 1)
after.group.ms <- after.group.ss / (N - p)

#  F-value is simply the ratio of these mean sum of squares
f.value <- group.ms / after.group.ms
f.value

B <- 1000
set.seed(1)
fvals <- replicate(n = B, expr = {
  Y <- rnorm(N,mean=42,7)
  m0 <- mean(Y)
  initial.ss <- sum( (Y-m0)^2)
  s <- split(x = Y, f = group)
  after.group.ss <- sum(sapply(X = s, FUN = compute_SSR))
  group.ss <- initial.ss - after.group.ss
  group.ms <- group.ss / (p - 1)
  after.group.ms <- after.group.ss / (N - p)
  f.value <- group.ms / after.group.ms
  f.value
})
mypar(1)
hist(fvals, col = "grey", freq = FALSE, breaks = 50)
cat(mean(fvals))

hist(fvals, col = "grey", freq = FALSE, breaks = 50)
# overlay theoretical F-distribution with params: df1 = p-1, df2 = N - p
xs <- seq(from=0, to=6, length = 100)
lines(xs, df(x = xs, df1 = p-1, df2 = N-p), col = "red")
