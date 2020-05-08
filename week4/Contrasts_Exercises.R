# Contrasts Exercises

species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))

# create model matrix with formula: ~ species + condition
model.matrix(~ species + condition)

# exercise 1
library(contrast)
# species=B and condition=control:   [1, 1, 0] => cf. model matrix above
# species=A and condition=treatment: [1, 0, 1] => cf. model matrix above
# answer [1,1,0] - [1,1,-1] = [0,1,-1]

### ALTERNATIVELY WITH COMPUTATION ###

# generate random y data for 4 samples
y <- rnorm(4)
# fit model
fit <- lm(y ~ species + condition)
ControlB_vs_TreatedA <- contrast(fit, 
                                 list(species="B",condition="control"), 
                                 list(species="A",condition="treated"))
# answer
cat(ControlB_vs_TreatedA$X)

# exercise 2

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

# build model with two variables
fitTL <- lm(data = spider, formula = friction ~ type + leg)
L4vsL2 <- contrast(fit = fitTL, 
                   list(leg='L4', type = "pull"),
                   list(leg='L2', type = "pull"))
L4vsL2

# exercise 3

# model matrix
X <- model.matrix(~ type + leg, data=spider)
# sigma matrix
Sigma <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X)
# contrast matrix
C <- matrix(c(0,0,-1,0,1),1,5)

# solution
cov_L4_L2 <- Sigma['legL4', 'legL2']
cov_L4_L2

# compute manually to confirm result is correct:

Y <- spider$friction
X <- model.matrix(~ type + leg, data=spider)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Y

cat(sqrt(C %*% Sigma %*% t(C)))
contrast(fit = fitTL, 
             list(type='pull', leg='L4'),
             list(type='pull', leg='L2'))$SE
