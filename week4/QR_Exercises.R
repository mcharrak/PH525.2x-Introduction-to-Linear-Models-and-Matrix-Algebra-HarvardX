# QR Exercises

# load spider data 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv("spider_wolff_gorb_2013.csv", skip=1) # skip=1, allows to skip the first line of the CSV file

fit <- lm(friction ~ type + leg, data = spider)
# get its beta solution from the fit object
betahat <- coef(fit)

# construct X and Y matrices
Y <- matrix(spider$friction, ncol = 1)
X <- model.matrix(~ type + leg, data = spider)

#ex1
QR <- qr(X)
Q <- qr.Q(QR)
R <- qr.R(QR)

#solution
cat(Q[1,1])

#ex2

#solution
cat(R[1,1])

#ex3

#solution
cat(crossprod(Q,Y)[1,1])

# compute betahat with QR method manually using: beta = R^(-1)*(Q'Y)
betahatQR <- backsolve(r = R, x = crossprod(Q,Y))

cat(betahat)
cat(betahatQR)