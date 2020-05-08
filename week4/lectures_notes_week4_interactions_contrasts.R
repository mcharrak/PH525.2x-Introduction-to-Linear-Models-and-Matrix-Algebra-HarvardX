# week 4 lecture notes: Interactions and Contrasts"

# lecture notes: Interactions and Contrasts I


url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv("spider_wolff_gorb_2013.csv", skip=1) # skip=1, allows to skip the first line of the CSV file


# recreate box plot from the paper
boxplot(spider$friction ~ spider$type * spider$leg,
        col = c('grey90', 'grey40'), las = 2,  # las argument makes labels perpendicular to each axis
        main = "Comparison of friction coefficients of different leg pairs")


# Linear model with one variable
spider.sub <- spider[spider$leg == "L1",] #subset the data, keep rows with L1 legs
fit <- lm(formula = friction ~ type, data = spider.sub)
summary(fit)
# extract the beat coefficients with coef() fct.
coefs <- coef(fit)
# split the data by groups/types
types <- spider.sub$type
s <- split(x = spider.sub$friction, f = types)

# beta0 is the average of the pull group
print(mean(s[["pull"]]))
print(coefs[1])

# beta1 is the difference between the push group and the pull group
print(mean(s[["push"]]) - mean(s[["pull"]]))
print(coefs[2])


# inspect model matrix which was used inside lm() function above
X <- model.matrix(object = friction ~ type, data = spider.sub)
colnames(X)
head(X)
library(rafalib)
library(rafalib)
imagemat(X, main = "Model matrix for linear model with ineractions")
# because the model matrix X has two columns, this graph has two columns; 
# first, the intercept term (all 1s i.e. black)
# second, the type column (50% push, 50% pull)


# examine the coefficients
# stripchart produces 1D scatterplots (or dot plots) of the data. Are good alternative to boxplots small sample size
stripchart(x = s, 
           vertical = TRUE, 
           pch = 1, # pch=1 gives emtpy circles
           method = "jitter", 
           las = 2, 
           xlim = c(0,3), 
           ylim = c(0,2))
# draw lines to represent intercept/average and difference between the two groups
a <- -0.25
lgth <- 0.1
library(RColorBrewer)
cols <- brewer.pal(n = 3, "Dark2")

arrows(x0 = 1+a, x1 = 1+a, y0 = 0, y1 = coefs[1], lwd = 3, col = cols[1], length = lgth)
abline(h = coefs[1], col = cols[1]) #draw horizontal line

arrows(x0 = 2+a, x1 = 2+a, y0 = coefs[1], y1 = coefs[1]+coefs[2], lwd = 3, col = cols[2], length = lgth)
abline(h = coefs[1]+coefs[2], col = cols[2]) #draw horizontal line

legend("right", names(coefs), fill = cols, cex = 0.75, bg = "white")


# lecture notes: Interactions and Contrasts II


# a linear model with two variables
X <- model.matrix(object = ~ type + leg, data = spider)
colnames(X)
head(X)
imagemat(x = X, main = "Model matrix for linear model with 2 factors") #factor here means variables (leg-type and push-type)
fit2 <- lm(data = spider, formula = friction ~ type +leg)
summary(fit2)
coefs <- coef(fit2)


# examining the coefficients
spider$group <- factor(paste0(spider$leg, spider$type))
# split divides the data in the vector x into the groups defined by f; split() returns a list
stripchart(x = split(x = spider$friction, f = spider$group),
           vertical = TRUE, pch = 1, method = "jitter", las = 2,
           xlim = c(0,11), ylim = c(0,2))
cols <- brewer.pal(n = 5, name = "Dark2") # creates nice looking set of colors
# draw arrows to visualize different references and effects
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")


# contrasting the coefficients
### COMPARING TWO GROUPS, WHERE NONE OF THEM IS THE REFERENCE LEVEL => USING A CONTRAST (COMPARISON OF COEFFICIENTS) ###
library(contrast)
L3vsL2 <- contrast(fit = fit2, list(leg = "L3", type = "pull"), list(leg = "L2", type = "pull"))
L3vsL2
coefs[4] - coefs[3] 
# coefs[4] refers to L3-L1; L1 being the reference
# coefs[3] refers to L2-L1; L1 being the reference
# THUS: coefs[4] - coefs[3] refers to L3-L1-(L2-L1) = L3-L2 => the difference between L3 and L2
C <- L3vsL2$X
C %*% coefs

# lets use push instead of pull and compute the L3-L2 coefficient again
L3vsL2.equiv <- contrast(fit = fit2, list(leg = "L3", type = "push"), list(leg = "L2", type = "push"))
L3vsL2.equiv$X
# OBSERVATION: SAME RESULT FOR L3 vs L2 comparison; because we are controlling for the push vs. pull difference

# lecture notes: Interactions and Contrasts III


library(rafalib)
# create model with interaction term (here between type and leg
X <- model.matrix(object = ~ type + leg + type:leg, data = spider)
# or alternatively using *
X <- model.matrix(object = ~ type*leg, data = spider)
colnames(X)
head(X)
imagemat(X, main = "Model matrix for linear model with interations")
# we see that we have 3 more columns: 6,7,8
# column 6 is the a product of: column2*column3 (overlapping both columns only one single black bar remains around 100 on the y axis)
# column 7 is the a product of: column2*column4
# column 8 is the a product of: column2*column5


fit3 <- lm(friction ~ type + leg + type:leg, data = spider)
summary(fit3)
coefs <- coef(fit3)


library(rafalib)
# examining the coefficients
spider$group <- factor(paste0(spider$leg, spider$type))
stripchart(split(spider$friction, spider$group),
           vertical = TRUE, pch = 1, method = "jitter",  las = 2, # jitter causes points to not be stacked or overplotted but plotted with a littl bitof noise compare to other options such as 'stack' and 'overplot'
           xlim = c(0,11), ylim = c(0,2))
cols <- brewer.pal(8,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
#now the interactions:
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(4+a,coefs[1]+coefs[2]+coefs[3],4+a,coefs[1]+coefs[2]+coefs[3]+coefs[6],lwd=3,col=cols[6],length=lgth)
segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(6+a,coefs[1]+coefs[4]+coefs[2],6+a,coefs[1]+coefs[4]+coefs[2]+coefs[7],lwd=3,col=cols[7],length=lgth)
segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
arrows(8+a,coefs[1]+coefs[5]+coefs[2],8+a,coefs[1]+coefs[5]+coefs[2]+coefs[8],lwd=3,col=cols[8],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")


# lecture notes: Interactions and Contrasts IV

library(contrast)
L2push.vs.pull <- contrast(fit3,
                           list(leg="L2", type="push"),
                           list(leg="L2", type="pull"))
L2push.vs.pull
# get the contrast vector C
C <- L2push.vs.pull$X
# compare the contrast parameter; here -0.618 to the summation of 2 effects 
# 1) L1 push vs pull => orange arrow
# 2) L2 interaction term with typepush => yellow line 
length_orange_arrow <- coefs[2]
length_yellow_arrow <- coefs[6]
# compare both:
cat(length_yellow_arrow + length_orange_arrow)
cat(L2push.vs.pull$Contrast)
# OBSERVATION: EQUAL RESULT


# contrast for differences of differences: e.g. to answer the question: Is the Push vs. Pull difference/effect is different for different legs
# example: we want to compare the effect of push VS pull between L2 and L3 legs -> in this case L1 is not anymore our reference
library(multcomp)
C <- matrix(data = c(0,0,0,0,0,-1,1,0), nrow = 1)
# inspectting coefs we see: typepush:legL2 is the 6th coefficient and typepush:legL3 is the 7th coefficient
# now compute the contrast for push vs pull, L3 vs L2
L3vsL2interaction <- glht(linfct = C, model = fit3)
summary(L3vsL2interaction)
L3vsL2interaction
cat(coefs[7] - coefs[6])


# answering the question: is the push vs pull difference/effect different across all the different legs? => anova() using an F-test
anova(fit3)
# ALL F-values are large (respectively their 'p-values' are very small) ==> means that the differences we see across leg pairs in the push vs. pull effect are more than we would expect to happen by pure chance
# => CONCLUSION: The push vs. pull effect is signfificantly different across leg pairs when looking at all 4 leg pairs together

# lecture notes: Interactions and Contrasts V

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv("spider_wolff_gorb_2013.csv", skip=1) # skip=1, allows to skip the first line of the CSV file


library(rafalib)
# a different specification for the same model as above with interactions
spider$group <- factor(paste0(spider$leg, spider$type))
X <- model.matrix(~ 0 + group, data = spider) # 0 says: we do not want an intercept in the model matrix X
colnames(X)
head(X)
imagemat(x = X, main = "Model matrix for linear model with interactions")
# we have 8 different combinations for {L1,L2,L3,L4} and {push,pull}. Thus, 8 columns in total.

fit4 <- lm(friction ~ 0 + group, data = spider)
summary(fit4)
coefs <- coef(fit4)


stripchart(x = split(x = spider$friction, f = spider$group),
           vertical = T, pch = 1, method = "jitter", las = 2,
           xlim = c(0,11), ylim = c(0,2))
library(RColorBrewer)
cols <- brewer.pal(n = 8, name = "Dark2")
a <- -0.25
lgth <- 0.1
abline(h=0)
for (i in 1:8) {
  arrows(i+a, 0, i+a, coefs[i],lwd=3,col=cols[i],length = lgth)
}
legend("right", names(coefs), fill=cols, cex=.75,bg="white")


#now let's make contrasts using the contrast package
library(contrast)
# example: if we want to contrast push vs pull in the L2 group:
groupL2push.vs.pull <- contrast(fit = fit4,
                                list(group="L2push"),
                                list(group="L2pull"))

groupL2push.vs.pull
coefs[4] - coefs[3] # for "groupL2push" - "groupL2pull" 


library(multcomp)
# contrasting the differenes of differences when ther is not an intercept
# example contrasting difference between L3 and L2 - push vs pull - effects
# manually done as follows: (coefs[6] - coefs[5]) - (coefs[4] - coefs[3]) - this is the difference of two differences
# which equals
cat(coefs[3] - coefs[4] - coefs[5] + coefs[6]) # from which we read the contrast vector C as: [0,0,  1,-1,  -1,1,  0,0]
C <- matrix(c(0,0,   1,-1,   -1,1,   0,0), nrow = 1)
groupL3vsL2interaction <- glht(linfct = C, model = fit4) # using generalized linear hypothesis function 
groupL3vsL2interaction