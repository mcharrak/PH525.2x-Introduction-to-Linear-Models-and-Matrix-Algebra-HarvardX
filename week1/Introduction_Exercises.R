## week 1: Introduction exercises

        ################################################################################################################

## If you haven't done so already, install the library UsingR
#install.packages("UsingR")
library(UsingR)
data('father.son', package = 'UsingR')

## exercise 1

avg_sheight_ex1 <- mean(father.son$sheight)
print(avg_sheight_ex1)

## exercise 2
library(dplyr)

## add new fheight variable as integer
father.son$fheight_int <- round(father.son$fheight)
sheight_ex2 <- father.son %>% filter(fheight_int == 71) %>% select(sheight) %>% unlist() #unlist to convert from data.frame to numeric named vector
avg_sheight_ex2 <- mean(x = sheight_ex2)
print(avg_sheight_ex2)

## exercise 3

# answer: Y = a + b^t + e
# explanation: because in every other case we can write the model as a linear combination of parameters and known covariates. However, 
# b^t is not a linear combination of b and t.

## exercise 4

# answer: 'Between-individual variability: people of the same weight vary in their height.'
# explanation: Remember that the model describes Y (outcome) across individuals and we fixed x. Therfore, people of the same height can
# vary greatly in other aspects of their physiology: e.g. a) different bone density or b) differing amounts of muscles and fat.