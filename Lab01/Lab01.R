#Question 1
# The mean is 4.5
# The Variance is 3.25

#Question 2

#make die
rollLoadedDie <- function(x) {
  probs <- c(rep(0.1, 5), 0.5)
  sample(1:6, size = x, replace = TRUE, prob = probs)
}

#roll die
myRolls <- rollLoadedDie(10000)
myRolls


#Question 3

#roll die
largeRolls <- rollLoadedDie(100000)

#create histogram
hist(largeRolls)

mean(largeRolls)
var(largeRolls)

#This is not a uniform distribution

#Question 4
rm(list=ls())
trialSizes <- c(10, 15, 20,25, 30, 40,50,100,200,300,400,500,1000,2000, 3000, 4000, 5000, 10000, 20000, 30000, 100000)
means<- vector(mode="double", length =length(trialSizes))
variances <- vector(mode = "double", length = length(trialSizes))

for (i in 1:length(trialSizes)){
  rolls <- vector(length=trialSizes[i], mode="double")
  
  for(j in 1:trialSizes[i]){
    #added probalities
    probs <- c(rep(0.1, 5), 0.5)
    rolls[j] <- sample(1:6,1,replace = TRUE, prob = probs)
    
  }
  means[i] <- mean(rolls);
  variances[i] <- var(rolls)
}

plot(log10(trialSizes), means)
#changed to correct mean
lines(log10(trialSizes), rep(4.5, length(trialSizes)))

plot(log10(trialSizes), variances)
#changed to correct variance
lines(log10(trialSizes), rep(3.25, length(trialSizes)))
