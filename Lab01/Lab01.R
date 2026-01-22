#Question 1 - What is the mean and variance for the loaded dice?

# The mean is 4.5
# The Variance is 3.25

#Question 2 - Make a function in R that “rolls” this dice; return a vector containing the rolls.

#make die
rollLoadedDie <- function(x) {
  probs <- c(rep(0.1, 5), 0.5)
  sample(1:6, size = x, replace = TRUE, prob = probs)
}

#roll die
myRolls <- rollLoadedDie(10000)
myRolls


#Question 3 - Make a histogram of some large number of rolls.  Do the rolls of the loaded die approximate a uniform distribution?

#roll die
largeRolls <- rollLoadedDie(100000)

#create histogram
hist(largeRolls)

mean(largeRolls)
var(largeRolls)

#This is not a uniform distribution

#Question 4 -Modify the code on Slide #57 of lecture #2 so that the means vs. trial size plots are from the loaded die.  
#Generate these plots a few times.  
#How many rolls appear to be necessary to get convergence on the expected values for the mean and variance?

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

#Log10 of trialSize 1000 is necessary to get convergence on the expected values for the mean and variance
