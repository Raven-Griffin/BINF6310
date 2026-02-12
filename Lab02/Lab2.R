
rm(list=ls())

data<-c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)

# Prior from lecture: P(Loaded)=0.01, P(Fair)=0.99
pL <- 0.01
pF <- 0.99

# Likelihoods
p_y_given_F <- function(y) 1/6
p_y_given_L <- function(y) ifelse(y == 6, 0.5, 0.1)  # 6:0.5 ; 1-5:0.1

postL <- numeric(length(data))

for (t in seq_along(data)) {
  y <- data[t]
  num <- pL * p_y_given_L(y)
  den <- num + pF * p_y_given_F(y)
  pL <- num / den
  pF <- 1 - pL
  postL[t] <- pL
}

plot(seq_along(data), postL, type="l",
     xlab="Number of rolls",
     ylab="Posterior P",
     main="Posterior probability ")
points(seq_along(data), postL, pch=16, cex=0.6)

postL[length(postL)]