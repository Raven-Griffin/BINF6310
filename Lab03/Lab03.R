rm(list=ls())
set.seed(1)


# (1A) Plot the prior

mySeqs <- seq(0, 1, by=0.001)
priorVals <- dexp(mySeqs, rate=5) / 0.9932621

plot(mySeqs, priorVals, type="l", lwd=2,
     main="(1A) Prior: truncated Exp(rate=5) on [0,1]",
     xlab="x = P(head)", ylab="prior density")


# 1B
piOld <- 0.5

numIterations <- 60000
posteriorDist <- vector(length=numIterations)

for( i in 1:numIterations )
{
  # exp prior * likelihood
  pOld <- (dexp(piOld, 5)/0.9932621) * dbinom(14, 24, piOld)
  
  piNew <- piOld + rnorm(1, 0, sd=0.03)
  
  if( piNew > 1 ) piNew = 1
  if( piNew < 0 ) piNew = 0
  
  pNew <- (dexp(piNew, 5)/0.9932621) * dbinom(14, 24, piNew)
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) )
    piOld = piNew
  
  posteriorDist[i] = piOld
}

myHist <- hist(posteriorDist, breaks=200, plot=FALSE)

# Metropolis curve (scaled like lecture)
plot(myHist$mids, myHist$counts/numIterations,
     type="l", lwd=2, col="blue",
     xlab="x = P(head)", ylab="posterior (scaled)",
     main="(1B) 14H,10T: Metropolis+Exp vs Grid+Exp vs Beta(40,40)")


# Grid approach 

numBreaks <- 1000
posteriorGrid <- vector(length=numBreaks+1)
xVals <- seq(0, 1, 1/numBreaks)

j <- 1
sumVal <- 0

for( x in xVals )
{
  posteriorGrid[j] <- (dexp(x,5)/0.9932621) * dbinom(14, 24, x)
  sumVal <- sumVal + posteriorGrid[j]
  j <- j + 1
}

# Grid curve overlay (normalized by sumVal like lecture)
lines(xVals, posteriorGrid/sumVal, lwd=2, col="black")


# Exact Beta(40,40) posterior

betaExact <- dbeta(xVals, 40+14, 40+10)
betaExact <- betaExact / sum(betaExact)

lines(xVals, betaExact, lwd=2, col="red")


# (1C) 583 heads, 417 tails

rm(list=ls())
set.seed(1)


# Metropolis (Exp prior)

piOld <- 0.5

numIterations <- 120000
someDistribution <- vector(length=numIterations)

for( i in 1:numIterations )
{
  pOld <- (dexp(piOld,5)/0.9932621) * dbinom(583, 1000, piOld)
  
  piNew <- piOld + rnorm(1, 0, sd=0.004)
  
  if( piNew > 1 ) piNew = 1
  if( piNew < 0 ) piNew = 0
  
  pNew <- (dexp(piNew,5)/0.9932621) * dbinom(583, 1000, piNew)
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) )
    piOld = piNew
  
  someDistribution[i] = piOld
}

myHist <- hist(someDistribution, breaks=250, plot=FALSE)

# Optional zoom window (recommended for 1C so you can see detail)
# data fraction is 583/1000 = 0.583
plot(myHist$mids,
     myHist$counts/length(someDistribution),
     type="l", lwd=2, col="blue",
     main="(1C) 583H,417T: Metropolis+Exp vs Grid+Exp vs Beta(40,40)",
     xlab="x = P(head)", ylab="scaled density",
     xlim=c(0.54, 0.62))   # <- remove if you want full 0..1

############################
# Grid curve (Exp prior) on SAME x-axis

gridVals <- (dexp(myHist$mids,5)/0.9932621) * dbinom(583, 1000, myHist$mids)
aSum <- sum(gridVals)
lines(myHist$mids, gridVals/aSum, col="black", lwd=2)


# Exact posterior from Beta(40,40) prior on SAME x-axis

betaVals <- dbeta(myHist$mids, 40+583, 40+417)
bSum <- sum(betaVals)
lines(myHist$mids, betaVals/bSum, col="red", lwd=2)

legend("topright",
       inset=c(-0.35,0),   # moves legend outside right
       legend=c("Metropolis + Exp prior",
                "Grid + Exp prior",
                "Exact posterior from Beta(40,40) prior"),
       col=c("blue","black","red"),
       lwd=2, bty="n")
