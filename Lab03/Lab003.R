


#1A

mySeqs <- seq(0, 1, by=0.001) 
priorVals <- dexp(mySeqs, rate=5) / 0.9932621 
plot(mySeqs, priorVals, type="l", lwd=2, 
     main="(1A) Prior: truncated Exp(rate=5) on [0,1]", 
     xlab="x = P(head)", ylab="prior density")

#1B
rm(list=ls())

#Declare variables
numIterations <- 500000
sdProp <- 0.01
piOld <- 0.05

breaksHist <- 200
rateExp <- 5
expNormConst <- 0.9932621   

#Prior
target <- function(x){
  (dexp(x, rateExp) / expNormConst) * dbinom(14, 24, x)
}

#METROPOLIS
posteriorSamples <- numeric(numIterations)

for(i in 1:numIterations)
{
  pOld <- target(piOld)
  
  piNew <- piOld + rnorm(1, 0, sd = sdProp)
  if(piNew > 1) piNew <- 1
  if(piNew < 0) piNew <- 0
  
  pNew <- target(piNew)
  ratio <- pNew / pOld
  
  if(ratio > 1 || ratio >= runif(1))
    piOld <- piNew
  
  posteriorSamples[i] <- piOld
}


myHist <- hist(posteriorSamples, breaks=breaksHist, plot=FALSE)

xGrid <- myHist$mids

# 1) Histogram scaled to sum to 1 (matches lecture)
histY <- myHist$counts / length(posteriorSamples)

# 2) Numerical posterior on same xGrid, scaled to sum to 1
numPostY <- target(xGrid)
numPostY <- numPostY / sum(numPostY)

# 3) Beta posterior on same xGrid, scaled to sum to 1
# Exact Beta
betaPostY <- dbeta(xGrid, 40+14, 40+10)
betaPostY <- betaPostY / sum(betaPostY)


yMax <- max(c(histY, numPostY, betaPostY))

plot(xGrid, histY, type="l", lwd=2,
     main="Posterior: Metropolis Histogram vs Numerical vs Beta",
     xlab="p", ylab="Scaled probability / mass",
     ylim=c(0, yMax))

lines(xGrid, numPostY, col="blue", lwd=2)
lines(xGrid, betaPostY, col="red", lwd=2)

legend("topright",
       legend=c("Metropolis (hist counts/n)", "Grid exp prior * like)", "Beta posterior"),
       col=c("black","blue","red"),
       lwd=2)

#1C

