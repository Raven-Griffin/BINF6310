


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


rm(list=ls())

numIterations <- 500000
sdProp        <- 0.005      
piOld         <- 0.55       
breaksHist    <- 300
rateExp       <- 5
expNormConst  <- 0.9932621

heads <- 583
tails <- 417
total <- heads + tails


target_1c <- function(x) {
  (dexp(x, rateExp) / expNormConst) * dbinom(heads, total, x)
}

# --- Metropolis ---
posteriorSamples_1c <- numeric(numIterations)

for (i in 1:numIterations) {
  pOld  <- target_1c(piOld)
  piNew <- piOld + rnorm(1, 0, sd = sdProp)
  if (piNew > 1) piNew <- 1
  if (piNew < 0) piNew <- 0
  pNew  <- target_1c(piNew)
  ratio <- pNew / pOld
  if (ratio > 1 || ratio >= runif(1)) piOld <- piNew
  posteriorSamples_1c[i] <- piOld
}

myHist_1c <- hist(posteriorSamples_1c, breaks = breaksHist, plot = FALSE)
xGrid_1c  <- myHist_1c$mids

# 1) Metropolis histogram scaled to sum to 1
histY_1c <- myHist_1c$counts / length(posteriorSamples_1c)

# 2) Grid approximation (exp prior * likelihood), scaled to sum to 1
numPostY_1c <- target_1c(xGrid_1c)
numPostY_1c <- numPostY_1c / sum(numPostY_1c)

# 3) Exact analytical: Beta(40 + 583, 40 + 417) = Beta(623, 457)
betaPostY_1c <- dbeta(xGrid_1c, 40 + heads, 40 + tails)
betaPostY_1c <- betaPostY_1c / sum(betaPostY_1c)

yMax_1c <- max(c(histY_1c, numPostY_1c, betaPostY_1c))

# zoom x-axis in to where the action is
xLo <- max(0,   min(posteriorSamples_1c) - 0.02)
xHi <- min(1,   max(posteriorSamples_1c) + 0.02)

plot(xGrid_1c, histY_1c, type = "l", lwd = 2,
     main = "(1C) Posterior: 583 heads / 417 tails\nMetropolis vs Grid (exp prior) vs Beta(40,40) prior",
     xlab = "p = P(head)", ylab = "Scaled probability mass",
     xlim = c(xLo, xHi),
     ylim = c(0, yMax_1c * 1.1))

lines(xGrid_1c, numPostY_1c,  col = "blue", lwd = 2)
lines(xGrid_1c, betaPostY_1c, col = "red",  lwd = 2)

legend("topright",
       legend = c("Metropolis (exp prior)", "Grid (exp prior)", "Beta(40,40) prior"),
       col    = c("black", "blue", "red"),
       lwd    = 2)


