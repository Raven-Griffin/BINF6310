# grid approach to beta prior using a naive (brute force) numerical integration algorithm
rm(list=ls())

numBreaks=1000;
posteriorDist <- vector(length=numBreaks)
xVals <- seq(0,1,1/numBreaks);

i <- 1;
sum <- 0;
for( x in xVals )
{
  # our prior with 9 heads and 9 tails
  # our new data with 14 heads and 10 tails
  posteriorDist[i] <- (dexp(piOld, 5)/0.9932621) * dbinom( 14, 24, x)
  sum = sum + posteriorDist[i];
  i <- i + 1;	
}

plot( posteriorDist / sum ) 
lines( dbeta(xVals, 10+14, 10+10)/ sum(dbeta(xVals, 10+14, 10+10)), col="red")  

rm(list=ls())
piOld <- 0.05

numIterations <- 500000
posteiorDist <- vector()

for( i in 1:numIterations )
{
  # our prior with 9 heads and 9 tails
  # our new data with 14 heads and 10 tails
  pOld <- (dexp(piOld, 5)/0.9932621) * dbinom( 14, 24, piOld )
  
  piNew <- piOld + rnorm(1, 0, sd =0.01);
  
  if( piNew > 1) 
    piNew = 1;
  
  if( piNew < 0 ) 
    piNew =0;
  
  pNew <- (dexp(piNew, 5)/0.9932621) * dbinom(14, 24, piNew)
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) ) 
    piOld = piNew;
  
  posteiorDist[i] = piOld;	
  
  if( i %% 100 == 0 )
  {	
    myHist <- hist(posteiorDist,breaks=200,plot=FALSE)
    plot( myHist$mids, myHist$counts/i, main = paste("iteration", i)) 
    dbetasum = sum(dbeta(myHist$mids, 10+14, 10+10))
    lines( myHist$mids, dbeta(myHist$mids, 10+14, 10+10)/dbetasum,col="red") 	
    Sys.sleep(.1)
  }
}
