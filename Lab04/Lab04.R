
myT <- read.table("nc101_scaff_dataCounts.txt", header=TRUE, row.names=1)
plot(log10(myT$D2_01 + 1),
     log10(myT$D2_02 + 1),
     xlab="D2_01 (log10 counts)",
     ylab="D2_02 (log10 counts)",
     main="Biological Replicates: D2_01 vs D2_02",
     pch=16, cex=0.5, col=rgb(0,0,1,0.3))
abline(a=0, b=1, col="red")

geneMeans <- apply(myT, 1, mean)
geneVars  <- apply(myT, 1, var)

plot(log10(geneMeans + 1),
     log10(geneVars  + 1),
     xlab="log10(Mean)", ylab="log10(Variance)",
     main="Mean vs Variance (all genes)",
     pch=16, cex=0.5, col=rgb(0,0,0,0.3))
abline(a=0, b=1, col="red", lwd=2) 

gene1   <- myT[1, ]
totalD1 <- sum(myT$D2_01)
totalD2 <- sum(myT$D2_02)

ct <- matrix(
  c(gene1$D2_01,
    totalD1 - gene1$D2_01,
    gene1$D2_02,
    totalD2 - gene1$D2_02),
  nrow=2,
  dimnames=list(
    c("Assigned", "Not assigned"),
    c("D2_01", "D2_02")))

print(ct)
fisher.test(ct, alternative="two.sided")


totalD1 <- sum(myT$D2_01)
totalD2 <- sum(myT$D2_02)

fisher_p <- apply(myT, 1, function(row) {
  ct <- matrix(c(row["D2_01"],
                 totalD1 - row["D2_01"],
                 row["D2_02"],
                 totalD2 - row["D2_02"]), nrow=2)
  fisher.test(ct, alternative="two.sided")$p.value
})

hist(fisher_p, breaks=50, main="Fisher p-values (all genes)",
     xlab="p-value", col="steelblue")

# Filter low-abundance genes and redo
myT_filt <- myT[ (myT$D2_01 + myT$D2_02 > 50), ]
totalD1f <- sum(myT_filt$D2_01)
totalD2f <- sum(myT_filt$D2_02)

fisher_p_filt <- apply(myT_filt, 1, function(row) {
  ct <- matrix(c(row["D2_01"], totalD1f - row["D2_01"],
                 row["D2_02"], totalD2f - row["D2_02"]), nrow=2)
  fisher.test(ct, alternative="two.sided")$p.value
})
hist(fisher_p_filt, breaks=50, main="Fisher p-values (abundance > 50)",
     xlab="p-value", col="tomato")

totalD1 <- sum(myT$D2_01)
totalD2 <- sum(myT$D2_02)

poisson_p <- apply(myT, 1, function(row) {
  p_hat  <- row["D2_01"] / totalD1
  poisson.test(x=row["D2_02"], T=totalD2,
               r=p_hat, alternative="two.sided")$p.value
})

# log10-log10 comparison plot
plot(log10(fisher_p  + 1e-300),
     log10(poisson_p + 1e-300),
     xlab="log10(Fisher p)", ylab="log10(Poisson p)",
     main="Fisher vs Poisson p-values",
     pch=16, cex=0.4, col=rgb(0,0,0,0.3))
abline(a=0, b=1, col="red")