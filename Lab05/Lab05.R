
cancer <- read.table("cancerRisk.txt", header=TRUE, sep="\t")

plot(log10(cancer$CumulativeCellDivisions),
     log10(cancer$Lifetime_cancer_risk),
     xlab="log10(Cumulative Cell Divisions)",
     ylab="log10(Lifetime Cancer Risk)",
     main="Cancer Risk vs. Cell Divisions",
     pch=16, col="steelblue")

myLm <- lm(log10(Lifetime_cancer_risk) ~ log10(CumulativeCellDivisions),
           data=cancer)

abline(myLm, col="red", lwd=2)

summary(myLm)

# Pull out specific values:
r2  <- summary(myLm)$r.squared
pval <- summary(myLm)$coefficients[2, 4]  # slope p-value
cat("R-squared:", r2, "\n")
cat("p-value for slope:", pval, "\n")


# Plot residuals — look for constant variance and normality
par(mfrow=c(2,2))
plot(myLm)
par(mfrow=c(1,1))

# Or check individually:
hist(residuals(myLm), main="Residuals", xlab="Residual")
plot(fitted(myLm), residuals(myLm),
     xlab="Fitted", ylab="Residuals")
abline(h=0, col="red")


caseCtrl <- read.table("caseControlData.txt",
                       header=TRUE, sep="\t", row.names=1)
bmiData  <- read.table("BMI_Data.txt",
                       header=TRUE, sep="\t")

# Fix sample IDs to match BMI file
sampleIDs <- rownames(caseCtrl)
keys <- sapply(sampleIDs, function(sampleID) {
  key <- sub("case",    "", sampleID)
  key <- sub("control", "", key)
  key <- strsplit(key, "_")[[1]][1]
  key
})


# Match subjects between the two tables
matchedBMI <- bmiData$BMI[ match(keys, bmiData$sample) ]

# Keep only rows with valid BMI
validIdx <- !is.na(matchedBMI)
caseCtrl  <- caseCtrl[validIdx, ]
bmiVec    <- matchedBMI[validIdx]

# p-value for each OTU (column)
pvals <- apply(caseCtrl, 2, function(otuCol) {
  myLm <- lm(bmiVec ~ otuCol)
  anova(myLm)$"Pr(>F)"[1]
})

hist(pvals, breaks=50,
     main="BMI vs OTU p-values",
     xlab="p-value", col="steelblue")

# Benjamini-Hochberg FDR correction
adjPvals <- p.adjust(pvals, method="BH")

# How many OTUs significant at 10% FDR?
sigOTUs <- sum(adjPvals < 0.10)
cat("Significant OTUs at 10% FDR:", sigOTUs, "\n")

# Show top hits
head(sort(adjPvals))


matchedBMI <- bmiData$bmi[ match(keys, bmiData$studyid) ]
validIdx   <- !is.na(matchedBMI)
caseCtrl   <- caseCtrl[validIdx, ]
bmiVec     <- matchedBMI[validIdx]

pvals <- apply(caseCtrl, 2, function(otuCol) {
  myLm <- lm(bmiVec ~ otuCol)
  anova(myLm)$"Pr(>F)"[1]
})
cat("=== Question 1C ===\n")
cat("p-value for slope:", pval, "\n")
cat("R-squared:", r2, "\n")

cat("\n=== Question 1D ===\n")
cat("Min residual:", min(residuals(myLm)), "\n")
cat("Max residual:", max(residuals(myLm)), "\n")
cat("Mean residual:", mean(residuals(myLm)), "\n")
