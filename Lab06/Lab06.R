# Lab 6 - RNA-seq ANOVA Analysis
# E. Coli genes from mice (longitudinal RNAseq data)

# ── Load & normalize ──────────────────────────────────────────────────────────
myT <- read.table("/mnt/user-data/uploads/nc101_scaff_dataCounts.txt",
                  sep = "\t", header = TRUE, row.names = 1)

# Remove rare genes (median count <= 5)
myT <- myT[apply(myT, 1, median) > 5, ]

# Normalize each column to relative abundance
myTNorm <- myT
for (i in 1:ncol(myT)) {
  colSum      <- sum(myT[, i])
  myTNorm[, i] <- myTNorm[, i] / colSum
}

cat("Genes after filtering:", nrow(myTNorm), "\n")
cat("Columns:", colnames(myTNorm), "\n")

# ── Group / time definitions ──────────────────────────────────────────────────
# Cols 1-3: Day 2 | Cols 4-6: Week 12 (≈86 days) | Cols 7-11: Week 18/20 (≈128 days)
cats <- factor(c(rep("day2", 3), rep("week12", 3), rep("week20", 5)))
days <- c(rep(2, 3), rep(86, 3), rep(128, 5))   # numeric time in days

# ── Storage vectors ───────────────────────────────────────────────────────────
pValuesOneWayAnova <- vector()
pValuesRegression  <- vector()
pValueModelDiff    <- vector()
index              <- vector()

# ── Main loop ─────────────────────────────────────────────────────────────────
for (i in 1:nrow(myTNorm)) {
  index[i] <- i
  myData    <- as.numeric(myTNorm[i, ])
  
  # (A) One-way ANOVA: three-category factor model
  lmA <- lm(myData ~ cats)
  pValuesOneWayAnova[i] <- anova(lmA)$"Pr(>F)"[1]
  
  # (B) Linear regression over numeric time
  lmB <- lm(myData ~ days)
  pValuesRegression[i] <- anova(lmB)$"Pr(>F)"[1]
  
  # (C) Compare models A (3-param) vs B (2-param) via F-test
  # Model A: intercept + 2 group dummies = 3 params (df_resid = n - 3 = 8)
  # Model B: intercept + slope           = 2 params (df_resid = n - 2 = 9)
  rssA <- sum(residuals(lmA)^2)
  rssB <- sum(residuals(lmB)^2)
  n    <- length(myData)
  
  df1  <- 1          # extra parameter in model A vs B
  df2  <- n - 3      # residual df of the fuller model (A)
  
  Fstat <- ((rssB - rssA) / df1) / (rssA / df2)
  pValueModelDiff[i] <- pf(Fstat, df1, df2, lower.tail = FALSE)
}

# ── Build data frame & BH correction ─────────────────────────────────────────
myFrame <- data.frame(index, pValuesOneWayAnova, pValuesRegression, pValueModelDiff)

bh_A    <- p.adjust(pValuesOneWayAnova, method = "BH")
bh_B    <- p.adjust(pValuesRegression,  method = "BH")
bh_C    <- p.adjust(pValueModelDiff,    method = "BH")

sigA <- sum(bh_A < 0.05, na.rm = TRUE)
sigB <- sum(bh_B < 0.05, na.rm = TRUE)
sigC <- sum(bh_C < 0.05, na.rm = TRUE)

cat("\n=== Significant genes at BH FDR < 0.05 ===\n")
cat("(A) One-way ANOVA (3 categories):        ", sigA, "\n")
cat("(B) Linear regression over time:         ", sigB, "\n")
cat("(C) Model comparison (A vs B):           ", sigC, "\n")

# ── Plots ─────────────────────────────────────────────────────────────────────
png("/home/claude/lab6_plots.png", width = 1400, height = 1800, res = 130)
par(mfrow = c(3, 2), mar = c(5, 5, 4, 2))

# --- Histogram A
hist(pValuesOneWayAnova, breaks = 40, col = "steelblue", border = "white",
     main = "(A) One-Way ANOVA p-values\n(3 Categories: Day2, Week12, Week20)",
     xlab = "p-value", ylab = "Frequency")
legend("topright", legend = paste("BH sig. genes:", sigA), bty = "n", cex = 1.1)

# --- Histogram B
hist(pValuesRegression, breaks = 40, col = "darkorange", border = "white",
     main = "(B) Linear Regression p-values\n(Continuous Time in Days)",
     xlab = "p-value", ylab = "Frequency")
legend("topright", legend = paste("BH sig. genes:", sigB), bty = "n", cex = 1.1)

# --- Histogram C
hist(pValueModelDiff, breaks = 40, col = "forestgreen", border = "white",
     main = "(C) Model Comparison p-values\n(3-Parameter vs 2-Parameter Model)",
     xlab = "p-value", ylab = "Frequency")
legend("topright", legend = paste("BH sig. genes:", sigC), bty = "n", cex = 1.1)

# ── Top-gene plots ────────────────────────────────────────────────────────────
# Order frames
frameA <- myFrame[order(myFrame$pValuesOneWayAnova), ]
frameB <- myFrame[order(myFrame$pValuesRegression),  ]
frameC <- myFrame[order(myFrame$pValueModelDiff),     ]

topNameA <- rownames(myTNorm)[frameA$index[1]]
topNameB <- rownames(myTNorm)[frameB$index[1]]
topNameC <- rownames(myTNorm)[frameC$index[1]]

# Plot A top gene – boxplot by category
boxplot(as.numeric(myTNorm[frameA$index[1], ]) ~ cats,
        col    = c("steelblue", "coral", "gold"),
        main   = paste0("(A) Top Gene: ", topNameA,
                        "\np = ", formatC(frameA$pValuesOneWayAnova[1], digits = 3, format = "e")),
        xlab   = "Time Point",
        ylab   = "Relative Abundance",
        names  = c("Day 2", "Week 12", "Week 20"))

# Plot B top gene – scatter + regression line
topDataB <- as.numeric(myTNorm[frameB$index[1], ])
lmTop    <- lm(topDataB ~ days)

plot(days, topDataB,
     pch  = 19, col = "darkorange", cex = 1.4,
     main = paste0("(B) Top Gene: ", topNameB,
                   "\np = ", formatC(frameB$pValuesRegression[1], digits = 3, format = "e")),
     xlab = "Time (days)",
     ylab = "Relative Abundance")
abline(lmTop, col = "red", lwd = 2)
legend("topleft", legend = "Regression line", col = "red", lwd = 2, bty = "n")

# Plot C top gene – boxplot by category
boxplot(as.numeric(myTNorm[frameC$index[1], ]) ~ cats,
        col    = c("steelblue", "coral", "gold"),
        main   = paste0("(C) Top Gene: ", topNameC,
                        "\np = ", formatC(frameC$pValueModelDiff[1], digits = 3, format = "e")),
        xlab   = "Time Point",
        ylab   = "Relative Abundance",
        names  = c("Day 2", "Week 12", "Week 20"))

dev.off()
cat("\nPlot saved to lab6_plots.png\n")