
# Lab02 — Bayesian Inference
#Raven Griffin

# PROBLEM 1 — The Occasionally Dishonest Casino



rolls <- c(2,3,2,6,3,5,6,2,3,2,6,3,5,6,2,3,2,6,6,5,6,6,6,4,6,3,3,3,6,6,5,6,6)

# Likelihood of a single roll under each hypothesis (Lecture 5, slide 67)
likelihood_loaded <- function(x) ifelse(x == 6, 1/2, 1/10)
likelihood_fair   <- function(x) 1/6


# Each posterior becomes the prior for the next roll
prior_loaded <- 0.01
posterior    <- numeric(length(rolls))

p_loaded <- prior_loaded
for (i in seq_along(rolls)) {

  p_loaded_new <- p_loaded       * likelihood_loaded(rolls[i])
  p_fair_new   <- (1 - p_loaded) * likelihood_fair(rolls[i])

  p_loaded <- p_loaded_new / (p_loaded_new + p_fair_new)
  posterior[i] <- p_loaded
}

# Plot it 
png("/mnt/user-data/outputs/problem1_posterior.png", width = 800, height = 500)
plot(seq_along(rolls), posterior,
     type = "b", pch = 16, col = "steelblue",
     xlab = "Number of rolls observed",
     ylab = "P(loaded | rolls)",
     main = "Posterior probability of a loaded die after each roll\n(Dishonest Casino — Lecture 5)",
     ylim = c(0, 1))
abline(h = 0.999,   col = "red",    lty = 2, lwd = 1.5)
abline(h = 0.99999, col = "orange", lty = 3, lwd = 1.5)
legend("topleft",
       legend = c("Posterior P(loaded)", "0.999 threshold", "0.99999 threshold"),
       col    = c("steelblue", "red", "orange"),
       lty    = c(1, 2, 3), pch = c(16, NA, NA))
dev.off()

cat("Problem 1 Results\n")
cat("Posterior P(loaded | data) after each roll:\n")
print(round(posterior, 6))

# Also show for the observed sequence
first_999   <- which(posterior >= 0.999)[1]
first_99999 <- which(posterior >= 0.99999)[1]
cat(sprintf("\nIn the given roll sequence:\n"))
cat(sprintf("  99.9%%   certainty first reached at roll #%s\n",
            ifelse(is.na(first_999),   "never", first_999)))
cat(sprintf("  99.999%% certainty first reached at roll #%s\n",
            ifelse(is.na(first_99999), "never", first_99999)))
cat(sprintf("  Final posterior: %.6f\n", tail(posterior, 1)))

# Part 2: How many rolls on average to reach 99.999% certainty?

set.seed(42)

sim_rolls_needed <- function(n_sim = 10000, target = 0.99999) {
  counts <- integer(n_sim)
  for (s in 1:n_sim) {
    p <- 0.01    
    for (k in 1:2000) {
      
      roll     <- sample(1:6, 1, prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
      l_loaded <- ifelse(roll == 6, 1/2, 1/10)
      l_fair   <- 1/6
      p <- (p * l_loaded) / (p * l_loaded + (1 - p) * l_fair)
      if (p >= target) { counts[s] <- k; break }
    }
    if (counts[s] == 0L) counts[s] <- 2000L   # cap if not reached
  }
  counts
}

counts <- sim_rolls_needed()
cat("\nPart 2 — Simulation: rolls needed to reach 99.999% certainty (loaded die):\n")
cat(sprintf("  Mean:   %.1f rolls\n", mean(counts)))
cat(sprintf("  Median: %.0f rolls\n", median(counts)))
cat(sprintf("  90th percentile: %.0f rolls\n", quantile(counts, 0.90)))



# PROBLEM 2 — Hospital Diagnostic Test


cat("\n\n Problem 2 Results\n")

prevalence    <- 0.001
sensitivity   <- 0.91          # P(+|disease)
specificity   <- 0.84          # P(-|no disease)
fp_rate       <- 1 - specificity   # P(+|no disease) = 0.16
fn_rate       <- 1 - sensitivity   # P(-|disease)    = 0.09
target_post   <- 0.99999

# Bayesian update for one test result
bayes_update <- function(prior, result) {
  if (result == 1) {             
    p_pos_d  <- sensitivity
    p_pos_nd <- fp_rate
  } else {                       
    p_pos_d  <- fn_rate
    p_pos_nd <- specificity
  }
  num <- prior * p_pos_d
  den <- num + (1 - prior) * p_pos_nd
  num / den
}

# Simulate one patient
simulate_patient <- function(has_disease) {
  prior <- prevalence
  n_tests <- 0
  repeat {
    result <- if (has_disease) {
      rbinom(1, 1, sensitivity)   
    } else {
      rbinom(1, 1, fp_rate)       
    }
    prior   <- bayes_update(prior, result)
    n_tests <- n_tests + 1
    if (prior >= target_post || (1 - prior) >= target_post) break
    if (n_tests >= 500) break   
  }
  n_tests
}

#Part 1: Simulate patients WITH disease
set.seed(123)
n_sim <- 10000

tests_diseased    <- replicate(n_sim, simulate_patient(TRUE))
tests_not_diseased <- replicate(n_sim, simulate_patient(FALSE))

cat(sprintf("\n(1) Average tests for a patient WITH disease:    %.2f\n",
            mean(tests_diseased)))
cat(sprintf("(2) Average tests for a patient WITHOUT disease: %.2f\n",
            mean(tests_not_diseased)))

#Part 3: Annual budget for 1 million patients/year
# Mix according to prevalence
expected_tests_per_patient <- prevalence        * mean(tests_diseased) +
  (1 - prevalence)  * mean(tests_not_diseased)

annual_patients  <- 1e6
cost_per_test    <- 1   
annual_cost      <- annual_patients * expected_tests_per_patient * cost_per_test

cat(sprintf("\n(3) Expected tests per patient (mixed population): %.2f\n",
            expected_tests_per_patient))
cat(sprintf("    Total annual tests (1 million patients):       %.0f\n",
            annual_patients * expected_tests_per_patient))
cat(sprintf("    Recommended annual budget:                     $%.0f\n",
            annual_cost))

#Histogram of tests needed
png("/mnt/user-data/outputs/problem2_tests_histogram.png", width = 900, height = 500)
par(mfrow = c(1, 2))

hist(tests_diseased, breaks = 30, col = "tomato", border = "white",
     main = "Tests needed — patient HAS disease",
     xlab = "Number of tests", ylab = "Frequency")
abline(v = mean(tests_diseased), col = "darkred", lwd = 2, lty = 2)
legend("topright", legend = sprintf("Mean = %.1f", mean(tests_diseased)),
       col = "darkred", lty = 2, lwd = 2)

hist(tests_not_diseased, breaks = 30, col = "steelblue", border = "white",
     main = "Tests needed — patient does NOT have disease",
     xlab = "Number of tests", ylab = "Frequency")
abline(v = mean(tests_not_diseased), col = "navy", lwd = 2, lty = 2)
legend("topright", legend = sprintf("Mean = %.1f", mean(tests_not_diseased)),
       col = "navy", lty = 2, lwd = 2)

dev.off()

cat("\nPlots saved to:\n")
cat("  problem1_posterior.png\n")
cat("  problem2_tests_histogram.png\n")