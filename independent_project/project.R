library(dplyr)
library(survival)
library(survminer)

# ---- Load data ----
clinical  <- read.delim("clinical.tsv",  stringsAsFactors = FALSE, na.strings = c("'--", "NA", ""))
follow_up <- read.delim("follow_up.tsv", stringsAsFactors = FALSE, na.strings = c("'--", "NA", ""))

# ---- Identify trisomy 4+10 patients ----
trisomy_4 <- follow_up %>%
  filter(molecular_tests.chromosome == "chr4") %>%
  pull(cases.case_id) %>%
  unique()

trisomy_10 <- follow_up %>%
  filter(molecular_tests.chromosome == "chr10") %>%
  pull(cases.case_id) %>%
  unique()

trisomy_4_10_ids <- intersect(trisomy_4, trisomy_10)
cat("Patients with trisomy 4+10:", length(trisomy_4_10_ids), "\n")

# ---- Build survival dataset (all patients, not just dead) ----
# Survival analysis requires ALL patients with censoring for those still alive
survival_data <- clinical %>%
  distinct(cases.case_id, .keep_all = TRUE) %>%
  mutate(
    trisomy_4_10 = ifelse(cases.case_id %in% trisomy_4_10_ids, "Positive", "Negative"),
    # Event indicator: 1 = dead, 0 = alive/censored
    event = ifelse(demographic.vital_status == "Dead", 1, 0),
    # Use days_to_death for dead patients, days_to_last_follow_up for alive
    time = as.numeric(ifelse(
      demographic.vital_status == "Dead",
      demographic.days_to_death,
      diagnoses.days_to_last_follow_up
    ))
  ) %>%
  filter(!is.na(time) & time > 0)

cat("Patients in survival analysis:", nrow(survival_data), "\n")
print(table(survival_data$trisomy_4_10, survival_data$event,
            dnn = c("Trisomy 4+10", "Event (1=Dead)")))

# ---- Dead patients only (t-test section) ----
dead_patients <- survival_data %>%
  filter(demographic.vital_status == "Dead") %>%
  rename(days_to_death = time)

cat("\nDead patients:", nrow(dead_patients), "\n")
print(table(dead_patients$trisomy_4_10))

days_positive <- dead_patients %>% filter(trisomy_4_10 == "Positive") %>% pull(days_to_death) %>% na.omit()
days_negative <- dead_patients %>% filter(trisomy_4_10 == "Negative") %>% pull(days_to_death) %>% na.omit()

cat("\nPositive - mean:", round(mean(days_positive)), "days, n:", length(days_positive), "\n")
cat("Negative - mean:", round(mean(days_negative)), "days, n:", length(days_negative), "\n")

ttest_result <- t.test(days_positive, days_negative, var.equal = FALSE)
print(ttest_result)

# ---- Boxplot ----
par(mar = c(5, 4, 4, 2))
boxplot(
  days_to_death ~ trisomy_4_10,
  data  = dead_patients,
  main  = "Days to Death by Trisomy 4+10 Status",
  xlab  = "Trisomy 4+10 Status",
  ylab  = "Days to Death",
  col   = c("lightblue", "salmon")
)
p_label <- ifelse(ttest_result$p.value < 0.001, "p < 0.001",
                  paste0("p = ", round(ttest_result$p.value, 3)))
mtext(paste("Welch t-test:", p_label), side = 3, line = 0.3, cex = 0.9)

# ============================================================
# ---- Kaplan-Meier survival curves ----
# ============================================================
km_fit <- survfit(
  Surv(time, event) ~ trisomy_4_10,
  data = survival_data
)

cat("\n---- Kaplan-Meier Summary ----\n")
print(summary(km_fit)$table)

# Plot KM curves with survminer (publication-ready)
km_plot <- ggsurvplot(
  km_fit,
  data          = survival_data,
  pval          = TRUE,          # adds log-rank p-value automatically
  pval.method   = TRUE,          # labels it as "Log-rank"
  conf.int      = TRUE,
  risk.table    = TRUE,          # number at risk table below plot
  legend.labs   = c("Negative", "Positive"),
  legend.title  = "Trisomy 4+10",
  palette       = c("steelblue", "salmon"),
  title         = "Overall Survival by Trisomy 4+10 Status",
  xlab          = "Time (days)",
  ylab          = "Survival Probability",
  ggtheme       = theme_bw()
)
print(km_plot)

# ============================================================
# ---- Log-rank test ----
# ============================================================
# Note: ggsurvplot above displays this automatically via pval = TRUE,
# but running it explicitly gives you the full test object to report.
logrank_result <- survdiff(
  Surv(time, event) ~ trisomy_4_10,
  data = survival_data
)

cat("\n---- Log-rank Test ----\n")
print(logrank_result)

# Extract and print p-value cleanly
logrank_p <- 1 - pchisq(logrank_result$chisq, df = length(logrank_result$n) - 1)
cat(sprintf("Chi-squared: %.3f, df: %d, p-value: %.4f\n",
            logrank_result$chisq,
            length(logrank_result$n) - 1,
            logrank_p))

# ============================================================
# ---- Cox Proportional Hazards Regression ----
# ============================================================
# Relevel so Negative is the reference group
survival_data$trisomy_4_10 <- relevel(factor(survival_data$trisomy_4_10), ref = "Negative")

cox_fit <- coxph(
  Surv(time, event) ~ trisomy_4_10,
  data = survival_data
)

cat("\n---- Cox Proportional Hazards Model ----\n")
print(summary(cox_fit))

# Extract hazard ratio and 95% CI for clean reporting
cox_summary <- summary(cox_fit)
hr    <- round(cox_summary$conf.int[, "exp(coef)"], 3)
hr_lo <- round(cox_summary$conf.int[, "lower .95"],  3)
hr_hi <- round(cox_summary$conf.int[, "upper .95"],  3)
cox_p <- round(cox_summary$coefficients[, "Pr(>|z|)"], 4)

cat(sprintf("\nHazard Ratio (Positive vs Negative): %.3f (95%% CI: %.3f–%.3f), p = %.4f\n",
            hr, hr_lo, hr_hi, cox_p))

# Forest plot of HR
ggforest(cox_fit, data = survival_data,
         main = "Hazard Ratio: Trisomy 4+10 (Positive vs. Negative)")

# ---- Test Cox PH assumption (Schoenfeld residuals) ----
ph_test <- cox.zph(cox_fit)
cat("\n---- Proportional Hazards Assumption Test ----\n")
print(ph_test)
# p > 0.05 means the PH assumption holds
ggcoxzph(ph_test)

