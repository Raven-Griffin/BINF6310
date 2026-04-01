library(dplyr)

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

# ---- Dead patients only ----
dead_patients <- clinical %>%
  distinct(cases.case_id, .keep_all = TRUE) %>%
  filter(demographic.vital_status == "Dead") %>%
  mutate(
    trisomy_4_10 = ifelse(cases.case_id %in% trisomy_4_10_ids, "Positive", "Negative"),
    days_to_death = as.numeric(demographic.days_to_death)
  )

cat("Dead patients:", nrow(dead_patients), "\n")
print(table(dead_patients$trisomy_4_10))

# ---- T-test: days to death by trisomy 4+10 status ----
days_positive <- dead_patients %>%
  filter(trisomy_4_10 == "Positive") %>%
  pull(days_to_death) %>%
  na.omit()

days_negative <- dead_patients %>%
  filter(trisomy_4_10 == "Negative") %>%
  pull(days_to_death) %>%
  na.omit()

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
