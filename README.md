# BINF 6310 – Advanced Statistics for Genomics

**University of North Carolina at Charlotte** | Graduate Coursework | Spring 2025  
**Language:** R

---

## Project Overview

This repository contains lab assignments from BINF 6310, a graduate-level course in statistical methods applied to genomics and bioinformatics. Each lab applies core probabilistic and Bayesian techniques to real-world-style problems in computational biology and clinical diagnostics — implemented entirely in R.

The labs demonstrate hands-on proficiency in:
- Probability distributions and statistical simulation
- Bayesian inference and posterior probability estimation
- Markov model foundations used in sequence analysis
- Data visualization and convergence analysis

---

## Labs

### Lab 01 – Probability Distributions & the Law of Large Numbers
Simulates a loaded die modeled after the "occasionally dishonest casino" from Durbin et al.'s *Biological Sequence Analysis* — a foundational text for Hidden Markov Models in genomics.

- Derived the theoretical mean and variance for a non-uniform discrete distribution
- Implemented a custom R function to simulate rolls of the loaded die
- Visualized the distribution via histograms and compared it to a uniform distribution
- Plotted sample mean convergence across increasing trial sizes to demonstrate the Law of Large Numbers

---

### Lab 02 – Bayesian Inference & Clinical Diagnostics
Applied Bayesian updating to two real-world scenarios: detecting a loaded die and modeling a clinical diagnostic test.

- Computed and plotted posterior probability updates over a sequence of die rolls
- Determined the number of rolls needed to reach 99.999% confidence that a die is loaded
- Modeled a diagnostic test with known sensitivity and specificity for a disease with 0.1% prevalence
- Simulated test repetitions for diseased and healthy patients to reach a posterior threshold of 0.99999
- Estimated total testing costs for a hospital running 1 million patients per year

---

### Lab 03 – Bayesian Posterior Estimation: Metropolis Algorithm vs. Grid Approximation
Compared two computational approaches to Bayesian posterior estimation using coin flip data.

- Plotted an exponential prior distribution for the probability of heads
- Implemented both the **Metropolis-Hastings algorithm** and **grid approximation** to estimate posteriors for two datasets (14/10 and 583/417 heads/tails)
- Benchmarked both methods against the exact analytical solution derived from a Beta(40,40) prior
- Analyzed how increasing data volume causes the posterior to dominate over the prior, illustrating a key principle of Bayesian inference

---

## Technologies Used

| Tool | Purpose |
|------|---------|
| R | All simulation, statistical modeling, and visualization |
| ggplot2 / base R graphics | Plotting distributions and convergence curves |
| GitHub | Version control and reproducibility |

---

## About

These labs were completed as part of an M.S. in Bioinformatics at UNC Charlotte. The course applies statistical reasoning — probability theory, simulation, and Bayesian methods — to problems in genomics and healthcare, building the foundation for more advanced work in sequence analysis and Hidden Markov Models.
