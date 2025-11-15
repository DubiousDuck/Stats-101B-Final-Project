# ---- 1. Install & load pwr package (run install.packages only once) ----
# install.packages("pwr")   # ← uncomment the first time
library(pwr)

# ---- 2. Baseline calculation: medium effect (f = 0.25) ----
# This returns n = sample size *per* group
baseline <- pwr.anova.test(k = 3,          # number of groups
                           f = 0.25,       # Cohen's f (medium)
                           sig.level = 0.05,
                           power = 0.80)
baseline

# ---- 3. Table of sample sizes for multiple effect sizes ----
effect_sizes <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)

sample_table <- data.frame(
  f             = effect_sizes,
  n_per_group   = sapply(effect_sizes, function(f)
    ceiling(pwr.anova.test(k = 3,
                           f = f,
                           sig.level = 0.05,
                           power = 0.80)$n)),
  n_total       = sapply(effect_sizes, function(f)
    ceiling(pwr.anova.test(k = 3,
                           f = f,
                           sig.level = 0.05,
                           power = 0.80)$n) * 3)
)

print(sample_table)

# ---- 4. (Optional) Plot a power curve for a chosen effect size ----
# Example: visualise how sample size affects power for f = 0.25
grp_size  <- seq(10, 100, by = 5)          # participants per group
power_vec <- sapply(grp_size, function(n)
  pwr.anova.test(k = 3,
                 f = 0.25,
                 n = n,
                 sig.level = 0.05)$power)
plot(grp_size, power_vec, type = "b",
     xlab = "Participants per group",
     ylab = "Power (f = 0.25)",
     main = "Power Curve for One-Way ANOVA (3 groups)")
abline(h = 0.80, lty = 2)                  # 80% target line
