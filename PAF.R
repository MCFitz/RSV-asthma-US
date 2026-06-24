# ---------------------------
# PAF Calculations - Abreo supplement
# ---------------------------

set.seed(1234)
trials <- 10000

# ---------------------------
# Helper function
# ---------------------------
calc_paf <- function(name, OR, OR_l, OR_h, prev, trials = 10000) {
  
  OR_sd <- (log(OR_h) - log(OR_l)) / (1.96 * 2)
  OR_sample <- rnorm(trials, mean = log(OR), sd = OR_sd)
  OR_u <- exp(OR_sample)
  
  paf_pe <- (prev * (OR - 1)) / (prev * (OR - 1) + 1)
  paf_u  <- (prev * (OR_u - 1)) / (prev * (OR_u - 1) + 1)
  
  data.frame(
    RiskFactor = name,
    OR = OR,
    OR_l = OR_l,
    OR_h = OR_h,
    Prevalence = prev,
    PAF = paf_pe,
    PAF_l = quantile(paf_u, probs = 0.05, na.rm = TRUE),
    PAF_h = quantile(paf_u, probs = 0.95, na.rm = TRUE)
  )
}

# ---------------------------
# Input data
# ---------------------------

#averaging traffic polution
traffic_bc_OR <- 1.20
traffic_bc_OR_l <- 1.05
traffic_bc_OR_h <- 1.38
traffic_bc_OR_sd <- (log(traffic_bc_OR_h) - log(traffic_bc_OR_l))/ (1.96*2) # standard deviation
traffic_bc_OR_sample <- rnorm(trials, log(traffic_bc_OR), traffic_bc_OR_sd) # normal dist of log
traffic_bc_OR_u <- exp(traffic_bc_OR_sample)


traffic_nd_OR <- 1.20
traffic_nd_OR_l <- 1.05
traffic_nd_OR_h <- 1.38
traffic_nd_OR_sd <- (log(traffic_nd_OR_h) - log(traffic_nd_OR_l))/ (1.96*2) # standard deviation
traffic_nd_OR_sample <- rnorm(trials, log(traffic_nd_OR), traffic_nd_OR_sd) # normal dist of log
traffic_nd_OR_u <- exp(traffic_nd_OR_sample)

traffic_pm_OR <- 1.20
traffic_pm_OR_l <- 1.05
traffic_pm_OR_h <- 1.38
traffic_pm_OR_sd <- (log(traffic_pm_OR_h) - log(traffic_pm_OR_l))/ (1.96*2) # standard deviation
traffic_pm_OR_sample <- rnorm(trials, log(traffic_pm_OR), traffic_pm_OR_sd) # normal dist of log
traffic_pm_OR_u <- exp(traffic_pm_OR_sample)

traffic_ave_OR <- (traffic_pm_OR + traffic_nd_OR + traffic_bc_OR)/3
traffic_ave_OR_u <- (traffic_pm_OR_u + traffic_nd_OR_u + traffic_bc_OR_u)/3
traffic_ave_OR_u_l <- quantile(traffic_ave_OR_u, probs = 0.05, na.rm = TRUE)
traffic_ave_OR_u_h <-  quantile(traffic_ave_OR_u, probs = 0.95, na.rm = TRUE)

risk_data <- data.frame(
  RiskFactor = c(
    "Acetaminophen, prenatal",
    "Antibiotic use, prenatal",
    "Cesarean section",
    "Folic acid, maternal",
    "Infection, antenatal",
    "Prenatal maternal stress",
    "Preterm delivery",
    "Smoking, prenatal",
    "Vitamin D sufficient in utero",
    "Vitamin E maternal intake",
    "Acetaminophen, infant",
    "Antibiotic use, infant",
    "Breastfeeding, ever",
    "RSV infection, infant",
    "Allergic rhinitis, mold",
    "Food sensitization <=2 years",
    "Fruit intake, adequate",
    "Obesity, BMI >95th percentile",
    "Overweight, BMI 85-94th percentile",
    "Rhinovirus-induced wheezing <=3 years",
    "Traffic pollution, average",
    "Traffic pollution, black carbon",
    "Traffic pollution, nitrogen dioxide",
    "Traffic pollution, particulate matter",
    "Vegetable intake, adequate",
    "Gas stove cooking",
    "H pylori",
    "Omega-3 fatty acids",
    "Pets, cats",
    "Pets, dogs",
    "Physical activity, inadequate",
    "Probiotics",
    "Secondhand smoke"
  ),
  
  OR = c(
    1.28, 1.20, 1.16, 1.01, 1.55, 1.45, 1.074, 1.85,
    0.84, 0.98, 1.47, 1.27, 0.88, 3.84, 1.09, 2.80,
    0.98, 1.46, 1.23, 2.00, 1.2, 1.20, 1.09, 1.14, 0.95,
    1.32, 0.88, 0.94, 1.00, 0.77, 1.32, 0.96, 1.32
  ),
  
  OR_l = c(
    1.13, 1.13, 1.14, 0.78, 1.24, 1.25, 1.072, 1.35,
    0.70, 0.96, 1.36, 1.12, 0.82, 3.23, 0.90, 2.10,
    0.96, 1.36, 1.17, 1.62, 1.12, 1.05, 0.96, 1.00, 0.92,
    1.18, 0.76, 0.78, 0.78, 0.58, 0.95, 0.85, 1.23
  ),
  
  OR_h = c(
    1.39, 1.27, 1.29, 1.30, 1.92, 1.68, 1.075, 2.53,
    1.01, 0.99, 1.56, 1.43, 0.95, 4.58, 1.32, 3.90,
    1.00, 1.57, 1.29, 2.49, 1.29, 1.38, 1.23, 1.30, 0.98,
    1.48, 1.02, 1.13, 1.28, 1.03, 1.84, 1.07, 1.42
  ),
  
  prev = c(
    0.655, 0.40, 0.32, 0.40, 0.40, 0.25, 0.0963, 0.084,
    0.72, 0.198, 0.878, 0.66, 0.819, 0.20, 0.043, 0.058,
    0.498, 0.094, 0.139, 0.158, 0.038, 0.038, 0.038, 0.038, 0.217,
    0.093, 0.167, 0.011, 0.304, 0.365, 0.574, 0.005, 0.406
  )
)

# ---------------------------
# Calculate PAFs
# ---------------------------
paf_results <- do.call(
  rbind,
  lapply(seq_len(nrow(risk_data)), function(i) {
    calc_paf(
      name = risk_data$RiskFactor[i],
      OR = risk_data$OR[i],
      OR_l = risk_data$OR_l[i],
      OR_h = risk_data$OR_h[i],
      prev = risk_data$prev[i],
      trials = trials
    )
  })
)

# Convert to percentages
paf_results <- paf_results |>
  transform(
    Prevalence_percent = Prevalence * 100,
    PAF_percent = PAF * 100,
    PAF_l_percent = PAF_l * 100,
    PAF_h_percent = PAF_h * 100
  )

# View results
print(paf_results)

# Optional: save results
write.csv(paf_results, "abreo_paf_results.csv", row.names = FALSE)

#--------------------------
#PAF performed at bounds:

# ---------------------------
# PAF Calculations - Abreo
# Direct calculation using OR confidence interval bounds
# No Monte Carlo simulation
# ---------------------------

# ---------------------------
# Helper function
# ---------------------------

calc_paf_bounds <- function(name, OR, OR_l, OR_h, prev) {
  
  paf_pe <- (prev * (OR - 1)) / ((prev * (OR - 1)) + 1)
  paf_l  <- (prev * (OR_l - 1)) / ((prev * (OR_l - 1)) + 1)
  paf_h  <- (prev * (OR_h - 1)) / ((prev * (OR_h - 1)) + 1)
  
  data.frame(
    RiskFactor = name,
    OR = OR,
    OR_l = OR_l,
    OR_h = OR_h,
    Prevalence = prev,
    PAF = paf_pe,
    PAF_l = paf_l,
    PAF_h = paf_h,
    PAF_percent = paf_pe * 100,
    PAF_l_percent = paf_l * 100,
    PAF_h_percent = paf_h * 100
  )
}

# ---------------------------
# Input data
# ---------------------------

risk_data <- data.frame(
  RiskFactor = c(
    "Acetaminophen, prenatal",
    "Antibiotic use, prenatal",
    "Cesarean section",
    "Folic acid, maternal",
    "Infection, antenatal",
    "Prenatal maternal stress",
    "Preterm delivery",
    "Smoking, prenatal",
    "Vitamin D sufficient in utero",
    "Vitamin E maternal intake",
    "Acetaminophen, infant",
    "Antibiotic use, infant",
    "Breastfeeding, ever",
    "RSV infection, infant",
    "Allergic rhinitis, mold",
    "Food sensitization <=2 years",
    "Fruit intake, adequate",
    "Obesity, BMI >95th percentile",
    "Overweight, BMI 85-94th percentile",
    "Rhinovirus-induced wheezing <=3 years",
    "Traffic pollution, average",
    "Traffic pollution, black carbon",
    "Traffic pollution, nitrogen dioxide",
    "Traffic pollution, particulate matter",
    "Vegetable intake, adequate",
    "Gas stove cooking",
    "H pylori",
    "Omega-3 fatty acids",
    "Pets, cats",
    "Pets, dogs",
    "Physical activity, inadequate",
    "Probiotics",
    "Secondhand smoke"
  ),
  
  OR = c(
    1.28, 1.20, 1.16, 1.01, 1.55, 1.45, 1.074, 1.85,
    0.84, 0.98, 1.47, 1.27, 0.88, 3.84, 1.09, 2.80,
    0.98, 1.46, 1.23, 2.00, 1.2, 1.20, 1.09, 1.14, 0.95,
    1.32, 0.88, 0.94, 1.00, 0.77, 1.32, 0.96, 1.32
  ),
  
  OR_l = c(
    1.13, 1.13, 1.14, 0.78, 1.24, 1.25, 1.072, 1.35,
    0.70, 0.96, 1.36, 1.12, 0.82, 3.23, 0.90, 2.10,
    0.96, 1.36, 1.17, 1.62, 1.12, 1.05, 0.96, 1.00, 0.92,
    1.18, 0.76, 0.78, 0.78, 0.58, 0.95, 0.85, 1.23
  ),
  
  OR_h = c(
    1.39, 1.27, 1.29, 1.30, 1.92, 1.68, 1.075, 2.53,
    1.01, 0.99, 1.56, 1.43, 0.95, 4.58, 1.32, 3.90,
    1.00, 1.57, 1.29, 2.49, 1.29, 1.38, 1.23, 1.30, 0.98,
    1.48, 1.02, 1.13, 1.28, 1.03, 1.84, 1.07, 1.42
  ),
  
  prev = c(
    0.655, 0.40, 0.32, 0.40, 0.40, 0.25, 0.0963, 0.084,
    0.72, 0.198, 0.878, 0.66, 0.819, 0.20, 0.043, 0.058,
    0.498, 0.094, 0.139, 0.158, 0.038, 0.038, 0.038, 0.038, 0.217,
    0.093, 0.167, 0.011, 0.304, 0.365, 0.574, 0.005, 0.406
  )
)

# ---------------------------
# Calculate PAFs at OR bounds
# ---------------------------

paf_results_bounds <- do.call(
  rbind,
  lapply(seq_len(nrow(risk_data)), function(i) {
    calc_paf_bounds(
      name = risk_data$RiskFactor[i],
      OR = risk_data$OR[i],
      OR_l = risk_data$OR_l[i],
      OR_h = risk_data$OR_h[i],
      prev = risk_data$prev[i]
    )
  })
)

# ---------------------------
# Format results
# ---------------------------

paf_results_bounds$PAF_display <- sprintf(
  "%.1f%% (%.1f%% to %.1f%%)",
  paf_results_bounds$PAF_percent,
  paf_results_bounds$PAF_l_percent,
  paf_results_bounds$PAF_h_percent
)

# View results
print(paf_results_bounds)

# Save results
write.csv(
  paf_results_bounds,
  "abreo_paf_results_direct_OR_bounds.csv",
  row.names = FALSE
)

p0 <- 0.088

or_to_rr_zhang_yu <- function(or, p0) {
  or / ((1 - p0) + (p0 * or))
}

calc_paf_bounds_rr <- function(name, OR, OR_l, OR_h, prev, p0) {
  
  RR   <- or_to_rr_zhang_yu(OR, p0)
  RR_l <- or_to_rr_zhang_yu(OR_l, p0)
  RR_h <- or_to_rr_zhang_yu(OR_h, p0)
  
  paf_pe <- (prev * (RR - 1)) / ((prev * (RR - 1)) + 1)
  paf_l  <- (prev * (RR_l - 1)) / ((prev * (RR_l - 1)) + 1)
  paf_h  <- (prev * (RR_h - 1)) / ((prev * (RR_h - 1)) + 1)
  
  data.frame(
    RiskFactor = name,
    OR = OR, OR_l = OR_l, OR_h = OR_h,
    RR = RR, RR_l = RR_l, RR_h = RR_h,
    Prevalence = prev,
    PAF = paf_pe,
    PAF_l = paf_l,
    PAF_h = paf_h,
    PAF_percent = paf_pe * 100,
    PAF_l_percent = paf_l * 100,
    PAF_h_percent = paf_h * 100
  )
}

paf_results_bounds_rr <- do.call(
  rbind,
  lapply(seq_len(nrow(risk_data)), function(i) {
    calc_paf_bounds_rr(
      name = risk_data$RiskFactor[i],
      OR = risk_data$OR[i],
      OR_l = risk_data$OR_l[i],
      OR_h = risk_data$OR_h[i],
      prev = risk_data$prev[i],
      p0 = p0
    )
  })
)

paf_results_bounds_rr$PAF_display <- sprintf(
  "%.1f%% (%.1f%% to %.1f%%)",
  paf_results_bounds_rr$PAF_percent,
  paf_results_bounds_rr$PAF_l_percent,
  paf_results_bounds_rr$PAF_h_percent
)

print(paf_results_bounds_rr)

write.csv(
  paf_results_bounds_rr,
  "abreo_paf_results_direct_RR_bounds.csv",
  row.names = FALSE
)

write.csv(
  paf_results_bounds_rr[, c(
    "RiskFactor",
    "RR_display",
    "PAF_display"
  )],
  "abreo_RR_and_PAF_summary.csv",
  row.names = FALSE
)
