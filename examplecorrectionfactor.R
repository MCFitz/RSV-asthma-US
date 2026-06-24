# ---------------------------
# RSV LRTI visit-to-episode correction factor
# Sensitive RSV definition
# MarketScan Commercial + Medicaid
# Creating uncertainty around ratio of multiple visits per one episode during first year of life
# ---------------------------

set.seed(123)

# Inputs
episode_private  <- 69068
visit_private    <- 107338

episode_medicaid <- 170744
visit_medicaid   <- 256495

percent_private  <- 0.46
percent_medicaid <- 0.54

n_sim <- 1000

# ---------------------------
# Point estimates
# ---------------------------

cf_private  <- episode_private / visit_private
cf_medicaid <- episode_medicaid / visit_medicaid

cf_weighted <- percent_private  * cf_private +
  percent_medicaid * cf_medicaid

cf_private
cf_medicaid
cf_weighted

# ---------------------------
# Beta uncertainty for episode/visit proportions
# ---------------------------

cf_private_sim <- rbeta(
  n_sim,
  episode_private + 1,
  visit_private - episode_private + 1
)

cf_medicaid_sim <- rbeta(
  n_sim,
  episode_medicaid + 1,
  visit_medicaid - episode_medicaid + 1
)

cf_weighted_sim <-
  percent_private * cf_private_sim +
  percent_medicaid * cf_medicaid_sim

# ---------------------------
# Summary
# ---------------------------

summary_df <- data.frame(
  estimate = cf_weighted,
  lower_95 = quantile(cf_weighted_sim, 0.025),
  upper_95 = quantile(cf_weighted_sim, 0.975)
)

summary_df

# Optional: percent form
summary_df_percent <- summary_df * 100
summary_df_percent
