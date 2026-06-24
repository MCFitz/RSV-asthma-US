# ---------------------------
# RSV LRTI visit-to-episode correction factor
# Sensitive RSV definition
# MarketScan Commercial + Medicaid
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
# Jeffreys-style or uniform prior options
# Here: uniform Beta(1,1) prior
# ---------------------------

cf_private_sim <- rbeta(
  n_sim,
  shape1 = episode_private + 1,
  shape2 = visit_private - episode_private + 1
)

cf_medicaid_sim <- rbeta(
  n_sim,
  shape1 = episode_medicaid + 1,
  shape2 = visit_medicaid - episode_medicaid + 1
)

# ---------------------------
# Insurance-mix uncertainty
# Treat payer mix as binomial uncertainty
# Need an effective sample size for payer mix.
# Replace this with your preferred N if available.
# ---------------------------

payer_mix_n <- 10000

private_mix_sim <- rbeta(
  n_sim,
  shape1 = percent_private * payer_mix_n + 1,
  shape2 = percent_medicaid * payer_mix_n + 1
)

medicaid_mix_sim <- 1 - private_mix_sim

# ---------------------------
# Final weighted correction factor
# ---------------------------

cf_weighted_sim <- private_mix_sim  * cf_private_sim +
  medicaid_mix_sim * cf_medicaid_sim

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