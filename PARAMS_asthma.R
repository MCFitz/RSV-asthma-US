# Asthma-related parameters

# baseline rate of wheeze/asthma among US pop by 6yo (regardless of RSV)
# CDC IAN_ADD details on citation
prev_tot <- 0.088
prev_tot_uci <- 0.096
prev_tot_lci <- 0.081

prev_tot_sd <- (prev_tot_uci - prev_tot_lci)/(1.96*2)
prev_tot_u <- rnorm(trials, prev_tot, prev_tot_sd)

# Approximate risk ratio of wheeze/asthma given RSV-LRTI
# based on Brunwasser et al. 2020 adjusted odds ratio
# 2.45, 95% CI (1.23, 4.88)
rr_w <- 2.45

rr_w_l <- 1.23 # lower bound
rr_w_h <- 4.88 # higher bound

# checking to make sure distance is similar for log-normal
log(rr_w) - log(rr_w_h)
log(rr_w) - log(rr_w_h)

rr_w_sd <- (log(rr_w_h) - log(rr_w_l))/ (1.96*2) # standard deviation
rr_w_sample <- rnorm(trials, log(rr_w), rr_w_sd) # normal dist of log
rr_w_u <- exp(rr_w_sample) # retransformed uncertainty distribution



