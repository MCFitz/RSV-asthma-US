# Asthma-related parameters USA

# Under 5 mortality - world bank data 2023/4 - uncertainty 6.0-6.5/1000. National vital statistics report notes 5.61 infant deaths per 1000 but in 2022- https://pubmed.ncbi.nlm.nih.gov/39412861/
# https://databank.worldbank.org/reports.aspx?source=2&series=SH.DYN.MORT&country= 
U5_mort <- 6.3/1000

# 5-9 mortality - Unicef mortality rate data 2023/4 - uncertainty .5619-.6050 - https://data.unicef.org/topic/child-survival/child-and-youth-mortality-age-5-24/#data
U9_mort <- .58/1000

# total population birth cohort from Hutton et. al - use of national center 
# for health statistics data from 2021
pop_totO <- 3664292

# total population surviving to 6 years of age (calculated from excel using 
# mortality data). Use of 6 y/o in Mali due to ISAAC survery for RR 
# asthma/wheeze and wanting to align magnitude of risk
pop_tot <- 3639791

# total population surviving to 5 y/o (calculated from excel using 
# mortality data)
pop_total5 <- 3640213

# baseline rate of wheeze/asthma among US pop by 6yo (regardless of RSV)
# CDC IAN_ADD details on citation - weighted estimate 2018-2021 National Health Interview Survery data https://stacks.cdc.gov/view/cdc/109086
# CAN ALSO USE MORE RECENT DATA BUT WITHOUT CI - 7.0% SE .56: https://www.cdc.gov/asthma-data/about/most-recent-asthma-data.html
prev_tot <- 0.088 # number represents point estimate, ideally would have calculated number
prev_tot_uci <- 0.096
prev_tot_lci <- 0.081

prev_tot_sd <- (prev_tot_uci - prev_tot_lci)/(1.96*2)
prev_tot_u <- rnorm(trials, prev_tot, prev_tot_sd)

# Approximate risk ratio of wheeze/asthma given RSV-LRTI
# based on Brunwasser et al. 2020 adjusted odds ratio controlling genetic effects 2-5 y/o - https://pubmed.ncbi.nlm.nih.gov/32763206/ 
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



