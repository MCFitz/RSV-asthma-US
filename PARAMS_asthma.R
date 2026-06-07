# -----------------------------
# USA Asthma related input parameters 
# -----------------------------

trials <- 1000

# total population birth cohort use of national center 
# for health statistics data from 2021
pop_tot <- 3664292

# baseline rate of wheeze/asthma among US pop by 6yo (regardless of RSV)
# CDC - details on citation - weighted estimate 2018-2021 National Health Interview Survery data https://stacks.cdc.gov/view/cdc/109086
prev_tot <- 0.088 
prev_tot_uci <- 0.096
prev_tot_lci <- 0.081
prev_tot_sd <- (prev_tot_uci - prev_tot_lci)/(1.96*2)
prev_tot_u <- rnorm(trials, prev_tot, prev_tot_sd)

# Approximate risk ratio (aOR) of wheeze/asthma given RSV-LRTI
# based on Brunwasser et al. 2020 adjusted odds ratio 
# controlling genetic effects 2-6 y/o - 2.45, 95% CI (1.23, 4.88)
aOR_w <- 2.45 
aOR_w_l <- 1.23 # lower bound
rr_w_h <- 4.88 # higher bound

log(aOR_w) - log(aOR_w_h) # checking to make sure distance is similar for log-normal
log(aOR_w) - log(aOR_w_l) # checking to make sure distance is similar for log-normal
aOR_w_sd <- (log(aOR_w_h) - log(aOR_w_l))/ (1.96*2) # standard deviation
aOR_w_sample <- rnorm(trials, log(aOR_w), aOR_w_sd) # normal dist of log
aOR_w_u <- exp(aOR_w_sample) # retransformed uncertainty distribution

#EXTRA/OLD CODE

# -----------------------------
# DATA FOR COST SAVINGS INCORPORATION
# -----------------------------

#USA Asthma Cost Calculation: https://www.cdc.gov/national-asthma-control-program/php/cost-calculator/index.html
#Source data from Medical Expenditure panel survey, Behavioral Risk Factor Surveillance System, National Survey of Children's Health, US Census
#Uses Predictive model called Random Forest to estimate medical cost 
#Age 0-5 Per-person incremental cost per asthma in 2021 dollars (averaged amongst states)
#zerotofivecost_a <- 2463.13

# -----------------------------
# ADDITIONAL RSV ASSOCIATED OUTCOMES
# -----------------------------

## RSV/LRTI associated risk of additional disease 0-6 mo
#Pneumonia after RSV bronchiolitis
#pn_05 <- 1.36
#pn_UL_05 <- 1.54
#pn_LL_05 <- 1.20
#distance - -.120/.128
#rr_pn05_sd <- (log(pn_UL_05) - log(pn_LL_05))/ (1.96*2) # standard deviation
#rr_pn05_sample <- rnorm(trials, log(pn_05), rr_pn05_sd) # normal dist of log
#rr_pn05_u <- exp(rr_pn05_sample) # retransformed uncertainty distribution

#Otitis media after RSV bronchiolitis
#OM_05 <- 1.32
#OM_UL_05 <- 1.38
#OM_LL_05 <- 1.26
#distance - -.0445/.0465
#rr_OM05_sd <- (log(OM_UL_05) - log(OM_LL_05))/ (1.96*2) # standard deviation
#rr_OM05_sample <- rnorm(trials, log(OM_05), rr_OM05_sd) # normal dist of log
#rr_OM05_u <- exp(rr_OM05_sample) # retransformed uncertainty distribution

#Inappropriate abx fills after RSV bronchiolitis
#ab_05 <- 1.24
#ab_UL_05 <- 1.32
#ab_LL_05 <- 1.16
#distance - -.0625/.0667
#rr_ab05_sd <- (log(ab_UL_05) - log(ab_LL_05))/ (1.96*2) # standard deviation
#rr_ab05_sample <- rnorm(trials, log(ab_05), rr_ab05_sd) # normal dist of log
#rr_ab05_u <- exp(rr_ab05_sample) # retransformed uncertainty distribution

## RSV/LRTI associated risk of additional disease 0-6 mo
#Pneumonia after RSV bronchiolitis
#pn_611 <- 1.34
#pn_UL_611 <- 1.59
#pn_LL_611 <- 1.13
#distance - -.171/.170
#rr_pn611_sd <- (log(pn_UL_611) - log(pn_LL_611))/ (1.96*2) # standard deviation
#rr_pn611_sample <- rnorm(trials, log(pn_611), rr_pn611_sd) # normal dist of log
#rr_pn611_u <- exp(rr_pn611_sample) # retransformed uncertainty distribution

#Otitis media after RSV bronchiolitis
#OM_611 <- 1.26
#OM_UL_611 <- 1.34
#OM_LL_611 <- 1.18
#distance - -.0616/.0656
#rr_OM611_sd <- (log(OM_UL_611) - log(OM_LL_611))/ (1.96*2) # standard deviation
#rr_OM611_sample <- rnorm(trials, log(OM_611), rr_OM611_sd) # normal dist of log
#rr_OM611_u <- exp(rr_OM611_sample) # retransformed uncertainty distribution

#Inappropriate abx fills after RSV bronchiolitis
#ab_611 <- 1.23
#ab_UL_611 <- 1.34
#ab_LL_611 <- 1.14
#distance - -.0857/.0760
#rr_ab611_sd <- (log(ab_UL_611) - log(ab_LL_611))/ (1.96*2) # standard deviation
#rr_ab611_sample <- rnorm(trials, log(ab_611), rr_ab611_sd) # normal dist of log
#rr_ab611_u <- exp(rr_ab611_sample) # retransformed uncertainty distribution

# -----------------------------
# CONSIDERED SENSITIVITY ANALYSIS FOR SEVERE ASTHMA 
# DIFFERENTIAL ASTHMA ASSOICATION WITH SEVERITY OF RSV INFECTION
# -----------------------------

#Severe asthma https://pmc.ncbi.nlm.nih.gov/articles/PMC10330473/ - echo consortium for recurrent exacerbation as surrogate for severe asthma 5-9 y/o
#prev_tot_severe <- 4.9/1000
#prev_tot_uci_severe <- 5.47/1000
#prev_tot_lci_severe <- 4.32/1000
#prev_tot_sd_severe <- (prev_tot_uci_severe - prev_tot_lci_severe)/(1.96*2)
#prev_tot_u_severe <- rnorm(trials, prev_tot_severe, prev_tot_sd_severe)

#Differential rate of asthma prevelance based on SEVERITY of exposure? Using brunwasser RR
# Risk for wheeze/asthma given RSV-LRTI hospitalization?: Caroll 2009 - https://pmc.ncbi.nlm.nih.gov/articles/PMC2703291/
#rr_w_hosp <- 2.82 
#rr_w_l_hosp <- 2.61 # lower bound
#rr_w_h_hosp <- 3.02 # higher bound
#rr_w_sd_hosp <- (log(rr_w_h_hosp) - log(rr_w_l_hosp))/ (1.96*2) # standard deviation
#rr_w_sample_hosp <- rnorm(trials, log(rr_w_hosp), rr_w_sd_hosp) # normal dist of log
#rr_w_u_hosp <- exp(rr_w_sample_hosp) # retransformed uncertainty distribution

# -----------------------------
# MORTALITY ADJUSTMENT
# -----------------------------

#Mortality adjustment - Deemed to not be important re our project in comparison to Mali due to low infant mortality rate in USA
# Under 5 mortality - world bank data 2023/4 - uncertainty 6.0-6.5/1000. National vital statistics report notes 5.61 infant deaths per 1000 but in 2022- https://pubmed.ncbi.nlm.nih.gov/39412861/
# https://databank.worldbank.org/reports.aspx?source=2&series=SH.DYN.MORT&country= 
#U5_mort <- 6.3/1000

# 5-9 mortality - Unicef mortality rate data 2023/4 - uncertainty .5619-.6050 - https://data.unicef.org/topic/child-survival/child-and-youth-mortality-age-5-24/#data
#U9_mort <- .58/1000

# total population surviving to 5 years of age (calculated from excel using 
# mortality data). Use of 5 y/o in Mali due to ISAAC survery for RR 
# asthma/wheeze and wanting to align magnitude of risk
#pop_tot <- 3639791

# total population surviving to 5 y/o (calculated from excel using 
# mortality data)
#pop_total5 <- 3640213
