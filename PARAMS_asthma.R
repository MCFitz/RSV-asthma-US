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

# Risk for wheeze/asthma given RSV-LRTI hospitalization: Caroll 2009 - https://pmc.ncbi.nlm.nih.gov/articles/PMC2703291/

rr_w_hosp <- 2.82 

rr_w_l_hosp <- 2.61 # lower bound
rr_w_h_hosp <- 3.02 # higher bound

# checking to make sure distance is similar for log-normal
log(rr_w) - log(rr_w_h)
log(rr_w) - log(rr_w_l)

rr_w_sd <- (log(rr_w_h) - log(rr_w_l))/ (1.96*2) # standard deviation
rr_w_sample <- rnorm(trials, log(rr_w), rr_w_sd) # normal dist of log
rr_w_u <- exp(rr_w_sample) # retransformed uncertainty distribution

rr_w_sd_hosp <- (log(rr_w_h_hosp) - log(rr_w_l_hosp))/ (1.96*2) # standard deviation
rr_w_sample_hosp <- rnorm(trials, log(rr_w_hosp), rr_w_sd_hosp) # normal dist of log
rr_w_u_hosp <- exp(rr_w_sample_hosp) # retransformed uncertainty distribution

#USA Asthma Cost Calculation: https://www.cdc.gov/national-asthma-control-program/php/cost-calculator/index.html
#Source data from Medical Expenditure panel survey, Behavioral Risk Factor Surveillance System, National Survey of Children's Health, US Census
#Uses Predictive model called Random Forest to estimate medical cost 
#Age 0-5 Per-person incremental cost per asthma in 2021 dollars (averaged amongst states)
zerotofivecost_a <- 2463.13

## RSV/LRTI associated risk of additional disease 0-6 mo
#Pneumonia after RSV bronchiolitis
pn_05 <- 1.41
pn_UL_05 <- 1.59
pn_LL_05 <- 1.24
#distance - -.120/.128
rr_pn05_sd <- (log(pn_UL_05) - log(pn_LL_05))/ (1.96*2) # standard deviation
rr_pn05_sample <- rnorm(trials, log(pn_05), rr_pn05_sd) # normal dist of log
rr_pn05_u <- exp(rr_pn05_sample) # retransformed uncertainty distribution

#Otitis media after RSV bronchiolitis
OM_05 <- 1.32
OM_UL_05 <- 1.38
OM_LL_05 <- 1.26
#distance - -.0445/.0465
rr_OM05_sd <- (log(OM_UL_05) - log(OM_LL_05))/ (1.96*2) # standard deviation
rr_OM05_sample <- rnorm(trials, log(OM_05), rr_OM05_sd) # normal dist of log
rr_OM05_u <- exp(rr_OM05_sample) # retransformed uncertainty distribution

#Inappropriate abx fills after RSV bronchiolitis
ab_05 <- 1.24
ab_UL_05 <- 1.32
ab_LL_05 <- 1.16
#distance - -.0625/.0667
rr_ab05_sd <- (log(ab_UL_05) - log(ab_LL_05))/ (1.96*2) # standard deviation
rr_ab05_sample <- rnorm(trials, log(ab_05), rr_ab05_sd) # normal dist of log
rr_ab05_u <- exp(rr_ab05_sample) # retransformed uncertainty distribution

## RSV/LRTI associated risk of additional disease 0-6 mo
#Pneumonia after RSV bronchiolitis
pn_611 <- 1.34
pn_UL_611 <- 1.59
pn_LL_611 <- 1.13
#distance - -.171/.170
rr_pn611_sd <- (log(pn_UL_611) - log(pn_LL_611))/ (1.96*2) # standard deviation
rr_pn611_sample <- rnorm(trials, log(pn_611), rr_pn611_sd) # normal dist of log
rr_pn611_u <- exp(rr_pn611_sample) # retransformed uncertainty distribution

#Otitis media after RSV bronchiolitis
OM_611 <- 1.26
OM_UL_611 <- 1.34
OM_LL_611 <- 1.18
#distance - -.0616/.0656
rr_OM611_sd <- (log(OM_UL_611) - log(OM_LL_611))/ (1.96*2) # standard deviation
rr_OM611_sample <- rnorm(trials, log(OM_611), rr_OM611_sd) # normal dist of log
rr_OM611_u <- exp(rr_OM611_sample) # retransformed uncertainty distribution

#Inappropriate abx fills after RSV bronchiolitis
ab_611 <- 1.23
ab_UL_611 <- 1.34
ab_LL_611 <- 1.14
#distance - -.0857/.0760
rr_ab611_sd <- (log(ab_UL_611) - log(ab_LL_611))/ (1.96*2) # standard deviation
rr_ab611_sample <- rnorm(trials, log(ab_611), rr_ab611_sd) # normal dist of log
rr_ab611_u <- exp(rr_ab611_sample) # retransformed uncertainty distribution

