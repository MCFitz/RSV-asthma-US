# USA Asthma Project: hospitalizations - adopted from Mali

################################################################################

# Import data from Hutton/Parameters/Functions
source("ImportHuttonData.R")
source("PARAMS_asthma.R")
source("asthmafunctionsMali.R")
source("adjustmentformultipleepisode.R")

# number of kids without RSV-LRTI associated hosp for each strategy
tot_wo_RSV_no_u <- pop_tot - (Hosps_no_df+ED_no_df+OP_no_df)
tot_wo_RSV_mAb_u <- pop_tot - (Hosps_mAb_df+ED_mAb_df+OP_mAb_df)
tot_wo_RSV_rsvPreF_u <- pop_tot - (Hosps_rsvPreF_df+ED_rsvPreF_df+OP_rsvPreF_df)

# calculate rate/prevalence of asthma among those without RSV-LRTI hosp
r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot0, rr_w_u, tot_RSV_no_u, tot_wo_RSV_no_u)

# number of asthma cases among those without RSV-LRTI
asth_wo_RSV_no_u <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u)
asth_wo_RSV_mAb_u <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u)
asth_wo_RSV_rsvPreF_u <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u)
#asth_wo_RSV_Combined_u <- asth_no_rsv_func(tot_wo_RSV_Combined_u, r_asth_norsv_u)

# number of asthma cases among those with RSV-LRTI
asth_RSV_no_u <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u, rr_w_u)
asth_RSV_mAb_u <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u, rr_w_u)
asth_RSV_rsvPreF_u <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u, rr_w_u)
#asth_RSV_Combined_u <- asth_rsv_func(tot_RSV_Combined_u, r_asth_norsv_u, rr_w_u)

# total with asthma just for hospitalized patients
tot_asth_no_u <- tot_asth_func(asth_RSV_no_u, asth_wo_RSV_no_u)
tot_asth_mAb_u <- tot_asth_func(asth_RSV_mAb_u, asth_wo_RSV_mAb_u)
tot_asth_rsvPreF_u <- tot_asth_func(asth_RSV_rsvPreF_u, asth_wo_RSV_rsvPreF_u)
#tot_asth_Combined_u <- tot_asth_func(asth_RSV_Combined_u, asth_wo_RSV_Combined_u)

# total asthma per 10,000 population
tot_asth_no_pr_u <- tot_asth_no_u / pop_tot * 10000
tot_asth_mAb_pr_u <- tot_asth_mAb_u / pop_tot * 10000
tot_asth_rsvPreF_pr_u <- tot_asth_rsvPreF_u / pop_tot * 10000
#tot_asth_Combined_pr_u <- tot_asth_Combined_u / pop_tot * 10000

# total asthma percent decrease from status quo outpatient
tot_asth_mAb_pd_u <- (tot_asth_no_u - tot_asth_mAb_u) / tot_asth_no_u * 100
tot_asth_rsvPreF_pd_u <- (tot_asth_no_u - tot_asth_rsvPreF_u) / tot_asth_no_u * 100
#tot_asth_Combined_pd_u <- (tot_asth_no_u - tot_asth_Combined_u) / tot_asth_no_u * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u)
asth_null_mAb_u <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u)
asth_null_rsvPreF_u <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u)
#asth_null_Combined_u <-asth_rsv_null_func(tot_RSV_Combined_u, r_asth_norsv_u)

# RSV-LRTI attributable asthma
att_no_u <- asth_rsv_att_func(asth_RSV_no_u, asth_null_no_u)
att_mAb_u <- asth_rsv_att_func(asth_RSV_mAb_u, asth_null_mAb_u)
att_rsvPreF_u <-asth_rsv_att_func(asth_RSV_rsvPreF_u, asth_null_rsvPreF_u)
#att_Combined_u <-asth_rsv_att_func(asth_RSV_Combined_u, asth_null_Combined_u)

# RSV-LRTI attributable asthma per 10,000 population
att_no_pr_u <- att_no_u / pop_tot * 10000
att_mAb_pr_u <- att_mAb_u / pop_tot * 10000
att_rsvPreF_pr_u <- att_rsvPreF_u / pop_tot * 10000
#att_Combined_pr_u <- att_Combined_u / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u <- (att_no_u - att_mAb_u) / att_no_u * 100
att_rsvPreF_pd_u <- (att_no_u - att_rsvPreF_u) / att_no_u * 100
#att_Combined_pd_u <- (att_no_u - att_Combined_u) / att_no_u * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u <- asth_no_rsv_func(pop_tot, r_asth_norsv_u)
all_rsv_prev_pr_u <- all_rsv_prev_u / pop_tot * 10000
all_rsv_prev_pd_u <- (tot_asth_no_u - all_rsv_prev_u) / tot_asth_no_u * 100

# CI work
#Figure 1
# RSV LRTI Cases Hospitalization 1 yr
quantile(tot_RSV_no_u, probs = c(0.05, 0.95))
quantile(tot_RSV_mAb_u, probs = c(0.05, 0.95))
quantile(tot_RSV_rsvPreF_u, probs = c(0.05, 0.95))
# Percent decrease from status quo
quantile((tot_RSV_no_u-tot_RSV_mAb_u)/tot_RSV_no_u, probs = c(0.05, 0.95))
quantile((tot_RSV_no_u-tot_RSV_rsvPreF_u)/tot_RSV_no_u, probs = c(0.05, 0.95))
# Total asthma cases by intervention
quantile(tot_asth_no_u, probs = c(0.05, 0.95))
quantile(tot_asth_mAb_u, probs = c(0.05, 0.95))
quantile(tot_asth_rsvPreF_u, probs = c(0.05, 0.95))
quantile(all_rsv_prev_u, probs = c(0.05, 0.95))
# Difference in outcomes
quantile(tot_asth_no_u-tot_asth_mAb_u, probs = c(0.05, 0.95))
quantile(tot_asth_no_u-tot_asth_rsvPreF_u, probs = c(0.05, 0.95))
quantile(tot_asth_no_u-all_rsv_prev_u, probs = c(0.05, 0.95))
# Total Asthma per 10,000 
quantile(tot_asth_no_pr_u, probs = c(0.05, 0.95))
quantile(tot_asth_mAb_pr_u, probs = c(0.05, 0.95))
quantile(tot_asth_rsvPreF_pr_u, probs = c(0.05, 0.95))
quantile(all_rsv_prev_pr_u, probs = c(0.05, 0.95))
# Total Asthma percent reduction
quantile(tot_asth_mAb_pd_u, probs = c(0.05, 0.95))
quantile(tot_asth_rsvPreF_pd_u, probs = c(0.05, 0.95))
quantile(all_rsv_prev_pd_u, probs = c(0.05, 0.95))
# RSV Attributable asthma
quantile(att_no_u, probs = c(0.05, 0.95))
quantile(att_mAb_u, probs = c(0.05, 0.95))
quantile(att_rsvPreF_u, probs = c(0.05, 0.95))
# Absoklute case reduction from no intervention
quantile(att_no_u-att_mAb_u, probs = c(0.05, 0.95))
quantile(att_no_u-att_rsvPreF_u, probs = c(0.05, 0.95))
quantile(att_no_u, probs = c(0.05, 0.95))
# RSV Attributable asthma per 10,000
quantile(att_no_pr_u, probs = c(0.05, 0.95))
quantile(att_mAb_pr_u, probs = c(0.05, 0.95))
quantile(att_rsvPreF_pr_u, probs = c(0.05, 0.95))
# RSV Attributable asthma percent decrease
quantile(att_mAb_pd_u, probs = c(0.05, 0.95))
quantile(att_rsvPreF_pd_u, probs = c(0.05, 0.95))

att_no_u






## MALI TRANSFORMATION
# Transform data structure to be rows = trials, columns = age in months - 1 row each for UL, LL, PE
#unique_ages <- unique(hosps_u_df$Age)

#hosps_age_no <- hosps_no_u_df %>% 
#  filter(Age == unique_ages[1]) %>% 
#  select(value)
#names(hosps_age_no) <- paste0(names(hosps_age_no), "_age", unique_ages[1])
#for (idx in 2:length(unique_ages)) {
#  temp <- hosps_no_u_df %>% 
#    filter(Age == unique_ages[idx]) %>% 
#    select(value)
#  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
#  hosps_age_no <- cbind(hosps_age_no, 
#                        temp)
#}

#hosps_age_mAb <- hosps_mAb_u_df %>% 
#  filter(Age == unique_ages[1]) %>% 
#  select(value)
#names(hosps_age_mAb) <- paste0(names(hosps_age_mAb), "_age", unique_ages[1])
#for (idx in 2:length(unique_ages)) {
#  temp2 <- hosps_mAb_u_df %>% 
#    filter(Age == unique_ages[idx]) %>% 
#    select(value)
#  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
#  hosps_age_mAb <- cbind(hosps_age_mAb, 
#                          temp2)
#}


#hosps_age_rsvPreF <- hosps_rsvPreF_u_df %>% 
#  filter(Age == unique_ages[1]) %>% 
#  select(value)
#names(hosps_age_rsvPreF) <- paste0(names(hosps_age_rsvPreF), "_age", unique_ages[1])
#for (idx in 2:length(unique_ages)) {
#  temp3 <- hosps_rsvPreF_u_df %>% 
#    filter(Age == unique_ages[idx]) %>% 
#    select(value)
#  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
#  hosps_age_rsvPreF <- cbind(hosps_age_rsvPreF, 
#                               temp3)
#}

#hosps_age_Combined <- hosps_Combined_u_df %>% 
#  filter(Age == unique_ages[1]) %>% 
#  select(value)
#names(hosps_age_Combined) <- paste0(names(hosps_age_Combined), "_age", unique_ages[1])
#for (idx in 2:length(unique_ages)) {
#  temp3 <- hosps_Combined_u_df %>% 
#    filter(Age == unique_ages[idx]) %>% 
#    select(value)
#  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
#  hosps_age_Combined <- cbind(hosps_age_Combined, 
#                             temp3)
#}


# Three estimates - PE, UL, LL while also setting UL and LL on asthma prevention by intervention
# Transform data structure to be rows = trials, columns = age in months - 1 row each for UL, LL, PE
# Mali transformation as below

#hosps_age_no <- as.data.frame(t(hosps_no_u_df[,4:6]))
#nms_no <- as.data.frame(t(hosps_no_u_df$Age))
#hosps_age_no <- setNames(hosps_age_no,nms_no)

#hosps_age_mAb <- as.data.frame(t(hosps_mAb_u_df[,4:6]))
#nms_mAb <- as.data.frame(t(hosps_mAb_u_df$Age))
#hosps_age_mAb <- setNames(hosps_age_mAb,nms_mAb)

#hosps_age_rsvPreF <- as.data.frame(t(hosps_rsvPreF_u_df[,4:6]))
#nms_rsvPreF <- as.data.frame(t(hosps_rsvPreF_u_df$Age))
#hosps_age_rsvPreF <- setNames(hosps_age_rsvPreF,nms_rsvPreF)

#hosps_age_Combined <- as.data.frame(t(hosps_Combined_u_df[,4:6]))
#nms_Combined <- as.data.frame(t(hosps_Combined_u_df$Age))
#hosps_age_Combined <- setNames(hosps_age_Combined,nms_Combined)

# Bin to age categories
#age_cats <- c("0-<6", "6-<12")

#hosps_no_agebin <- cbind(rowSums(hosps_age_no[1, 1:6]), rowSums(hosps_age_no[1, 7:12]))
#colnames(hosps_no_agebin) <- age_cats

#hosps_mAb_agebin <- cbind(rowSums(hosps_age_mAb[1, 1:6]), rowSums(hosps_age_mAb[1, 7:12]))
#colnames(hosps_mAb_agebin) <- age_cats

#hosps_rsvPreF_agebin <- cbind(rowSums(hosps_age_rsvPreF[1, 1:6]), rowSums(hosps_age_rsvPreF[1, 7:12]))
#colnames(hosps_rsvPreF_agebin) <- age_cats

#hosps_Combined_agebin <- cbind(rowSums(hosps_age_Combined[1, 1:6]), rowSums(hosps_age_Combined[1, 7:12]))
#colnames(hosps_Combined_agebin) <- age_cats

# total hosps
#hosps_tot_no <- rowSums(hosps_age_no[1,])
#hosps_tot_mAb <- rowSums(hosps_age_mAb[1,])
#hosps_tot_rsvPreF <- rowSums(hosps_age_rsvPreF[1,])
#hosps_tot_Combined <- rowSums(hosps_age_Combined[1,])

# total hosps percent decrease from status quo
#hosps_pd_tot_mAb <- (hosps_tot_no - hosps_tot_mAb) / hosps_tot_no * 100
#hosps_pd_tot_rsvPreF <- (hosps_tot_no - hosps_tot_rsvPreF) / hosps_tot_no * 100
#hosps_pd_tot_Combined <- (hosps_tot_no - hosps_tot_Combined) / hosps_tot_no * 100

# hosps percent decrease from status quo, with age bins
#hosps_pd_mAb <- (hosps_no_agebin - hosps_mAb_agebin) / hosps_no_agebin * 100
#hosps_pd_rsvPreF <- (hosps_no_agebin - hosps_rsvPreF_agebin) / hosps_no_agebin * 100
#hosps_pd_Combined <- (hosps_no_agebin - hosps_Combined_agebin) / hosps_no_agebin * 100

## point estimate calculations, binned into 6 mo period
#sum(Hosps_no_df[1:6,4])
#sum(Hosps_no_df[7:12,4])
#sum(Hosps_mAb_df[1:6,4])
#sum(Hosps_mAb_df[7:12,4])
#sum(Hosps_rsvPreF_df[1:6,4])
#sum(Hosps_rsvPreF_df[7:12,4])
#sum(Hosps_Combined_df[1:6,4])
#sum(Hosps_Combined_df[7:12,4])

# adjust number of Hospitalizations to account for all-cause mortality out to 6 years
#tot_hosps_no_u <- mort_adj_func(rowSums(hosps_age_no[1,]), U5 = U5_mort, U9 = U9_mort)

# adjust number of Hospitalizations to account for all-cause mortality out to 6 years
#tot_hosps_no_u <- mort_adj_func(rowSums(hosps_age_no[1,]), U5 = U5_mort, U9 = U9_mort)
#tot_hosps_mAb_u <- mort_adj_func(rowSums(hosps_age_mAb[1,]), U5 = U5_mort, U9 = U9_mort)
#tot_hosps_rsvPreF_u <-mort_adj_func(rowSums(hosps_age_rsvPreF[1,]), U5 = U5_mort, U9 = U9_mort)
#tot_hosps_Combined_u <-mort_adj_func(rowSums(hosps_age_Combined[1,]), U5 = U5_mort, U9 = U9_mort)

#tot_hosps_no_u <- hosps_tot_no
#tot_hosps_mAb_u <- hosps_tot_mAb
#tot_hosps_rsvPreF_u <- hosps_tot_rsvPreF
#tot_hosps_Combined_u <- hosps_tot_Combined

#tot_hosps_no_uLCL <- rowSums(hosps_age_no[2,])
#tot_hosps_mAb_uLCL <- rowSums(hosps_age_mAb[2,])
#tot_hosps_rsvPreF_uLCL <- rowSums(hosps_age_rsvPreF[2,])
#tot_hosps_Combined_uLCL <- rowSums(hosps_age_Combined[2,])

#tot_hosps_no_uUCL <- rowSums(hosps_age_no[3,])
#tot_hosps_mAb_uUCL <- rowSums(hosps_age_mAb[3,])
#tot_hosps_rsvPreF_uUCL <- rowSums(hosps_age_rsvPreF[3,])
#tot_hosps_Combined_uUCL <- rowSums(hosps_age_Combined[3,])

# number of kids surviving to age 6 without RSV-LRTI associated hosp for each strategy
#tot_wo_hosps_no_u <- pop_tot - tot_hosps_no_u
#tot_wo_hosps_mAb_u <- pop_tot - tot_hosps_mAb_u
#tot_wo_hosps_rsvPreF_u <- pop_tot - tot_hosps_rsvPreF_u
#tot_wo_hosps_Combined_u <- pop_tot - tot_hosps_Combined_u

#tot_wo_hosps_no_uLCL <- pop_tot - tot_hosps_no_uLCL
#tot_wo_hosps_mAb_uLCL <- pop_tot - tot_hosps_mAb_uLCL
#tot_wo_hosps_rsvPreF_uLCL <- pop_tot - tot_hosps_rsvPreF_uLCL
#tot_wo_hosps_Combined_uLCL <- pop_tot - tot_hosps_Combined_uLCL

#tot_wo_hosps_no_uUCL <- pop_tot - tot_hosps_no_uUCL
#tot_wo_hosps_mAb_uUCL <- pop_tot - tot_hosps_mAb_uUCL
#tot_wo_hosps_rsvPreF_uUCL <- pop_tot - tot_hosps_rsvPreF_uUCL
#tot_wo_hosps_Combined_uUCL <- pop_tot - tot_hosps_Combined_uUCL

#r_asth_norsv_uLCL <- prev_no_rsv_func(prev_tot_u, pop_tot, rr_w_u_hosp, tot_hosps_no_uLCL, tot_wo_hosps_no_uLCL)

#r_asth_norsv_uUCL <- prev_no_rsv_func(prev_tot_u, pop_tot, rr_w_u_hosp, tot_hosps_no_uUCL, tot_wo_hosps_no_uUCL)

#asth_wo_hosps_no_uLCL <- asth_no_rsv_func(tot_wo_hosps_no_uLCL, r_asth_norsv_uLCL)
#asth_wo_hosps_mAb_uLCL <- asth_no_rsv_func(tot_wo_hosps_mAb_uLCL, r_asth_norsv_uLCL)
#asth_wo_hosps_rsvPreF_uLCL <- asth_no_rsv_func(tot_wo_hosps_rsvPreF_uLCL, r_asth_norsv_uLCL)
#asth_wo_hosps_Combined_uLCL <- asth_no_rsv_func(tot_wo_hosps_Combined_uLCL, r_asth_norsv_uLCL)

#asth_wo_hosps_no_uUCL <- asth_no_rsv_func(tot_wo_hosps_no_uUCL, r_asth_norsv_uUCL)
#asth_wo_hosps_mAb_uUCL <- asth_no_rsv_func(tot_wo_hosps_mAb_uUCL, r_asth_norsv_uUCL)
#asth_wo_hosps_rsvPreF_uUCL <- asth_no_rsv_func(tot_wo_hosps_rsvPreF_uUCL, r_asth_norsv_uUCL)
#asth_wo_hosps_Combined_uUCL <- asth_no_rsv_func(tot_wo_hosps_Combined_uUCL, r_asth_norsv_uUCL)

#asth_hosps_no_uLCL <- asth_rsv_func(tot_hosps_no_uLCL, r_asth_norsv_uLCL, rr_w_u_hosp)
#asth_hosps_mAb_uLCL <- asth_rsv_func(tot_hosps_mAb_uLCL, r_asth_norsv_uLCL, rr_w_u_hosp)
#asth_hosps_rsvPreF_uLCL <- asth_rsv_func(tot_hosps_rsvPreF_uLCL, r_asth_norsv_uLCL, rr_w_u_hosp)
#asth_hosps_Combined_uLCL <- asth_rsv_func(tot_hosps_Combined_uLCL, r_asth_norsv_uLCL, rr_w_u_hosp)

#asth_hosps_no_uUCL <- asth_rsv_func(tot_hosps_no_uUCL, r_asth_norsv_uUCL, rr_w_u_hosp)
#asth_hosps_mAb_uUCL <- asth_rsv_func(tot_hosps_mAb_uUCL, r_asth_norsv_uUCL, rr_w_u_hosp)
#asth_hosps_rsvPreF_uUCL <- asth_rsv_func(tot_hosps_rsvPreF_uUCL, r_asth_norsv_uUCL, rr_w_u_hosp)
#asth_hosps_Combined_uUCL <- asth_rsv_func(tot_hosps_Combined_uUCL, r_asth_norsv_uUCL, rr_w_u_hosp)

#tot_asth_no_uLCL <- tot_asth_func(asth_hosps_no_uLCL, asth_wo_hosps_no_uLCL)
#tot_asth_mAb_uLCL <- tot_asth_func(asth_hosps_mAb_uLCL, asth_wo_hosps_mAb_uLCL)
#tot_asth_rsvPreF_uLCL <- tot_asth_func(asth_hosps_rsvPreF_uLCL, asth_wo_hosps_rsvPreF_uLCL)
#tot_asth_Combined_uLCL <- tot_asth_func(asth_hosps_Combined_uLCL, asth_wo_hosps_Combined_uLCL)

#tot_asth_no_uUCL <- tot_asth_func(asth_hosps_no_uUCL, asth_wo_hosps_no_uUCL)
#tot_asth_mAb_uUCL <- tot_asth_func(asth_hosps_mAb_uUCL, asth_wo_hosps_mAb_uUCL)
#tot_asth_rsvPreF_uUCL <- tot_asth_func(asth_hosps_rsvPreF_uUCL, asth_wo_hosps_rsvPreF_uUCL)
#tot_asth_Combined_uUCL <- tot_asth_func(asth_hosps_Combined_uUCL, asth_wo_hosps_Combined_uUCL)

#tot_asth_no_pr_uUCL <- tot_asth_no_uUCL / pop_tot * 10000
#tot_asth_mAb_pr_uUCL <- tot_asth_mAb_uUCL / pop_tot * 10000
#tot_asth_rsvPreF_pr_uUCL <- tot_asth_rsvPreF_uUCL / pop_tot * 10000
#tot_asth_Combined_pr_uUCL <- tot_asth_Combined_uUCL / pop_tot * 10000

#tot_asth_no_pr_uLCL <- tot_asth_no_uLCL / pop_tot * 10000
#tot_asth_mAb_pr_uLCL <- tot_asth_mAb_uLCL / pop_tot * 10000
#tot_asth_rsvPreF_pr_uLCL <- tot_asth_rsvPreF_uLCL / pop_tot * 10000
#tot_asth_Combined_pr_uLCL <- tot_asth_Combined_uLCL / pop_tot * 10000

#tot_asth_mAb_pd_uLCL <- (tot_asth_no_uLCL - tot_asth_mAb_uLCL) / tot_asth_no_uLCL * 100
#tot_asth_rsvPreF_pd_uLCL <- (tot_asth_no_uLCL - tot_asth_rsvPreF_uLCL) / tot_asth_no_uLCL * 100
#tot_asth_Combined_pd_uLCL <- (tot_asth_no_uLCL - tot_asth_Combined_uLCL) / tot_asth_no_uLCL * 100

#tot_asth_mAb_pd_uUCL <- (tot_asth_no_uUCL - tot_asth_mAb_uUCL) / tot_asth_no_uUCL * 100
#tot_asth_rsvPreF_pd_uUCL <- (tot_asth_no_uUCL - tot_asth_rsvPreF_uUCL) / tot_asth_no_uUCL * 100
#tot_asth_Combined_pd_uUCL <- (tot_asth_no_uUCL - tot_asth_Combined_uUCL) / tot_asth_no_uUCL * 100

#asth_null_no_uLCL <- asth_rsv_null_func(tot_hosps_no_uLCL, r_asth_norsv_uLCL)
#asth_null_mAb_uLCL <- asth_rsv_null_func(tot_hosps_mAb_uLCL, r_asth_norsv_uLCL)
#asth_null_rsvPreF_uLCL <-asth_rsv_null_func(tot_hosps_rsvPreF_uLCL, r_asth_norsv_uLCL)
#asth_null_Combined_uLCL <-asth_rsv_null_func(tot_hosps_Combined_uLCL, r_asth_norsv_uLCL)

#asth_null_no_uUCL <- asth_rsv_null_func(tot_hosps_no_uUCL, r_asth_norsv_uUCL)
#asth_null_mAb_uUCL <- asth_rsv_null_func(tot_hosps_mAb_uUCL, r_asth_norsv_uUCL)
#asth_null_rsvPreF_uUCL <-asth_rsv_null_func(tot_hosps_rsvPreF_uUCL, r_asth_norsv_uUCL)
#asth_null_Combined_uUCL <-asth_rsv_null_func(tot_hosps_Combined_uUCL, r_asth_norsv_uUCL)

#att_no_uLCL <- asth_rsv_att_func(asth_hosps_no_uLCL, asth_null_no_uLCL)
#att_mAb_uLCL <- asth_rsv_att_func(asth_hosps_mAb_uLCL, asth_null_mAb_uLCL)
#att_rsvPreF_uLCL <-asth_rsv_att_func(asth_hosps_rsvPreF_uLCL, asth_null_rsvPreF_uLCL)
#att_Combined_uLCL <-asth_rsv_att_func(asth_hosps_Combined_uLCL, asth_null_Combined_uLCL)

#att_no_uUCL <- asth_rsv_att_func(asth_hosps_no_uUCL, asth_null_no_uUCL)
#att_mAb_uUCL <- asth_rsv_att_func(asth_hosps_mAb_uUCL, asth_null_mAb_uUCL)
#att_rsvPreF_uUCL <-asth_rsv_att_func(asth_hosps_rsvPreF_uUCL, asth_null_rsvPreF_uUCL)
#att_Combined_uUCL <-asth_rsv_att_func(asth_hosps_Combined_uUCL, asth_null_Combined_uUCL)

#att_no_pr_uLCL <- att_no_uLCL / pop_tot * 10000
#att_mAb_pr_uLCL <- att_mAb_uLCL / pop_tot * 10000
#att_rsvPreF_pr_uLCL <- att_rsvPreF_uLCL / pop_tot * 10000
#att_Combined_pr_uLCL <- att_Combined_uLCL / pop_tot * 10000

#att_no_pr_uUCL <- att_no_uUCL / pop_tot * 10000
#att_mAb_pr_uUCL <- att_mAb_uUCL / pop_tot * 10000
#att_rsvPreF_pr_uUCL <- att_rsvPreF_uUCL / pop_tot * 10000
#att_Combined_pr_uUCL <- att_Combined_uUCL / pop_tot * 10000

#att_mAb_pd_uLCL <- (att_no_uLCL - att_mAb_uLCL) / att_no_uLCL * 100
#att_rsvPreF_pd_uLCL <- (att_no_uLCL - att_rsvPreF_uLCL) / att_no_uLCL * 100
#att_Combined_pd_uLCL <- (att_no_uLCL - att_Combined_uLCL) / att_no_uLCL * 100

#att_mAb_pd_uUCL <- (att_no_uUCL - att_mAb_uUCL) / att_no_uUCL * 100
#att_rsvPreF_pd_uUCL <- (att_no_uUCL - att_rsvPreF_uUCL) / att_no_uUCL * 100
#att_Combined_pd_uUCL <- (att_no_uUCL - att_Combined_uUCL) / att_no_uUCL * 100

#all_rsv_prev_uLCL <- asth_no_rsv_func(pop_tot, r_asth_norsv_uLCL)
#all_rsv_prev_pr_uLCL <- all_rsv_prev_uLCL / pop_tot * 10000
#all_rsv_prev_pd_uLCl <- (tot_asth_no_uLCL - all_rsv_prev_uLCL) / tot_asth_no_uLCL * 100

#all_rsv_prev_uUCL <- asth_no_rsv_func(pop_tot, r_asth_norsv_uUCL)
#all_rsv_prev_pr_uUCL <- all_rsv_prev_uUCL / pop_tot * 10000
#all_rsv_prev_pd_uUCL <- (tot_asth_no_uUCL - all_rsv_prev_uUCL) / tot_asth_no_uUCL * 100
