# USA Asthma Project: hospitalizations - adopted from Mali

################################################################################

hdatahosp <- read.csv("healthoutcomeshosp.csv")

# Extract just hospitalizations
hosps_u_df <- hdatahosp%>%filter(Metric == "Hospitalizations")

hosps_no_u_df <- hosps_u_df%>%filter(Intervention == "no intervention")
hosps_mAb_u_df <- hosps_u_df%>%filter(Intervention == "Nirsevimab")
hosps_rsvPreF_u_df <- hosps_u_df%>%filter(Intervention == "RSVpreF")
hosps_Combined_u_df <- hosps_u_df%>%filter(Intervention == "Combined")

# Transform data structure to be rows = trials, columns = age in months
unique_ages <- unique(hosps_u_df$Age)

hosps_age_no <- hosps_no_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_no) <- paste0(names(hosps_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- hosps_no_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  hosps_age_no <- cbind(hosps_age_no, 
                        temp)
}


hosps_age_mAb <- hosps_mAb_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_mAb) <- paste0(names(hosps_age_mAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- hosps_mAb_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  hosps_age_mAb <- cbind(hosps_age_mAb, 
                          temp2)
}


hosps_age_rsvPreF <- hosps_rsvPreF_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_rsvPreF) <- paste0(names(hosps_age_rsvPreF), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- hosps_rsvPreF_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  hosps_age_rsvPreF <- cbind(hosps_age_rsvPreF, 
                               temp3)
}

hosps_age_Combined <- hosps_Combined_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_Combined) <- paste0(names(hosps_age_Combined), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- hosps_Combined_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  hosps_age_Combined <- cbind(hosps_age_Combined, 
                             temp3)
}

# Bin to age categories
age_cats <- c("0-<6", "6-<12")

hosps_no_agebin <- cbind(rowSums(hosps_age_no[, 1:6]), rowSums(hosps_age_no[, 7:12]))
colnames(hosps_no_agebin) <- age_cats

hosps_mAb_agebin <- cbind(rowSums(hosps_age_mAb[, 1:6]), rowSums(hosps_age_mAb[, 7:12]))
colnames(hosps_mAb_agebin) <- age_cats

hosps_rsvPreF_agebin <- cbind(rowSums(hosps_age_rsvPreF[, 1:6]), rowSums(hosps_age_rsvPreF[, 7:12]))
colnames(hosps_rsvPreF_agebin) <- age_cats

hosps_Combined_agebin <- cbind(rowSums(hosps_age_Combined[, 1:6]), rowSums(hosps_age_Combined[, 7:12]))
colnames(hosps_Combined_agebin) <- age_cats

# total hosps
hosps_tot_no <- rowSums(hosps_age_no)
hosps_tot_mAb <- rowSums(hosps_age_mAb)
hosps_tot_rsvPreF <- rowSums(hosps_age_rsvPreF)
hosps_tot_Combined <- rowSums(hosps_age_Combined)

# total hosps percent decrease from status quo
hosps_pd_tot_mAb <- (hosps_tot_no - hosps_tot_mAb) / hosps_tot_no * 100
hosps_pd_tot_rsvPreF <- (hosps_tot_no - hosps_tot_rsvPreF) / hosps_tot_no * 100
hosps_pd_tot_Combined <- (hosps_tot_no - hosps_tot_Combined) / hosps_tot_no * 100

# hosps percent decrease from status quo, with age bins
hosps_pd_mAb <- (hosps_no_agebin - hosps_mAb_agebin) / hosps_no_agebin * 100
hosps_pd_rsvPreF <- (hosps_no_agebin - hosps_rsvPreF_agebin) / hosps_no_agebin * 100
hosps_pd_Combined <- (hosps_no_agebin - hosps_Combined_agebin) / hosps_no_agebin * 100

## point estimate calculations
hosps_df <- hdata%>%filter(Metric == "Hospitalizations")
hosps_no_df <- hosps_df%>%filter(Intervention == "no intervention")
hosps_mAb_df <- hosps_df%>%filter(Intervention == "Nirsevimab")
hosps_rsvPreF_df <- hosps_df%>%filter(Intervention == "RSVpreF")
hosps_Combined_df <- hosps_df%>%filter(Intervention == "Combined")

sum(hosps_no_df[1:6,4])
sum(hosps_no_df[7:12,4])

sum(hosps_mAb_df[1:6,4])
sum(hosps_mAb_df[7:12,4])

sum(hosps_rsvPreF_df[1:6,4])
sum(hosps_rsvPreF_df[7:12,4])

sum(hosps_Combined_df[1:6,4])
sum(hosps_Combined_df[7:12,4])

# adjust number of Hospitalizations to account for all-cause mortality out to 6 years
tot_hosps_no_u <- mort_adj_func(rowSums(hosps_age_no), U5 = U5_mort, U9 = U9_mort)

# adjust number of Hospitalizations to account for all-cause mortality out to 6 years
tot_hosps_no_u <- mort_adj_func(rowSums(hosps_age_no), U5 = U5_mort, U9 = U9_mort)
tot_hosps_mAb_u <- mort_adj_func(rowSums(hosps_age_mAb), U5 = U5_mort, U9 = U9_mort)
tot_hosps_rsvPreF_u <-mort_adj_func(rowSums(hosps_age_rsvPreF), U5 = U5_mort, U9 = U9_mort)
tot_hosps_Combined_u <-mort_adj_func(rowSums(hosps_age_Combined), U5 = U5_mort, U9 = U9_mort)

# number of kids surviving to age 6 without RSV-LRTI associated hosp for each strategy
tot_wo_hosps_no_u <- pop_tot - tot_hosps_no_u
tot_wo_hosps_mAb_u <- pop_tot - tot_hosps_mAb_u
tot_wo_hosps_rsvPreF_u <- pop_tot - tot_hosps_rsvPreF_u
tot_wo_hosps_Combined_u <- pop_tot - tot_hosps_Combined_u

# calculate rate/prevalence of asthma among those without RSV-LRTI hosp
r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot, rr_w_u, tot_hosps_no_u, tot_wo_hosps_no_u)

# adjust number of Hospitalizations to account for all-cause mortality out to 6 years
tot_hosps_no_u <- mort_adj_func(rowSums(hosps_age_no), U5 = U5_mort, U9 = U9_mort)
tot_hosps_mAb_u <- mort_adj_func(rowSums(hosps_age_mAb), U5 = U5_mort, U9 = U9_mort)
tot_hosps_rsvPreF_u <-mort_adj_func(rowSums(hosps_age_rsvPreF), U5 = U5_mort, U9 = U9_mort)
tot_hosps_Combined_u <-mort_adj_func(rowSums(hosps_age_Combined), U5 = U5_mort, U9 = U9_mort)

# number of kids surviving to age 6 without RSV-LRTI associated hosp for each strategy
tot_wo_hosps_no_u <- pop_tot - tot_hosps_no_u
tot_wo_hosps_mAb_u <- pop_tot - tot_hosps_mAb_u
tot_wo_hosps_rsvPreF_u <- pop_tot - tot_hosps_rsvPreF_u
tot_wo_hosps_Combined_u <- pop_tot - tot_hosps_Combined_u

# calculate rate/prevalence of asthma among those without RSV-LRTI hosp
r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot, rr_w_u, tot_hosps_no_u, tot_wo_hosps_no_u)

# number of asthma cases among those without RSV-LRTI
asth_wo_hosps_no_u <- asth_no_rsv_func(tot_wo_hosps_no_u, r_asth_norsv_u)
asth_wo_hosps_mAb_u <- asth_no_rsv_func(tot_wo_hosps_mAb_u, r_asth_norsv_u)
asth_wo_hosps_rsvPreF_u <- asth_no_rsv_func(tot_wo_hosps_rsvPreF_u, r_asth_norsv_u)
asth_wo_hosps_Combined_u <- asth_no_rsv_func(tot_wo_hosps_Combined_u, r_asth_norsv_u)

# number of asthma cases among those with RSV-LRTI
asth_hosps_no_u <- asth_rsv_func(tot_hosps_no_u, r_asth_norsv_u, rr_w_u)
asth_hosps_mAb_u <- asth_rsv_func(tot_hosps_mAb_u, r_asth_norsv_u, rr_w_u)
asth_hosps_rsvPreF_u <- asth_rsv_func(tot_hosps_rsvPreF_u, r_asth_norsv_u, rr_w_u)
asth_hosps_Combined_u <- asth_rsv_func(tot_hosps_Combined_u, r_asth_norsv_u, rr_w_u)

# total with asthma just for hospitalized patients
tot_asth_no_u <- tot_asth_func(asth_hosps_no_u, asth_wo_hosps_no_u)
tot_asth_mAb_u <- tot_asth_func(asth_hosps_mAb_u, asth_wo_hosps_mAb_u)
tot_asth_rsvPreF_u <- tot_asth_func(asth_hosps_rsvPreF_u, asth_wo_hosps_rsvPreF_u)
tot_asth_Combined_u <- tot_asth_func(asth_hosps_Combined_u, asth_wo_hosps_Combined_u)

# total asthma per 10,000 population
tot_asth_no_pr_u <- tot_asth_no_u / pop_tot * 10000
tot_asth_mAb_pr_u <- tot_asth_mAb_u / pop_tot * 10000
tot_asth_rsvPreF_pr_u <- tot_asth_rsvPreF_u / pop_tot * 10000
tot_asth_Combined_pr_u <- tot_asth_Combined_u / pop_tot * 10000

# total asthma percent decrease from status quo outpatient
tot_asth_mAb_pd_u <- (tot_asth_no_u - tot_asth_mAb_u) / tot_asth_no_u * 100
tot_asth_rsvPreF_pd_u <- (tot_asth_no_u - tot_asth_rsvPreF_u) / tot_asth_no_u * 100
tot_asth_Combined_pd_u <- (tot_asth_no_u - tot_asth_Combined_u) / tot_asth_no_u * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u <- asth_rsv_null_func(tot_hosps_no_u, r_asth_norsv_u)
asth_null_mAb_u <- asth_rsv_null_func(tot_hosps_mAb_u, r_asth_norsv_u)
asth_null_rsvPreF_u <-asth_rsv_null_func(tot_hosps_rsvPreF_u, r_asth_norsv_u)
asth_null_Combined_u <-asth_rsv_null_func(tot_hosps_Combined_u, r_asth_norsv_u)

# RSV-LRTI attributable asthma
att_no_u <- asth_rsv_att_func(asth_hosps_no_u, asth_null_no_u)
att_mAb_u <- asth_rsv_att_func(asth_hosps_mAb_u, asth_null_mAb_u)
att_rsvPreF_u <-asth_rsv_att_func(asth_hosps_rsvPreF_u, asth_null_rsvPreF_u)
att_Combined_u <-asth_rsv_att_func(asth_hosps_Combined_u, asth_null_Combined_u)

# RSV-LRTI attributable asthma per 10,000 population
att_no_pr_u <- att_no_u / pop_tot * 10000
att_mAb_pr_u <- att_mAb_u / pop_tot * 10000
att_rsvPreF_pr_u <- att_rsvPreF_u / pop_tot * 10000
att_Combined_pr_u <- att_Combined_u / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u <- (att_no_u - att_mAb_u) / att_no_u * 100
att_rsvPreF_pd_u <- (att_no_u - att_rsvPreF_u) / att_no_u * 100
att_Combined_pd_u <- (att_no_u - att_Combined_u) / att_no_u * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u <- asth_no_rsv_func(pop_tot, r_asth_norsv_u)
all_rsv_prev_pr_u <- all_rsv_prev_u / pop_tot * 10000
all_rsv_prev_pd_u <- (tot_asth_no_u - all_rsv_prev_u) / tot_asth_no_u * 100

