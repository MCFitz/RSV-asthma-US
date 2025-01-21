# Master Script for RSV-asthma-US analysis
# Created by Meagan Fitzpatrick and Ian Galbreath

library(tidyverse)

# Set number of trials
trials <- 1000

# Import data from Hutton
source("ImportHuttonData.R")
source("PARAMS_asthma.R")

# sum cases by intervention type
# assuming all recorded patients are mutually exclusive
# IAN_ADD please check if this is what Hutton assumed
# except hospitalization and death?
# subtract deaths from hospitalizations first?
# Outpatient episodes: Extracting just outpatient episodes of RSV-LRTI
Outpatient_df <- hdata%>%filter(Metric == "Outpatient")

#Outpatient episodes: Delineate RSV-LRTI events by intervention   
Outpatient_no_df <- Outpatient_df%>%filter(Intervention == "no intervention")
Outpatient_mAb_df <- Outpatient_df%>%filter(Intervention == "Nirsevimab")
Outpatient_rsvPreF_df <- Outpatient_df%>%filter(Intervention == "RSVpreF")
Outpatient_Combined_df <- Outpatient_df%>%filter(Intervention == "Combined")

# Outpatient episodes: sum point estimates across years and ages
# Replicate for upper and lower CI
RSV_no <- sum(Outpatient_no_df[1:12,4:15])
RSV_mAb <- sum(Outpatient_mAb_df[1:12,4:15])
RSV_mat <- sum(Outpatient_rsvPreF_df[1:12,4:15])
RSV_com <- sum(Outpatient_Combined_df[1:12,4:15])

############## ADAPTED MALI CODE
# S.5
# adjust number of outpatient/ED LRTI (total) to account for all-cause mortality out to 6 years

tot_OP_no <- mort_adj_func(RSV_no, U5 = U5_mort, U9 = U9_mort)
tot_OP_mAb <- mort_adj_func(RSV_mAb, U5 = U5_mort, U9 = U9_mort)
tot_OP_rsvPreF <-mort_adj_func(RSV_mat, U5 = U5_mort, U9 = U9_mort)
tot_OP_Combined <-mort_adj_func(RSV_comb, U5 = U5_mort, U9 = U9_mort)

# number of kids surviving to age 6 without RSV-LRTI outpatient or ED encounter for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_OP_no <- pop_tot - tot_OP_no
tot_wo_OP_mAb <- pop_tot - tot_OP_mAb
tot_wo_OP_rsvPreF <- pop_tot - tot_OP_rsvPreF
tot_wo_OP_Combined <- pop_tot - tot_OP_Combined

# number of kids surviving to age 6 without RSV-LRTI outpatient or ED encounter for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_OP_no <- pop_tot - tot_OP_no
tot_wo_OP_mAb <- pop_tot - tot_OP_mAb
tot_wo_OP_rsvPreF <- pop_tot - tot_OP_rsvPreF
tot_wo_OP_Combined <- pop_tot - tot_OP_Combined

# calculate rate/prevalence of asthma among those without RSV-LRTI outpatient or ED encounter
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, rr_w, tot_OP_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI hospitalization or ED encounter
asth_wo_OP_no <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv)
asth_wo_OP_mAb <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv)
asth_wo_OP_rsvPreF <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv)
asth_wo_OP_Combined <- asth_no_rsv_func(tot_wo_OP_Combined, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI outpatient or ED encounter
asth_OP_no <- asth_rsv_func(tot_OP_no, r_asth_norsv, rr_w)
asth_OP_mAb <- asth_rsv_func(tot_OP_ED_mAb, r_asth_norsv, rr_w)
asth_OP_rsvPreF <- asth_rsv_func(tot_OP_rsvPreF, r_asth_norsv, rr_w)
asth_OP_Combined <- asth_rsv_func(tot_OP_Combined, r_asth_norsv, rr_w)

# total with asthma
tot_asth_no <- tot_asth_func(asth_OP_no, asth_wo_OP_no)
tot_asth_mAb <- tot_asth_func(asth_OP_mAb, asth_wo_OP_mAb)
tot_asth_rsvPreF <-tot_asth_func(asth_OP_rsvPreF, asth_wo_OP_rsvPreF)
tot_asth_Combined <-tot_asth_func(asth_OP_Combined, asth_wo_OP_Combined)

# number of asthma cases among those with RSV-LRTI  outpatient or ED encounter had they not been infected
asth_null_no <- asth_rsv_null_func(tot_OP_no, r_asth_norsv)
asth_null_mAb <- asth_rsv_null_func(tot_OP_mAb, r_asth_norsv)
asth_null_rsvPreF <-asth_rsv_null_func(tot_OP_rsvPreF, r_asth_norsv)
asth_null_Combined <-asth_rsv_null_func(tot_OP_Combined, r_asth_norsv)

# RSV-LRTI outpatient or ED encoutner attributable asthma
att_no <- asth_rsv_att_func(asth_OP_no, asth_null_no)
att_mAb <- asth_rsv_att_func(asth_OP_mAb, asth_null_mAb)
att_rsvPreF <-asth_rsv_att_func(asth_OP_rsvPreF, asth_null_rsvPreF)
att_Combined <-asth_rsv_att_func(asth_OP_Combined, asth_null_Combined)

# total recurrent wheeze/asthma, all RSV LRTI outpatient or ED encounter  prevented
asth_all_RSV_prev <- asth_null_no + asth_wo_OP_no