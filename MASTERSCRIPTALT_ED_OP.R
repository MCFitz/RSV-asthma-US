# Master Script for RSV-asthma-US analysis - ALTERNATIVE SCENARIO W/ OUTPATIENT AND ED VISITS ADDED
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
# Outpatient/ED episodes: Extracting just outpatient/ED episodes of RSV-LRTI into individualized DF
Outpatient_df <- hdata%>%filter(Metric == "Outpatient")
ED_df <- hdata%>%filter(Metric == "ED")
#Outpatient/ED episodes: Delineate RSV-LRTI events by intervention   
Outpatient_no_df <- Outpatient_df%>%filter(Intervention == "no intervention")
Outpatient_mAb_df <- Outpatient_df%>%filter(Intervention == "Nirsevimab")
Outpatient_rsvPreF_df <- Outpatient_df%>%filter(Intervention == "RSVpreF")
Outpatient_Combined_df <- Outpatient_df%>%filter(Intervention == "Combined")
ED_no_df <- ED_df%>%filter(Intervention == "no intervention")
ED_mAb_df <- ED_df%>%filter(Intervention == "Nirsevimab")
ED_rsvPreF_df <- ED_df%>%filter(Intervention == "RSVpreF")
ED_Combined_df <- ED_df%>%filter(Intervention == "Combined")
#Outpatient episodes: sum point estimates 
num_OP_ED_no <- sum(Outpatient_no_df[1:12,4:15])+sum(ED_no_df[1:12,4:15])
num_OP_ED_mAb <- sum(Outpatient_mAb_df[1:12,4:15])+sum(ED_mAb_df[1:12,4:15])
num_OP_ED_rsvPreF <- sum(Outpatient_rsvPreF_df[1:12,4:15])+sum(ED_rsvPreF_df[1:12,4:15])
num_OP_ED_combined <- sum(Outpatient_Combined_df[1:12,4:15])+sum(ED_Combined_df[1:12,4:15])

############## ADAPTED MALI CODE
# S.5
# adjust number of outpatient/ED LRTI (total) to account for all-cause mortality out to 6 years

tot_OP_ED_no <- mort_adj_func(num_OP_ED_no, U5 = U5_mort, U9 = U9_mort)
tot_OP_ED_mAb <- mort_adj_func(num_OP_ED_mAb, U5 = U5_mort, U9 = U9_mort)
tot_OP_ED_rsvPreF <-mort_adj_func(num_OP_ED_rsvPreF, U5 = U5_mort, U9 = U9_mort)
tot_OP_ED_Combined <-mort_adj_func(num_OP_ED_combined, U5 = U5_mort, U9 = U9_mort)

# number of kids surviving to age 6 without RSV-LRTI outpatient or ED encounter for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_OP_ED_no <- pop_tot - tot_OP_ED_no
tot_wo_OP_ED_mAb <- pop_tot - tot_OP_ED_mAb
tot_wo_OP_ED_rsvPreF <- pop_tot - tot_OP_ED_rsvPreF
tot_wo_OP_ED_Combined <- pop_tot - tot_OP_ED_Combined

# number of kids surviving to age 6 without RSV-LRTI outpatient or ED encounter for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_OP_ED_no <- pop_tot - tot_OP_ED_no
tot_wo_OP_ED_mAb <- pop_tot - tot_OP_ED_mAb
tot_wo_OP_ED_rsvPreF <- pop_tot - tot_OP_ED_rsvPreF
tot_wo_OP_ED_Combined <- pop_tot - tot_OP_ED_Combined

# calculate rate/prevalence of asthma among those without RSV-LRTI outpatient or ED encounter
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, rr_w, tot_OP_ED_no, tot_wo_OP_ED_no)

# number of asthma cases among those without RSV-LRTI hospitalization or ED encounter
asth_wo_OP_ED_no <- asth_no_rsv_func(tot_wo_OP_ED_no, r_asth_norsv)
asth_wo_OP_ED_mAb <- asth_no_rsv_func(tot_wo_OP_ED_mAb, r_asth_norsv)
asth_wo_OP_ED_rsvPreF <- asth_no_rsv_func(tot_wo_OP_ED_rsvPreF, r_asth_norsv)
asth_wo_OP_ED_Combined <- asth_no_rsv_func(tot_wo_OP_ED_Combined, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI outpatient or ED encounter
asth_OP_ED_no <- asth_rsv_func(tot_OP_ED_no, r_asth_norsv, rr_w)
asth_OP_ED_mAb <- asth_rsv_func(tot_OP_ED_mAb, r_asth_norsv, rr_w)
asth_OP_ED_rsvPreF <- asth_rsv_func(tot_OP_ED_rsvPreF, r_asth_norsv, rr_w)
asth_OP_ED_Combined <- asth_rsv_func(tot_OP_ED_Combined, r_asth_norsv, rr_w)

# total with asthma
tot_asth_no <- tot_asth_func(asth_OP_ED_no, asth_wo_OP_ED_no)
tot_asth_mAb <- tot_asth_func(asth_OP_ED_mAb, asth_wo_OP_ED_mAb)
tot_asth_rsvPreF <-tot_asth_func(asth_OP_ED_rsvPreF, asth_wo_OP_ED_rsvPreF)
tot_asth_Combined <-tot_asth_func(asth_OP_ED_Combined, asth_wo_OP_ED_Combined)

# number of asthma cases among those with RSV-LRTI  outpatient or ED encounter had they not been infected
asth_null_no <- asth_rsv_null_func(tot_OP_ED_no, r_asth_norsv)
asth_null_mAb <- asth_rsv_null_func(tot_OP_ED_mAb, r_asth_norsv)
asth_null_rsvPreF <-asth_rsv_null_func(tot_OP_ED_rsvPreF, r_asth_norsv)
asth_null_Combined <-asth_rsv_null_func(tot_OP_ED_Combined, r_asth_norsv)

# RSV-LRTI outpatient or ED encoutner attributable asthma
att_no <- asth_rsv_att_func(asth_OP_ED_no, asth_null_no)
att_mAb <- asth_rsv_att_func(asth_OP_ED_mAb, asth_null_mAb)
att_rsvPreF <-asth_rsv_att_func(asth_OP_ED_rsvPreF, asth_null_rsvPreF)
att_Combined <-asth_rsv_att_func(asth_OP_ED_Combined, asth_null_Combined)

# total recurrent wheeze/asthma, all RSV LRTI outpatient or ED encounter  prevented
asth_all_RSV_prev <- asth_null_no + asth_wo_OP_ED_no

# S.6
#source("uncertaintyLRTIUSA.R")

# S.7
#source("uncertaintyhosp.R")
#source("uncertaintydeaths.R")
#source("asthmacredibleintervals.R")

################################################################################

# Mali Asthma Project

################################################################################
# Step 1: load in health outcomes data for base case and uncertainty analysis

# Step 2: load asthma params

# Step 3: create functions for asthma calcs

# Step 4: transform data to desired structures

# Step 5: apply asthma functions to obtain output

# Step 6: repeat for uncertainty analysis

# Step 7: obtain 95% credible intervals for all calculations
################################################################################
