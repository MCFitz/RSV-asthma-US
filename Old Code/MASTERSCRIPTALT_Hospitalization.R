# Master Script for RSV-asthma-US analysis - ALTERNATIVE CONSIDERATION W/ HOSPS
# Created by Meagan Fitzpatrick and Ian Galbreath

# Import data from Hutton/Parameters/Functions
source("ImportHuttonData.R")
source("PARAMS_asthma.R")
source("asthmafunctionsMali.R")

#Import Hutton data imports and rearranges data to usable form

############## ADAPTED MALI CODE
# S.5

tot_Hosps_no <- RSV_Hosps_no
tot_Hosps_mAb <- RSV_Hosps_mAb
tot_Hosps_rsvPreF <- RSV_Hosps_rsvPreF
tot_Hosps_Combined <- RSV_Hosps_Combined

# number of kids surviving to age 6 without RSV-LRTI hospitalization for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_Hosps_no <- pop_tot - RSV_Hosps_no
tot_wo_Hosps_mAb <- pop_tot - RSV_Hosps_mAb
tot_wo_Hosps_rsvPreF <- pop_tot - RSV_Hosps_rsvPreF
tot_wo_Hosps_Combined <- pop_tot - RSV_Hosps_Combined

# calculate rate/prevalence of asthma among those without RSV-LRTI hospitalization
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, rr_w_hosp, tot_Hosps_no, tot_wo_Hosps_no)

# number of asthma cases among those without RSV-LRTI hospitalization
asth_wo_Hosps_no <- asth_no_rsv_func(tot_wo_Hosps_no, r_asth_norsv)
asth_wo_Hosps_mAb <- asth_no_rsv_func(tot_wo_Hosps_mAb, r_asth_norsv)
asth_wo_Hosps_rsvPreF <- asth_no_rsv_func(tot_wo_Hosps_rsvPreF, r_asth_norsv)
asth_wo_Hosps_Combined <- asth_no_rsv_func(tot_wo_Hosps_Combined, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI hospitalization
asth_Hosps_no <- asth_rsv_func(tot_Hosps_no, r_asth_norsv, rr_w_hosp)
asth_Hosps_mAb <- asth_rsv_func(tot_Hosps_mAb, r_asth_norsv, rr_w_hosp)
asth_Hosps_rsvPreF <- asth_rsv_func(tot_Hosps_rsvPreF, r_asth_norsv, rr_w_hosp)
asth_Hosps_Combined <- asth_rsv_func(tot_Hosps_Combined, r_asth_norsv, rr_w_hosp)

# total with asthma
tot_asth_no <- tot_asth_func(asth_Hosps_no, asth_wo_Hosps_no)
tot_asth_mAb <- tot_asth_func(asth_Hosps_mAb, asth_wo_Hosps_mAb)
tot_asth_rsvPreF <-tot_asth_func(asth_Hosps_rsvPreF, asth_wo_Hosps_rsvPreF)
tot_asth_Combined <-tot_asth_func(asth_Hosps_Combined, asth_wo_Hosps_Combined)

# number of asthma cases among those with RSV-LRTI hospitalization had they not been infected
asth_null_no <- asth_rsv_null_func(tot_Hosps_no, r_asth_norsv)
asth_null_mAb <- asth_rsv_null_func(tot_Hosps_mAb, r_asth_norsv)
asth_null_rsvPreF <-asth_rsv_null_func(tot_Hosps_rsvPreF, r_asth_norsv)
asth_null_Combined <-asth_rsv_null_func(tot_Hosps_Combined, r_asth_norsv)

# RSV-LRTI hospitalization attributable asthma
att_no <- asth_rsv_att_func(asth_Hosps_no, asth_null_no)
att_mAb <- asth_rsv_att_func(asth_Hosps_mAb, asth_null_mAb)
att_rsvPreF <-asth_rsv_att_func(asth_Hosps_rsvPreF, asth_null_rsvPreF)
att_Combined <-asth_rsv_att_func(asth_Hosps_Combined, asth_null_Combined)

# total recurrent wheeze/asthma, all RSV LRTI hospitalization prevented
asth_all_RSV_prev <- asth_null_no + asth_wo_Hosps_no

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
