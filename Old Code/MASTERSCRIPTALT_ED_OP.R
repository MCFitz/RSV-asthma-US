# Master Script for RSV-asthma-US analysis - ALTERNATIVE SCENARIO W/ OUTPATIENT AND ED VISITS ADDED
# Created by Meagan Fitzpatrick and Ian Galbreath

# Import data from Hutton
source("ImportHuttonData.R")
source("PARAMS_asthma.R")
source("asthmafunctionsMali.R")
source("adjustmentformultipleepisode.R")

#Import Hutton Data file rearranges data and adds OP to ED

############## ADAPTED MALI CODE
# S.5
# adjust number of outpatient/ED LRTI (total) to account for all-cause mortality out to 6 years - changed
# Question to Justin, do we need to adjust given there is no initial adjustment within 1st year

tot_OP_ED_Hosp_no <- num_OP_ED_Hosp_no
tot_OP_ED_Hosp_mAb <- num_OP_ED_Hosp_mAb
tot_OP_ED_Hosp_rsvPreF <- num_OP_ED_Hosp_rsvPreF
tot_OP_ED_Hosp_Combined <- num_OP_ED_Hosp_Combined

# number of kids surviving to age 6 without RSV-LRTI outpatient or ED encounter for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_OP_ED_Hosp_no <- pop_tot - tot_OP_ED_Hosp_no
tot_wo_OP_ED_Hosp_mAb <- pop_tot - tot_OP_ED_Hosp_mAb
tot_wo_OP_ED_Hosp_rsvPreF <- pop_tot - tot_OP_ED_Hosp_rsvPreF
tot_wo_OP_ED_Hosp_Combined <- pop_tot - tot_OP_ED_Hosp_Combined

# number of kids surviving to age 6 without RSV-LRTI outpatient or ED encounter for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_OP_ED_Hosp_no <- pop_tot - tot_OP_ED_Hosp_no
tot_wo_OP_ED_Hosp_mAb <- pop_tot - tot_OP_ED_Hosp_mAb
tot_wo_OP_ED_Hosp_rsvPreF <- pop_tot - tot_OP_ED_Hosp_rsvPreF
tot_wo_OP_ED_Hosp_Combined <- pop_tot - tot_OP_ED_Hosp_Combined

# calculate rate/prevalence of asthma among those without RSV-LRTI outpatient or ED encounter
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, rr_w, tot_OP_ED_Hosp_no, tot_wo_OP_ED_Hosp_no)

# number of asthma cases among those without RSV-LRTI hospitalization or ED encounter
asth_wo_OP_ED_Hosp_no <- asth_no_rsv_func(tot_wo_OP_ED_Hosp_no, r_asth_norsv)
asth_wo_OP_ED_Hosp_mAb <- asth_no_rsv_func(tot_wo_OP_ED_Hosp_mAb, r_asth_norsv)
asth_wo_OP_ED_Hosp_rsvPreF <- asth_no_rsv_func(tot_wo_OP_ED_Hosp_rsvPreF, r_asth_norsv)
asth_wo_OP_ED_Hosp_Combined <- asth_no_rsv_func(tot_wo_OP_ED_Hosp_Combined, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI outpatient or ED encounter
asth_OP_ED_Hosp_no <- asth_rsv_func(tot_OP_ED_Hosp_no, r_asth_norsv, rr_w)
asth_OP_ED_Hosp_mAb <- asth_rsv_func(tot_OP_ED_Hosp_mAb, r_asth_norsv, rr_w)
asth_OP_ED_Hosp_rsvPreF <- asth_rsv_func(tot_OP_ED_Hosp_rsvPreF, r_asth_norsv, rr_w)
asth_OP_ED_Hosp_Combined <- asth_rsv_func(tot_OP_ED_Hosp_Combined, r_asth_norsv, rr_w)

# total with asthma
tot_asth_no <- tot_asth_func(asth_OP_ED_Hosp_no, asth_wo_OP_ED_Hosp_no)
tot_asth_mAb <- tot_asth_func(asth_OP_ED_Hosp_mAb, asth_wo_OP_ED_Hosp_mAb)
tot_asth_rsvPreF <-tot_asth_func(asth_OP_ED_Hosp_rsvPreF, asth_wo_OP_ED_Hosp_rsvPreF)
tot_asth_Combined <-tot_asth_func(asth_OP_ED_Hosp_Combined, asth_wo_OP_ED_Hosp_Combined)

# number of asthma cases among those with RSV-LRTI  outpatient or ED encounter had they not been infected
asth_null_no <- asth_rsv_null_func(tot_OP_ED_Hosp_no, r_asth_norsv)
asth_null_mAb <- asth_rsv_null_func(tot_OP_ED_Hosp_mAb, r_asth_norsv)
asth_null_rsvPreF <-asth_rsv_null_func(tot_OP_ED_Hosp_rsvPreF, r_asth_norsv)
asth_null_Combined <-asth_rsv_null_func(tot_OP_ED_Hosp_Combined, r_asth_norsv)

# RSV-LRTI outpatient or ED encounter attributable asthma
att_no <- asth_rsv_att_func(asth_OP_ED_Hosp_no, asth_null_no)
att_mAb <- asth_rsv_att_func(asth_OP_ED_Hosp_mAb, asth_null_mAb)
att_rsvPreF <-asth_rsv_att_func(asth_OP_ED_Hosp_rsvPreF, asth_null_rsvPreF)
att_Combined <-asth_rsv_att_func(asth_OP_ED_Hosp_Combined, asth_null_Combined)

# total recurrent wheeze/asthma, all RSV LRTI outpatient or ED encounter  prevented
asth_all_RSV_prev <- asth_null_no + asth_wo_OP_ED_Hosp_no

########################################## new from uncertainty file
# total asthma per 10,000 population
tot_asth_no_pr <- tot_asth_no / pop_tot * 10000
tot_asth_mAb_pr <- tot_asth_mAb / pop_tot * 10000
tot_asth_rsvPreF_pr <- tot_asth_rsvPreF / pop_tot * 10000
#tot_asth_Combined_pr <- tot_asth_Combined / pop_tot * 10000

# total asthma percent decrease from status quo outpatient
tot_asth_mAb_pd <- (tot_asth_no - tot_asth_mAb) / tot_asth_no * 100
tot_asth_rsvPreF_pd <- (tot_asth_no - tot_asth_rsvPreF) / tot_asth_no * 100
#tot_asth_Combined_pd <- (tot_asth_no - tot_asth_Combined) / tot_asth_no * 100

# RSV-LRTI attributable asthma per 10,000 population
att_no_pr <- att_no / pop_tot * 10000
att_mAb_pr <- att_mAb / pop_tot * 10000
att_rsvPreF_pr <- att_rsvPreF / pop_tot * 10000
#att_Combined_pr <- att_Combined / pop_tot * 10000


# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd <- (att_no - att_mAb) / att_no * 100
att_rsvPreF_pd <- (att_no - att_rsvPreF) / att_no * 100
#att_Combined_pd <- (att_no - att_Combined) / att_no * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev <- asth_no_rsv_func(pop_tot, r_asth_norsv)
all_rsv_prev_pr <- all_rsv_prev / pop_tot * 10000
all_rsv_prev_pd <- (tot_asth_no - all_rsv_prev) / tot_asth_no * 100
########################################## new from uncertainty file

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
