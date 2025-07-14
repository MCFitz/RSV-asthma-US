# Master Script for RSV-asthma-US analysis
# Created by Meagan Fitzpatrick and Ian Galbreath

library(tidyverse)

# Import data from Hutton
source("ImportHuttonData.R")
source("PARAMS_asthma.R")
source("asthmafunctionsMali.R")
source("adjustmentformultipleepisode.R")

#Import Hutton data imports and rearranges data to usable form

############## ADAPTED MALI CODE
# S.5
# adjust number of outpatient/ED LRTI (total) to account for all-cause mortality out to 6 years - No need 
# to adjust for mortality rate given no adjustment within first year?

#tot_OP_no <- mort_adj_func(RSV_no, U5 = U5_mort, U9 = U9_mort)
#tot_OP_mAb <- mort_adj_func(RSV_mAb, U5 = U5_mort, U9 = U9_mort)
#tot_OP_rsvPreF <-mort_adj_func(RSV_mat, U5 = U5_mort, U9 = U9_mort)
#tot_OP_Combined <-mort_adj_func(RSV_com, U5 = U5_mort, U9 = U9_mort)

# number of kids without RSV-LRTI for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
tot_wo_OP_no <- pop_tot - num_OP_ED_Hosp_no
tot_wo_OP_mAb <- pop_tot - num_OP_ED_Hosp_mAb
tot_wo_OP_rsvPreF <- pop_tot - num_OP_ED_Hosp_rsvPreF
tot_wo_OP_Combined <- pop_tot - num_OP_ED_Hosp_Combined

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, rr_w, num_OP_ED_Hosp_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv)
asth_wo_OP_mAb <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv)
asth_wo_OP_rsvPreF <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv)
asth_wo_OP_Combined <- asth_no_rsv_func(tot_wo_OP_Combined, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI
asth_OP_no <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv, rr_w)
asth_OP_mAb <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv, rr_w)
asth_OP_rsvPreF <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv, rr_w)
asth_OP_Combined <- asth_rsv_func(num_OP_ED_Hosp_Combined, r_asth_norsv, rr_w)

# total with asthma
tot_asth_no <- tot_asth_func(asth_OP_no, asth_wo_OP_no)
tot_asth_mAb <- tot_asth_func(asth_OP_mAb, asth_wo_OP_mAb)
tot_asth_rsvPreF <-tot_asth_func(asth_OP_rsvPreF, asth_wo_OP_rsvPreF)
tot_asth_Combined <-tot_asth_func(asth_OP_Combined, asth_wo_OP_Combined)

# number of asthma cases among those with RSV-LRTI encounter had they not been infected
asth_null_no <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv)
asth_null_mAb <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv)
asth_null_rsvPreF <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv)
asth_null_Combined <-asth_rsv_null_func(num_OP_ED_Hosp_Combined, r_asth_norsv)

# RSV-LRTI encounter attributable asthma
att_no <- asth_rsv_att_func(asth_OP_no, asth_null_no)
att_mAb <- asth_rsv_att_func(asth_OP_mAb, asth_null_mAb)
att_rsvPreF <-asth_rsv_att_func(asth_OP_rsvPreF, asth_null_rsvPreF)
att_Combined <-asth_rsv_att_func(asth_OP_Combined, asth_null_Combined)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev <- asth_null_no + asth_wo_OP_no
