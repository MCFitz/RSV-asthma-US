#Tornado plots - fixing each outcome at upper and lower limits and performing calculations at those limits
source("ImportData.R")
source("PARAMS_asthma.R")
source("asthmafunctions.R")
source("adjustmentformultipleepisode.R")
library(tidyverse)

prev_tot
aOR_w_asthonly
ratio_multiple_ep_1st
num_OP_ED_Hosp_no_1st
num_OP_ED_Hosp_mAb_1st

#creating upper and low bounds
prev_tot_up <- 0.096
prev_tot_low <- 0.081
aOR_w_asthonly_up <- 5.11
aOR_w_asthonly_low <- 1.79
ratio_multiple_ep_CI_1st_up <- quantile(ratio_multiple_ep_CI_1st, probs = 0.95)
ratio_multiple_ep_CI_1st_low <- quantile(ratio_multiple_ep_CI_1st, probs = 0.05)
tot_RSV_no_up <- quantile(tot_RSV_no_u, probs = 0.95)
tot_RSV_no_low <- quantile(tot_RSV_no_u, probs = 0.05)
tot_RSV_mAb_up <- quantile(tot_RSV_mAb_u, probs = 0.95)
tot_RSV_mAb_low <- quantile(tot_RSV_mAb_u, probs = 0.05)
ratio_multiple_ep_CI_1st_upspec <- quantile(ratio_multiple_ep_CI_1st_spec, probs = 0.95)
ratio_multiple_ep_CI_1st_lowspec <- quantile(ratio_multiple_ep_CI_1st_spec, probs = 0.05)

# -----------------------------
# Prev - upper
# -----------------------------

#Total RSV-LRTI cases
num_OP_ED_Hosp_no_prevup <- OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
num_OP_ED_Hosp_mAb_prevup <- OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a

#Total without RSV-LRTI
tot_wo_OP_no_prevup <- pop_tot - num_OP_ED_Hosp_no_prevup
tot_wo_OP_mAb_prevup <- pop_tot - num_OP_ED_Hosp_mAb_prevup

#  rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_prevup <- prev_no_rsv_func(prev_tot_up, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_prevup, tot_wo_OP_no_prevup)

#RR from OR
RR_w_prevup <- aOR_w_asthonly/((1-(r_asth_norsv_prevup))+(r_asth_norsv_prevup*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_prevup <- prev_no_rsv_func(prev_tot_up, pop_tot, RR_w_prevup, num_OP_ED_Hosp_no_prevup, tot_wo_OP_no_prevup)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_prevup <- asth_no_rsv_func(tot_wo_OP_no_prevup, r_asth_norsv_prevup)
asth_wo_OP_mAb_prevup <- asth_no_rsv_func(tot_wo_OP_mAb_prevup, r_asth_norsv_prevup)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_prevup <- asth_rsv_func(num_OP_ED_Hosp_no_prevup, r_asth_norsv_prevup, RR_w_prevup)
asth_OP_mAb_prevup <- asth_rsv_func(num_OP_ED_Hosp_mAb_prevup, r_asth_norsv_prevup, RR_w_prevup)

#All cause asthma
tot_asth_no_prevup <- tot_asth_func(asth_OP_no_prevup, asth_wo_OP_no_prevup)
tot_asth_mAb_prevup <- tot_asth_func(asth_OP_mAb_prevup, asth_wo_OP_mAb_prevup)

# total asthma per 100,000 population
tot_asth_no_pr_prevup <- tot_asth_no_prevup / pop_tot * 100000
tot_asth_mAb_pr_prevup <- tot_asth_mAb_prevup / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_prevup <- (tot_asth_no_1st_adj - tot_asth_mAb_1st_adj) / tot_asth_no_1st_adj * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_prevup <- asth_rsv_null_func(num_OP_ED_Hosp_no_prevup, r_asth_norsv_prevup)
asth_null_mAb_prevup <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_prevup, r_asth_norsv_prevup)

# RSV-LRTI attributable asthma
att_no_prevup <- asth_rsv_att_func(asth_OP_no_prevup, asth_null_no_prevup)
att_mAb_prevup <- asth_rsv_att_func(asth_OP_mAb_prevup, asth_null_mAb_prevup)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_prevup <- asth_null_no_prevup + asth_wo_OP_no_prevup

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_prevup <- att_no_prevup / pop_tot * 100000
att_mAb_pr_prevup <- att_mAb_prevup / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_prevup <- (att_no_prevup - att_mAb_prevup) / att_no_prevup * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_prevup <- asth_no_rsv_func(pop_tot, r_asth_norsv_prevup)
all_rsv_prev_pr_prevup <- all_rsv_prev_prevup / pop_tot * 100000
all_rsv_prev_pd_prevup <- (tot_asth_no_prevup - all_rsv_prev_prevup) / tot_asth_no_prevup * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_prevup/pop_tot)*(RR_w_prevup-1))/(((num_OP_ED_Hosp_no_prevup/pop_tot)*(RR_w_prevup-1)+1))
(tot_asth_no_1st_adj-all_rsv_prev_prevup)/tot_asth_no_prevup

# -----------------------------
# Prev - lower
# -----------------------------

#Total RSV-LRTI cases
num_OP_ED_Hosp_no_prevlow <- OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
num_OP_ED_Hosp_mAb_prevlow <- OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a

# number without RSV-LRTI for each strategy
tot_wo_OP_no_prevlow <- pop_tot - num_OP_ED_Hosp_no_prevlow
tot_wo_OP_mAb_prevlow <- pop_tot - num_OP_ED_Hosp_mAb_prevlow

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_prevlow <- prev_no_rsv_func(prev_tot_low, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_prevlow, tot_wo_OP_no_prevlow)

#RR from OR
RR_w_prevlow <- aOR_w_asthonly/((1-(r_asth_norsv_prevlow))+(r_asth_norsv_prevlow*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_prevlow <- prev_no_rsv_func(prev_tot_low, pop_tot, RR_w_prevlow, num_OP_ED_Hosp_no_prevlow, tot_wo_OP_no_prevlow)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_prevlow <- asth_no_rsv_func(tot_wo_OP_no_prevlow, r_asth_norsv_prevlow)
asth_wo_OP_mAb_prevlow <- asth_no_rsv_func(tot_wo_OP_mAb_prevlow, r_asth_norsv_prevlow)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_prevlow <- asth_rsv_func(num_OP_ED_Hosp_no_prevlow, r_asth_norsv_prevlow, RR_w_prevlow)
asth_OP_mAb_prevlow <- asth_rsv_func(num_OP_ED_Hosp_mAb_prevlow, r_asth_norsv_prevlow, RR_w_prevlow)

# total with asthma
tot_asth_no_prevlow <- tot_asth_func(asth_OP_no_prevlow, asth_wo_OP_no_prevlow)
tot_asth_mAb_prevlow <- tot_asth_func(asth_OP_mAb_prevlow, asth_wo_OP_mAb_prevlow)

# total asthma per 100,000 population
tot_asth_no_pr_prevlow <- tot_asth_no_prevlow / pop_tot * 100000
tot_asth_mAb_pr_prevlow <- tot_asth_mAb_prevlow / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_prevlow <- (tot_asth_no_prevlow - tot_asth_mAb_prevlow) / tot_asth_no_prevlow * 100
tot_asth_rsvPreF_pd_prevlow <- (tot_asth_no_prevlow - tot_asth_rsvPreF_prevlow) / tot_asth_no_prevlow * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_prevlow <- asth_rsv_null_func(num_OP_ED_Hosp_no_prevlow, r_asth_norsv_prevlow)
asth_null_mAb_prevlow <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_prevlow, r_asth_norsv_prevlow)

# RSV-LRTI attributable asthma
att_no_prevlow <- asth_rsv_att_func(asth_OP_no_prevlow, asth_null_no_prevlow)
att_mAb_prevlow <- asth_rsv_att_func(asth_OP_mAb_prevlow, asth_null_mAb_prevlow)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_prevlow <- asth_null_no_prevlow + asth_wo_OP_no_prevlow

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_prevlow <- att_no_prevlow / pop_tot * 100000
att_mAb_pr_prevlow <- att_mAb_prevlow / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_prevlow <- (att_no_prevlow - att_mAb_prevlow) / att_no_prevlow * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_prevlow <- asth_no_rsv_func(pop_tot, r_asth_norsv_prevlow)
all_rsv_prev_pr_prevlow <- all_rsv_prev_prevlow / pop_tot * 100000
all_rsv_prev_pd_prevlow <- (tot_asth_no_prevlow - all_rsv_prev_prevlow) / tot_asth_no_prevlow * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_prevlow/pop_tot)*(RR_w_prevlow-1))/(((num_OP_ED_Hosp_no_prevlow/pop_tot)*(RR_w_prevlow-1)+1))
(tot_asth_no_prevlow-all_rsv_prev_prevlow)/tot_asth_no_prevlow

# -----------------------------
# OR - upper
# -----------------------------

#Total RSV-LRTI cases
num_OP_ED_Hosp_no_ORup <- OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
num_OP_ED_Hosp_mAb_ORup <- OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a

# number  without RSV-LRTI 
tot_wo_OP_no_ORup <- pop_tot - num_OP_ED_Hosp_no_ORup
tot_wo_OP_mAb_ORup <- pop_tot - num_OP_ED_Hosp_mAb_ORup

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_ORup <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly_up, num_OP_ED_Hosp_no_ORup, tot_wo_OP_no_ORup)

#RR from OR
RR_w_ORup <- aOR_w_asthonly_up/((1-(r_asth_norsv_ORup))+(r_asth_norsv_ORup*aOR_w_asthonly_up))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_ORup <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_ORup, num_OP_ED_Hosp_no_ORup, tot_wo_OP_no_ORup)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_ORup <- asth_no_rsv_func(tot_wo_OP_no_ORup, r_asth_norsv_ORup)
asth_wo_OP_mAb_ORup <- asth_no_rsv_func(tot_wo_OP_mAb_ORup, r_asth_norsv_ORup)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_ORup <- asth_rsv_func(num_OP_ED_Hosp_no_ORup, r_asth_norsv_ORup, RR_w_ORup)
asth_OP_mAb_ORup <- asth_rsv_func(num_OP_ED_Hosp_mAb_ORup, r_asth_norsv_ORup, RR_w_ORup)

# All cause asthma
tot_asth_no_ORup <- tot_asth_func(asth_OP_no_ORup, asth_wo_OP_no_ORup)
tot_asth_mAb_ORup <- tot_asth_func(asth_OP_mAb_ORup, asth_wo_OP_mAb_ORup)

# total asthma per 100,000 population
tot_asth_no_pr_ORup <- tot_asth_no_ORup / pop_tot * 100000
tot_asth_mAb_pr_ORup <- tot_asth_mAb_ORup / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_ORup<- (tot_asth_no_ORup - tot_asth_mAb_ORup) / tot_asth_no_ORup * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_ORup <- asth_rsv_null_func(num_OP_ED_Hosp_no_ORup, r_asth_norsv_ORup)
asth_null_mAb_ORup <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_ORup, r_asth_norsv_ORup)

# RSV-LRTI attributable asthma
att_no_ORup <- asth_rsv_att_func(asth_OP_no_ORup, asth_null_no_ORup)
att_mAb_ORup <- asth_rsv_att_func(asth_OP_mAb_ORup, asth_null_mAb_ORup)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_ORup <- asth_null_no_ORup + asth_wo_OP_no_ORup

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_ORup <- att_no_ORup / pop_tot * 100000
att_mAb_pr_ORup <- att_mAb_ORup / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_ORup <- (att_no_ORup - att_mAb_ORup) / att_no_ORup * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_ORup <- asth_no_rsv_func(pop_tot, r_asth_norsv_ORup)
all_rsv_prev_pr_ORup <- all_rsv_prev_ORup / pop_tot * 100000
all_rsv_prev_pd_ORup <- (tot_asth_no_ORup - all_rsv_prev_ORup) / tot_asth_no_ORup * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_ORup/pop_tot)*(RR_w_ORup-1))/(((num_OP_ED_Hosp_no_ORup/pop_tot)*(RR_w_ORup-1)+1))
(tot_asth_no_ORup-all_rsv_prev_ORup)/tot_asth_no_ORup

# -----------------------------
# OR lower
# -----------------------------

#Total RSV-LRTI cases
num_OP_ED_Hosp_no_ORlow <- OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
num_OP_ED_Hosp_mAb_ORlow <- OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a

# number without RSV-LRTI
tot_wo_OP_no_ORlow <- pop_tot - num_OP_ED_Hosp_no_ORlow
tot_wo_OP_mAb_ORlow <- pop_tot - num_OP_ED_Hosp_mAb_ORlow

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_ORlow <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly_low, num_OP_ED_Hosp_no_ORlow, tot_wo_OP_no_ORlow)

#RR from OR
RR_w_ORlow <- aOR_w_asthonly_low/((1-(r_asth_norsv_ORlow))+(r_asth_norsv_ORlow*aOR_w_asthonly_low))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_ORlow <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_ORlow, num_OP_ED_Hosp_no_ORlow, tot_wo_OP_no_ORlow)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_ORlow <- asth_no_rsv_func(tot_wo_OP_no_ORlow, r_asth_norsv_ORlow)
asth_wo_OP_mAb_ORlow <- asth_no_rsv_func(tot_wo_OP_mAb_ORlow, r_asth_norsv_ORlow)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_ORlow <- asth_rsv_func(num_OP_ED_Hosp_no_ORlow, r_asth_norsv_ORlow, RR_w_ORlow)
asth_OP_mAb_ORlow <- asth_rsv_func(num_OP_ED_Hosp_mAb_ORlow, r_asth_norsv_ORlow, RR_w_ORlow)

# all cause asthma
tot_asth_no_1st_adj <- tot_asth_func(asth_OP_no_ORlow, asth_wo_OP_no_ORlow)
tot_asth_mAb_1st_adj <- tot_asth_func(asth_OP_mAb_ORlow, asth_wo_OP_mAb_ORlow)

# total asthma per 100,000 population
tot_asth_no_pr_ORlow <- tot_asth_no_ORlow / pop_tot * 100000
tot_asth_mAb_pr_ORlow <- tot_asth_mAb_ORlow / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_ORlow <- (tot_asth_no_ORlow - tot_asth_mAb_ORlow) / tot_asth_no_ORlow * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_ORlow <- asth_rsv_null_func(num_OP_ED_Hosp_no_ORlow, r_asth_norsv_ORlow)
asth_null_mAb_ORlow <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_ORlow, r_asth_norsv_ORlow)

# RSV-LRTI attributable asthma
att_no_ORlow <- asth_rsv_att_func(asth_OP_no_ORlow, asth_null_no_ORlow)
att_mAb_ORlow <- asth_rsv_att_func(asth_OP_mAb_ORlow, asth_null_mAb_ORlow)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_ORlow <- asth_null_no_ORlow + asth_wo_OP_no_ORlow

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_ORlow <- att_no_ORlow / pop_tot * 100000
att_mAb_pr_ORlow <- att_mAb_ORlow / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_ORlow <- (att_no_ORlow - att_mAb_ORlow) / att_no_ORlow * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_ORlow <- asth_no_rsv_func(pop_tot, r_asth_norsv_ORlow)
all_rsv_prev_pr_ORlow <- all_rsv_prev_ORlow / pop_tot * 100000
all_rsv_prev_pd_ORlow <- (tot_asth_no_ORlow - all_rsv_prev_ORlow) / tot_asth_no_ORlow * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_ORlow/pop_tot)*(RR_w_ORlow-1))/(((num_OP_ED_Hosp_no_ORlow/pop_tot)*(RR_w_ORlow-1)+1))
(tot_asth_no_ORlow-all_rsv_prev_ORlow)/tot_asth_no_ORlow

# -----------------------------
# adjustment factor upper
# -----------------------------

#PE adjustment
OP_no_PE_afup = OP_no_PE_1st*ratio_multiple_ep_CI_1st_up
ED_no_PE_afup = ED_no_PE_1st*ratio_multiple_ep_CI_1st_up
Hosps_no_PE_afup = Hosps_no_PE_1st*ratio_multiple_ep_CI_1st_up
OP_mAb_PE_afup = OP_mAb_PE_1st*ratio_multiple_ep_CI_1st_up
ED_mAb_PE_afup = ED_mAb_PE_1st*ratio_multiple_ep_CI_1st_up
Hosps_mAb_PE_afup = Hosps_mAb_PE_1st*ratio_multiple_ep_CI_1st_up

#Total RSV-LRTI cases 
num_OP_ED_Hosp_no_afup <- OP_no_PE_afup+ED_no_PE_afup+Hosps_no_PE_afup
num_OP_ED_Hosp_mAb_afup <- OP_mAb_PE_afup+ED_mAb_PE_afup+Hosps_mAb_PE_afup

# number without RSV-LRTI
tot_wo_OP_no_afup <- pop_tot - num_OP_ED_Hosp_no_afup
tot_wo_OP_mAb_afup <- pop_tot - num_OP_ED_Hosp_mAb_afup
tot_wo_OP_rsvPreF_afup <- pop_tot - num_OP_ED_Hosp_rsvPreF_afup

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_afup <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_afup, tot_wo_OP_no_afup)

#RR from OR
RR_w_afup <- aOR_w_asthonly/((1-(r_asth_norsv_afup))+(r_asth_norsv_afup*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_afup <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_afup, num_OP_ED_Hosp_no_afup, tot_wo_OP_no_afup)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_afup <- asth_no_rsv_func(tot_wo_OP_no_afup, r_asth_norsv_afup)
asth_wo_OP_mAb_afup <- asth_no_rsv_func(tot_wo_OP_mAb_afup, r_asth_norsv_afup)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_afup <- asth_rsv_func(num_OP_ED_Hosp_no_afup, r_asth_norsv_afup, RR_w_afup)
asth_OP_mAb_afup <- asth_rsv_func(num_OP_ED_Hosp_mAb_afup, r_asth_norsv_afup, RR_w_afup)

# all cause asthma
tot_asth_no_afup <- tot_asth_func(asth_OP_no_afup, asth_wo_OP_no_afup)
tot_asth_mAb_afup <- tot_asth_func(asth_OP_mAb_afup, asth_wo_OP_mAb_afup)

# total asthma per 100,000 population
tot_asth_no_pr_afup <- tot_asth_no_afup / pop_tot * 100000
tot_asth_mAb_pr_afup <- tot_asth_mAb_afup / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_afup <- (tot_asth_no_afup - tot_asth_mAb_afup) / tot_asth_no_afup * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_afup <- asth_rsv_null_func(num_OP_ED_Hosp_no_afup, r_asth_norsv_afup)
asth_null_mAb_afup <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_afup, r_asth_norsv_afup)

# RSV-LRTI attributable asthma
att_no_afup <- asth_rsv_att_func(asth_OP_no_afup, asth_null_no_afup)
att_mAb_afup <- asth_rsv_att_func(asth_OP_mAb_afup, asth_null_mAb_afup)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_afup <- asth_null_no_afup + asth_wo_OP_no_afup

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_afup <- att_no_afup / pop_tot * 100000
att_mAb_pr_afup <- att_mAb_afup / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_afup <- (att_no_afup - att_mAb_afup) / att_no_afup * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_afup <- asth_no_rsv_func(pop_tot, r_asth_norsv_afup)
all_rsv_prev_pr_afup <- all_rsv_prev_afup / pop_tot * 100000
all_rsv_prev_pd_afup <- (tot_asth_no_afup - all_rsv_prev_afup) / tot_asth_no_afup * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_afup/pop_tot)*(RR_w_afup-1))/(((num_OP_ED_Hosp_no_afup/pop_tot)*(RR_w_afup-1)+1))
(tot_asth_no_afup-all_rsv_prev_afup)/tot_asth_no_afup

# -----------------------------
# Adjustment factor lower
# -----------------------------

#PE adjustment
OP_no_PE_aflow = OP_no_PE_1st*ratio_multiple_ep_CI_1st_low
ED_no_PE_aflow = ED_no_PE_1st*ratio_multiple_ep_CI_1st_low
Hosps_no_PE_aflow = Hosps_no_PE_1st*ratio_multiple_ep_CI_1st_low
OP_mAb_PE_aflow = OP_mAb_PE_1st*ratio_multiple_ep_CI_1st_low
ED_mAb_PE_aflow = ED_mAb_PE_1st*ratio_multiple_ep_CI_1st_low
Hosps_mAb_PE_aflow = Hosps_mAb_PE_1st*ratio_multiple_ep_CI_1st_low

#Total RSV-LRTI episodes
num_OP_ED_Hosp_no_aflow <- OP_no_PE_aflow+ED_no_PE_aflow+Hosps_no_PE_aflow
num_OP_ED_Hosp_mAb_aflow <- OP_mAb_PE_aflow+ED_mAb_PE_aflow+Hosps_mAb_PE_aflow

# number without RSV-LRTI 
tot_wo_OP_no_aflow <- pop_tot - num_OP_ED_Hosp_no_aflow
tot_wo_OP_mAb_aflow <- pop_tot - num_OP_ED_Hosp_mAb_aflow

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_aflow <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_aflow, tot_wo_OP_no_aflow)

#RR from OR
RR_w_aflow <- aOR_w_asthonly/((1-(r_asth_norsv_aflow))+(r_asth_norsv_aflow*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_aflow <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_aflow, num_OP_ED_Hosp_no_aflow, tot_wo_OP_no_aflow)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_aflow <- asth_no_rsv_func(tot_wo_OP_no_aflow, r_asth_norsv_aflow)
asth_wo_OP_mAb_aflow <- asth_no_rsv_func(tot_wo_OP_mAb_aflow, r_asth_norsv_aflow)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_aflow <- asth_rsv_func(num_OP_ED_Hosp_no_aflow, r_asth_norsv_aflow, RR_w_aflow)
asth_OP_mAb_aflow <- asth_rsv_func(num_OP_ED_Hosp_mAb_aflow, r_asth_norsv_aflow, RR_w_aflow)

# all cause asthma
tot_asth_no_aflow <- tot_asth_func(asth_OP_no_aflow, asth_wo_OP_no_aflow)
tot_asth_mAb_aflow <- tot_asth_func(asth_OP_mAb_aflow, asth_wo_OP_mAb_aflow)

# total asthma per 100,000 population
tot_asth_no_pr_aflow <- tot_asth_no_aflow / pop_tot * 100000
tot_asth_mAb_pr_aflow <- tot_asth_mAb_aflow / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_aflow <- (tot_asth_no_aflow - tot_asth_mAb_aflow) / tot_asth_no_aflow * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_aflow <- asth_rsv_null_func(num_OP_ED_Hosp_no_aflow, r_asth_norsv_aflow)
asth_null_mAb_aflow <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_aflow, r_asth_norsv_aflow)

# RSV-LRTI attributable asthma
att_no_aflow <- asth_rsv_att_func(asth_OP_no_aflow, asth_null_no_aflow)
att_mAb_aflow <- asth_rsv_att_func(asth_OP_mAb_aflow, asth_null_mAb_aflow)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_aflow <- asth_null_no_aflow + asth_wo_OP_no_aflow

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_aflow <- att_no_aflow / pop_tot * 100000
att_mAb_pr_aflow <- att_mAb_aflow / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_aflow <- (att_no_aflow - att_mAb_aflow) / att_no_aflow * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_aflow <- asth_no_rsv_func(pop_tot, r_asth_norsv_aflow)
all_rsv_prev_pr_aflow <- all_rsv_prev_aflow / pop_tot * 100000
all_rsv_prev_pd_aflow <- (tot_asth_no_aflow - all_rsv_prev_aflow) / tot_asth_no_aflow * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_aflow/pop_tot)*(RR_w_aflow-1))/(((num_OP_ED_Hosp_no_aflow/pop_tot)*(RR_w_aflow-1)+1))
(tot_asth_no_1st_adj-all_rsv_prev_aflow)/tot_asth_no_aflow

# -----------------------------
# RSV LRTI encounter upper (both mAb and natural hx)
# -----------------------------

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no_encup <- tot_RSV_no_up
num_OP_ED_Hosp_mAb_encup <- tot_RSV_mAb_up

# number without RSV-LRTI
tot_wo_OP_no_encup <- pop_tot - num_OP_ED_Hosp_no_encup
tot_wo_OP_mAb_encup <- pop_tot - num_OP_ED_Hosp_mAb_encup

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_encup <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_encup, tot_wo_OP_no_encup)

#RR from OR
RR_w_1st_adj <- aOR_w_asthonly/((1-(r_asth_norsv_encup))+(r_asth_norsv_encup*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_encup <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_encup, num_OP_ED_Hosp_no_encup, tot_wo_OP_no_encup)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_encup <- asth_no_rsv_func(tot_wo_OP_no_encup, r_asth_norsv_encup)
asth_wo_OP_mAb_encup <- asth_no_rsv_func(tot_wo_OP_mAb_encup, r_asth_norsv_encup)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_encup <- asth_rsv_func(num_OP_ED_Hosp_no_encup, r_asth_norsv_encup, RR_w_encup)
asth_OP_mAb_encup <- asth_rsv_func(num_OP_ED_Hosp_mAb_encup, r_asth_norsv_encup, RR_w_encup)

# all cause asthma
tot_asth_no_encup <- tot_asth_func(asth_OP_no_encup, asth_wo_OP_no_encup)
tot_asth_mAb_encup <- tot_asth_func(asth_OP_mAb_encup, asth_wo_OP_mAb_encup)

# total asthma per 100,000 population
tot_asth_no_pr_encup <- tot_asth_no_encup / pop_tot * 100000
tot_asth_mAb_pr_encup <- tot_asth_mAb_encup / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_encup <- (tot_asth_no_encup - tot_asth_mAb_encup) / tot_asth_no_encup * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_encup <- asth_rsv_null_func(num_OP_ED_Hosp_no_encup, r_asth_norsv_encup)
asth_null_mAb_encup <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_encup, r_asth_norsv_encup)

# RSV-LRTI attributable asthma
att_no_encup <- asth_rsv_att_func(asth_OP_no_encup, asth_null_no_encup)
att_mAb_encup <- asth_rsv_att_func(asth_OP_mAb_encup, asth_null_mAb_encup)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_encup <- asth_null_no_encup + asth_wo_OP_no_encup

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_encup <- att_no_encup/ pop_tot * 100000
att_mAb_pr_encup <- att_mAb_encup / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_encup<- (att_no_encup - att_mAb_encup) / att_no_encup * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_encup <- asth_no_rsv_func(pop_tot, r_asth_norsv_encup)
all_rsv_prev_pr_encup <- all_rsv_prev_encup / pop_tot * 100000
all_rsv_prev_pd_encup <- (tot_asth_no_encup - all_rsv_prev_encup) / tot_asth_no_encup * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_encup/pop_tot)*(RR_w_encup-1))/(((num_OP_ED_Hosp_no_encup/pop_tot)*(RR_w_encup-1)+1))
(tot_asth_no_encup-all_rsv_prev_encup)/tot_asth_no_encup

# -----------------------------
# RSV Healthcare encounter lower (both natural history and mAb)
# -----------------------------

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no_enclow <- tot_RSV_no_low
num_OP_ED_Hosp_mAb_enclow <- tot_RSV_mAb_low

# number without RSV-LRTI
tot_wo_OP_no_enclow <- pop_tot - num_OP_ED_Hosp_no_enclow
tot_wo_OP_mAb_1st <- pop_tot - num_OP_ED_Hosp_mAb_enclow

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_1st <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_enclow, tot_wo_OP_no_enclow)

#RR from OR
RR_w_enclow <- aOR_w_asthonly/((1-(r_asth_norsv_enclow))+(r_asth_norsv_enclow*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_enclow <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_enclow, num_OP_ED_Hosp_no_enclow, tot_wo_OP_no_enclow)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_enclow <- asth_no_rsv_func(tot_wo_OP_no_1st, r_asth_norsv_enclow)
asth_wo_OP_mAb_enclow <- asth_no_rsv_func(tot_wo_OP_mAb_1st, r_asth_norsv_enclow)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_enclow <- asth_rsv_func(num_OP_ED_Hosp_no_enclow, r_asth_norsv_enclow, RR_w_enclow)
asth_OP_mAb_enclow <- asth_rsv_func(num_OP_ED_Hosp_mAb_enclow, r_asth_norsv_enclow, RR_w_enclow)

# all cause asthma
tot_asth_no_enclow <- tot_asth_func(asth_OP_no_enclow, asth_wo_OP_no_enclow)
tot_asth_mAb_enclow <- tot_asth_func(asth_OP_mAb_enclow, asth_wo_OP_mAb_enclow)

# total asthma per 100,000 population
tot_asth_no_pr_enclow <- tot_asth_no_enclow / pop_tot * 100000
tot_asth_mAb_pr_enclow <- tot_asth_mAb_enclow / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_enclow <- (tot_asth_no_enclow - tot_asth_mAb_enclow) / tot_asth_no_enclow * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_enclow <- asth_rsv_null_func(num_OP_ED_Hosp_no_enclow, r_asth_norsv_enclow)
asth_null_mAb_enclow <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_enclow, r_asth_norsv_enclow)

# RSV-LRTI attributable asthma
att_no_enclow <- asth_rsv_att_func(asth_OP_no_enclow, asth_null_no_enclow)
att_mAb_enclow <- asth_rsv_att_func(asth_OP_mAb_enclow, asth_null_mAb_enclow)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_enclow <- asth_null_no_enclow + asth_wo_OP_no_enclow

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_enclow <- att_no_enclow / pop_tot * 100000
att_mAb_pr_enclow <- att_mAb_enclow / pop_tot * 100000
att_rsvPreF_pr_enclow<- att_rsvPreF_enclow / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_enclow <- (att_no_enclow - att_mAb_enclow) / att_no_enclow * 100
att_rsvPreF_pd_enclow<- (att_no_enclow - att_rsvPreF_enclow) / att_no_enclow * 100


# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_enclow <- asth_no_rsv_func(pop_tot, r_asth_norsv_enclow)
all_rsv_prev_pr_enclow <- all_rsv_prev_enclow / pop_tot * 100000
all_rsv_prev_pd_enclow <- (tot_asth_no_enclow - all_rsv_prev_enclow) / tot_asth_no_enclow * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_enclow/pop_tot)*(RR_w_enclow-1))/(((num_OP_ED_Hosp_no_enclow/pop_tot)*(RR_w_enclow-1)+1))
(tot_asth_no_enclow-all_rsv_prev_enclow)/tot_asth_no_enclow


# -----------------------------
# adjustment factor upper spec
# -----------------------------

#PE adjustment
OP_no_PE_upspec = OP_no_PE_1st*ratio_multiple_ep_CI_1st_upspec
ED_no_PE_upspec = ED_no_PE_1st*ratio_multiple_ep_CI_1st_upspec
Hosps_no_PE_upspec = Hosps_no_PE_1st*ratio_multiple_ep_CI_1st_upspec
OP_mAb_PE_upspec = OP_mAb_PE_1st*ratio_multiple_ep_CI_1st_upspec
ED_mAb_PE_upspec = ED_mAb_PE_1st*ratio_multiple_ep_CI_1st_upspec
Hosps_mAb_PE_upspec = Hosps_mAb_PE_1st*ratio_multiple_ep_CI_1st_upspec

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no_upspec <- OP_no_PE_upspec+ED_no_PE_upspec+Hosps_no_PE_upspec
num_OP_ED_Hosp_mAb_upspec <- OP_mAb_PE_upspec+ED_mAb_PE_upspec+Hosps_mAb_PE_upspec

# number without RSV-LRTI 
tot_wo_OP_no_upspec <- pop_tot - num_OP_ED_Hosp_no_upspec
tot_wo_OP_mAb_upspec <- pop_tot - num_OP_ED_Hosp_mAb_upspec

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_upspec <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_upspec, tot_wo_OP_no_upspec)

#RR from OR
RR_w_upspec <- aOR_w_asthonly/((1-(r_asth_norsv_upspec))+(r_asth_norsv_upspec*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_upspec <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_upspec, num_OP_ED_Hosp_no_upspec, tot_wo_OP_no_upspec)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_upspec <- asth_no_rsv_func(tot_wo_OP_no_upspec, r_asth_norsv_upspec)
asth_wo_OP_mAb_upspec <- asth_no_rsv_func(tot_wo_OP_mAb_upspec, r_asth_norsv_upspec)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_upspec <- asth_rsv_func(num_OP_ED_Hosp_no_upspec, r_asth_norsv_upspec, RR_w_upspec)
asth_OP_mAb_upspec <- asth_rsv_func(num_OP_ED_Hosp_mAb_upspec, r_asth_norsv_upspec, RR_w_upspec)

# all cause with asthma
tot_asth_no_upspec <- tot_asth_func(asth_OP_no_upspec, asth_wo_OP_no_upspec)
tot_asth_mAb_upspec <- tot_asth_func(asth_OP_mAb_upspec, asth_wo_OP_mAb_upspec)

# total asthma per 100,000 population
tot_asth_no_pr_upspec <- tot_asth_no_upspec / pop_tot * 100000
tot_asth_mAb_pr_upspec <- tot_asth_mAb_upspec / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_upspec <- (tot_asth_no_upspec - tot_asth_mAb_upspec) / tot_asth_no_upspec * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_upspec <- asth_rsv_null_func(num_OP_ED_Hosp_no_upspec, r_asth_norsv_upspec)
asth_null_mAb_upspec <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_upspec, r_asth_norsv_upspec)

# RSV-LRTI attributable asthma
att_no_upspec <- asth_rsv_att_func(asth_OP_no_upspec, asth_null_no_upspec)
att_mAb_upspec <- asth_rsv_att_func(asth_OP_mAb_upspec, asth_null_mAb_upspec)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_upspec <- asth_null_no_upspec + asth_wo_OP_no_upspec

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_upspec <- att_no_upspec / pop_tot * 100000
att_mAb_pr_upspec <- att_mAb_upspec / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_upspec <- (att_no_upspec - att_mAb_upspec) / att_no_upspec * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_upspec <- asth_no_rsv_func(pop_tot, r_asth_norsv_upspec)
all_rsv_prev_pr_upspec <- all_rsv_prev_upspec / pop_tot * 100000
all_rsv_prev_pd_upspec <- (tot_asth_no_upspec - all_rsv_prev_upspec) / tot_asth_no_upspec * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_upspec/pop_tot)*(RR_w_upspec-1))/(((num_OP_ED_Hosp_no_upspec/pop_tot)*(RR_w_upspec-1)+1))
(tot_asth_no_upspec-all_rsv_prev_upspec)/tot_asth_no_upspec

# -----------------------------
# Adjustment factor lower spec
# -----------------------------

#PE adjustment 1st year of life
OP_no_PE_lowspec = OP_no_PE_1st*ratio_multiple_ep_CI_1st_lowspec
ED_no_PE_lowspec = ED_no_PE_1st*ratio_multiple_ep_CI_1st_lowspec
Hosps_no_PE_lowspec = Hosps_no_PE_1st*ratio_multiple_ep_CI_1st_lowspec
OP_mAb_PE_lowspec = OP_mAb_PE_1st*ratio_multiple_ep_CI_1st_lowspec
ED_mAb_PE_lowspec = ED_mAb_PE_1st*ratio_multiple_ep_CI_1st_lowspec
Hosps_mAb_PE_lowspec = Hosps_mAb_PE_1st*ratio_multiple_ep_CI_1st_lowspec

#Total RSV-LRTI Encounters
num_OP_ED_Hosp_no_lowspec <- OP_no_PE_lowspec+ED_no_PE_lowspec+Hosps_no_PE_lowspec
num_OP_ED_Hosp_mAb_lowspec <- OP_mAb_PE_lowspec+ED_mAb_PE_lowspec+Hosps_mAb_PE_lowspec

# number without RSV-LRTI
tot_wo_OP_no_lowspec <- pop_tot - num_OP_ED_Hosp_no_lowspec
tot_wo_OP_mAb_lowspec <- pop_tot - num_OP_ED_Hosp_mAb_lowspec

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_lowspec <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_lowspec, tot_wo_OP_no_lowspec)

#RR from OR
RR_w_lowspec <- aOR_w_asthonly/((1-(r_asth_norsv_lowspec))+(r_asth_norsv_lowspec*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_lowspec <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_lowspec, num_OP_ED_Hosp_no_lowspec, tot_wo_OP_no_lowspec)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_lowspec <- asth_no_rsv_func(tot_wo_OP_no_lowspec, r_asth_norsv_lowspec)
asth_wo_OP_mAb_lowspec <- asth_no_rsv_func(tot_wo_OP_mAb_lowspec, r_asth_norsv_lowspec)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_lowspec <- asth_rsv_func(num_OP_ED_Hosp_no_lowspec, r_asth_norsv_lowspec, RR_w_lowspec)
asth_OP_mAb_lowspec <- asth_rsv_func(num_OP_ED_Hosp_mAb_lowspec, r_asth_norsv_lowspec, RR_w_lowspec)

# all cause asthma
tot_asth_no_lowspec <- tot_asth_func(asth_OP_no_lowspec, asth_wo_OP_no_lowspec)
tot_asth_mAb_lowspec <- tot_asth_func(asth_OP_mAb_lowspec, asth_wo_OP_mAb_lowspec)

# total asthma per 100,000 population
tot_asth_no_pr_lowspec <- tot_asth_no_lowspec / pop_tot * 100000
tot_asth_mAb_pr_lowspec <- tot_asth_mAb_lowspec / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_lowspec <- (tot_asth_no_lowspec - tot_asth_mAb_lowspec) / tot_asth_no_lowspec * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_lowspec <- asth_rsv_null_func(num_OP_ED_Hosp_no_lowspec, r_asth_norsv_lowspec)
asth_null_mAb_lowspec <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_lowspec, r_asth_norsv_lowspec)

# RSV-LRTI attributable asthma
att_no_lowspec <- asth_rsv_att_func(asth_OP_no_lowspec, asth_null_no_lowspec)
att_mAb_lowspec <- asth_rsv_att_func(asth_OP_mAb_lowspec, asth_null_mAb_lowspec)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_lowspec <- asth_null_no_lowspec + asth_wo_OP_no_lowspec

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_lowspec <- att_no_lowspec / pop_tot * 100000
att_mAb_pr_lowspec <- att_mAb_lowspec / pop_tot * 100000
att_rsvPreF_pr_lowspec <- att_rsvPreF_lowspec / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_lowspec <- (att_no_lowspec - att_mAb_lowspec) / att_no_lowspec * 100
att_rsvPreF_pd_lowspec <- (att_no_lowspec - att_rsvPreF_lowspec) / att_no_lowspec * 100


# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_lowspec <- asth_no_rsv_func(pop_tot, r_asth_norsv_lowspec)
all_rsv_prev_pr_lowspec <- all_rsv_prev_lowspec / pop_tot * 100000
all_rsv_prev_pd_lowspec <- (tot_asth_no_lowspec - all_rsv_prev_lowspec) / tot_asth_no_lowspec * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_lowspec/pop_tot)*(RR_w_lowspec-1))/(((num_OP_ED_Hosp_no_lowspec/pop_tot)*(RR_w_lowspec-1)+1))
(tot_asth_no_lowspec-all_rsv_prev_lowspec)/tot_asth_no_lowspec


##------------------------------------##
## DATA OF INTEREST-PAF --------------##
##------------------------------------##

#Asthma prev - upper bound
((num_OP_ED_Hosp_no_prevup/pop_tot)*(RR_w_prevup-1))/(((num_OP_ED_Hosp_no_prevup/pop_tot)*(RR_w_prevup-1)+1))
(tot_asth_no_1st_adj-all_rsv_prev_prevup)/tot_asth_no_prevup

#Asthma prev - lower bound
((num_OP_ED_Hosp_no_prevlow/pop_tot)*(RR_w_prevlow-1))/(((num_OP_ED_Hosp_no_prevlow/pop_tot)*(RR_w_prevlow-1)+1))
(tot_asth_no_prevlow-all_rsv_prev_prevlow)/tot_asth_no_prevlow

#OR - upper bound
((num_OP_ED_Hosp_no_ORup/pop_tot)*(RR_w_ORup-1))/(((num_OP_ED_Hosp_no_ORup/pop_tot)*(RR_w_ORup-1)+1))
(tot_asth_no_ORup-all_rsv_prev_ORup)/tot_asth_no_ORup

#OR - lower bound
((num_OP_ED_Hosp_no_ORlow/pop_tot)*(RR_w_ORlow-1))/(((num_OP_ED_Hosp_no_ORlow/pop_tot)*(RR_w_ORlow-1)+1))
(tot_asth_no_ORlow-all_rsv_prev_ORlow)/tot_asth_no_ORlow

#adjustment factor - upper bound
((num_OP_ED_Hosp_no_afup/pop_tot)*(RR_w_afup-1))/(((num_OP_ED_Hosp_no_afup/pop_tot)*(RR_w_afup-1)+1))
(tot_asth_no_afup-all_rsv_prev_afup)/tot_asth_no_afup

#adjustment factor - lower bound
((num_OP_ED_Hosp_no_aflow/pop_tot)*(RR_w_aflow-1))/(((num_OP_ED_Hosp_no_aflow/pop_tot)*(RR_w_aflow-1)+1))
(tot_asth_no_1st_adj-all_rsv_prev_aflow)/tot_asth_no_aflow

#RSV LRTI encounter - upper bound
((num_OP_ED_Hosp_no_encup/pop_tot)*(RR_w_encup-1))/(((num_OP_ED_Hosp_no_encup/pop_tot)*(RR_w_encup-1)+1))
(tot_asth_no_encup-all_rsv_prev_encup)/tot_asth_no_encup

#RSV LRTI encounter - lower bound
((num_OP_ED_Hosp_no_enclow/pop_tot)*(RR_w_enclow-1))/(((num_OP_ED_Hosp_no_enclow/pop_tot)*(RR_w_enclow-1)+1))
(tot_asth_no_enclow-all_rsv_prev_enclow)/tot_asth_no_enclow

#specific def - upper bound 
((num_OP_ED_Hosp_no_upspec/pop_tot)*(RR_w_upspec-1))/(((num_OP_ED_Hosp_no_upspec/pop_tot)*(RR_w_upspec-1)+1))
(tot_asth_no_upspec-all_rsv_prev_upspec)/tot_asth_no_upspec

#specific def - lower bound 
((num_OP_ED_Hosp_no_lowspec/pop_tot)*(RR_w_lowspec-1))/(((num_OP_ED_Hosp_no_lowspec/pop_tot)*(RR_w_lowspec-1)+1))
(tot_asth_no_lowspec-all_rsv_prev_lowspec)/tot_asth_no_lowspec


library(ggplot2)
library(dplyr)
library(tidyr)

baseline <- 14.5

tornado_data <- data.frame(
  parameter = c(
    "Prevalence",
    "Odds ratio",
    "Adjustment factor",
    "Encounter"
  ),
  low = c(14.2, 6.6, 14.4, 9.0),
  high = c(14.7, 24.2, 14.5, 18.9)
) %>%
  mutate(
    parameter = reorder(parameter, high - low)
  )

# Create left and right portions of each bar
plot_data <- tornado_data %>%
  mutate(
    left = pmin(low, baseline),
    right = pmax(low, baseline)
  )

plot_data2 <- tornado_data %>%
  mutate(
    left = pmin(high, baseline),
    right = pmax(high, baseline)
  )

ggplot() +
  
  # Lower-bound side
  geom_rect(
    data = plot_data,
    aes(
      xmin = left,
      xmax = right,
      ymin = as.numeric(parameter) - 0.32,
      ymax = as.numeric(parameter) + 0.32
    ),
    fill = "grey70"
  ) +
  
  # Upper-bound side
  geom_rect(
    data = plot_data2,
    aes(
      xmin = left,
      xmax = right,
      ymin = as.numeric(parameter) - 0.32,
      ymax = as.numeric(parameter) + 0.32
    ),
    fill = "grey40"
  ) +
  
  # Baseline
  geom_vline(
    xintercept = baseline,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Low values
  geom_text(
    data = tornado_data,
    aes(
      x = low,
      y = parameter,
      label = paste0(low, "%")
    ),
    hjust = ifelse(tornado_data$low < baseline, 1.15, -0.15),
    size = 4,
    family = "Arial"
  ) +
  
  # High values
  geom_text(
    data = tornado_data,
    aes(
      x = high,
      y = parameter,
      label = paste0(high, "%")
    ),
    hjust = ifelse(tornado_data$high < baseline, 1.15, -0.15),
    size = 4,
    family = "Arial"
  ) +
  
  scale_x_continuous(
    name = "Population Attributable Fraction",
    labels = function(x) paste0(x, "%"),
    limits = c(4, 27),
    breaks = seq(5, 25, 5)
  ) +
  
  labs(y = NULL) +
  
  theme_classic(base_family = "Arial") +
  
  theme(
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      color = "black"
    ),
    axis.text.x = element_text(
      size = 11,
      color = "black"
    ),
    axis.title.x = element_text(
      size = 12
    ),
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

