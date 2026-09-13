# -----------------------------
# USA RSV-LRTI related Asthma - Uncertainty Interval data
# -----------------------------

source("ImportData.R")
source("PARAMS_asthma.R")
source("asthmafunctions.R")
source("adjustmentformultipleepisode.R")

library(tidyverse)

# -----------------------------
# OR 3.0, 10,000 samples, original waning curve, 50% coverage, sensitive AF definition
# -----------------------------

#RSV encounters - post adjustment
tot_RSV_no_u <- OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a
tot_RSV_mAb_u <- OP_mAb_u_1st_a+ED_mAb_u_1st_a+Hosps_mAb_u_1st_a
tot_RSV_rsvPreF_u <- OP_rsvPreF_u_1st_a+ED_rsvPreF_u_1st_a+Hosps_rsvPreF_u_1st_a

# number without RSV-LRTI
tot_wo_RSV_no_u <- pop_tot - (tot_RSV_no_u)
tot_wo_RSV_mAb_u <- pop_tot - (tot_RSV_mAb_u)
tot_wo_RSV_rsvPreF_u <- pop_tot - (tot_RSV_rsvPreF_u)

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot, aOR_w_u_asthonly, tot_RSV_no_u, tot_wo_RSV_no_u)

# Generating new RR
RR_w_u <- aOR_w_u_asthonly/((1-(r_asth_norsv_u))+(r_asth_norsv_u*aOR_w_u_asthonly))
RR_l <- quantile(RR_w_u, 0.025, na.rm = TRUE)
RR_h <- quantile(RR_w_u, 0.975, na.rm = TRUE)

#-------------------------------#
# Using aRR from OR 
#-------------------------------#

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u, tot_RSV_no_u, tot_wo_RSV_no_u)

# number of asthma cases among those without RSV-LRTI
asth_wo_RSV_no_u <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u)
asth_wo_RSV_mAb_u <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u)
asth_wo_RSV_rsvPreF_u <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u)

# number of asthma cases among those with RSV-LRTI
asth_RSV_no_u <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u, RR_w_u)
asth_RSV_mAb_u <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u, RR_w_u)
asth_RSV_rsvPreF_u <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u, RR_w_u)

# all cause asthma
tot_asth_no_u <- tot_asth_func(asth_RSV_no_u, asth_wo_RSV_no_u)
tot_asth_mAb_u <- tot_asth_func(asth_RSV_mAb_u, asth_wo_RSV_mAb_u)
tot_asth_rsvPreF_u <- tot_asth_func(asth_RSV_rsvPreF_u, asth_wo_RSV_rsvPreF_u)

# total asthma per 100,000 population
tot_asth_no_pr_u <- tot_asth_no_u / pop_tot * 100000
tot_asth_mAb_pr_u <- tot_asth_mAb_u / pop_tot * 100000
tot_asth_rsvPreF_pr_u <- tot_asth_rsvPreF_u / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_u <- (tot_asth_no_u - tot_asth_mAb_u) / tot_asth_no_u * 100
tot_asth_rsvPreF_pd_u <- (tot_asth_no_u - tot_asth_rsvPreF_u) / tot_asth_no_u * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u)
asth_null_mAb_u <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u)
asth_null_rsvPreF_u <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u)

# RSV-LRTI attributable asthma
att_no_u <- asth_rsv_att_func(asth_RSV_no_u, asth_null_no_u)
att_mAb_u <- asth_rsv_att_func(asth_RSV_mAb_u, asth_null_mAb_u)
att_rsvPreF_u <-asth_rsv_att_func(asth_RSV_rsvPreF_u, asth_null_rsvPreF_u)

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_u <- att_no_u / pop_tot * 100000
att_mAb_pr_u <- att_mAb_u / pop_tot * 100000
att_rsvPreF_pr_u <- att_rsvPreF_u / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u <- (att_no_u - att_mAb_u) / att_no_u * 100
att_rsvPreF_pd_u <- (att_no_u - att_rsvPreF_u) / att_no_u * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u <- asth_no_rsv_func(pop_tot, r_asth_norsv_u)
all_rsv_prev_pr_u <- all_rsv_prev_u / pop_tot * 100000
all_rsv_prev_pd_u <- (tot_asth_no_u - all_rsv_prev_u) / tot_asth_no_u * 100

# CI work
#Figure 1
# RSV LRTI Cases 1 yr
quantile(tot_RSV_no_u, probs = c(0.025, 0.975))
quantile(tot_RSV_mAb_u, probs = c(0.025, 0.975))
quantile(tot_RSV_rsvPreF_u, probs = c(0.025, 0.975))
# RSV LRTI total encounter reduction
quantile(tot_RSV_no_u-tot_RSV_mAb_u, probs = c(0.025, 0.975))
# Percent decrease from status quo
quantile((tot_RSV_no_u-tot_RSV_mAb_u)/tot_RSV_no_u, probs = c(0.025, 0.975))
quantile((tot_RSV_no_u-tot_RSV_rsvPreF_u)/tot_RSV_no_u, probs = c(0.025, 0.975))
# All cause asthma cases by intervention
quantile(tot_asth_no_u, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_u, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_u, probs = c(0.025, 0.975))
quantile(all_rsv_prev_u, probs = c(0.025, 0.975))
# Difference in outcomes
quantile(tot_asth_no_u-tot_asth_mAb_u, probs = c(0.025, 0.975))
quantile(tot_asth_no_u-tot_asth_rsvPreF_u, probs = c(0.025, 0.975))
quantile(tot_asth_no_u-all_rsv_prev_u, probs = c(0.025, 0.975))
# all cause Asthma per 100,000 
quantile(tot_asth_no_pr_u, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_pr_u, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_pr_u, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pr_u, probs = c(0.025, 0.975))
# all cause Asthma percent reduction
quantile(tot_asth_mAb_pd_u, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_pd_u, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pd_u, probs = c(0.025, 0.975))
# RSV Attributable asthma
quantile(att_no_u, probs = c(0.025, 0.975))
quantile(att_mAb_u, probs = c(0.025, 0.975))
quantile(att_rsvPreF_u, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
quantile(att_no_u-att_mAb_u, probs = c(0.025, 0.975))
quantile(att_no_u-att_rsvPreF_u, probs = c(0.025, 0.975))
quantile(att_no_u, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
quantile(att_no_pr_u, probs = c(0.025, 0.975))
quantile(att_mAb_pr_u, probs = c(0.025, 0.975))
quantile(att_rsvPreF_pr_u, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
quantile(att_mAb_pd_u, probs = c(0.025, 0.975))
quantile(att_rsvPreF_pd_u, probs = c(0.025, 0.975))
#PAF - Levin vs direct calculation
quantile(((tot_RSV_no_u/pop_tot)*(RR_w_u-1))/(((tot_RSV_no_u/pop_tot)*(RR_w_u-1)+1)), probs = c(0.025, 0.975))
quantile((tot_asth_no_u-all_rsv_prev_u)/tot_asth_no_u, probs = c(0.025, 0.975))


# -----------------------------
# OR 2.45, 10,000 samples, original waning curve, 50% coverage, sensitive AF definition
# -----------------------------

#RSV encounters - post adjustment
tot_RSV_no_u_oldOR <- OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a
tot_RSV_mAb_u_oldOR <- OP_mAb_u_1st_a+ED_mAb_u_1st_a+Hosps_mAb_u_1st_a
tot_RSV_rsvPreF_u_oldOR <- OP_rsvPreF_u_1st_a+ED_rsvPreF_u_1st_a+Hosps_rsvPreF_u_1st_a

# number without RSV-LRTI
tot_wo_RSV_no_u_oldOR <- pop_tot - (tot_RSV_no_u_oldOR)
tot_wo_RSV_mAb_u_oldOR <- pop_tot - (tot_RSV_mAb_u_oldOR)
tot_wo_RSV_rsvPreF_u_oldOR <- pop_tot - (tot_RSV_rsvPreF_u_oldOR)

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_oldOR <- prev_no_rsv_func(prev_tot_u, pop_tot, aOR_w_u, tot_RSV_no_u_oldOR, tot_wo_RSV_no_u_oldOR)

# Generating new RR
RR_w_u_oldOR <- aOR_w_u/((1-(r_asth_norsv_u_oldOR))+(r_asth_norsv_u_oldOR*aOR_w_u))
RR_l <- quantile(RR_w_u_oldOR, 0.025, na.rm = TRUE)
RR_h <- quantile(RR_w_u_oldOR, 0.975, na.rm = TRUE)

#-------------------------------#
# Using aRR from OR 
#-------------------------------#

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_oldOR <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u_oldOR, tot_RSV_no_u_oldOR, tot_wo_RSV_no_u_oldOR)

# number of asthma cases among those without RSV-LRTI
asth_wo_RSV_no_u_oldOR <- asth_no_rsv_func(tot_wo_RSV_no_u_oldOR, r_asth_norsv_u_oldOR)
asth_wo_RSV_mAb_u_oldOR <- asth_no_rsv_func(tot_wo_RSV_mAb_u_oldOR, r_asth_norsv_u_oldOR)
asth_wo_RSV_rsvPreF_u_oldOR <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u_oldOR, r_asth_norsv_u_oldOR)

# number of asthma cases among those with RSV-LRTI
asth_RSV_no_u_oldOR <- asth_rsv_func(tot_RSV_no_u_oldOR, r_asth_norsv_u_oldOR, RR_w_u_oldOR)
asth_RSV_mAb_u_oldOR <- asth_rsv_func(tot_RSV_mAb_u_oldOR, r_asth_norsv_u_oldOR, RR_w_u_oldOR)
asth_RSV_rsvPreF_u_oldOR <- asth_rsv_func(tot_RSV_rsvPreF_u_oldOR, r_asth_norsv_u_oldOR, RR_w_u_oldOR)

# all cause asthma
tot_asth_no_u_oldOR <- tot_asth_func(asth_RSV_no_u_oldOR, asth_wo_RSV_no_u_oldOR)
tot_asth_mAb_u_oldOR <- tot_asth_func(asth_RSV_mAb_u_oldOR, asth_wo_RSV_mAb_u_oldOR)
tot_asth_rsvPreF_u_oldOR <- tot_asth_func(asth_RSV_rsvPreF_u_oldOR, asth_wo_RSV_rsvPreF_u_oldOR)

# total asthma per 100,000 population
tot_asth_no_pr_u_oldOR <- tot_asth_no_u_oldOR / pop_tot * 100000
tot_asth_mAb_pr_u_oldOR <- tot_asth_mAb_u_oldOR / pop_tot * 100000
tot_asth_rsvPreF_pr_u_oldOR <- tot_asth_rsvPreF_u_oldOR / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_u_oldOR <- (tot_asth_no_u_oldOR - tot_asth_mAb_u_oldOR) / tot_asth_no_u_oldOR * 100
tot_asth_rsvPreF_pd_u_oldOR <- (tot_asth_no_u_oldOR - tot_asth_rsvPreF_u_oldOR) / tot_asth_no_u_oldOR * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u_oldOR <- asth_rsv_null_func(tot_RSV_no_u_oldOR, r_asth_norsv_u_oldOR)
asth_null_mAb_u_oldOR <- asth_rsv_null_func(tot_RSV_mAb_u_oldOR, r_asth_norsv_u_oldOR)
asth_null_rsvPreF_u_oldOR <-asth_rsv_null_func(tot_RSV_rsvPreF_u_oldOR, r_asth_norsv_u_oldOR)

# RSV-LRTI attributable asthma
att_no_u_oldOR <- asth_rsv_att_func(asth_RSV_no_u_oldOR, asth_null_no_u_oldOR)
att_mAb_u_oldOR <- asth_rsv_att_func(asth_RSV_mAb_u_oldOR, asth_null_mAb_u_oldOR)
att_rsvPreF_u_oldOR <-asth_rsv_att_func(asth_RSV_rsvPreF_u_oldOR, asth_null_rsvPreF_u_oldOR)

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_u_oldOR <- att_no_u_oldOR / pop_tot * 100000
att_mAb_pr_u_oldOR <- att_mAb_u_oldOR / pop_tot * 100000
att_rsvPreF_pr_u_oldOR <- att_rsvPreF_u_oldOR / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u_oldOR <- (att_no_u_oldOR - att_mAb_u_oldOR) / att_no_u_oldOR * 100
att_rsvPreF_pd_u_oldOR <- (att_no_u_oldOR - att_rsvPreF_u_oldOR) / att_no_u_oldOR * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u_oldOR <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_oldOR)
all_rsv_prev_pr_u_oldOR<- all_rsv_prev_u_oldOR / pop_tot * 100000
all_rsv_prev_pd_u_oldOR <- (tot_asth_no_u_oldOR - all_rsv_prev_u_oldOR) / tot_asth_no_u_oldOR * 100

# CI work
#Figure 1
# RSV LRTI Cases 1 yr
quantile(tot_RSV_no_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_RSV_mAb_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_RSV_rsvPreF_u_oldOR, probs = c(0.025, 0.975))
# RSV LRTI total encounter reduction
quantile(tot_RSV_no_u_oldOR-tot_RSV_mAb_u_oldOR, probs = c(0.025, 0.975))
# Percent decrease from status quo
quantile((tot_RSV_no_u_oldOR-tot_RSV_mAb_u_oldOR)/tot_RSV_no_u_oldOR, probs = c(0.025, 0.975))
quantile((tot_RSV_no_u_oldOR-tot_RSV_rsvPreF_u_oldOR)/tot_RSV_no_u_oldOR, probs = c(0.025, 0.975))
# All cause asthma cases by intervention
quantile(tot_asth_no_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_u_oldOR, probs = c(0.025, 0.975))
quantile(all_rsv_prev_u_oldOR, probs = c(0.025, 0.975))
# Difference in outcomes
quantile(tot_asth_no_u_oldOR-tot_asth_mAb_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_asth_no_u_oldOR-tot_asth_rsvPreF_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_asth_no_u_oldOR-all_rsv_prev_u_oldOR, probs = c(0.025, 0.975))
# all cause Asthma per 100,000 
quantile(tot_asth_no_pr_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_pr_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_pr_u_oldOR, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pr_u_oldOR, probs = c(0.025, 0.975))
# all cause Asthma percent reduction
quantile(tot_asth_mAb_pd_u_oldOR, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_pd_u_oldOR, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pd_u_oldOR, probs = c(0.025, 0.975))
# RSV Attributable asthma
quantile(att_no_u_oldOR, probs = c(0.025, 0.975))
quantile(att_mAb_u_oldOR, probs = c(0.025, 0.975))
quantile(att_rsvPreF_u_oldOR, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
quantile(att_no_u_oldOR-att_mAb_u_oldOR, probs = c(0.025, 0.975))
quantile(att_no_u_oldOR-att_rsvPreF_u_oldOR, probs = c(0.025, 0.975))
quantile(att_no_u_oldOR, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
quantile(att_no_pr_u_oldOR, probs = c(0.025, 0.975))
quantile(att_mAb_pr_u_oldOR, probs = c(0.025, 0.975))
quantile(att_rsvPreF_pr_u_oldOR, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
quantile(att_mAb_pd_u_oldOR, probs = c(0.025, 0.975))
quantile(att_rsvPreF_pd_u_oldOR, probs = c(0.025, 0.975))
#PAF - Levin vs direct calculation
quantile(((tot_RSV_no_u_oldOR/pop_tot)*(RR_w_u_oldOR-1))/(((tot_RSV_no_u_oldOR/pop_tot)*(RR_w_u_oldOR-1)+1)), probs = c(0.025, 0.975))
quantile((tot_asth_no_u_oldOR-all_rsv_prev_u_oldOR)/tot_asth_no_u_oldOR, probs = c(0.025, 0.975))


# -----------------------------
# OR 3.0, 50% coverage, original waning curve, specific definition
# -----------------------------

#RSV LRTI encounters
tot_RSV_no_u_spec <- OP_no_u_1st_a_spec+ED_no_u_1st_a_spec+Hosps_no_u_1st_a_spec
tot_RSV_mAb_u_spec <- OP_mAb_u_1st_a_spec+ED_mAb_u_1st_a_spec+Hosps_mAb_u_1st_a_spec

# number without RSV-LRTI
tot_wo_RSV_no_u_spec <- pop_tot - (tot_RSV_no_u_spec)
tot_wo_RSV_mAb_u_spec <- pop_tot - (tot_RSV_mAb_u_spec)

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_spec <- prev_no_rsv_func(prev_tot_u, pop_tot, aOR_w_u_asthonly, tot_RSV_no_u_spec, tot_wo_RSV_no_u_spec)

# Generating RR from OR
RR_w_u_spec <- aOR_w_u_asthonly /((1-(r_asth_norsv_u_spec))+(r_asth_norsv_u_spec*aOR_w_u_asthonly))
RR_l_spec <- quantile(RR_w_u_spec, 0.025, na.rm = TRUE)
RR_h_spec <- quantile(RR_w_u_spec, 0.975, na.rm = TRUE)

#-------------------------------#
# Using aRR from OR 
#-------------------------------#

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_spec <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u_spec, tot_RSV_no_u_spec, tot_wo_RSV_no_u_spec)

# number of asthma cases among those without RSV-LRTI
asth_wo_RSV_no_u_spec <- asth_no_rsv_func(tot_wo_RSV_no_u_spec, r_asth_norsv_u_spec)
asth_wo_RSV_mAb_u_spec <- asth_no_rsv_func(tot_wo_RSV_mAb_u_spec, r_asth_norsv_u_spec)

# number of asthma cases among those with RSV-LRTI
asth_RSV_no_u_spec <- asth_rsv_func(tot_RSV_no_u_spec, r_asth_norsv_u_spec, RR_w_u_spec)
asth_RSV_mAb_u_spec <- asth_rsv_func(tot_RSV_mAb_u_spec, r_asth_norsv_u_spec, RR_w_u_spec)

# all cause with asthma
tot_asth_no_u_spec <- tot_asth_func(asth_RSV_no_u_spec, asth_wo_RSV_no_u_spec)
tot_asth_mAb_u_spec <- tot_asth_func(asth_RSV_mAb_u_1st_adj_spec, asth_wo_RSV_mAb_u_spec)

# total asthma per 100,000 population
tot_asth_no_pr_u_spec <- tot_asth_no_u_spec / pop_tot * 100000
tot_asth_mAb_pr_u_spec <- tot_asth_mAb_u_spec / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_u_spec <- (tot_asth_no_u_spec - tot_asth_mAb_u_spec) / tot_asth_no_u_spec * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u_spec <- asth_rsv_null_func(tot_RSV_no_u_spec, r_asth_norsv_u_spec)
asth_null_mAb_u_spec <- asth_rsv_null_func(tot_RSV_mAb_u_spec, r_asth_norsv_u_spec)

# RSV-LRTI attributable asthma
att_no_u_spec <- asth_rsv_att_func(asth_RSV_no_u_spec, asth_null_no_u_spec)
att_mAb_u_spec <- asth_rsv_att_func(asth_RSV_mAb_u_spec, asth_null_mAb_u_spec)

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_u_spec <- att_no_u_spec / pop_tot * 100000
att_mAb_pr_u_spec <- att_mAb_u_spec / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u_spec <- (att_no_u_spec - att_mAb_u_spec) / att_no_u_spec * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u_spec <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_spec)
all_rsv_prev_pr_u_spec <- all_rsv_prev_u_spec / pop_tot * 100000
all_rsv_prev_pd_u_spec <- (tot_asth_no_u_spec - all_rsv_prev_u_spec) / tot_asth_no_u_spec * 100

# CI work
#Figure 1
# RSV LRTI encounters
quantile(tot_RSV_no_u_spec, probs = c(0.025, 0.975))
quantile(tot_RSV_mAb_u_spec, probs = c(0.025, 0.975))
# RSV LRTI total encounter reduction
quantile(tot_RSV_no_u_spec-tot_RSV_mAb_u_spec, probs = c(0.025, 0.975))
# Percent decrease from status quo
quantile((tot_RSV_no_u_spec-tot_RSV_mAb_u_spec)/tot_RSV_no_u_spec, probs = c(0.025, 0.975))
# all cause asthma cases by intervention
quantile(tot_asth_no_u_spec, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_u_spec, probs = c(0.025, 0.975))
quantile(all_rsv_prev_u_spec, probs = c(0.025, 0.975))
# Difference in all cause asthma outcomes
quantile(tot_asth_no_u_spec-tot_asth_mAb_u_spec, probs = c(0.025, 0.975))
quantile(tot_asth_no_u_spec-all_rsv_prev_u_spec, probs = c(0.025, 0.975))
# all cause Asthma per 100,000 
quantile(tot_asth_no_pr_u_spec, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_pr_u_spec, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pr_u_spec, probs = c(0.025, 0.975))
# all cause Asthma percent reduction
quantile(tot_asth_mAb_pd_u_spec, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pd_u_spec, probs = c(0.025, 0.975))
# RSV Attributable asthma
quantile(att_no_u_spec, probs = c(0.025, 0.975))
quantile(att_mAb_u_spec, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
quantile(att_no_u_spec-att_mAb_u_spec, probs = c(0.025, 0.975))
quantile(att_no_u_spec, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
quantile(att_no_pr_u_spec, probs = c(0.025, 0.975))
quantile(att_mAb_pr_u_spec, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
quantile(att_mAb_pd_u_spec, probs = c(0.025, 0.975))
#PAF - Levin vs direct calculation
quantile(((tot_RSV_no_u_spec/pop_tot)*(RR_w_u_spec-1))/(((tot_RSV_no_u_spec/pop_tot)*(RR_w_u_spec-1)+1)), probs = c(0.025, 0.975))
quantile((tot_asth_no_u_spec-all_rsv_prev_u_spec)/tot_asth_no_u_spec, probs = c(0.025, 0.975))


# -----------------------------
# 10,000 samples, OR 3.0, new weaning curve, 50% coverage, sensitive definition
# -----------------------------

#Combining yearly DFs by scenario - post adjustment for outcomes during 1st year
tot_RSV_no_u_adjc <- OP_no_u_adjc_a+ED_no_u_adjc_a+Hosps_no_u_adjc_a
tot_RSV_mAb_u_adjc <- OP_mAb_u_adjc_a+ED_mAb_u_adjc_a+Hosps_mAb_u_adjc_a

# number without RSV-LRTI
tot_wo_RSV_no_u_adjc <- pop_tot - (tot_RSV_no_u_adjc)
tot_wo_RSV_mAb_u_adjc <- pop_tot - (tot_RSV_mAb_u_adjc)

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_adjc <- prev_no_rsv_func(prev_tot_u, pop_tot, aOR_w_u_asthonly, tot_RSV_no_u_adjc, tot_wo_RSV_no_u_adjc)

# generating RR from OR
RR_w_u_adjc <- aOR_w_u_asthonly/((1-(r_asth_norsv_u_adjc))+(r_asth_norsv_u_adjc*aOR_w_u_asthonly))
RR_l_adjc <- quantile(RR_w_u_adjc, 0.025, na.rm = TRUE)
RR_h_adjc <- quantile(RR_w_u_adjc, 0.975, na.rm = TRUE)

#-------------------------------#
# Using aRR from OR 
#-------------------------------#

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_adjc <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u_adjc, tot_RSV_no_u_adjc, tot_wo_RSV_no_u_adjc)

# number of asthma cases among those without RSV-LRTI
asth_wo_RSV_no_u_adjc <- asth_no_rsv_func(tot_wo_RSV_no_u_adjc, r_asth_norsv_u_adjc)
asth_wo_RSV_mAb_u_adjc <- asth_no_rsv_func(tot_wo_RSV_mAb_u_adjc, r_asth_norsv_u_adjc)

# number of asthma cases among those with RSV-LRTI
asth_RSV_no_u_adjc <- asth_rsv_func(tot_RSV_no_u_adjc, r_asth_norsv_u_adjc, RR_w_u_adjc)
asth_RSV_mAb_u_adjc <- asth_rsv_func(tot_RSV_mAb_u_adjc, r_asth_norsv_u_adjc, RR_w_u_adjc)

# all cause  asthma
tot_asth_no_u_adjc <- tot_asth_func(asth_RSV_no_u_adjc, asth_wo_RSV_no_u_adjc)
tot_asth_mAb_u_adjc <- tot_asth_func(asth_RSV_mAb_u_adjc, asth_wo_RSV_mAb_u_adjc)

# total asthma per 100,000 population
tot_asth_no_pr_u_adjc <- tot_asth_no_u_adjc / pop_tot * 100000
tot_asth_mAb_pr_u_adjc <- tot_asth_mAb_u_adjc / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_u_adjc <- (tot_asth_no_u_adjc - tot_asth_mAb_u_adjc) / tot_asth_no_u_adjc * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u_adjc <- asth_rsv_null_func(tot_RSV_no_u_adjc, r_asth_norsv_u_adjc)
asth_null_mAb_u_adjc <- asth_rsv_null_func(tot_RSV_mAb_u_adjc, r_asth_norsv_u_adjc)

# RSV-LRTI attributable asthma
att_no_u_adjc <- asth_rsv_att_func(asth_RSV_no_u_adjc, asth_null_no_u_adjc)
att_mAb_u_adjc <- asth_rsv_att_func(asth_RSV_mAb_u_adjc, asth_null_mAb_u_adjc)

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_u_adjc <- att_no_u_adjc / pop_tot * 100000
att_mAb_pr_u_adjc <- att_mAb_u_adjc / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u_adjc <- (att_no_u_adjc - att_mAb_u_adjc) / att_no_u_adjc * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u_adjc <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_adjc)
all_rsv_prev_pr_u_adjc <- all_rsv_prev_u_adjc / pop_tot * 100000
all_rsv_prev_pd_u_adjc <- (tot_asth_no_u_adjc - all_rsv_prev_u_adjc) / tot_asth_no_u_adjc * 100

# CI work
#Figure 1
# RSV LRTI encounters
quantile(tot_RSV_no_u_adjc, probs = c(0.025, 0.975))
quantile(tot_RSV_mAb_u_adjc, probs = c(0.025, 0.975))
#absolute RSV LRTI encounter reduction
quantile(tot_RSV_no_u_adjc-tot_RSV_mAb_u_adjc, probs = c(0.025, 0.975))
# Percent decrease from status quo
quantile((tot_RSV_no_u_adjc-tot_RSV_mAb_u_adjc)/tot_RSV_no_u_adjc, probs = c(0.025, 0.975))
# all cause asthma cases by intervention
quantile(tot_asth_no_u_adjc, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_u_adjc, probs = c(0.025, 0.975))
quantile(all_rsv_prev_u_adjc, probs = c(0.025, 0.975))
# Difference in outcomes
quantile(tot_asth_no_u_adjc-tot_asth_mAb_u_adjc, probs = c(0.025, 0.975))
quantile(tot_asth_no_u_adjc-all_rsv_prev_u_adjc, probs = c(0.025, 0.975))
# Total Asthma per 100,000 
quantile(tot_asth_no_pr_u_adjc, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_pr_u_adjc, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pr_u_adjc, probs = c(0.025, 0.975))
# Total Asthma percent reduction
quantile(tot_asth_mAb_pd_u_adjc, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pd_u_adjc, probs = c(0.025, 0.975))
# RSV Attributable asthma
quantile(att_no_u_adjc, probs = c(0.025, 0.975))
quantile(att_mAb_u_adjc, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
quantile(att_no_u_adjc-att_mAb_u_adjc, probs = c(0.025, 0.975))
quantile(att_no_u_adjc, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
quantile(att_no_pr_u_adjc, probs = c(0.025, 0.975))
quantile(att_mAb_pr_u_adjc, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
quantile(att_mAb_pd_u_adjc, probs = c(0.025, 0.975))
#PAF - Levin vs direct calculation
quantile(((tot_RSV_no_u_adjc/pop_tot)*(RR_w_u_adjc-1))/(((tot_RSV_no_u_adjc/pop_tot)*(RR_w_u_adjc-1)+1)), probs = c(0.025, 0.975))
quantile((tot_asth_no_u_adjc_adj-all_rsv_prev_u_adjc)/tot_asth_no_u_adjc, probs = c(0.025, 0.975))

# -----------------------------
# 10,000 samples, OR 3.0, original waning curve, 90% coverage, sensitive definition
# -----------------------------

#RSV LRTI encounters
tot_RSV_no_u_adjcov <- OP_no_u_adjcov_a+ED_no_u_adjcov_a+Hosps_no_u_adjcov_a
tot_RSV_mAb_u_adjcov <- OP_mAb_u_adjcov_a+ED_mAb_u_adjcov_a+Hosps_mAb_u_adjcov_a

# number without RSV-LRTI
tot_wo_RSV_no_u_adjcov <- pop_tot - (tot_RSV_no_u_adjcov)
tot_wo_RSV_mAb_u_adjcov <- pop_tot - (tot_RSV_mAb_u_adjcov)

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_adjcov <- prev_no_rsv_func(prev_tot_u, pop_tot, aOR_w_u_asthonly, tot_RSV_no_u_adjcov, tot_wo_RSV_no_u_adjcov)

# RR from OR
RR_w_u_adjcov <- aOR_w_u_asthonly/((1-(r_asth_norsv_u_adjcov))+(r_asth_norsv_u_adjcov*aOR_w_u_asthonly))
RR_l_adjcov <- quantile(RR_w_u_adjcov, 0.025, na.rm = TRUE)
RR_h_adjcov <- quantile(RR_w_u_adjcov, 0.975, na.rm = TRUE)

#-------------------------------#
# Using aRR from OR 
#-------------------------------#

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_adjcov <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u_adjcov, tot_RSV_no_u_adjcov, tot_wo_RSV_no_u_adjcov)

# number of asthma cases among those without RSV-LRTI
asth_wo_RSV_no_u_adjcov <- asth_no_rsv_func(tot_wo_RSV_no_u_adjcov, r_asth_norsv_u_adjcov)
asth_wo_RSV_mAb_u_adjcov <- asth_no_rsv_func(tot_wo_RSV_mAb_u_adjcov, r_asth_norsv_u_adjcov)

# number of asthma cases among those with RSV-LRTI
asth_RSV_no_u_adjcov <- asth_rsv_func(tot_RSV_no_u_adjcov, r_asth_norsv_u_adjcov, RR_w_u_adjcov)
asth_RSV_mAb_u_adjcov <- asth_rsv_func(tot_RSV_mAb_u_adjcov, r_asth_norsv_u_adjcov, RR_w_u_adjcov)

# all cause with asthma
tot_asth_no_u_adjcov <- tot_asth_func(asth_RSV_no_u_adjcov, asth_wo_RSV_no_u_adjcov)
tot_asth_mAb_u_adjcov <- tot_asth_func(asth_RSV_mAb_u_adjcov, asth_wo_RSV_mAb_u_adjcov)

# total asthma per 100,000 population
tot_asth_no_pr_u_adjcov <- tot_asth_no_u_adjcov / pop_tot * 100000
tot_asth_mAb_pr_u_adjcov <- tot_asth_mAb_u_adjcov / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_u_adjcov <- (tot_asth_no_u_adjcov - tot_asth_mAb_u_adjcov) / tot_asth_no_u_adjcov * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u_adjcov <- asth_rsv_null_func(tot_RSV_no_u_adjcov, r_asth_norsv_u_adjcov)
asth_null_mAb_u_adjcov <- asth_rsv_null_func(tot_RSV_mAb_u_adjcov, r_asth_norsv_u_adjcov)

# RSV-LRTI attributable asthma
att_no_u_adjcov <- asth_rsv_att_func(asth_RSV_no_u_adjcov, asth_null_no_u_adjcov)
att_mAb_u_adjcov <- asth_rsv_att_func(asth_RSV_mAb_u_adjcov, asth_null_mAb_u_adjcov)

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_u_adjcov <- att_no_u_adjcov / pop_tot * 100000
att_mAb_pr_u_adjcov <- att_mAb_u_adjcov / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u_adjcov <- (att_no_u_adjcov - att_mAb_u_adjcov) / att_no_u_adjcov * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u_adjcov <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_adjcov)
all_rsv_prev_pr_u_adjcov <- all_rsv_prev_u_adjcov / pop_tot * 100000
all_rsv_prev_pd_u_adjcov <- (tot_asth_no_u_adjcov - all_rsv_prev_u_adjcov) / tot_asth_no_u_adjcov * 100

# CI work
#Figure 1
# RSV LRTI Cases 1 yr
quantile(tot_RSV_no_u_adjcov, probs = c(0.025, 0.975))
quantile(tot_RSV_mAb_u_adjcov, probs = c(0.025, 0.975))
# RSV LRTI encounters difference
quantile(tot_RSV_no_u_adjcov-tot_RSV_mAb_u_adjcov, probs = c(0.025, 0.975))
# Percent decrease from status quo
quantile((tot_RSV_no_u_adjcov-tot_RSV_mAb_u_adjcov)/tot_RSV_no_u_adjcov, probs = c(0.025, 0.975))
# all cause asthma 
quantile(tot_asth_no_u_adjcov, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_u_adjcov, probs = c(0.025, 0.975))
# Difference in outcomes
quantile(tot_asth_no_u_adjcov-tot_asth_mAb_u_adjcov, probs = c(0.025, 0.975))
# Total Asthma per 100,000 
quantile(tot_asth_no_pr_u_adjcov, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_pr_u_adjcov, probs = c(0.025, 0.975))
# Total Asthma percent reduction
quantile(tot_asth_mAb_pd_u_adjcov, probs = c(0.025, 0.975))
# RSV Attributable asthma
quantile(att_no_u_adjcov, probs = c(0.025, 0.975))
quantile(att_mAb_u_adjcov, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
quantile(att_no_u_adjcov-att_mAb_u_adjcov, probs = c(0.025, 0.975))
quantile(att_no_u_adjcov, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
quantile(att_no_pr_u_adjcov, probs = c(0.025, 0.975))
quantile(att_mAb_pr_u_adjcov, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
quantile(att_mAb_pd_u_adjcov, probs = c(0.025, 0.975))
#PAF - Levin vs direct calculation
quantile(((tot_RSV_no_u_adjcov/pop_tot)*(RR_w_u_adjcov-1))/(((tot_RSV_no_u_adjcov/pop_tot)*(RR_w_u_adjcov-1)+1)), probs = c(0.025, 0.975))
quantile((tot_asth_no_u_adjcov-all_rsv_prev_u_adjcov)/tot_asth_no_u_adjcov, probs = c(0.025, 0.975))



# -----------------------------
# OR 2.45, 10,000 samples, original waning curve, 50% coverage, sensitive AF definition
# -----------------------------

#RSV encounters - post adjustment
tot_RSV_no_u_old <- OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a
tot_RSV_mAb_u_old <- OP_mAb_u_old_1st_a+ED_mAb_u_old_1st_a+Hosps_mAb_u_old_1st_a
tot_RSV_rsvPreF_u_old <- OP_rsvPreF_u_old_1st_a+ED_rsvPreF_u_old_1st_a+Hosps_rsvPreF_u_old_1st_a

# number without RSV-LRTI
tot_wo_RSV_no_u_old <- pop_tot - (tot_RSV_no_u_old)
tot_wo_RSV_mAb_u_old <- pop_tot - (tot_RSV_mAb_u_old)
tot_wo_RSV_rsvPreF_u_old <- pop_tot - (tot_RSV_rsvPreF_u_old)

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_old <- prev_no_rsv_func(prev_tot_u_old, pop_tot, aOR_w_u_old, tot_RSV_no_u_old, tot_wo_RSV_no_u_old)

# Generating new RR
RR_w_u_old <- aOR_w_u_old/((1-(r_asth_norsv_u_old))+(r_asth_norsv_u_old*aOR_w_u_old))
RR_l <- quantile(RR_w_u_old, 0.025, na.rm = TRUE)
RR_h <- quantile(RR_w_u_old, 0.975, na.rm = TRUE)

#-------------------------------#
# Using aRR from OR 
#-------------------------------#

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_u_old <- prev_no_rsv_func(prev_tot_u_old, pop_tot, RR_w_u_old, tot_RSV_no_u_old, tot_wo_RSV_no_u_old)

# number of asthma cases among those without RSV-LRTI
asth_wo_RSV_no_u_old <- asth_no_rsv_func(tot_wo_RSV_no_u_old, r_asth_norsv_u_old)
asth_wo_RSV_mAb_u_old <- asth_no_rsv_func(tot_wo_RSV_mAb_u_old, r_asth_norsv_u_old)
asth_wo_RSV_rsvPreF_u_old <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u_old, r_asth_norsv_u_old)

# number of asthma cases among those with RSV-LRTI
asth_RSV_no_u_old <- asth_rsv_func(tot_RSV_no_u_old, r_asth_norsv_u_old, RR_w_u_old)
asth_RSV_mAb_u_old <- asth_rsv_func(tot_RSV_mAb_u_old, r_asth_norsv_u_old, RR_w_u_old)
asth_RSV_rsvPreF_u_old <- asth_rsv_func(tot_RSV_rsvPreF_u_old, r_asth_norsv_u_old, RR_w_u_old)

# all cause asthma
tot_asth_no_u_old <- tot_asth_func(asth_RSV_no_u_old, asth_wo_RSV_no_u_old)
tot_asth_mAb_u_old <- tot_asth_func(asth_RSV_mAb_u_old, asth_wo_RSV_mAb_u_old)
tot_asth_rsvPreF_u_old <- tot_asth_func(asth_RSV_rsvPreF_u_old, asth_wo_RSV_rsvPreF_u_old)

# total asthma per 100,000 population
tot_asth_no_pr_u_old <- tot_asth_no_u_old / pop_tot * 100000
tot_asth_mAb_pr_u_old <- tot_asth_mAb_u_old / pop_tot * 100000
tot_asth_rsvPreF_pr_u_old <- tot_asth_rsvPreF_u_old / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_u_old <- (tot_asth_no_u_old - tot_asth_mAb_u_old) / tot_asth_no_u_old * 100
tot_asth_rsvPreF_pd_u_old <- (tot_asth_no_u_old - tot_asth_rsvPreF_u_old) / tot_asth_no_u_old * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_u_old <- asth_rsv_null_func(tot_RSV_no_u_old, r_asth_norsv_u_old)
asth_null_mAb_u_old <- asth_rsv_null_func(tot_RSV_mAb_u_old, r_asth_norsv_u_old)
asth_null_rsvPreF_u_old <-asth_rsv_null_func(tot_RSV_rsvPreF_u_old, r_asth_norsv_u_old)

# RSV-LRTI attributable asthma
att_no_u_old <- asth_rsv_att_func(asth_RSV_no_u_old, asth_null_no_u_old)
att_mAb_u_old <- asth_rsv_att_func(asth_RSV_mAb_u_old, asth_null_mAb_u_old)
att_rsvPreF_u_old <-asth_rsv_att_func(asth_RSV_rsvPreF_u_old, asth_null_rsvPreF_u_old)

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_u_old <- att_no_u_old / pop_tot * 100000
att_mAb_pr_u_old <- att_mAb_u_old / pop_tot * 100000
att_rsvPreF_pr_u_old <- att_rsvPreF_u_old / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_u_old <- (att_no_u_old - att_mAb_u_old) / att_no_u_old * 100
att_rsvPreF_pd_u_old <- (att_no_u_old - att_rsvPreF_u_old) / att_no_u_old * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_u_old <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_old)
all_rsv_prev_pr_u_old<- all_rsv_prev_u_old / pop_tot * 100000
all_rsv_prev_pd_u_old <- (tot_asth_no_u_old - all_rsv_prev_u_old) / tot_asth_no_u_old * 100

# CI work
#Figure 1
# RSV LRTI Cases 1 yr
quantile(tot_RSV_no_u_old, probs = c(0.025, 0.975))
quantile(tot_RSV_mAb_u_old, probs = c(0.025, 0.975))
quantile(tot_RSV_rsvPreF_u_old, probs = c(0.025, 0.975))
# RSV LRTI total encounter reduction
quantile(tot_RSV_no_u_old-tot_RSV_mAb_u_old, probs = c(0.025, 0.975))
# Percent decrease from status quo
quantile((tot_RSV_no_u_old-tot_RSV_mAb_u_old)/tot_RSV_no_u_old, probs = c(0.025, 0.975))
quantile((tot_RSV_no_u_old-tot_RSV_rsvPreF_u_old)/tot_RSV_no_u_old, probs = c(0.025, 0.975))
# All cause asthma cases by intervention
quantile(tot_asth_no_u_old, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_u_old, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_u_old, probs = c(0.025, 0.975))
quantile(all_rsv_prev_u_old, probs = c(0.025, 0.975))
# Difference in outcomes
quantile(tot_asth_no_u_old-tot_asth_mAb_u_old, probs = c(0.025, 0.975))
quantile(tot_asth_no_u_old-tot_asth_rsvPreF_u_old, probs = c(0.025, 0.975))
quantile(tot_asth_no_u_old-all_rsv_prev_u_old, probs = c(0.025, 0.975))
# all cause Asthma per 100,000 
quantile(tot_asth_no_pr_u_old, probs = c(0.025, 0.975))
quantile(tot_asth_mAb_pr_u_old, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_pr_u_old, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pr_u_old, probs = c(0.025, 0.975))
# all cause Asthma percent reduction
quantile(tot_asth_mAb_pd_u_old, probs = c(0.025, 0.975))
quantile(tot_asth_rsvPreF_pd_u_old, probs = c(0.025, 0.975))
quantile(all_rsv_prev_pd_u_old, probs = c(0.025, 0.975))
# RSV Attributable asthma
quantile(att_no_u_old, probs = c(0.025, 0.975))
quantile(att_mAb_u_old, probs = c(0.025, 0.975))
quantile(att_rsvPreF_u_old, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
quantile(att_no_u_old-att_mAb_u_old, probs = c(0.025, 0.975))
quantile(att_no_u_old-att_rsvPreF_u_old, probs = c(0.025, 0.975))
quantile(att_no_u_old, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
quantile(att_no_pr_u_old, probs = c(0.025, 0.975))
quantile(att_mAb_pr_u_old, probs = c(0.025, 0.975))
quantile(att_rsvPreF_pr_u_old, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
quantile(att_mAb_pd_u_old, probs = c(0.025, 0.975))
quantile(att_rsvPreF_pd_u_old, probs = c(0.025, 0.975))
#PAF - Levin vs direct calculation
quantile(((tot_RSV_no_u_old/pop_tot)*(RR_w_u_old-1))/(((tot_RSV_no_u_old/pop_tot)*(RR_w_u_old-1)+1)), probs = c(0.025, 0.975))
quantile((tot_asth_no_u_old-all_rsv_prev_u_old)/tot_asth_no_u_old, probs = c(0.025, 0.975))


#extra code/left over

# -----------------------------
# Combining 1st and 2nd year of life
# -----------------------------

#Combining yearly DFs by scenario - post adjustment for outcomes during three years
#tot_RSV_no_u <- OP_no_u_1st_a+OP_no_u_2nd_a+ED_no_u_1st_a+ED_no_u_2nd_a+Hosps_no_u_1st_a+Hosps_no_u_2nd_a
#tot_RSV_mAb_u <- OP_mAb_u_1st_a+OP_mAb_u_2nd_a+ED_mAb_u_1st_a+ED_mAb_u_2nd_a+Hosps_mAb_u_1st_a+Hosps_mAb_u_2nd_a
#tot_RSV_rsvPreF_u <- OP_rsvPreF_df_1st_a+OP_rsvPreF_df_2nd_a+ED_rsvPreF_df_1st_a+ED_rsvPreF_df_2nd_a+Hosps_rsvPreF_df_1st_a+Hosps_rsvPreF_df_2nd_a

# number of kids without RSV-LRTI associated for each strategy
#tot_wo_RSV_no_u <- pop_tot - (tot_RSV_no_u)
#tot_wo_RSV_mAb_u <- pop_tot - (tot_RSV_mAb_u)
#tot_wo_RSV_rsvPreF_u <- pop_tot - (tot_RSV_rsvPreF_u)

# calculate rate/prevalence of asthma among those without RSV-LRTI -> will feed into OR to RR 
#r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot, aOR_w_u, tot_RSV_no_u, tot_wo_RSV_no_u)

#OR -> RR calculation
#RR_w_u <- aOR_w_u/((1-(r_asth_norsv_u))+(r_asth_norsv_u*aOR_w_u))
#RR_l <- quantile(RR_w_u, 0.025, na.rm = TRUE)
#RR_h <- quantile(RR_w_u, 0.975, na.rm = TRUE)

# Using RR calculated from OR
# ------------------------------------- #

# calculate rate/prevalence of asthma among those without RSV-LRTI - new RR value
#r_asth_norsv_u_adj <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u, tot_RSV_no_u, tot_wo_RSV_no_u)

# number of asthma cases among those without RSV-LRTI
#asth_wo_RSV_no_u_adj <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u_adj)
#asth_wo_RSV_mAb_u_adj <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u_adj)
#asth_wo_RSV_rsvPreF_u_adj <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u_adj)
#asth_wo_RSV_Combined_u <- asth_no_rsv_func(tot_wo_RSV_Combined_u, r_asth_norsv_u)

# number of asthma cases among those with RSV-LRTI
#asth_RSV_no_u_adj <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u_adj, RR_w_u)
#asth_RSV_mAb_u_adj <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u_adj, RR_w_u)
#asth_RSV_rsvPreF_u_adj <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj, RR_w_u)
#asth_RSV_Combined_u <- asth_rsv_func(tot_RSV_Combined_u, r_asth_norsv_u, OR1_w_u)

# total with asthma
#tot_asth_no_u_adj <- tot_asth_func(asth_RSV_no_u_adj, asth_wo_RSV_no_u_adj)
#tot_asth_mAb_u_adj <- tot_asth_func(asth_RSV_mAb_u_adj, asth_wo_RSV_mAb_u_adj)
#tot_asth_rsvPreF_u_adj <- tot_asth_func(asth_RSV_rsvPreF_u_adj, asth_wo_RSV_rsvPreF_u_adj)
#tot_asth_Combined_u_adj <- tot_asth_func(asth_RSV_Combined_u_adj, asth_wo_RSV_Combined_u_adj)

# total asthma per 100,000 population
#tot_asth_no_pr_u_adj <- tot_asth_no_u_adj / pop_tot * 100000
#tot_asth_mAb_pr_u_adj <- tot_asth_mAb_u_adj / pop_tot * 100000
#tot_asth_rsvPreF_pr_u_adj <- tot_asth_rsvPreF_u_adj / pop_tot * 100000
#tot_asth_Combined_pr_u_adj <- tot_asth_Combined_u_adj / pop_tot * 10000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_u_adj <- (tot_asth_no_u_adj - tot_asth_mAb_u_adj) / tot_asth_no_u_adj * 100
#tot_asth_rsvPreF_pd_u_adj <- (tot_asth_no_u_adj - tot_asth_rsvPreF_u_adj) / tot_asth_no_u_adj * 100
#tot_asth_Combined_pd_u_adj <- (tot_asth_no_u_adj - tot_asth_Combined_u_adj) / tot_asth_no_u_adj * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_u_adj <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u_adj)
#asth_null_mAb_u_adj <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u_adj)
#asth_null_rsvPreF_u_adj <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj)
#asth_null_Combined_u_adj <-asth_rsv_null_func(tot_RSV_Combined_u, r_asth_norsv_u_adj)

# RSV-LRTI attributable asthma
#att_no_u_adj <- asth_rsv_att_func(asth_RSV_no_u_adj, asth_null_no_u_adj)
#att_mAb_u_adj <- asth_rsv_att_func(asth_RSV_mAb_u_adj, asth_null_mAb_u_adj)
#att_rsvPreF_u_adj <-asth_rsv_att_func(asth_RSV_rsvPreF_u_adj, asth_null_rsvPreF_u_adj)
#att_Combined_u_adj <-asth_rsv_att_func(asth_RSV_Combined_u_adj, asth_null_Combined_u_adj)

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_u_adj <- att_no_u_adj / pop_tot * 100000
#att_mAb_pr_u_adj <- att_mAb_u_adj / pop_tot * 100000
#att_rsvPreF_pr_u_adj <- att_rsvPreF_u_adj / pop_tot * 100000
#att_Combined_pr_u_adj <- att_Combined_u_adj / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_u_adj <- (att_no_u_adj - att_mAb_u_adj) / att_no_u_adj * 100
#att_rsvPreF_pd_u_adj <- (att_no_u_adj - att_rsvPreF_u_adj) / att_no_u_adj * 100
#att_Combined_pd_u_adj <- (att_no_u_adj - att_Combined_u_adj) / att_no_u_adj * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_u_adj <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_adj)
#all_rsv_prev_pr_u_adj <- all_rsv_prev_u_adj / pop_tot * 100000
#all_rsv_prev_pd_u_adj <- (tot_asth_no_u_adj - all_rsv_prev_u_adj) / tot_asth_no_u_adj * 100

#PAF
#quantile(((tot_RSV_no_u/pop_tot*(RR_w_u-1))/(tot_RSV_no_u/pop_tot*(RR_w_u-1)+1)), probs = c(0.05, 0.95))

# RSV LRTI Cases 2 yr
#quantile(tot_RSV_no_u, probs = c(0.025, 0.975))
#quantile(tot_RSV_mAb_u, probs = c(0.025, 0.975))
#quantile(tot_RSV_rsvPreF_u, probs = c(0.025, 0.975))
# Percent decrease from status quo
#quantile((tot_RSV_no_u-tot_RSV_mAb_u)/tot_RSV_no_u, probs = c(0.025, 0.975))
#quantile((tot_RSV_no_u-tot_RSV_rsvPreF_u)/tot_RSV_no_u, probs = c(0.025, 0.975))
# Total asthma cases by intervention
#quantile(tot_asth_no_u_adj, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_u_adj, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_u_adj, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_u_adj, probs = c(0.025, 0.975))
# Difference in outcomes
#quantile(tot_asth_no_u_adj-tot_asth_mAb_u_adj, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj-tot_asth_rsvPreF_u_adj, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj-all_rsv_prev_u_adj, probs = c(0.025, 0.975))
# Total Asthma per 100,000 
#quantile(tot_asth_no_pr_u_adj, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_pr_u_adj, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pr_u_adj, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pr_u_adj, probs = c(0.025, 0.975))
# Total Asthma percent reduction
#quantile(tot_asth_mAb_pd_u_adj, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pd_u_adj, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pd_u_adj, probs = c(0.025, 0.975))
# RSV Attributable asthma
#quantile(att_no_u_adj, probs = c(0.025, 0.975))
#quantile(att_mAb_u_adj, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_u_adj, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
#quantile(att_no_u_adj-att_mAb_u_adj, probs = c(0.025, 0.975))
#quantile(att_no_u_adj-att_rsvPreF_u_adj, probs = c(0.025, 0.975))
#quantile(att_no_u_adj, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
#quantile(att_no_pr_u_adj, probs = c(0.025, 0.975))
#quantile(att_mAb_pr_u_adj, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pr_u_adj, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
#quantile(att_mAb_pd_u_adj, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pd_u_adj, probs = c(0.025, 0.975))

# ------------------------------------- #
# Sensitivity analysis - 1st and 2nd year
# ------------------------------------- #

# 75%, 50%, 25% - sensitivity analysis
#RR_w_u75 <- RR_w_u*.75
#RR_l75 <- quantile(RR_w_u75, 0.025, na.rm = TRUE)
#RR_h75 <- quantile(RR_w_u75, 0.975, na.rm = TRUE)
#RR_w_u50 <- RR_w_u*.50
#RR_l50 <- quantile(RR_w_u50, 0.025, na.rm = TRUE)
#RR_h50 <- quantile(RR_w_u50, 0.975, na.rm = TRUE)
#RR_w_u25 <- RR_w_u*.25
#RR_l25 <- quantile(RR_w_u25, 0.025, na.rm = TRUE)
#RR_h25 <- quantile(RR_w_u25, 0.975, na.rm = TRUE)

#75
# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv_u_adj75 <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u75, tot_RSV_no_u, tot_wo_RSV_no_u)

# number of asthma cases among those without RSV-LRTI
#asth_wo_RSV_no_u_adj75 <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u_adj75)
#asth_wo_RSV_mAb_u_adj75 <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u_adj75)
#asth_wo_RSV_rsvPreF_u_adj75 <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u_adj75)

# number of asthma cases among those with RSV-LRTI
#asth_RSV_no_u_adj75 <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u_adj75, RR_w_u75)
#asth_RSV_mAb_u_adj75 <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u_adj75, RR_w_u75)
#asth_RSV_rsvPreF_u_adj75 <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj75, RR_w_u75)

# total with asthma
#tot_asth_no_u_adj75 <- tot_asth_func(asth_RSV_no_u_adj75, asth_wo_RSV_no_u_adj75)
#tot_asth_mAb_u_adj75 <- tot_asth_func(asth_RSV_mAb_u_adj75, asth_wo_RSV_mAb_u_adj75)
#tot_asth_rsvPreF_u_adj75 <- tot_asth_func(asth_RSV_rsvPreF_u_adj75, asth_wo_RSV_rsvPreF_u_adj75)

# total asthma per 100,000 population
#tot_asth_no_pr_u_adj75 <- tot_asth_no_u_adj75 / pop_tot * 100000
#tot_asth_mAb_pr_u_adj75 <- tot_asth_mAb_u_adj75 / pop_tot * 100000
#tot_asth_rsvPreF_pr_u_adj75 <- tot_asth_rsvPreF_u_adj75 / pop_tot * 100000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_u_adj75 <- (tot_asth_no_u_adj75 - tot_asth_mAb_u_adj75) / tot_asth_no_u_adj75 * 100
#tot_asth_rsvPreF_pd_u_adj75 <- (tot_asth_no_u_adj75 - tot_asth_rsvPreF_u_adj75) / tot_asth_no_u_adj75 * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_u_adj75 <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u_adj75)
#asth_null_mAb_u_adj75 <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u_adj75)
#asth_null_rsvPreF_u_adj75 <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj75)

# RSV-LRTI attributable asthma
#att_no_u_adj75 <- asth_rsv_att_func(asth_RSV_no_u_adj75, asth_null_no_u_adj75)
#att_mAb_u_adj75 <- asth_rsv_att_func(asth_RSV_mAb_u_adj75, asth_null_mAb_u_adj75)
#att_rsvPreF_u_adj75 <-asth_rsv_att_func(asth_RSV_rsvPreF_u_adj75, asth_null_rsvPreF_u_adj75)

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_u_adj75 <- att_no_u_adj75 / pop_tot * 100000
#att_mAb_pr_u_adj75 <- att_mAb_u_adj75 / pop_tot * 100000
#att_rsvPreF_pr_u_adj75 <- att_rsvPreF_u_adj75 / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_u_adj75 <- (att_no_u_adj75 - att_mAb_u_adj75) / att_no_u_adj75 * 100
#att_rsvPreF_pd_u_adj75 <- (att_no_u_adj75 - att_rsvPreF_u_adj75) / att_no_u_adj75 * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_u_adj75 <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_adj75)
#all_rsv_prev_pr_u_adj75 <- all_rsv_prev_u_adj75 / pop_tot * 100000
#all_rsv_prev_pd_u_adj75 <- (tot_asth_no_u_adj75 - all_rsv_prev_u_adj75) / tot_asth_no_u_adj75 * 100

# Total asthma cases by intervention
#quantile(tot_asth_no_u_adj75, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_u_adj75, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_u_adj75, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_u_adj75, probs = c(0.025, 0.975))
# Difference in outcomes
#quantile(tot_asth_no_u_adj75-tot_asth_mAb_u_adj75, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj75-tot_asth_rsvPreF_u_adj75, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj75-all_rsv_prev_u_adj75, probs = c(0.025, 0.975))
# Total Asthma per 100,000 
#quantile(tot_asth_no_pr_u_adj75, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_pr_u_adj75, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pr_u_adj75, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pr_u_adj75, probs = c(0.025, 0.975))
# Total Asthma percent reduction
#quantile(tot_asth_mAb_pd_u_adj75, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pd_u_adj75, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pd_u_adj75, probs = c(0.025, 0.975))
# RSV Attributable asthma
#quantile(att_no_u_adj75, probs = c(0.025, 0.975))
#quantile(att_mAb_u_adj75, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_u_adj75, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
#quantile(att_no_u_adj75-att_mAb_u_adj75, probs = c(0.025, 0.975))
#quantile(att_no_u_adj75-att_rsvPreF_u_adj75, probs = c(0.025, 0.975))
#quantile(att_no_u_adj75, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
#quantile(att_no_pr_u_adj75, probs = c(0.025, 0.975))
#quantile(att_mAb_pr_u_adj75, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pr_u_adj75, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
#quantile(att_mAb_pd_u_adj75, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pd_u_adj75, probs = c(0.025, 0.975))

#50
# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv_u_adj50 <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u50, tot_RSV_no_u, tot_wo_RSV_no_u)

# number of asthma cases among those without RSV-LRTI
#asth_wo_RSV_no_u_adj50 <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u_adj50)
#asth_wo_RSV_mAb_u_adj50 <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u_adj50)
#asth_wo_RSV_rsvPreF_u_adj50 <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u_adj50)

# number of asthma cases among those with RSV-LRTI
#asth_RSV_no_u_adj50 <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u_adj50, RR_w_u50)
#asth_RSV_mAb_u_adj50 <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u_adj50, RR_w_u50)
#asth_RSV_rsvPreF_u_adj50 <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj50, RR_w_u50)

# total with asthma
#tot_asth_no_u_adj50 <- tot_asth_func(asth_RSV_no_u_adj50, asth_wo_RSV_no_u_adj50)
#tot_asth_mAb_u_adj50 <- tot_asth_func(asth_RSV_mAb_u_adj50, asth_wo_RSV_mAb_u_adj50)
#tot_asth_rsvPreF_u_adj50 <- tot_asth_func(asth_RSV_rsvPreF_u_adj50, asth_wo_RSV_rsvPreF_u_adj50)

# total asthma per 1000,000 population
#tot_asth_no_pr_u_adj50 <- tot_asth_no_u_adj50 / pop_tot * 100000
#tot_asth_mAb_pr_u_adj50 <- tot_asth_mAb_u_adj50 / pop_tot * 100000
#tot_asth_rsvPreF_pr_u_adj50 <- tot_asth_rsvPreF_u_adj50 / pop_tot * 100000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_u_adj50 <- (tot_asth_no_u_adj50 - tot_asth_mAb_u_adj50) / tot_asth_no_u_adj50 * 100
#tot_asth_rsvPreF_pd_u_adj50 <- (tot_asth_no_u_adj50 - tot_asth_rsvPreF_u_adj50) / tot_asth_no_u_adj50 * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_u_adj50 <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u_adj50)
#asth_null_mAb_u_adj50 <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u_adj50)
#asth_null_rsvPreF_u_adj50 <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj50)

# RSV-LRTI attributable asthma
#att_no_u_adj50 <- asth_rsv_att_func(asth_RSV_no_u_adj50, asth_null_no_u_adj50)
#att_mAb_u_adj50 <- asth_rsv_att_func(asth_RSV_mAb_u_adj50, asth_null_mAb_u_adj50)
#att_rsvPreF_u_adj50 <-asth_rsv_att_func(asth_RSV_rsvPreF_u_adj50, asth_null_rsvPreF_u_adj50)

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_u_adj50 <- att_no_u_adj50 / pop_tot * 100000
#att_mAb_pr_u_adj50 <- att_mAb_u_adj50 / pop_tot * 100000
#att_rsvPreF_pr_u_adj50 <- att_rsvPreF_u_adj50 / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_u_adj50 <- (att_no_u_adj50 - att_mAb_u_adj50) / att_no_u_adj50 * 100
#att_rsvPreF_pd_u_adj50 <- (att_no_u_adj50 - att_rsvPreF_u_adj50) / att_no_u_adj50 * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_u_adj50 <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_adj50)
#all_rsv_prev_pr_u_adj50 <- all_rsv_prev_u_adj50 / pop_tot * 100000
#all_rsv_prev_pd_u_adj50 <- (tot_asth_no_u_adj50 - all_rsv_prev_u_adj50) / tot_asth_no_u_adj50 * 100

# Total asthma cases by intervention
#quantile(tot_asth_no_u_adj50, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_u_adj50, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_u_adj50, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_u_adj50, probs = c(0.025, 0.975))
# Difference in outcomes
#quantile(tot_asth_no_u_adj50-tot_asth_mAb_u_adj50, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj50-tot_asth_rsvPreF_u_adj50, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj50-all_rsv_prev_u_adj50, probs = c(0.025, 0.975))
# Total Asthma per 100,000 
#quantile(tot_asth_no_pr_u_adj50, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_pr_u_adj50, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pr_u_adj50, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pr_u_adj50, probs = c(0.025, 0.975))
# Total Asthma percent reduction
#quantile(tot_asth_mAb_pd_u_adj50, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pd_u_adj50, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pd_u_adj50, probs = c(0.025, 0.975))
# RSV Attributable asthma
#quantile(att_no_u_adj50, probs = c(0.025, 0.975))
#quantile(att_mAb_u_adj50, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_u_adj50, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
#quantile(att_no_u_adj50-att_mAb_u_adj50, probs = c(0.025, 0.975))
#quantile(att_no_u_adj50-att_rsvPreF_u_adj50, probs = c(0.025, 0.975))
#quantile(att_no_u_adj50, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
#quantile(att_no_pr_u_adj50, probs = c(0.025, 0.975))
#quantile(att_mAb_pr_u_adj50, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pr_u_adj50, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
#quantile(att_mAb_pd_u_adj50, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pd_u_adj50, probs = c(0.025, 0.975))

#25
# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv_u_adj25 <- prev_no_rsv_func(prev_tot_u, pop_tot, RR_w_u25, tot_RSV_no_u, tot_wo_RSV_no_u)

# number of asthma cases among those without RSV-LRTI
#asth_wo_RSV_no_u_adj25 <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u_adj25)
#asth_wo_RSV_mAb_u_adj25 <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u_adj25)
#asth_wo_RSV_rsvPreF_u_adj25 <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u_adj25)

# number of asthma cases among those with RSV-LRTI
#asth_RSV_no_u_adj25 <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u_adj25, RR_w_u25)
#asth_RSV_mAb_u_adj25 <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u_adj25, RR_w_u25)
#asth_RSV_rsvPreF_u_adj25 <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj25, RR_w_u25)

# total with asthma
#tot_asth_no_u_adj25 <- tot_asth_func(asth_RSV_no_u_adj25, asth_wo_RSV_no_u_adj25)
#tot_asth_mAb_u_adj25 <- tot_asth_func(asth_RSV_mAb_u_adj25, asth_wo_RSV_mAb_u_adj25)
#tot_asth_rsvPreF_u_adj25 <- tot_asth_func(asth_RSV_rsvPreF_u_adj25, asth_wo_RSV_rsvPreF_u_adj25)

# total asthma per 100,000 population
#tot_asth_no_pr_u_adj25 <- tot_asth_no_u_adj25 / pop_tot * 100000
#tot_asth_mAb_pr_u_adj25 <- tot_asth_mAb_u_adj25 / pop_tot * 100000
#tot_asth_rsvPreF_pr_u_adj25 <- tot_asth_rsvPreF_u_adj25 / pop_tot * 100000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_u_adj25 <- (tot_asth_no_u_adj25 - tot_asth_mAb_u_adj25) / tot_asth_no_u_adj25 * 100
#tot_asth_rsvPreF_pd_u_adj25 <- (tot_asth_no_u_adj25 - tot_asth_rsvPreF_u_adj25) / tot_asth_no_u_adj25 * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_u_adj25 <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u_adj25)
#asth_null_mAb_u_adj25 <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u_adj25)
#asth_null_rsvPreF_u_adj25 <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u_adj25)

# RSV-LRTI attributable asthma
#att_no_u_adj25 <- asth_rsv_att_func(asth_RSV_no_u_adj25, asth_null_no_u_adj25)
#att_mAb_u_adj25 <- asth_rsv_att_func(asth_RSV_mAb_u_adj25, asth_null_mAb_u_adj25)
#att_rsvPreF_u_adj25 <-asth_rsv_att_func(asth_RSV_rsvPreF_u_adj25, asth_null_rsvPreF_u_adj25)

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_u_adj25 <- att_no_u_adj25 / pop_tot * 100000
#att_mAb_pr_u_adj25 <- att_mAb_u_adj25 / pop_tot * 100000
#att_rsvPreF_pr_u_adj25 <- att_rsvPreF_u_adj25 / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_u_adj25 <- (att_no_u_adj25 - att_mAb_u_adj25) / att_no_u_adj25 * 100
#att_rsvPreF_pd_u_adj25 <- (att_no_u_adj25 - att_rsvPreF_u_adj25) / att_no_u_adj25 * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_u_adj25 <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_adj25)
#all_rsv_prev_pr_u_adj25 <- all_rsv_prev_u_adj25 / pop_tot * 100000
#all_rsv_prev_pd_u_adj25 <- (tot_asth_no_u_adj25 - all_rsv_prev_u_adj25) / tot_asth_no_u_adj25 * 100

# Total asthma cases by intervention
#quantile(tot_asth_no_u_adj25, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_u_adj25, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_u_adj25, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_u_adj25, probs = c(0.025, 0.975))
# Difference in outcomes
#quantile(tot_asth_no_u_adj25-tot_asth_mAb_u_adj25, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj25-tot_asth_rsvPreF_u_adj25, probs = c(0.025, 0.975))
#quantile(tot_asth_no_u_adj25-all_rsv_prev_u_adj25, probs = c(0.025, 0.975))
# Total Asthma per 100,000 
#quantile(tot_asth_no_pr_u_adj25, probs = c(0.025, 0.975))
#quantile(tot_asth_mAb_pr_u_adj25, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pr_u_adj25, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pr_u_adj25, probs = c(0.025, 0.975))
# Total Asthma percent reduction
#quantile(tot_asth_mAb_pd_u_adj25, probs = c(0.025, 0.975))
#quantile(tot_asth_rsvPreF_pd_u_adj25, probs = c(0.025, 0.975))
#quantile(all_rsv_prev_pd_u_adj25, probs = c(0.025, 0.975))
# RSV Attributable asthma
#quantile(att_no_u_adj25, probs = c(0.025, 0.975))
#quantile(att_mAb_u_adj25, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_u_adj25, probs = c(0.025, 0.975))
# Absolute case reduction from no intervention
#quantile(att_no_u_adj25-att_mAb_u_adj25, probs = c(0.025, 0.975))
#quantile(att_no_u_adj25-att_rsvPreF_u_adj25, probs = c(0.025, 0.975))
#quantile(att_no_u_adj25, probs = c(0.025, 0.975))
# RSV Attributable asthma per 100,000
#quantile(att_no_pr_u_adj25, probs = c(0.025, 0.975))
#quantile(att_mAb_pr_u_adj25, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pr_u_adj25, probs = c(0.025, 0.975))
# RSV Attributable asthma percent decrease
#quantile(att_mAb_pd_u_adj25, probs = c(0.025, 0.975))
#quantile(att_rsvPreF_pd_u_adj25, probs = c(0.025, 0.975))

#EXTRA/LEFT OVER CODE

# -----------------------------
# USING NON-ADJUSTED OR - 2.45 - 1ST AND 2ND YEARS OF LIFE DATA
# -----------------------------

# number of asthma cases among those without RSV-LRTI
#asth_wo_RSV_no_u <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u)
#asth_wo_RSV_mAb_u <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u)
#asth_wo_RSV_rsvPreF_u <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u)
#asth_wo_RSV_Combined_u <- asth_no_rsv_func(tot_wo_RSV_Combined_u, r_asth_norsv_u)

# number of asthma cases among those with RSV-LRTI
#asth_RSV_no_u <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u, rr_w_u)
#asth_RSV_mAb_u <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u, rr_w_u)
#asth_RSV_rsvPreF_u <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u, rr_w_u)
#asth_RSV_Combined_u <- asth_rsv_func(tot_RSV_Combined_u, r_asth_norsv_u, rr_w_u)

# total with asthma
#tot_asth_no_u <- tot_asth_func(asth_RSV_no_u, asth_wo_RSV_no_u)
#tot_asth_mAb_u <- tot_asth_func(asth_RSV_mAb_u, asth_wo_RSV_mAb_u)
#tot_asth_rsvPreF_u <- tot_asth_func(asth_RSV_rsvPreF_u, asth_wo_RSV_rsvPreF_u)
#tot_asth_Combined_u <- tot_asth_func(asth_RSV_Combined_u, asth_wo_RSV_Combined_u)

# total asthma per 10,000 population
#tot_asth_no_pr_u <- tot_asth_no_u / pop_tot * 10000
#tot_asth_mAb_pr_u <- tot_asth_mAb_u / pop_tot * 10000
#tot_asth_rsvPreF_pr_u <- tot_asth_rsvPreF_u / pop_tot * 10000
#tot_asth_Combined_pr_u <- tot_asth_Combined_u / pop_tot * 10000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_u <- (tot_asth_no_u - tot_asth_mAb_u) / tot_asth_no_u * 100
#tot_asth_rsvPreF_pd_u <- (tot_asth_no_u - tot_asth_rsvPreF_u) / tot_asth_no_u * 100
#tot_asth_Combined_pd_u <- (tot_asth_no_u - tot_asth_Combined_u) / tot_asth_no_u * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_u <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u)
#asth_null_mAb_u <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u)
#asth_null_rsvPreF_u <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u)
#asth_null_Combined_u <-asth_rsv_null_func(tot_RSV_Combined_u, r_asth_norsv_u)

# RSV-LRTI attributable asthma
#att_no_u <- asth_rsv_att_func(asth_RSV_no_u, asth_null_no_u)
#att_mAb_u <- asth_rsv_att_func(asth_RSV_mAb_u, asth_null_mAb_u)
#att_rsvPreF_u <-asth_rsv_att_func(asth_RSV_rsvPreF_u, asth_null_rsvPreF_u)
#att_Combined_u <-asth_rsv_att_func(asth_RSV_Combined_u, asth_null_Combined_u)

# RSV-LRTI attributable asthma per 10,000 population
#att_no_pr_u <- att_no_u / pop_tot * 10000
#att_mAb_pr_u <- att_mAb_u / pop_tot * 10000
#att_rsvPreF_pr_u <- att_rsvPreF_u / pop_tot * 10000
#att_Combined_pr_u <- att_Combined_u / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_u <- (att_no_u - att_mAb_u) / att_no_u * 100
#att_rsvPreF_pd_u <- (att_no_u - att_rsvPreF_u) / att_no_u * 100
#att_Combined_pd_u <- (att_no_u - att_Combined_u) / att_no_u * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_u <- asth_no_rsv_func(pop_tot, r_asth_norsv_u)
#all_rsv_prev_pr_u <- all_rsv_prev_u / pop_tot * 10000
#all_rsv_prev_pd_u <- (tot_asth_no_u - all_rsv_prev_u) / tot_asth_no_u * 100

#PAF
#((tot_RSV_no_u/pop_tot*(rr_w_u-1))/(tot_RSV_no_u/pop_tot*(rr_w_u-1)+1))

# RSV LRTI Cases 2 yr
#quantile(tot_RSV_no_u, probs = c(0.05, 0.95))
#quantile(tot_RSV_mAb_u, probs = c(0.05, 0.95))
#quantile(tot_RSV_rsvPreF_u, probs = c(0.05, 0.95))
# Percent decrease from status quo
#quantile((tot_RSV_no_u-tot_RSV_mAb_u)/tot_RSV_no_u, probs = c(0.05, 0.95))
#quantile((tot_RSV_no_u-tot_RSV_rsvPreF_u)/tot_RSV_no_u, probs = c(0.05, 0.95))
# Total asthma cases by intervention
#quantile(tot_asth_no_u, probs = c(0.05, 0.95))
#quantile(tot_asth_mAb_u, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_u, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_u, probs = c(0.05, 0.95))
# Difference in outcomes
#quantile(tot_asth_no_u-tot_asth_mAb_u, probs = c(0.05, 0.95))
#quantile(tot_asth_no_u-tot_asth_rsvPreF_u, probs = c(0.05, 0.95))
#quantile(tot_asth_no_u-all_rsv_prev_u, probs = c(0.05, 0.95))
# Total Asthma per 10,000 
#quantile(tot_asth_no_pr_u, probs = c(0.05, 0.95))
#quantile(tot_asth_mAb_pr_u, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_pr_u, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_pr_u, probs = c(0.05, 0.95))
# Total Asthma percent reduction
#quantile(tot_asth_mAb_pd_u, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_pd_u, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_pd_u, probs = c(0.05, 0.95))
# RSV Attributable asthma
#quantile(att_no_u, probs = c(0.05, 0.95))
#quantile(att_mAb_u, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_u, probs = c(0.05, 0.95))
# Absolute case reduction from no intervention
#quantile(att_no_u_1st-att_mAb_u, probs = c(0.05, 0.95))
#quantile(att_no_u_1st-att_rsvPreF_u, probs = c(0.05, 0.95))
#quantile(att_no_u, probs = c(0.05, 0.95))
# RSV Attributable asthma per 10,000
#quantile(att_no_pr_u, probs = c(0.05, 0.95))
#quantile(att_mAb_pr_u, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_pr_u, probs = c(0.05, 0.95))
# RSV Attributable asthma percent decrease
#quantile(att_mAb_pd_u, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_pd_u, probs = c(0.05, 0.95))

# -----------------------------
# USING NON-ADJUSTED OR - 2.45 - 1ST YEAR OF LIFE DATA
# -----------------------------

# number of asthma cases among those without RSV-LRTI
#asth_wo_RSV_no_u_1st <- asth_no_rsv_func(tot_wo_RSV_no_u_1st, r_asth_norsv_u_1st)
#asth_wo_RSV_mAb_u_1st <- asth_no_rsv_func(tot_wo_RSV_mAb_u_1st, r_asth_norsv_u_1st)
#asth_wo_RSV_rsvPreF_u_1st <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u_1st, r_asth_norsv_u_1st)

# number of asthma cases among those with RSV-LRTI
#asth_RSV_no_u_1st <- asth_rsv_func(tot_RSV_no_u_1st, r_asth_norsv_u_1st, rr_w_u)
#asth_RSV_mAb_u_1st <- asth_rsv_func(tot_RSV_mAb_u_1st, r_asth_norsv_u_1st, rr_w_u)
#asth_RSV_rsvPreF_u_1st <- asth_rsv_func(tot_RSV_rsvPreF_u_1st, r_asth_norsv_u_1st, rr_w_u)

# total with asthma
#tot_asth_no_u_1st <- tot_asth_func(asth_RSV_no_u_1st, asth_wo_RSV_no_u_1st)
#tot_asth_mAb_u_1st <- tot_asth_func(asth_RSV_mAb_u_1st, asth_wo_RSV_mAb_u_1st)
#tot_asth_rsvPreF_u_1st <- tot_asth_func(asth_RSV_rsvPreF_u_1st, asth_wo_RSV_rsvPreF_u_1st)

# total asthma per 10,000 population
#tot_asth_no_pr_u_1st <- tot_asth_no_u_1st / pop_tot * 10000
#tot_asth_mAb_pr_u_1st <- tot_asth_mAb_u_1st / pop_tot * 10000
#tot_asth_rsvPreF_pr_u_1st <- tot_asth_rsvPreF_u_1st / pop_tot * 10000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_u_1st <- (tot_asth_no_u_1st - tot_asth_mAb_u_1st) / tot_asth_no_u_1st * 100
#tot_asth_rsvPreF_pd_u_1st <- (tot_asth_no_u_1st - tot_asth_rsvPreF_u_1st) / tot_asth_no_u_1st * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_u_1st <- asth_rsv_null_func(tot_RSV_no_u_1st, r_asth_norsv_u_1st)
#asth_null_mAb_u_1st <- asth_rsv_null_func(tot_RSV_mAb_u_1st, r_asth_norsv_u_1st)
#asth_null_rsvPreF_u_1st <-asth_rsv_null_func(tot_RSV_rsvPreF_u_1st, r_asth_norsv_u_1st)

# RSV-LRTI attributable asthma
#att_no_u_1st <- asth_rsv_att_func(asth_RSV_no_u_1st, asth_null_no_u_1st)
#att_mAb_u_1st <- asth_rsv_att_func(asth_RSV_mAb_u_1st, asth_null_mAb_u_1st)
#att_rsvPreF_u_1st <-asth_rsv_att_func(asth_RSV_rsvPreF_u_1st, asth_null_rsvPreF_u_1st)

# RSV-LRTI attributable asthma per 10,000 population
#att_no_pr_u_1st <- att_no_u_1st / pop_tot * 10000
#att_mAb_pr_u_1st <- att_mAb_u_1st / pop_tot * 10000
#att_rsvPreF_pr_u_1st <- att_rsvPreF_u_1st / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_u_1st <- (att_no_u_1st - att_mAb_u_1st) / att_no_u_1st * 100
#att_rsvPreF_pd_u_1st <- (att_no_u_1st - att_rsvPreF_u_1st) / att_no_u_1st * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_u_1st <- asth_no_rsv_func(pop_tot, r_asth_norsv_u_1st)
#all_rsv_prev_pr_u_1st <- all_rsv_prev_u_1st / pop_tot * 10000
#all_rsv_prev_pd_u_1st <- (tot_asth_no_u_1st - all_rsv_prev_u_1st) / tot_asth_no_u_1st * 100

# CI work
#Figure 1
# RSV LRTI Cases 1 yr
#quantile(tot_RSV_no_u_1st, probs = c(0.05, 0.95))
#quantile(tot_RSV_mAb_u_1st, probs = c(0.05, 0.95))
#quantile(tot_RSV_rsvPreF_u_1st, probs = c(0.05, 0.95))
# Percent decrease from status quo
#quantile((tot_RSV_no_u_1st-tot_RSV_mAb_u_1st)/tot_RSV_no_u_1st, probs = c(0.05, 0.95))
#quantile((tot_RSV_no_u_1st-tot_RSV_rsvPreF_u_1st)/tot_RSV_no_u_1st, probs = c(0.05, 0.95))
# Total asthma cases by intervention
#quantile(tot_asth_no_u_1st, probs = c(0.05, 0.95))
#quantile(tot_asth_mAb_u_1st, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_u_1st, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_u_1st, probs = c(0.05, 0.95))
# Difference in outcomes
#quantile(tot_asth_no_u_1st-tot_asth_mAb_u_1st, probs = c(0.05, 0.95))
#quantile(tot_asth_no_u_1st-tot_asth_rsvPreF_u_1st, probs = c(0.05, 0.95))
#quantile(tot_asth_no_u_1st-all_rsv_prev_u_1st, probs = c(0.05, 0.95))
# Total Asthma per 10,000 
#quantile(tot_asth_no_pr_u_1st, probs = c(0.05, 0.95))
#quantile(tot_asth_mAb_pr_u_1st, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_pr_u_1st, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_pr_u_1st, probs = c(0.05, 0.95))
# Total Asthma percent reduction
#quantile(tot_asth_mAb_pd_u_1st, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_pd_u_1st, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_pd_u_1st, probs = c(0.05, 0.95))
# RSV Attributable asthma
#quantile(att_no_u_1st, probs = c(0.05, 0.95))
#quantile(att_mAb_u_1st, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_u_1st, probs = c(0.05, 0.95))
# Absolute case reduction from no intervention
#quantile(att_no_u_1st-att_mAb_u_1st, probs = c(0.05, 0.95))
#quantile(att_no_u_1st-att_rsvPreF_u_1st, probs = c(0.05, 0.95))
#quantile(att_no_u_1st, probs = c(0.05, 0.95))
# RSV Attributable asthma per 10,000
#quantile(att_no_pr_u_1st, probs = c(0.05, 0.95))
#quantile(att_mAb_pr_u_1st, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_pr_u_1st, probs = c(0.05, 0.95))
# RSV Attributable asthma percent decrease
#quantile(att_mAb_pd_u_1st, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_pd_u_1st, probs = c(0.05, 0.95))

# -----------------------------
# CONSIDERATION FOR 1ST 2ND AND 3RD YEAR OF LIFE DATA
# -----------------------------
#Combining yearly DFs by scenario - post adjustment for outcomes during three years
#tot_RSV_no_u <- OP_no_df_1st+OP_no_df_2nd+OP_no_df_3rd+ED_no_df_1st+ED_no_df_2nd+ED_no_df_3rd+Hosps_no_df_1st+Hosps_no_df_2nd+Hosps_no_df_3rd
#tot_RSV_mAb_u <- OP_mAb_df_1st+OP_mAb_df_2nd+OP_mAb_df_3rd+ED_mAb_df_1st+ED_mAb_df_2nd+ED_mAb_df_3rd+Hosps_mAb_df_1st+Hosps_mAb_df_2nd+Hosps_mAb_df_3rd
#tot_RSV_rsvPreF_u <- OP_rsvPreF_df_1st+OP_rsvPreF_df_2nd+OP_rsvPreF_df_3rd+ED_rsvPreF_df_1st+ED_rsvPreF_df_2nd+ED_rsvPreF_df_3rd+Hosps_rsvPreF_df_1st+Hosps_rsvPreF_df_2nd+Hosps_rsvPreF_df_3rd

#Combining yearly DFs by scenario - post adjustment for outcomes during 1st year
#tot_RSV_no_u_1st <- OP_no_df_1st+ED_no_df_1st+Hosps_no_df_1st
#tot_RSV_mAb_u_1st <- OP_mAb_df_1st+ED_mAb_df_1st+Hosps_mAb_df_1st
#tot_RSV_rsvPreF_u_1st <- OP_rsvPreF_df_1st+ED_rsvPreF_df_1st+Hosps_rsvPreF_df_1st

# number of kids without RSV-LRTI associated for each strategy
#tot_wo_RSV_no_u <- pop_tot - (tot_RSV_no_u)
#tot_wo_RSV_mAb_u <- pop_tot - (tot_RSV_mAb_u)
#tot_wo_RSV_rsvPreF_u <- pop_tot - (tot_RSV_rsvPreF_u)

# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv_u <- prev_no_rsv_func(prev_tot_u, pop_tot0, rr_w_u, tot_RSV_no_u, tot_wo_RSV_no_u)

# number of asthma cases among those without RSV-LRTI
#asth_wo_RSV_no_u <- asth_no_rsv_func(tot_wo_RSV_no_u, r_asth_norsv_u)
#asth_wo_RSV_mAb_u <- asth_no_rsv_func(tot_wo_RSV_mAb_u, r_asth_norsv_u)
#asth_wo_RSV_rsvPreF_u <- asth_no_rsv_func(tot_wo_RSV_rsvPreF_u, r_asth_norsv_u)
#asth_wo_RSV_Combined_u <- asth_no_rsv_func(tot_wo_RSV_Combined_u, r_asth_norsv_u)

# number of asthma cases among those with RSV-LRTI
#asth_RSV_no_u <- asth_rsv_func(tot_RSV_no_u, r_asth_norsv_u, rr_w_u)
#asth_RSV_mAb_u <- asth_rsv_func(tot_RSV_mAb_u, r_asth_norsv_u, rr_w_u)
#asth_RSV_rsvPreF_u <- asth_rsv_func(tot_RSV_rsvPreF_u, r_asth_norsv_u, rr_w_u)
#asth_RSV_Combined_u <- asth_rsv_func(tot_RSV_Combined_u, r_asth_norsv_u, rr_w_u)

# total with asthma
#tot_asth_no_u <- tot_asth_func(asth_RSV_no_u, asth_wo_RSV_no_u)
#tot_asth_mAb_u <- tot_asth_func(asth_RSV_mAb_u, asth_wo_RSV_mAb_u)
#tot_asth_rsvPreF_u <- tot_asth_func(asth_RSV_rsvPreF_u, asth_wo_RSV_rsvPreF_u)
#tot_asth_Combined_u <- tot_asth_func(asth_RSV_Combined_u, asth_wo_RSV_Combined_u)

# total asthma per 10,000 population
#tot_asth_no_pr_u <- tot_asth_no_u / pop_tot * 10000
#tot_asth_mAb_pr_u <- tot_asth_mAb_u / pop_tot * 10000
#tot_asth_rsvPreF_pr_u <- tot_asth_rsvPreF_u / pop_tot * 10000
#tot_asth_Combined_pr_u <- tot_asth_Combined_u / pop_tot * 10000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_u <- (tot_asth_no_u - tot_asth_mAb_u) / tot_asth_no_u * 100
#tot_asth_rsvPreF_pd_u <- (tot_asth_no_u - tot_asth_rsvPreF_u) / tot_asth_no_u * 100
#tot_asth_Combined_pd_u <- (tot_asth_no_u - tot_asth_Combined_u) / tot_asth_no_u * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_u <- asth_rsv_null_func(tot_RSV_no_u, r_asth_norsv_u)
#asth_null_mAb_u <- asth_rsv_null_func(tot_RSV_mAb_u, r_asth_norsv_u)
#asth_null_rsvPreF_u <-asth_rsv_null_func(tot_RSV_rsvPreF_u, r_asth_norsv_u)
#asth_null_Combined_u <-asth_rsv_null_func(tot_RSV_Combined_u, r_asth_norsv_u)

# RSV-LRTI attributable asthma
#att_no_u <- asth_rsv_att_func(asth_RSV_no_u, asth_null_no_u)
#att_mAb_u <- asth_rsv_att_func(asth_RSV_mAb_u, asth_null_mAb_u)
#att_rsvPreF_u <-asth_rsv_att_func(asth_RSV_rsvPreF_u, asth_null_rsvPreF_u)
#att_Combined_u <-asth_rsv_att_func(asth_RSV_Combined_u, asth_null_Combined_u)

# RSV-LRTI attributable asthma per 10,000 population
#att_no_pr_u <- att_no_u / pop_tot * 10000
#att_mAb_pr_u <- att_mAb_u / pop_tot * 10000
#att_rsvPreF_pr_u <- att_rsvPreF_u / pop_tot * 10000
#att_Combined_pr_u <- att_Combined_u / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_u <- (att_no_u - att_mAb_u) / att_no_u * 100
#att_rsvPreF_pd_u <- (att_no_u - att_rsvPreF_u) / att_no_u * 100
#att_Combined_pd_u <- (att_no_u - att_Combined_u) / att_no_u * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_u <- asth_no_rsv_func(pop_tot, r_asth_norsv_u)
#all_rsv_prev_pr_u <- all_rsv_prev_u / pop_tot * 10000
#all_rsv_prev_pd_u <- (tot_asth_no_u - all_rsv_prev_u) / tot_asth_no_u * 100

# CI work
#Figure 1
# RSV LRTI Cases 1 yr
#quantile(tot_RSV_no_u, probs = c(0.05, 0.95))
#quantile(tot_RSV_mAb_u, probs = c(0.05, 0.95))
#quantile(tot_RSV_rsvPreF_u, probs = c(0.05, 0.95))
# Percent decrease from status quo
#quantile((tot_RSV_no_u-tot_RSV_mAb_u)/tot_RSV_no_u, probs = c(0.05, 0.95))
#quantile((tot_RSV_no_u-tot_RSV_rsvPreF_u)/tot_RSV_no_u, probs = c(0.05, 0.95))
# Total asthma cases by intervention
#quantile(tot_asth_no_u, probs = c(0.05, 0.95))
#quantile(tot_asth_mAb_u, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_u, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_u, probs = c(0.05, 0.95))
# Difference in outcomes
#quantile(tot_asth_no_u-tot_asth_mAb_u, probs = c(0.05, 0.95))
#quantile(tot_asth_no_u-tot_asth_rsvPreF_u, probs = c(0.05, 0.95))
#quantile(tot_asth_no_u-all_rsv_prev_u, probs = c(0.05, 0.95))
# Total Asthma per 10,000 
#quantile(tot_asth_no_pr_u, probs = c(0.05, 0.95))
#quantile(tot_asth_mAb_pr_u, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_pr_u, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_pr_u, probs = c(0.05, 0.95))
# Total Asthma percent reduction
#quantile(tot_asth_mAb_pd_u, probs = c(0.05, 0.95))
#quantile(tot_asth_rsvPreF_pd_u, probs = c(0.05, 0.95))
#quantile(all_rsv_prev_pd_u, probs = c(0.05, 0.95))
# RSV Attributable asthma
#quantile(att_no_u, probs = c(0.05, 0.95))
#quantile(att_mAb_u, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_u, probs = c(0.05, 0.95))
# Absolute case reduction from no intervention
#quantile(att_no_u-att_mAb_u, probs = c(0.05, 0.95))
#quantile(att_no_u-att_rsvPreF_u, probs = c(0.05, 0.95))
#quantile(att_no_u, probs = c(0.05, 0.95))
# RSV Attributable asthma per 10,000
#quantile(att_no_pr_u, probs = c(0.05, 0.95))
#quantile(att_mAb_pr_u, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_pr_u, probs = c(0.05, 0.95))
# RSV Attributable asthma percent decrease
#quantile(att_mAb_pd_u, probs = c(0.05, 0.95))
#quantile(att_rsvPreF_pd_u, probs = c(0.05, 0.95))

# -----------------------------
## PRIOR SOURCE CODE - DATA PREVIOUSLY REQUIRED TRANSFORMATION/DF ADJUSTMENT
# -----------------------------
## TRANSFORMATION
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
# transformation as below

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

# -----------------------------
## MORTALITY ADJUSTMENT
# -----------------------------
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

