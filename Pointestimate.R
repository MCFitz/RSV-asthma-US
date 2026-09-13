# -----------------------------
# USA RSV-LRTI associated asthma - Point estimate work
# -----------------------------

# Import data 
source("ImportData.R")
source("PARAMS_asthma.R")
source("asthmafunctions.R")
source("adjustmentformultipleepisode.R")
library(tidyverse)

# -----------------------------
# OR 3.0, new primary analysis
# -----------------------------

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no <- OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
num_OP_ED_Hosp_mAb <- OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a
num_OP_ED_Hosp_rsvPreF<- OP_rsvPreF_PE_1st_a+ED_rsvPreF_PE_1st_a+Hosps_rsvPreF_PE_1st_a

# number without RSV-LRTI
tot_wo_OP_no <- pop_tot - num_OP_ED_Hosp_no
tot_wo_OP_mAb <- pop_tot - num_OP_ED_Hosp_mAb
tot_wo_OP_rsvPreF <- pop_tot - num_OP_ED_Hosp_rsvPreF

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no, tot_wo_OP_no)

#RR from OR
RR_w <- aOR_w_asthonly/((1-(r_asth_norsv))+(r_asth_norsv*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, RR_w, num_OP_ED_Hosp_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv)
asth_wo_OP_mAb <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv)
asth_wo_OP_rsvPreF <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI
asth_OP_no <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv, RR_w)
asth_OP_mAb <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv, RR_w)
asth_OP_rsvPreF <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv, RR_w)

# all cause asthma
tot_asth_no <- tot_asth_func(asth_OP_no, asth_wo_OP_no)
tot_asth_mAb <- tot_asth_func(asth_OP_mAb, asth_wo_OP_mAb)
tot_asth_rsvPreF <-tot_asth_func(asth_OP_rsvPreF, asth_wo_OP_rsvPreF)

# total asthma per 100,000 population
tot_asth_no_pr <- tot_asth_no / pop_tot * 100000
tot_asth_mAb_pr <- tot_asth_mAb / pop_tot * 100000
tot_asth_rsvPreF_pr <- tot_asth_rsvPreF / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd <- (tot_asth_no - tot_asth_mAb) / tot_asth_no * 100
tot_asth_rsvPreF_pd <- (tot_asth_no - tot_asth_rsvPreF) / tot_asth_no * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv)
asth_null_mAb <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv)
asth_null_rsvPreF <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv)

# RSV-LRTI attributable asthma
att_no <- asth_rsv_att_func(asth_OP_no, asth_null_no)
att_mAb <- asth_rsv_att_func(asth_OP_mAb, asth_null_mAb)
att_rsvPreF <-asth_rsv_att_func(asth_OP_rsvPreF, asth_null_rsvPreF)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev <- asth_null_no + asth_wo_OP_no

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr <- att_no / pop_tot * 100000
att_mAb_pr <- att_mAb / pop_tot * 100000
att_rsvPreF_pr <- att_rsvPreF / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd <- (att_no - att_mAb) / att_no * 100
att_rsvPreF_pd <- (att_no - att_rsvPreF) / att_no * 100


# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev <- asth_no_rsv_func(pop_tot, r_asth_norsv)
all_rsv_prev_pr <- all_rsv_prev / pop_tot * 100000
all_rsv_prev_pd <- (tot_asth_no - all_rsv_prev) / tot_asth_no * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no/pop_tot)*(RR_w-1))/(((num_OP_ED_Hosp_no/pop_tot)*(RR_w-1)+1))
(tot_asth_no-all_rsv_prev)/tot_asth_no

# -----------------------------
# OR 2.45, old primary analysis
# -----------------------------

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no_oldOR <- OP_no_PE_oldOR_1st_a+ED_no_PE_oldOR_1st_a+Hosps_no_PE_oldOR_1st_a
num_OP_ED_Hosp_mAb_oldOR <- OP_mAb_PE_oldOR_1st_a+ED_mAb_PE_oldOR_1st_a+Hosps_mAb_PE_oldOR_1st_a
num_OP_ED_Hosp_rsvPreF_oldOR <- OP_rsvPreF_PE_oldOR_1st_a+ED_rsvPreF_PE_oldOR_1st_a+Hosps_rsvPreF_PE_oldOR_1st_a

# number without RSV-LRTI
tot_wo_OP_no_oldOR <- pop_tot - num_OP_ED_Hosp_no_oldOR
tot_wo_OP_mAb_oldOR <- pop_tot - num_OP_ED_Hosp_mAb_oldOR
tot_wo_OP_rsvPreF_oldOR <- pop_tot - num_OP_ED_Hosp_rsvPreF_oldOR

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_oldOR <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w, num_OP_ED_Hosp_no_oldOR, tot_wo_OP_no_oldOR)

#RR from OR
RR_w_oldOR <- aOR_w/((1-(r_asth_norsv_oldOR))+(r_asth_norsv_oldOR*aOR_w))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_oldOR <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_oldOR, num_OP_ED_Hosp_no_oldOR, tot_wo_OP_no_oldOR)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_oldOR <- asth_no_rsv_func(tot_wo_OP_no_oldOR, r_asth_norsv_oldOR)
asth_wo_OP_mAb_oldOR <- asth_no_rsv_func(tot_wo_OP_mAb_oldOR, r_asth_norsv_oldOR)
asth_wo_OP_rsvPreF_oldOR <- asth_no_rsv_func(tot_wo_OP_rsvPreF_oldOR, r_asth_norsv_oldOR)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_oldOR <- asth_rsv_func(num_OP_ED_Hosp_no_oldOR, r_asth_norsv_oldOR, RR_w_oldOR)
asth_OP_mAb_oldOR <- asth_rsv_func(num_OP_ED_Hosp_mAb_oldOR, r_asth_norsv_oldOR, RR_w_oldOR)
asth_OP_rsvPreF_oldOR <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF_oldOR, r_asth_norsv_oldOR, RR_w_oldOR)

# all cause asthma
tot_asth_no_oldOR <- tot_asth_func(asth_OP_no_oldOR, asth_wo_OP_no_oldOR)
tot_asth_mAb_oldOR <- tot_asth_func(asth_OP_mAb_oldOR, asth_wo_OP_mAb_oldOR)
tot_asth_rsvPreF_oldOR <-tot_asth_func(asth_OP_rsvPreF_oldOR, asth_wo_OP_rsvPreF_oldOR)

# total asthma per 100,000 population
tot_asth_no_pr_oldOR <- tot_asth_no_oldOR / pop_tot * 100000
tot_asth_mAb_pr_oldOR <- tot_asth_mAb_oldOR / pop_tot * 100000
tot_asth_rsvPreF_pr_oldOR <- tot_asth_rsvPreF_oldOR / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_oldOR <- (tot_asth_no_oldOR - tot_asth_mAb_oldOR) / tot_asth_no_oldOR * 100
tot_asth_rsvPreF_pd_oldOR <- (tot_asth_no_oldOR - tot_asth_rsvPreF_oldOR) / tot_asth_no_oldOR * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_oldOR <- asth_rsv_null_func(num_OP_ED_Hosp_no_oldOR, r_asth_norsv_oldOR)
asth_null_mAb_oldOR <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_oldOR, r_asth_norsv_oldOR)
asth_null_rsvPreF_oldOR <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF_oldOR, r_asth_norsv_oldOR)

# RSV-LRTI attributable asthma
att_no_oldOR <- asth_rsv_att_func(asth_OP_no_oldOR, asth_null_no_oldOR)
att_mAb_oldOR <- asth_rsv_att_func(asth_OP_mAb_oldOR, asth_null_mAb_oldOR)
att_rsvPreF_oldOR <-asth_rsv_att_func(asth_OP_rsvPreF_oldOR, asth_null_rsvPreF_oldOR)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_oldOR <- asth_null_no_oldOR + asth_wo_OP_no_oldOR

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_oldOR <- att_no_oldOR / pop_tot * 100000
att_mAb_pr_oldOR <- att_mAb_oldOR / pop_tot * 100000
att_rsvPreF_pr_oldOR <- att_rsvPreF_oldOR / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_oldOR <- (att_no_oldOR - att_mAb_oldOR) / att_no_oldOR * 100
att_rsvPreF_pd_oldOR <- (att_no_oldOR - att_rsvPreF_oldOR) / att_no_oldOR * 100


# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_oldOR <- asth_no_rsv_func(pop_tot, r_asth_norsv_oldOR)
all_rsv_prev_pr_oldOR <- all_rsv_prev_oldOR / pop_tot * 100000
all_rsv_prev_pd_oldOR <- (tot_asth_no_oldOR - all_rsv_prev_oldOR) / tot_asth_no_oldOR * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_oldOR/pop_tot_oldOR)*(RR_w_oldOR-1))/(((num_OP_ED_Hosp_no_oldOR/pop_tot)*(RR_w_oldOR-1)+1))
(tot_asth_no_oldOR-all_rsv_prev_oldOR)/tot_asth_no_oldOR

# -----------------------------
# 1st year of life - specific definition
# -----------------------------

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no_spec <- OP_no_PE_1st_a_spec+ED_no_PE_1st_a_spec+Hosps_no_PE_1st_a_spec
num_OP_ED_Hosp_mAb_spec <- OP_mAb_PE_1st_a_spec+ED_mAb_PE_1st_a_spec+Hosps_mAb_PE_1st_a_spec

# number without RSV-LRTI
tot_wo_OP_no_spec <- pop_tot - num_OP_ED_Hosp_no_spec
tot_wo_OP_mAb_spec <- pop_tot - num_OP_ED_Hosp_mAb_spec

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_spec <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_spec, tot_wo_OP_no_spec)

#RR from OR
RR_w_spec <- aOR_w_asthonly/((1-(r_asth_norsv_spec))+(r_asth_norsv_spec*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_spec <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_spec, num_OP_ED_Hosp_no_spec, tot_wo_OP_no_spec)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_spec <- asth_no_rsv_func(tot_wo_OP_no_spec, r_asth_norsv_spec)
asth_wo_OP_mAb_spec <- asth_no_rsv_func(tot_wo_OP_mAb_spec, r_asth_norsv_spec)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_spec <- asth_rsv_func(num_OP_ED_Hosp_no_spec, r_asth_norsv_spec, RR_w_spec)
asth_OP_mAb_spec <- asth_rsv_func(num_OP_ED_Hosp_mAb_spec, r_asth_norsv_spec, RR_w_spec)

# all cause asthma
tot_asth_adj_spec <- tot_asth_func(asth_OP_no_spec, asth_wo_OP_no_spec)
tot_asth_adj_spec <- tot_asth_func(asth_OP_mAb_spec, asth_wo_OP_mAb_spec)

# total asthma per 100,000 population
tot_asth_no_pr_spec <- tot_asth_no_spec / pop_tot * 100000
tot_asth_mAb_pr_spec <- tot_asth_mAb_spec / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_spec <- (tot_asth_no_spec - tot_asth_mAb_spec) / tot_asth_no_spec * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_spec <- asth_rsv_null_func(num_OP_ED_Hosp_no_spec, r_asth_norsv_spec)
asth_null_mAb_spec <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_spec, r_asth_norsv_spec)

# RSV-LRTI attributable asthma
att_no_spec <- asth_rsv_att_func(asth_OP_no_spec, asth_null_no_spec)
att_mAb_spec <- asth_rsv_att_func(asth_OP_mAb_spec, asth_null_mAb_spec)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_spec <- asth_null_no_spec + asth_wo_OP_no_spec

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_spec <- att_no_spec / pop_tot * 100000
att_mAb_pr_spec <- att_mAb_spec / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_spec <- (att_no_spec - att_mAb_spec) / att_no_spec * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_spec <- asth_no_rsv_func(pop_tot, r_asth_norsv_spec)
all_rsv_prev_pr_spec <- all_rsv_prev_spec / pop_tot * 100000

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_spec/pop_tot)*(RR_w_spec-1))/(((num_OP_ED_Hosp_no_spec/pop_tot)*(RR_w_spec-1)+1))
(tot_asth_no_spec-all_rsv_prev_spec)/tot_asth_no_spec

# -----------------------------
# changed weaning curve
# -----------------------------

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no_adjc <- OP_no_PE_adjc_a+ED_no_PE_adjc_a+Hosps_no_PE_adjc_a
num_OP_ED_Hosp_mAb_adjc <- OP_mAb_PE_adjc_a+ED_mAb_PE_adjc_a+Hosps_mAb_PE_adjc_a

# number without RSV-LRTI 
tot_wo_OP_no_adjc <- pop_tot - num_OP_ED_Hosp_no_adjc
tot_wo_OP_mAb_adjc <- pop_tot - num_OP_ED_Hosp_mAb_adjc

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_adjc <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_adjc, tot_wo_OP_no_adjc)

#RR from OR
RR_w_adjc <- aOR_w_asthonly/((1-(r_asth_norsv_adjc))+(r_asth_norsv_adjc*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# recalculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_adjc <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_adjc, num_OP_ED_Hosp_no_adjc, tot_wo_OP_no_adjc)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_adjc <- asth_no_rsv_func(tot_wo_OP_no_adjc, r_asth_norsv_adjc)
asth_wo_OP_mAb_adjc <- asth_no_rsv_func(tot_wo_OP_mAb_adjc, r_asth_norsv_adjc)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_adjc <- asth_rsv_func(num_OP_ED_Hosp_no_adjc, r_asth_norsv_adjc, RR_w_adjc)
asth_OP_mAb_adjc <- asth_rsv_func(num_OP_ED_Hosp_mAb_adjc, r_asth_norsv_adjc, RR_w_adjc)

# all cause with asthma
tot_asth_no_adjc <- tot_asth_func(asth_OP_no_adjc, asth_wo_OP_no_adjc)
tot_asth_mAb_adjc <- tot_asth_func(asth_OP_mAb_adjc, asth_wo_OP_mAb_adjc)

# total asthma per 100,000 population
tot_asth_no_pr_adjc <- tot_asth_no_adjc / pop_tot * 100000
tot_asth_mAb_pr_adjc <- tot_asth_mAb_adjc / pop_tot * 100000
tot_asth_no_pr_1st-tot_asth_mAb_pr_adjc

# total asthma percent decrease from status quo
tot_asth_mAb_pd_adjc <- (tot_asth_no_adjc - tot_asth_mAb_adjc) / tot_asth_no_adjc * 100 

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_adjc <- asth_rsv_null_func(num_OP_ED_Hosp_no_adjc, r_asth_norsv_adjc)
asth_null_mAb_adjc <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_adjc, r_asth_norsv_adjc)

# RSV-LRTI attributable asthma
att_no_adjc <- asth_rsv_att_func(asth_OP_no_adjc, asth_null_no_adjc)
att_mAb_adjc <- asth_rsv_att_func(asth_OP_mAb_adjc, asth_null_mAb_adjc)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_adjc <- asth_null_no_adjc + asth_wo_OP_no_adjc

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_adjc <- att_no_adjc / pop_tot * 100000
att_mAb_pr_adjc <- att_mAb_adjc / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_adjc <- (att_no_adjc - att_mAb_adjc) / att_no_adjc * 100 

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_adjc <- asth_no_rsv_func(pop_tot, r_asth_norsv_adjc)
all_rsv_prev_pr_adjc <- all_rsv_prev_adjc / pop_tot * 100000
all_rsv_prev_pd_adjc <- (tot_asth_no_adjc - all_rsv_prev_adjc) / tot_asth_no_adjc * 100 

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_adjc/pop_tot)*(RR_w_adjc-1))/(((num_OP_ED_Hosp_no_adjc/pop_tot)*(RR_w_adjc-1)+1))
(tot_asth_no_adjc-all_rsv_prev_adjc)/tot_asth_no_adjc

# -----------------------------
# 90% coverage
# -----------------------------

#Total RSV-LRTI encounters
num_OP_ED_Hosp_no_adjcov <- OP_no_PE_adjcov_a+ED_no_PE_adjcov_a+Hosps_no_PE_adjcov_a
num_OP_ED_Hosp_mAb_adjcov <- OP_mAb_PE_adjcov_a+ED_mAb_PE_adjcov_a+Hosps_mAb_PE_adjcov_a

# number without RSV-LRTI 
tot_wo_OP_no_adjcov <- pop_tot - num_OP_ED_Hosp_no_adjcov
tot_wo_OP_mAb_adjcov <- pop_tot - num_OP_ED_Hosp_mAb_adjcov

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_adjcov <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w_asthonly, num_OP_ED_Hosp_no_adjcov, tot_wo_OP_no_adjcov)

#RR from OR
RR_w_adjcov <- aOR_w_asthonly/((1-(r_asth_norsv_adjcov))+(r_asth_norsv_adjcov*aOR_w_asthonly))

# ----------------------------- #
# Using aRR from OR
# ----------------------------- #

# calculate rate/prevalence of asthma among those without RSV-LRTI
r_asth_norsv_adjcov <- prev_no_rsv_func(prev_tot, pop_tot, RR_w_adjcov, num_OP_ED_Hosp_no_adjcov, tot_wo_OP_no_adjcov)

# number of asthma cases among those without RSV-LRTI
asth_wo_OP_no_adjcov <- asth_no_rsv_func(tot_wo_OP_no_adjcov, r_asth_norsv_adjcov)
asth_wo_OP_mAb_adjcov <- asth_no_rsv_func(tot_wo_OP_mAb_adjcov, r_asth_norsv_adjcov)

# number of asthma cases among those with RSV-LRTI
asth_OP_no_adjcov <- asth_rsv_func(num_OP_ED_Hosp_no_adjcov, r_asth_norsv_adjcov, RR_w_adjcov)
asth_OP_mAb_adjcov <- asth_rsv_func(num_OP_ED_Hosp_mAb_adjcov, r_asth_norsv_adjcov, RR_w_adjcov)

# all cause  asthma
tot_asth_no_adjcov <- tot_asth_func(asth_OP_no_adjcov, asth_wo_OP_no_adjcov)
tot_asth_mAb_adjcov <- tot_asth_func(asth_OP_mAb_adjcov, asth_wo_OP_mAb_adjcov)

# total asthma per 100,000 population
tot_asth_no_pr_adjcov <- tot_asth_no_adjcov / pop_tot * 100000
tot_asth_mAb_pr_adjcov <- tot_asth_mAb_adjcov / pop_tot * 100000

# total asthma percent decrease from status quo
tot_asth_mAb_pd_adjcov <- (tot_asth_no_adjcov - tot_asth_mAb_adjcov) / tot_asth_no_adjcov * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
asth_null_no_adjcov <- asth_rsv_null_func(num_OP_ED_Hosp_no_adjcov, r_asth_norsv_adjcov)
asth_null_mAb_adjcov <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_adjcov, r_asth_norsv_adjcov)

# RSV-LRTI attributable asthma
att_no_adjcov <- asth_rsv_att_func(asth_OP_no_adjcov, asth_null_no_adjcov)
att_mAb_adjcov <- asth_rsv_att_func(asth_OP_mAb_adjcov, asth_null_mAb_adjcov)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
asth_all_RSV_prev_adjcov <- asth_null_no_adjcov + asth_wo_OP_no_adjcov

# RSV-LRTI attributable asthma per 100,000 population
att_no_pr_adjcov <- att_no_adjcov / pop_tot * 100000
att_mAb_pr_adjcov <- att_mAb_adjcov / pop_tot * 100000
att_no_pr_1st-att_mAb_pr_adjcov

# RSV-LRTI attributable asthma percent decrease from status quo
att_mAb_pd_adjcov <- (att_no_adjcov - att_mAb_adjcov) / att_no_adjcov * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
all_rsv_prev_adjcov <- asth_no_rsv_func(pop_tot, r_asth_norsv_adjcov)
all_rsv_prev_pr_adjcov <- all_rsv_prev_adjcov / pop_tot * 100000
all_rsv_prev_pd_adjcov <- (tot_asth_no_adjcov - all_rsv_prev_adjcov) / tot_asth_no_adjcov * 100

#PAF - levin and direct calculation
((num_OP_ED_Hosp_no_adjcov/pop_tot)*(RR_w_adjcov-1))/(((num_OP_ED_Hosp_no_adjcov/pop_tot)*(RR_w_adjcov-1)+1))
(tot_asth_no_adjcov-all_rsv_prev_adjcov)/tot_asth_no_adjcov


# -----------------------------
# 1st and 2nd year of life point estimate data
# -----------------------------

#Total RSV-LRTI cases over three years: sum point estimates for outcomes
#num_OP_ED_Hosp_no <- OP_no_PE_1st_a+OP_no_PE_2nd_a+ED_no_PE_1st_a+ED_no_PE_2nd_a+Hosps_no_PE_1st_a+Hosps_no_PE_2nd_a
#num_OP_ED_Hosp_mAb <- OP_mAb_PE_1st_a+OP_mAb_PE_2nd_a+ED_mAb_PE_1st_a+ED_mAb_PE_2nd_a+Hosps_mAb_PE_1st_a+Hosps_mAb_PE_2nd_a
#num_OP_ED_Hosp_rsvPreF <- OP_rsvPreF_PE_1st_a+OP_rsvPreF_PE_2nd_a+ED_rsvPreF_PE_1st_a+ED_rsvPreF_PE_2nd_a+Hosps_rsvPreF_PE_1st_a+Hosps_rsvPreF_PE_2nd_a

# number of kids without RSV-LRTI for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
#tot_wo_OP_no <- pop_tot - num_OP_ED_Hosp_no
#tot_wo_OP_mAb <- pop_tot - num_OP_ED_Hosp_mAb
#tot_wo_OP_rsvPreF <- pop_tot - num_OP_ED_Hosp_rsvPreF

# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, aOR_w, num_OP_ED_Hosp_no, tot_wo_OP_no)

#OR -> RR calculation
#RR_w <- aOR_w/((1-r_asth_norsv)+(r_asth_norsv*aOR_w))

# Using RR calculated from OR (ie 2.22 [1.34-3.66])
# ------------------------------------- #

# calculate rate/prevalence of asthma among those without RSV-LRTI - new RR value
#r_asth_norsv_adj <- prev_no_rsv_func(prev_tot, pop_tot, RR_w, num_OP_ED_Hosp_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI
#asth_wo_OP_no_adj <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv_adj)
#asth_wo_OP_mAb_adj <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv_adj)
#asth_wo_OP_rsvPreF_adj <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv_adj)

# number of asthma cases among those with RSV-LRTI
#asth_OP_no_adj <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv_adj, RR_w)
#asth_OP_mAb_adj <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj, RR_w)
#asth_OP_rsvPreF_adj <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj, RR_w)

# total with asthma
#tot_asth_no_adj <- tot_asth_func(asth_OP_no_adj, asth_wo_OP_no_adj)
#tot_asth_mAb_adj <- tot_asth_func(asth_OP_mAb_adj, asth_wo_OP_mAb_adj)
#tot_asth_rsvPreF_adj <-tot_asth_func(asth_OP_rsvPreF_adj, asth_wo_OP_rsvPreF_adj)

# total asthma per 100,000 population
#tot_asth_no_pr_adj <- tot_asth_no_adj / pop_tot * 100000
#tot_asth_mAb_pr_adj <- tot_asth_mAb_adj / pop_tot * 100000
#tot_asth_rsvPreF_pr_adj <- tot_asth_rsvPreF_adj / pop_tot * 100000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_adj <- (tot_asth_no_adj - tot_asth_mAb_adj) / tot_asth_no_adj * 100
#tot_asth_rsvPreF_pd_adj <- (tot_asth_no_adj - tot_asth_rsvPreF_adj) / tot_asth_no_adj * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_adj <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv_adj)
#asth_null_mAb_adj <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj)
#asth_null_rsvPreF_adj <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj)

# RSV-LRTI attributable asthma
#att_no_adj <- asth_rsv_att_func(asth_OP_no_adj, asth_null_no_adj)
#att_mAb_adj <- asth_rsv_att_func(asth_OP_mAb_adj, asth_null_mAb_adj)
#att_rsvPreF_adj <-asth_rsv_att_func(asth_OP_rsvPreF_adj, asth_null_rsvPreF_adj)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
#asth_all_RSV_prev_adj <- asth_null_no_adj + asth_wo_OP_no_adj

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_adj <- att_no_adj / pop_tot * 100000
#att_mAb_pr_adj <- att_mAb_adj / pop_tot * 100000
#att_rsvPreF_pr_adj <- att_rsvPreF_adj / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_adj <- (att_no_adj - att_mAb_adj) / att_no_adj * 100
#att_rsvPreF_pd_adj <- (att_no_adj - att_rsvPreF_adj) / att_no_adj * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_adj <- asth_no_rsv_func(pop_tot, r_asth_norsv_adj)
#all_rsv_prev_pr_adj <- all_rsv_prev_adj / pop_tot * 100000
#all_rsv_prev_pd_adj <- (tot_asth_no_adj - all_rsv_prev_adj) / tot_asth_no_adj * 100

#------------------------------------#
#using OR -> RR (ie 2.22 [1.34-3.66]) - Sensitivity analysis for 1st + 2nd year
#------------------------------------#

#RR_w75 <- aOR_w/((1-r_asth_norsv)+(r_asth_norsv*aOR_w))*.75
#RR_w50 <- aOR_w/((1-r_asth_norsv)+(r_asth_norsv*aOR_w))*.50
#RR_w25 <- aOR_w/((1-r_asth_norsv)+(r_asth_norsv*aOR_w))*.25

#75%
# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv_adj75 <- prev_no_rsv_func(prev_tot, pop_tot, RR_w75, num_OP_ED_Hosp_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI
#asth_wo_OP_no_adj75 <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv_adj75)
#asth_wo_OP_mAb_adj75 <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv_adj75)
#asth_wo_OP_rsvPreF_adj75 <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv_adj75)

# number of asthma cases among those with RSV-LRTI
#asth_OP_no_adj75 <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv_adj75, RR_w75)
#asth_OP_mAb_adj75 <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj75, RR_w75)
#asth_OP_rsvPreF_adj75 <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj75, RR_w75)

# total with asthma
#tot_asth_no_adj75 <- tot_asth_func(asth_OP_no_adj75, asth_wo_OP_no_adj75)
#tot_asth_mAb_adj75 <- tot_asth_func(asth_OP_mAb_adj75, asth_wo_OP_mAb_adj75)
#tot_asth_rsvPreF_adj75 <-tot_asth_func(asth_OP_rsvPreF_adj75, asth_wo_OP_rsvPreF_adj75)

# total asthma per 100,000 population
#tot_asth_no_pr_adj75 <- tot_asth_no_adj75 / pop_tot * 100000
#tot_asth_mAb_pr_adj75 <- tot_asth_mAb_adj75 / pop_tot * 100000
#tot_asth_rsvPreF_pr_adj75 <- tot_asth_rsvPreF_adj75 / pop_tot * 100000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_adj75 <- (tot_asth_no_adj75 - tot_asth_mAb_adj75) / tot_asth_no_adj75 * 100
#tot_asth_rsvPreF_pd_adj75 <- (tot_asth_no_adj75 - tot_asth_rsvPreF_adj75) / tot_asth_no_adj75 * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_adj75 <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv_adj75)
#asth_null_mAb_adj75 <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj75)
#asth_null_rsvPreF_adj75 <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj75)

# RSV-LRTI attributable asthma
#att_no_adj75 <- asth_rsv_att_func(asth_OP_no_adj75, asth_null_no_adj75)
#att_mAb_adj75 <- asth_rsv_att_func(asth_OP_mAb_adj75, asth_null_mAb_adj75)
#att_rsvPreF_adj75 <-asth_rsv_att_func(asth_OP_rsvPreF_adj75, asth_null_rsvPreF_adj75)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
#asth_all_RSV_prev_adj75 <- asth_null_no_adj75 + asth_wo_OP_no_adj75

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_adj75 <- att_no_adj75 / pop_tot * 100000
#att_mAb_pr_adj75 <- att_mAb_adj75 / pop_tot * 100000
#att_rsvPreF_pr_adj75 <- att_rsvPreF_adj75 / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_adj75 <- (att_no_adj75 - att_mAb_adj75) / att_no_adj75 * 100
#att_rsvPreF_pd_adj75 <- (att_no_adj75 - att_rsvPreF_adj75) / att_no_adj75 * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_adj75 <- asth_no_rsv_func(pop_tot, r_asth_norsv_adj75)
#all_rsv_prev_pr_adj75 <- all_rsv_prev_adj75 / pop_tot * 100000
#all_rsv_prev_pd_adj75 <- (tot_asth_no_adj75 - all_rsv_prev_adj75) / tot_asth_no_adj75 * 100

#50%
# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv_adj50 <- prev_no_rsv_func(prev_tot, pop_tot, RR_w50, num_OP_ED_Hosp_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI
#asth_wo_OP_no_adj50 <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv_adj50)
#asth_wo_OP_mAb_adj50 <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv_adj50)
#asth_wo_OP_rsvPreF_adj50 <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv_adj50)

# number of asthma cases among those with RSV-LRTI
#asth_OP_no_adj50 <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv_adj50, RR_w50)
#asth_OP_mAb_adj50 <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj50, RR_w50)
#asth_OP_rsvPreF_adj50 <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj50, RR_w50)

# total with asthma
#tot_asth_no_adj50 <- tot_asth_func(asth_OP_no_adj50, asth_wo_OP_no_adj50)
#tot_asth_mAb_adj50 <- tot_asth_func(asth_OP_mAb_adj50, asth_wo_OP_mAb_adj50)
#tot_asth_rsvPreF_adj50 <-tot_asth_func(asth_OP_rsvPreF_adj50, asth_wo_OP_rsvPreF_adj50)

# total asthma per 100,000 population
#tot_asth_no_pr_adj50 <- tot_asth_no_adj50 / pop_tot * 100000
#tot_asth_mAb_pr_adj50 <- tot_asth_mAb_adj50 / pop_tot * 100000
#tot_asth_rsvPreF_pr_adj50 <- tot_asth_rsvPreF_adj50 / pop_tot * 100000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_adj50 <- (tot_asth_no_adj50 - tot_asth_mAb_adj50) / tot_asth_no_adj50 * 100
#tot_asth_rsvPreF_pd_adj50 <- (tot_asth_no_adj50 - tot_asth_rsvPreF_adj50) / tot_asth_no_adj50 * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_adj50 <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv_adj50)
#asth_null_mAb_adj50 <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj50)
#asth_null_rsvPreF_adj50 <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj50)

# RSV-LRTI attributable asthma
#att_no_adj50 <- asth_rsv_att_func(asth_OP_no_adj50, asth_null_no_adj50)
#att_mAb_adj50 <- asth_rsv_att_func(asth_OP_mAb_adj50, asth_null_mAb_adj50)
#att_rsvPreF_adj50 <-asth_rsv_att_func(asth_OP_rsvPreF_adj50, asth_null_rsvPreF_adj50)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
#asth_all_RSV_prev_adj50 <- asth_null_no_adj50 + asth_wo_OP_no_adj50

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_adj50 <- att_no_adj50 / pop_tot * 100000
#att_mAb_pr_adj50 <- att_mAb_adj50 / pop_tot * 100000
#att_rsvPreF_pr_adj50 <- att_rsvPreF_adj50 / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_adj50 <- (att_no_adj50 - att_mAb_adj50) / att_no_adj50 * 100
#att_rsvPreF_pd_adj50 <- (att_no_adj50 - att_rsvPreF_adj50) / att_no_adj50 * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_adj50 <- asth_no_rsv_func(pop_tot, r_asth_norsv_adj50)
#all_rsv_prev_pr_adj50 <- all_rsv_prev_adj50 / pop_tot * 100000
#all_rsv_prev_pd_adj50 <- (tot_asth_no_adj50 - all_rsv_prev_adj50) / tot_asth_no_adj50 * 100

#25%
# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv_adj25 <- prev_no_rsv_func(prev_tot, pop_tot, RR_w25, num_OP_ED_Hosp_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI
#asth_wo_OP_no_adj25 <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv_adj25)
#asth_wo_OP_mAb_adj25 <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv_adj25)
#asth_wo_OP_rsvPreF_adj25 <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv_adj25)

# number of asthma cases among those with RSV-LRTI
#asth_OP_no_adj25 <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv_adj25, RR_w25)
#asth_OP_mAb_adj25 <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj25, RR_w25)
#asth_OP_rsvPreF_adj25 <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj25, RR_w25)

# total with asthma
#tot_asth_no_adj25 <- tot_asth_func(asth_OP_no_adj25, asth_wo_OP_no_adj25)
#tot_asth_mAb_adj25 <- tot_asth_func(asth_OP_mAb_adj25, asth_wo_OP_mAb_adj25)
#tot_asth_rsvPreF_adj25 <-tot_asth_func(asth_OP_rsvPreF_adj25, asth_wo_OP_rsvPreF_adj25)

# total asthma per 100,000 population
#tot_asth_no_pr_adj25 <- tot_asth_no_adj25 / pop_tot * 100000
#tot_asth_mAb_pr_adj25 <- tot_asth_mAb_adj25 / pop_tot * 100000
#tot_asth_rsvPreF_pr_adj25 <- tot_asth_rsvPreF_adj25 / pop_tot * 100000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_adj25 <- (tot_asth_no_adj25 - tot_asth_mAb_adj25) / tot_asth_no_adj25 * 100
#tot_asth_rsvPreF_pd_adj25 <- (tot_asth_no_adj25 - tot_asth_rsvPreF_adj25) / tot_asth_no_adj25 * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_adj25 <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv_adj25)
#asth_null_mAb_adj25 <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv_adj25)
#asth_null_rsvPreF_adj25 <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv_adj25)

# RSV-LRTI attributable asthma
#att_no_adj25 <- asth_rsv_att_func(asth_OP_no_adj25, asth_null_no_adj25)
#att_mAb_adj25 <- asth_rsv_att_func(asth_OP_mAb_adj25, asth_null_mAb_adj25)
#att_rsvPreF_adj25 <-asth_rsv_att_func(asth_OP_rsvPreF_adj25, asth_null_rsvPreF_adj25)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
#asth_all_RSV_prev_adj25 <- asth_null_no_adj25 + asth_wo_OP_no_adj25

# RSV-LRTI attributable asthma per 100,000 population
#att_no_pr_adj25 <- att_no_adj25 / pop_tot * 100000
#att_mAb_pr_adj25 <- att_mAb_adj25 / pop_tot * 100000
#att_rsvPreF_pr_adj25 <- att_rsvPreF_adj25 / pop_tot * 100000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_adj25 <- (att_no_adj25 - att_mAb_adj25) / att_no_adj25 * 100
#att_rsvPreF_pd_adj25 <- (att_no_adj25 - att_rsvPreF_adj25) / att_no_adj25 * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_adj25 <- asth_no_rsv_func(pop_tot, r_asth_norsv_adj25)
#all_rsv_prev_pr_adj25 <- all_rsv_prev_adj25 / pop_tot * 100000
#all_rsv_prev_pd_adj25 <- (tot_asth_no_adj25 - all_rsv_prev_adj25) / tot_asth_no_adj25 * 100

#EXTRA/LEFT OVER CODE

# -----------------------------
# USING NON-ADJUSTED OR - 2.45 - 1ST AND 2ND YEARS OF LIFE DATA
# -----------------------------
# number of asthma cases among those without RSV-LRTI
#asth_wo_OP_no <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv)
#asth_wo_OP_mAb <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv)
#asth_wo_OP_rsvPreF <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI
#asth_OP_no <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv, rr_w)
#asth_OP_mAb <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv, rr_w)
#asth_OP_rsvPreF <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv, rr_w)

# total with asthma
#tot_asth_no <- tot_asth_func(asth_OP_no, asth_wo_OP_no)
#tot_asth_mAb <- tot_asth_func(asth_OP_mAb, asth_wo_OP_mAb)
#tot_asth_rsvPreF <-tot_asth_func(asth_OP_rsvPreF, asth_wo_OP_rsvPreF)

# total asthma per 10,000 population
#tot_asth_no_pr <- tot_asth_no / pop_tot * 10000
#tot_asth_mAb_pr <- tot_asth_mAb / pop_tot * 10000
#tot_asth_rsvPreF_pr <- tot_asth_rsvPreF / pop_tot * 10000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd <- (tot_asth_no - tot_asth_mAb) / tot_asth_no * 100
#tot_asth_rsvPreF_pd <- (tot_asth_no - tot_asth_rsvPreF) / tot_asth_no * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv)
#asth_null_mAb <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv)
#asth_null_rsvPreF <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv)

# RSV-LRTI attributable asthma
#att_no <- asth_rsv_att_func(asth_OP_no, asth_null_no)
#att_mAb <- asth_rsv_att_func(asth_OP_mAb, asth_null_mAb)
#att_rsvPreF <-asth_rsv_att_func(asth_OP_rsvPreF, asth_null_rsvPreF)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
#asth_all_RSV_prev <- asth_null_no + asth_wo_OP_no

# RSV-LRTI attributable asthma per 10,000 population
#att_no_pr <- att_no / pop_tot * 10000
#att_mAb_pr <- att_mAb / pop_tot * 10000
#att_rsvPreF_pr <- att_rsvPreF / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd <- (att_no - att_mAb) / att_no * 100
#att_rsvPreF_pd <- (att_no - att_rsvPreF) / att_no * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev <- asth_no_rsv_func(pop_tot, r_asth_norsv)
#all_rsv_prev_pr <- all_rsv_prev / pop_tot * 10000
#all_rsv_prev_pd <- (tot_asth_no - all_rsv_prev) / tot_asth_no * 100

# -----------------------------
# USING NON-ADJUSTED OR - 2.45 - 1ST YEAR OF LIFE 
# -----------------------------

# number of asthma cases among those without RSV-LRTI
#asth_wo_OP_no_1st <- asth_no_rsv_func(tot_wo_OP_no_1st, r_asth_norsv_1st)
#asth_wo_OP_mAb_1st <- asth_no_rsv_func(tot_wo_OP_mAb_1st, r_asth_norsv_1st)
#asth_wo_OP_rsvPreF_1st <- asth_no_rsv_func(tot_wo_OP_rsvPreF_1st, r_asth_norsv_1st)

# number of asthma cases among those with RSV-LRTI
#asth_OP_no_1st <- asth_rsv_func(num_OP_ED_Hosp_no_1st, r_asth_norsv_1st, rr_w)
#asth_OP_mAb_1st <- asth_rsv_func(num_OP_ED_Hosp_mAb_1st, r_asth_norsv_1st, rr_w)
#asth_OP_rsvPreF_1st <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF_1st, r_asth_norsv_1st, rr_w)

# total with asthma
#tot_asth_no_1st <- tot_asth_func(asth_OP_no_1st, asth_wo_OP_no_1st)
#tot_asth_mAb_1st <- tot_asth_func(asth_OP_mAb_1st, asth_wo_OP_mAb_1st)
#tot_asth_rsvPreF_1st <-tot_asth_func(asth_OP_rsvPreF_1st, asth_wo_OP_rsvPreF_1st)

# total asthma per 10,000 population
#tot_asth_no_pr_1st <- tot_asth_no_1st / pop_tot * 10000
#tot_asth_mAb_pr_1st <- tot_asth_mAb_1st / pop_tot * 10000
#tot_asth_rsvPreF_pr_1st <- tot_asth_rsvPreF_1st / pop_tot * 10000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd_1st <- (tot_asth_no_1st - tot_asth_mAb_1st) / tot_asth_no_1st * 100
#tot_asth_rsvPreF_pd_1st <- (tot_asth_no_1st - tot_asth_rsvPreF_1st) / tot_asth_no_1st * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no_1st <- asth_rsv_null_func(num_OP_ED_Hosp_no_1st, r_asth_norsv_1st)
#asth_null_mAb_1st <- asth_rsv_null_func(num_OP_ED_Hosp_mAb_1st, r_asth_norsv_1st)
#asth_null_rsvPreF_1st <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF_1st, r_asth_norsv_1st)

# RSV-LRTI attributable asthma
#att_no_1st <- asth_rsv_att_func(asth_OP_no_1st, asth_null_no_1st)
#att_mAb_1st <- asth_rsv_att_func(asth_OP_mAb_1st, asth_null_mAb_1st)
#att_rsvPreF_1st <-asth_rsv_att_func(asth_OP_rsvPreF_1st, asth_null_rsvPreF_1st)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
#asth_all_RSV_prev_1st <- asth_null_no_1st + asth_wo_OP_no_1st

# RSV-LRTI attributable asthma per 10,000 population
#att_no_pr_1st <- att_no_1st / pop_tot * 10000
#att_mAb_pr_1st <- att_mAb_1st / pop_tot * 10000
#att_rsvPreF_pr_1st <- att_rsvPreF_1st / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd_1st <- (att_no_1st - att_mAb_1st) / att_no_1st * 100
#att_rsvPreF_pd_1st <- (att_no_1st - att_rsvPreF_1st) / att_no_1st * 100
#att_Combined_pd <- (att_no - att_Combined) / att_no * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev_1st <- asth_no_rsv_func(pop_tot, r_asth_norsv_1st)
#all_rsv_prev_pr_1st <- all_rsv_prev_1st / pop_tot * 10000
#all_rsv_prev_pd_1st <- (tot_asth_no_1st - all_rsv_prev_1st) / tot_asth_no_1st * 100

#PAF RSV on wheeze
#((num_OP_ED_Hosp_no_1st/pop_tot*(rr_w-1))/(num_OP_ED_Hosp_no_1st/pop_tot*(rr_w-1)+1))

# -----------------------------
# 1st, 2nd and 3rd year of life point estimate data
# -----------------------------

#Total RSV-LRTI cases over three years: sum point estimates for outcomes
#num_OP_ED_Hosp_no <- OP_no_PE_1st+OP_no_PE_2nd+OP_no_PE_3rd+ED_no_PE_1st+ED_no_PE_2nd+ED_no_PE_3rd+Hosps_no_PE_1st+Hosps_no_PE_2nd+Hosps_no_PE_3rd
#num_OP_ED_Hosp_mAb <- OP_mAb_PE_1st+OP_mAb_PE_2nd+OP_mAb_PE_3rd+ED_mAb_PE_1st+ED_mAb_PE_2nd+ED_mAb_PE_3rd+Hosps_mAb_PE_1st+Hosps_mAb_PE_2nd+Hosps_mAb_PE_3rd
#num_OP_ED_Hosp_rsvPreF <- OP_rsvPreF_PE_1st+OP_rsvPreF_PE_2nd+OP_rsvPreF_PE_3rd+ED_rsvPreF_PE_1st+ED_rsvPreF_PE_2nd+ED_rsvPreF_PE_3rd+Hosps_rsvPreF_PE_1st+Hosps_rsvPreF_PE_2nd+Hosps_rsvPreF_PE_3rd

#Total RSV-LRTI cases over 1st year: sum point estimates for outcomes
#num_OP_ED_Hosp_no_1st <- OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st
#num_OP_ED_Hosp_mAb_1st <- OP_mAb_PE_1st+ED_mAb_PE_1st+Hosps_mAb_PE_1st
#num_OP_ED_Hosp_rsvPreF_1st <- OP_rsvPreF_PE_1st+ED_rsvPreF_PE_1st+Hosps_rsvPreF_PE_1st

# number of kids without RSV-LRTI for each strategy
# no intervention, mAb, rsvPreF, Combined strategies
#tot_wo_OP_no <- pop_tot - num_OP_ED_Hosp_no
#tot_wo_OP_mAb <- pop_tot - num_OP_ED_Hosp_mAb
#tot_wo_OP_rsvPreF <- pop_tot - num_OP_ED_Hosp_rsvPreF

# calculate rate/prevalence of asthma among those without RSV-LRTI
#r_asth_norsv <- prev_no_rsv_func(prev_tot, pop_tot, rr_w, num_OP_ED_Hosp_no, tot_wo_OP_no)

# number of asthma cases among those without RSV-LRTI
#asth_wo_OP_no <- asth_no_rsv_func(tot_wo_OP_no, r_asth_norsv)
#asth_wo_OP_mAb <- asth_no_rsv_func(tot_wo_OP_mAb, r_asth_norsv)
#asth_wo_OP_rsvPreF <- asth_no_rsv_func(tot_wo_OP_rsvPreF, r_asth_norsv)

# number of asthma cases among those with RSV-LRTI
#asth_OP_no <- asth_rsv_func(num_OP_ED_Hosp_no, r_asth_norsv, rr_w)
#asth_OP_mAb <- asth_rsv_func(num_OP_ED_Hosp_mAb, r_asth_norsv, rr_w)
#asth_OP_rsvPreF <- asth_rsv_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv, rr_w)

# total with asthma
#tot_asth_no <- tot_asth_func(asth_OP_no, asth_wo_OP_no)
#tot_asth_mAb <- tot_asth_func(asth_OP_mAb, asth_wo_OP_mAb)
#tot_asth_rsvPreF <-tot_asth_func(asth_OP_rsvPreF, asth_wo_OP_rsvPreF)

# total asthma per 10,000 population
#tot_asth_no_pr <- tot_asth_no / pop_tot * 10000
#tot_asth_mAb_pr <- tot_asth_mAb / pop_tot * 10000
#tot_asth_rsvPreF_pr <- tot_asth_rsvPreF / pop_tot * 10000

# total asthma percent decrease from status quo
#tot_asth_mAb_pd <- (tot_asth_no - tot_asth_mAb) / tot_asth_no * 100
#tot_asth_rsvPreF_pd <- (tot_asth_no - tot_asth_rsvPreF) / tot_asth_no * 100

# number of asthma cases among those with RSV-LRTI had they not been infected
#asth_null_no <- asth_rsv_null_func(num_OP_ED_Hosp_no, r_asth_norsv)
#asth_null_mAb <- asth_rsv_null_func(num_OP_ED_Hosp_mAb, r_asth_norsv)
#asth_null_rsvPreF <-asth_rsv_null_func(num_OP_ED_Hosp_rsvPreF, r_asth_norsv)

# RSV-LRTI attributable asthma
#att_no <- asth_rsv_att_func(asth_OP_no, asth_null_no)
#att_mAb <- asth_rsv_att_func(asth_OP_mAb, asth_null_mAb)
#att_rsvPreF <-asth_rsv_att_func(asth_OP_rsvPreF, asth_null_rsvPreF)

# total recurrent wheeze/asthma, all RSV LRTI or ED encounter  prevented
#asth_all_RSV_prev <- asth_null_no + asth_wo_OP_no

# RSV-LRTI attributable asthma per 10,000 population
#att_no_pr <- att_no / pop_tot * 10000
#att_mAb_pr <- att_mAb / pop_tot * 10000
#att_rsvPreF_pr <- att_rsvPreF / pop_tot * 10000

# RSV-LRTI attributable asthma percent decrease from status quo
#att_mAb_pd <- (att_no - att_mAb) / att_no * 100
#att_rsvPreF_pd <- (att_no - att_rsvPreF) / att_no * 100

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
# equal to the total population * the baseline rate of asthma among those w/o RSV
#all_rsv_prev <- asth_no_rsv_func(pop_tot, r_asth_norsv)
#all_rsv_prev_pr <- all_rsv_prev / pop_tot * 10000
#all_rsv_prev_pd <- (tot_asth_no - all_rsv_prev) / tot_asth_no * 100

# -----------------------------
# Mortality adjustment
# -----------------------------

# adjust number of outpatient/ED/Hosp LRTI (total) to account for all-cause mortality out to 6 years - No need 
# to adjust for mortality rate given no adjustment within first year?

#tot_OP_no <- mort_adj_func(RSV_no, U5 = U5_mort, U9 = U9_mort)
#tot_OP_mAb <- mort_adj_func(RSV_mAb, U5 = U5_mort, U9 = U9_mort)
#tot_OP_rsvPreF <-mort_adj_func(RSV_mat, U5 = U5_mort, U9 = U9_mort)
#tot_OP_Combined <-mort_adj_func(RSV_com, U5 = U5_mort, U9 = U9_mort)

#OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
#OP_no_PE_2nd_a+ED_no_PE_2nd_a+Hosps_no_PE_2nd_a


