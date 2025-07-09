#Adjusting for repeated visits to the hospital for one episode

#creating uncertainty around ratio of multiple visits per one episode
episode = 239812
visit = 363833
ratio_multiple_ep = 1-((363833-239812)/363833)
ratio_multiple_ep_CI = rbeta(1000, episode, visit-episode)

#Point Estimate combined value
num_OP_ED_Hosp_no = ratio_multiple_ep*num_OP_ED_Hosp_no
num_OP_ED_Hosp_mAb = ratio_multiple_ep*num_OP_ED_Hosp_mAb
num_OP_ED_Hosp_rsvPreF = ratio_multiple_ep*num_OP_ED_Hosp_rsvPreF
num_OP_ED_Hosp_Combined = ratio_multiple_ep*num_OP_ED_Hosp_Combined

#Point Estimate individual 
adj_RSV_no = ratio_multiple_ep*RSV_no
adj_RSV_mAb = ratio_multiple_ep*RSV_mAb 
adj_RSV_rsvPreF = ratio_multiple_ep*RSV_rsvPreF 
adj_RSV_Combined = ratio_multiple_ep*RSV_Combined

adj_RSV_ED_no = ratio_multiple_ep*RSV_ED_no
adj_RSV_ED_mAb = ratio_multiple_ep*RSV_ED_mAb 
adj_RSV_ED_rsvPreF = ratio_multiple_ep*RSV_ED_rsvPreF 
adj_RSV_ED_Combined = ratio_multiple_ep*RSV_ED_Combined

adj_RSV_Hosps_no = ratio_multiple_ep*RSV_Hosps_no
adj_RSV_Hosps_mAb = ratio_multiple_ep*RSV_Hosps_mAb 
adj_RSV_Hosps_rsvPreF = ratio_multiple_ep*RSV_Hosps_rsvPreF 
adj_RSV_Hosps_Combined = ratio_multiple_ep*RSV_Hosps_Combined

#Uncertainty adjustment

#LRTI adjustment for ED and OP? -> was the data given to us LRTI or all RSV infection?
#For Medically attended outpatient ages 0-5 mo, LRTI assumed to be .65 (65%) of incidence of total RSV incidence (URTI .35 [35%]). For 6-11mo, assumed to be .3 (30%) (URTI .7 [70%])
#OP_LRTI_PE_05 <- .65 #beta
#OP_LRTI_UL_05 <- 1.0
#OP_LRTI_LL_05 <- .25

#rr_OP05_sd <- (log(OP_LRTI_UL_05) - log(OP_LRTI_LL_05))/ (1.96*2) # standard deviation
#rr_OP05_sample <- rnorm(trials, log(OP_LRTI_PE_05), rr_OP05_sd) # normal dist of log
#rr_OP05_u <- exp(rr_OP05_sample) # retransformed uncertainty distribution

#OP_LRTI_PE_611 <- .3 #beta
#OP_LRTI_UL_611 <- 1.0
#OP_LRTI_LL_611 <- .1

#rr_OP611_sd <- (log(OP_LRTI_UL_611) - log(OP_LRTI_LL_611))/ (1.96*2) # standard deviation
#rr_OP611_sample <- rnorm(trials, log(OP_LRTI_PE_611), rr_OP611_sd) # normal dist of log
#rr_OP611_u <- exp(rr_OP611_sample) # retransformed uncertainty distribution

#For ED attended ages 0-5 mo, LRTI assumed to be .65 (65%) of total incidence (URTI .35 [35%]). Ages 6-11 mo LRTI .5 (50%) of total incidence (URTI .5 [50%]).
#ED_LRTI_PE_05 <- .65 #beta
#ED_LRTI_UL_05 <- 1.0
#ED_LRTI_LL_05 <- .25

#rr_ED05_sd <- (log(ED_LRTI_UL_05) - log(ED_LRTI_LL_05))/ (1.96*2) # standard deviation
#rr_ED05_sample <- rnorm(trials, log(ED_LRTI_PE_05), rr_ED05_sd) # normal dist of log
#rr_ED05_u <- exp(rr_ED05_sample) # retransformed uncertainty distribution

#ED_LRTI_PE_611 <- .5 #beta
#ED_LRTI_UL_611 <- 1.0
#ED_LRTI_LL_611 <- .25

#rr_ED611_sd <- (log(ED_LRTI_UL_611) - log(ED_LRTI_LL_611))/ (1.96*2) # standard deviation
#rr_ED611_sample <- rnorm(trials, log(ED_LRTI_PE_611), rr_ED611_sd) # normal dist of log
#rr_ED611_u <- exp(rr_ED611_sample) # retransformed uncertainty distribution

#OP adjustments for LRTI - PE

#OP_no_df <- rowSums(merge(OP_no_u_df[3:8]*.65,OP_no_u_df[9:14]*.3))
#OP_mAb_df <- rowSums(merge(OP_mAb_u_df[3:8]*.65,OP_mAb_u_df[9:14]*.3))
#OP_rsvPreF_df <- rowSums(merge(OP_rsvPreF_u_df[3:8]*.65,OP_rsvPreF_u_df[9:14]*.3))

#ED adjustments for LRTI - PE

#ED_no_df <- rowSums(merge(ED_no_u_df[3:8]*.65,ED_no_u_df[9:14]*.5))
#ED_mAb_df <- rowSums(merge(ED_mAb_u_df[3:8]*.65,ED_mAb_u_df[9:14]*.5))
#ED_rsvPreF_df <- rowSums(merge(ED_rsvPreF_u_df[3:8]*.65,ED_rsvPreF_u_df[9:14]*.5))

# Changing syntax
tot_RSV_no_u <- Hosps_no_df+ED_no_df+OP_no_df
tot_RSV_mAb_u <- Hosps_mAb_df+ED_mAb_df+OP_mAb_df
tot_RSV_rsvPreF_u <- Hosps_rsvPreF_df+ED_rsvPreF_df+OP_rsvPreF_df

#Adjusting for repeated visits
tot_RSV_no_u = ratio_multiple_ep_CI*tot_RSV_no_u
tot_RSV_mAb_u = ratio_multiple_ep_CI*tot_RSV_mAb_u
tot_RSV_rsvPreF_u = ratio_multiple_ep_CI*tot_RSV_rsvPreF_u


