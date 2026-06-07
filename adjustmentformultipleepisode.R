# -----------------------------
# ENCOUNTER ADJUSTMENT FOR MULTIPLE HEALTHCARE VISITS
# -----------------------------

#creating uncertainty around ratio of multiple visits per one episode during first-third year of life
episode = 239812
visit = 363833
ratio_multiple_ep_1st = 1-((363833-239812)/363833)
ratio_multiple_ep_CI_1st = rbeta(1000, episode, visit-episode)

#Adjusting 1st year of life
OP_no_PE_1st_a = OP_no_PE_1st*ratio_multiple_ep_1st
ED_no_PE_1st_a = ED_no_PE_1st*ratio_multiple_ep_1st
Hosps_no_PE_1st_a = Hosps_no_PE_1st*ratio_multiple_ep_1st
OP_mAb_PE_1st_a = OP_mAb_PE_1st*ratio_multiple_ep_1st
ED_mAb_PE_1st_a = ED_mAb_PE_1st*ratio_multiple_ep_1st
Hosps_mAb_PE_1st_a = Hosps_mAb_PE_1st*ratio_multiple_ep_1st
OP_rsvPreF_PE_1st_a = OP_rsvPreF_PE_1st*ratio_multiple_ep_1st
ED_rsvPreF_PE_1st_a = ED_rsvPreF_PE_1st*ratio_multiple_ep_1st
Hosps_rsvPreF_PE_1st_a = Hosps_rsvPreF_PE_1st*ratio_multiple_ep_1st

#Uncertainty adjustment 
#first year of life
OP_no_df_1st_a <- OP_no_df_1st*ratio_multiple_ep_CI_1st
ED_no_df_1st_a <- ED_no_df_1st*ratio_multiple_ep_CI_1st
Hosps_no_df_1st_a <- Hosps_no_df_1st*ratio_multiple_ep_CI_1st
OP_mAb_df_1st_a <- OP_mAb_df_1st*ratio_multiple_ep_CI_1st
ED_mAb_df_1st_a <- ED_mAb_df_1st*ratio_multiple_ep_CI_1st
Hosps_mAb_df_1st_a <- Hosps_mAb_df_1st*ratio_multiple_ep_CI_1st
OP_rsvPreF_df_1st_a <- OP_rsvPreF_df_1st*ratio_multiple_ep_CI_1st
ED_rsvPreF_df_1st_a <- ED_rsvPreF_df_1st*ratio_multiple_ep_CI_1st
Hosps_rsvPreF_df_1st_a <- Hosps_rsvPreF_df_1st*ratio_multiple_ep_CI_1st

# -----------------------------
# ADJUSTMENT OF 2ND YEAR OF LIFE https://pmc.ncbi.nlm.nih.gov/articles/PMC9150639/ ; https://pmc.ncbi.nlm.nih.gov/articles/PMC9934310/ ; vs using adjustment factor from first year of life
# -----------------------------
ratio_multiple_ep_2nd = ratio_multiple_ep_1st*(59/74) # PMID - 35668702 describes # of infections which represent new infections, also multiplied by same correction factor used in 1st year of life
ratio_multiple_ep_CI_2nd = ratio_multiple_ep_CI_1st*(59/74)

#adjusting 2nd year of life
OP_no_PE_2nd_a = OP_no_PE_2nd*ratio_multiple_ep_2nd
ED_no_PE_2nd_a = ED_no_PE_2nd*ratio_multiple_ep_2nd
Hosps_no_PE_2nd_a = Hosps_no_PE_2nd*ratio_multiple_ep_2nd
OP_mAb_PE_2nd_a = OP_mAb_PE_2nd*ratio_multiple_ep_2nd
ED_mAb_PE_2nd_a = ED_mAb_PE_2nd*ratio_multiple_ep_2nd
Hosps_mAb_PE_2nd_a = Hosps_mAb_PE_2nd*ratio_multiple_ep_2nd
OP_rsvPreF_PE_2nd_a = OP_rsvPreF_PE_2nd*ratio_multiple_ep_2nd
ED_rsvPreF_PE_2nd_a = ED_rsvPreF_PE_2nd*ratio_multiple_ep_2nd
Hosps_rsvPreF_PE_2nd_a = Hosps_rsvPreF_PE_2nd*ratio_multiple_ep_2nd

#second year of life
OP_no_df_2nd_a <- OP_no_df_2nd*ratio_multiple_ep_CI_2nd
ED_no_df_2nd_a <- ED_no_df_2nd*ratio_multiple_ep_CI_2nd
Hosps_no_df_2nd_a <- Hosps_no_df_2nd*ratio_multiple_ep_CI_2nd
OP_mAb_df_2nd_a <- OP_mAb_df_2nd*ratio_multiple_ep_CI_2nd
ED_mAb_df_2nd_a <- ED_mAb_df_2nd*ratio_multiple_ep_CI_2nd
Hosps_mAb_df_2nd_a <- Hosps_mAb_df_2nd*ratio_multiple_ep_CI_2nd
OP_rsvPreF_df_2nd_a <- OP_rsvPreF_df_2nd*ratio_multiple_ep_CI_2nd
ED_rsvPreF_df_2nd_a <- ED_rsvPreF_df_2nd*ratio_multiple_ep_CI_2nd
Hosps_rsvPreF_df_2nd_a <- Hosps_rsvPreF_df_2nd*ratio_multiple_ep_CI_2nd

#EXTRA WORK/OLD CODE

# -----------------------------
# ADJUSTMENT OF 3RD YEAR OF LIFE
# -----------------------------

#ratio_multiple_ep_3rd = .37 https://pmc.ncbi.nlm.nih.gov/articles/PMC9150639/ ; https://pmc.ncbi.nlm.nih.gov/articles/PMC9934310/ 
#ratio_multiple_ep_CI_3rd = ***

#Adjusting for multiple episodes of MA-RSV within each year of life - hospitalization data https://pubmed.ncbi.nlm.nih.gov/32706370/
#ratio_multiple_RSV_1st = .35/100
#ratio_multiple_RSV_CI_1st = 
#ratio_multiple_RSV_2nd = .35/100
#ratio_multiple_RSV_CI_2nd = 
#ratio_multiple_RSV_3rd = .35/100
#ratio_multiple_RSV_CI_3rd = 

#adjusting 3rd year of life
#OP_no_PE_3rd = OP_no_PE_3rd*ratio_multiple_ep_3rd
#ED_no_PE_3rd = ED_no_PE_3rd*ratio_multiple_ep_3rd
#Hosps_no_PE_3rd = Hosps_no_PE_3rd*ratio_multiple_ep_3rd
#OP_mAb_PE_3rd = OP_mAb_PE_3rd*ratio_multiple_ep_3rd
#ED_mAb_PE_3rd = ED_mAb_PE_3rd*ratio_multiple_ep_3rd
#Hosps_mAb_PE_3rd = Hosps_mAb_PE_3rd*ratio_multiple_ep_3rd
#OP_rsvPreF_PE_3rd = OP_rsvPreF_PE_3rd*ratio_multiple_ep_3rd
#ED_rsvPreF_PE_3rd = ED_rsvPreF_PE_3rd*ratio_multiple_ep_3rd
#Hosps_rsvPreF_PE_3rd = Hosps_rsvPreF_PE_3rd*ratio_multiple_ep_3rd

#third year of life
#OP_no_df_3rd <- OP_no_df_3rd*ratio_multiple_ep_CI_3rd
#ED_no_df_3rd <- ED_no_df_3rd*ratio_multiple_ep_CI_3rd
#Hosps_no_df_3rd <- Hosps_no_df_3rd*ratio_multiple_ep_CI_3rd
#OP_mAb_df_3rd <- OP_mAb_df_3rd*ratio_multiple_ep_CI_3rd
#ED_mAb_df_3rd <- ED_mAb_df_3rd*ratio_multiple_ep_CI_3rd
#Hosps_mAb_df_3rd <- Hosps_mAb_df_3rd*ratio_multiple_ep_CI_3rd
#OP_rsvPreF_df_3rd <- OP_rsvPreF_df_3rd*ratio_multiple_ep_CI_3rd
#ED_rsvPreF_df_3rd <- ED_rsvPreF_df_3rd*ratio_multiple_ep_CI_3rd
#Hosps_rsvPreF_df_3rd <- Hosps_rsvPreF_df_3rd*ratio_multiple_ep_CI_3rd

# -----------------------------
# PRIOR 1ST YEAR OF LIFE ADJUSTMENT
# -----------------------------
#Point Estimate combined value - first year of life
#num_OP_ED_Hosp_no = ratio_multiple_ep*num_OP_ED_Hosp_no
#num_OP_ED_Hosp_mAb = ratio_multiple_ep*num_OP_ED_Hosp_mAb
#num_OP_ED_Hosp_rsvPreF = ratio_multiple_ep*num_OP_ED_Hosp_rsvPreF
#num_OP_ED_Hosp_Combined = ratio_multiple_ep*num_OP_ED_Hosp_Combined

#Point Estimate individual - first year of life
#adj_RSV_no = ratio_multiple_ep*RSV_no
#adj_RSV_mAb = ratio_multiple_ep*RSV_mAb 
#adj_RSV_rsvPreF = ratio_multiple_ep*RSV_rsvPreF 
#adj_RSV_Combined = ratio_multiple_ep*RSV_Combined

#adj_RSV_ED_no = ratio_multiple_ep*RSV_ED_no
#adj_RSV_ED_mAb = ratio_multiple_ep*RSV_ED_mAb 
#adj_RSV_ED_rsvPreF = ratio_multiple_ep*RSV_ED_rsvPreF 
#adj_RSV_ED_Combined = ratio_multiple_ep*RSV_ED_Combined

#adj_RSV_Hosps_no = ratio_multiple_ep*RSV_Hosps_no
#adj_RSV_Hosps_mAb = ratio_multiple_ep*RSV_Hosps_mAb 
#adj_RSV_Hosps_rsvPreF = ratio_multiple_ep*RSV_Hosps_rsvPreF 
#adj_RSV_Hosps_Combined = ratio_multiple_ep*RSV_Hosps_Combined

#Uncertainty
#tot_RSV_no_u <- (Hosps_no_df+ED_no_df+OP_no_df)*ratio_multiple_ep_CI_1st
#tot_RSV_mAb_u <- (Hosps_mAb_df+ED_mAb_df+OP_mAb_df)*ratio_multiple_ep_CI_1st
#tot_RSV_rsvPreF_u <- (Hosps_rsvPreF_df+ED_rsvPreF_df+OP_rsvPreF_df)*ratio_multiple_ep_CI_1st

#Adjusting for repeated visits to the hospital for one episode
#creating uncertainty around ratio of multiple visits per one episode
#episode = 239812
#visit = 363833
#ratio_multiple_ep = 1-((363833-239812)/363833)
#ratio_multiple_ep_CI = rbeta(1000, episode, visit-episode)

#Uncertainty adjustment - no outpatient RSV/LRTI
# Changing syntax
#tot_RSV_no_u <- (Hosps_no_df+ED_no_df)*ratio_multiple_ep_CI
#tot_RSV_mAb_u <- (Hosps_mAb_df+ED_mAb_df)*ratio_multiple_ep_CI
#tot_RSV_rsvPreF_u <- (Hosps_rsvPreF_df+ED_rsvPreF_df)*ratio_multiple_ep_CI

#LRTI adjustment for ED and OP? -> was the data given to us LRTI or all RSV infection - this was answered and addressed. Original dataset with trials was combined. 
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
