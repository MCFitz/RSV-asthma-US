#RSV USA Uncertainty analysis - intervals (Based from Credible Intervals Mali RSV)

# total RSV-Outpatient cases through 2 years of age
#CI_Outpatient_tot_no <- CI_func(rowSums(Outpatient_age_no[,1:12]))
#CI_Outpatient_tot_mAb <- CI_func(rowSums(Outpatient_age_mAb[,1:12]))
#CI_Outpatient_tot_rsvPreF <- CI_func(rowSums(Outpatient_age_rsvPreF[,1:12]))
#CI_Outpatient_tot_Combined <- CI_func(rowSums(Outpatient_age_Combined[,1:12]))

# total RSV-ED Appts through 2 years of age
#CI_ED_tot_no <- CI_func(rowSums(ED_age_no[,1:12]))
#CI_ED_tot_mAb <- CI_func(rowSums(ED_age_mAb[,1:12]))
#CI_ED_tot_rsvPreF <- CI_func(rowSums(ED_age_rsvPreF[,1:12]))
#CI_ED_tot_Combined <- CI_func(rowSums(ED_age_Combined[,1:12]))

# total RSV-Hospitalizations through 2 years of age
CI_Hosp_tot_no <- CI_func(rowSums(hosps_age_no[,1:12]))
CI_Hosp_tot_mAb <- CI_func(rowSums(hosps_age_mAb[,1:12]))
CI_Hosp_tot_rsvPreF <- CI_func(rowSums(hosps_age_rsvPreF[,1:12]))
CI_Hosp_tot_Combined <- CI_func(rowSums(hosps_age_Combined[,1:12]))

# total RSV-Deaths through 2 years of age
#CI_Deaths_tot_no <- CI_func(rowSums(deaths_age_no[,1:12]))
#CI_Deaths_tot_mAb <- CI_func(rowSums(deaths_age_mAb[,1:12]))
#CI_Deaths_tot_rsvPreF <- CI_func(rowSums(deaths_age_rsvPreF[,1:12]))
#CI_Deaths_tot_Combined <- CI_func(rowSums(deaths_age_Combined[,1:12]))

# RSV-LRTI Outpatient cases through 1 years of age, with agebins
#CI_Outpatient_age_no <- cbind(apply(Outpatient_no_agebin, 2 ,CI_func), CI_Outpatient_tot_no)
#colnames(CI_Outpatient_age_no) <- c(age_cats, "total")
#CI_Outpatient_age_mAb <- cbind(apply(Outpatient_mAb_agebin, 2 ,CI_func), CI_Outpatient_tot_mAb)
#colnames(CI_Outpatient_age_mAb) <- c(age_cats, "total")
#CI_Outpatient_age_rsvPreF <- cbind(apply(Outpatient_rsvPreF_agebin, 2 ,CI_func), CI_Outpatient_tot_rsvPreF)
#colnames(CI_Outpatient_age_rsvPreF) <- c(age_cats, "total")
#CI_Outpatient_age_Combined <- cbind(apply(Outpatient_Combined_agebin, 2 ,CI_func), CI_Outpatient_tot_Combined)
#colnames(CI_Outpatient_age_Combined) <- c(age_cats, "total")

# RSV-LRTI ED Cases through 1 years of age, with agebins
#CI_ED_age_no <- cbind(apply(ED_no_agebin, 2 ,CI_func), CI_ED_tot_no)
#colnames(CI_ED_age_no) <- c(age_cats, "total")
#CI_ED_age_mAb <- cbind(apply(ED_mAb_agebin, 2 ,CI_func), CI_ED_tot_mAb)
#colnames(CI_ED_age_mAb) <- c(age_cats, "total")
#CI_ED_age_rsvPreF <- cbind(apply(ED_rsvPreF_agebin, 2 ,CI_func), CI_ED_tot_rsvPreF)
#colnames(CI_ED_age_rsvPreF) <- c(age_cats, "total")
#CI_ED_age_Combined <- cbind(apply(ED_Combined_agebin, 2 ,CI_func), CI_Outpatient_tot_Combined)
#colnames(CI_ED_age_Combined) <- c(age_cats, "total")

# RSV-LRTI Hospitalizations through 1 years of age, with agebins
CI_Hosp_age_no <- cbind(apply(hosps_no_agebin, 2 ,CI_func), CI_Hosp_tot_no)
colnames(CI_Hosp_age_no) <- c(age_cats, "total")
CI_Hosp_age_mAb <- cbind(apply(hosps_mAb_agebin, 2 ,CI_func), CI_Hosp_tot_mAb)#
colnames(CI_Hosp_age_mAb) <- c(age_cats, "total")
CI_Hosp_age_rsvPreF <- cbind(apply(hosps_rsvPreF_agebin, 2 ,CI_func), CI_Hosp_tot_rsvPreF)
colnames(CI_Hosp_age_rsvPreF) <- c(age_cats, "total")
CI_Hosp_age_Combined <- cbind(apply(hosps_Combined_agebin, 2 ,CI_func), CI_Hosp_tot_Combined)
colnames(CI_Hosp_age_Combined) <- c(age_cats, "total")

# RSV-LRTI Deaths through 1 years of age, with agebins
#CI_Deaths_age_no <- cbind(apply(deaths_no_agebin, 2 ,CI_func), CI_Deaths_tot_no)
#colnames(CI_Deaths_age_no) <- c(age_cats, "total")
#CI_Deaths_age_mAb <- cbind(apply(deaths_mAb_agebin, 2 ,CI_func), CI_Deaths_tot_mAb)
#colnames(CI_Deaths_age_mAb) <- c(age_cats, "total")
#CI_Deaths_age_rsvPreF <- cbind(apply(deaths_rsvPreF_agebin, 2 ,CI_func), CI_Deaths_tot_rsvPreF)
#colnames(CI_Deaths_age_rsvPreF) <- c(age_cats, "total")
#CI_Deaths_age_Combined <- cbind(apply(deaths_Combined_agebin, 2 ,CI_func), CI_Deaths_tot_Combined)
#colnames(CI_Deaths_age_Combined) <- c(age_cats, "total")

# total RSV-LRTI Outpatient percent decrease from status quo
#CI_Outpatient_pd_tot_mAb <- CI_func(Outpatient_pd_tot_mAb)
#CI_Outpatient_pd_tot_rsvPreF <- CI_func(Outpatient_pd_tot_rsvPreF)
#CI_Outpatient_pd_tot_Combined <- CI_func(Outpatient_pd_tot_Combined)

# RSV-LRTI Outpatient percent decrease from status quo, by age bin
#CI_Outpatient_pd_mAb <- cbind(apply(Outpatient_pd_mAb, 2, CI_func), CI_Outpatient_pd_tot_mAb)
#colnames(CI_Outpatient_pd_mAb) <- c(age_cats, "total")
#CI_Outpatient_pd_rsvPreF <- cbind(apply(Outpatient_pd_rsvPreF, 2, CI_func), CI_Outpatient_pd_tot_rsvPreF)
#colnames(CI_Outpatient_pd_rsvPreF) <- c(age_cats, "total")
#CI_Outpatient_pd_Combined <- cbind(apply(Outpatient_pd_Combined, 2, CI_func), CI_Outpatient_pd_tot_Combined)
#colnames(CI_Outpatient_pd_Combined) <- c(age_cats, "total")

# total RSV-LRTI ED visits percent decrease from status quo
#CI_ED_pd_tot_mAb <- CI_func(ED_pd_tot_mAb)
#CI_ED_pd_tot_rsvPreF <- CI_func(ED_pd_tot_rsvPreF)
#CI_ED_pd_tot_Combined <- CI_func(ED_pd_tot_Combined)

# RSV-LRTI ED visits percent decrease from status quo, by age bin
#CI_ED_pd_mAb <- cbind(apply(ED_pd_mAb, 2, CI_func), CI_ED_pd_tot_mAb)
#colnames(CI_ED_pd_mAb) <- c(age_cats, "total")
#CI_ED_pd_rsvPreF <- cbind(apply(ED_pd_rsvPreF, 2, CI_func), CI_ED_pd_tot_rsvPreF)
#colnames(CI_ED_pd_rsvPreF) <- c(age_cats, "total")
#CI_ED_pd_Combined <- cbind(apply(ED_pd_Combined, 2, CI_func), CI_ED_pd_tot_Combined)
#colnames(CI_ED_pd_Combined) <- c(age_cats, "total")

# total RSV-LRTI Hospitalizations percent decrease from status quo
CI_Hosp_pd_tot_mAb <- CI_func(hosps_pd_tot_mAb)
CI_Hosp_pd_tot_rsvPreF <- CI_func(hosps_pd_tot_rsvPreF)
CI_Hosp_pd_tot_Combined <- CI_func(hosps_pd_tot_Combined)

# RSV-LRTI Hospitalizations percent decrease from status quo, by age bin
CI_Hosp_pd_mAb <- cbind(apply(hosps_pd_mAb, 2, CI_func), CI_Hosp_pd_tot_mAb)
colnames(CI_Hosp_pd_mAb) <- c(age_cats, "total")
CI_Hosp_pd_rsvPreF <- cbind(apply(hosps_pd_rsvPreF, 2, CI_func), CI_Hosp_pd_tot_rsvPreF)
colnames(CI_Hosp_pd_rsvPreF) <- c(age_cats, "total")
CI_Hosp_pd_Combined <- cbind(apply(hosps_pd_Combined, 2, CI_func), CI_Hosp_pd_tot_Combined)
colnames(CI_Hosp_pd_Combined) <- c(age_cats, "total")

# total RSV-LRTI Deaths percent decrease from status quo
#CI_Deaths_pd_tot_mAb <- CI_func(deaths_pd_tot_mAb)
#CI_Deaths_pd_tot_rsvPreF <- CI_func(deaths_pd_tot_rsvPreF)
#CI_Deaths_pd_tot_Combined <- CI_func(deaths_pd_tot_Combined)
                                  
# RSV-LRTI Deaths percent decrease from status quo, by age bin
#CI_Deaths_pd_mAb <- cbind(apply(deaths_pd_mAb, 2, CI_func), CI_Deaths_pd_tot_mAb)
#colnames(CI_Deaths_pd_mAb) <- c(age_cats, "total")
#CI_Deaths_pd_rsvPreF <- cbind(apply(deaths_pd_rsvPreF, 2, CI_func), CI_Deaths_pd_tot_rsvPreF)
#colnames(CI_Deaths_pd_rsvPreF) <- c(age_cats, "total")
#CI_Deaths_pd_Combined <- cbind(apply(deaths_pd_Combined, 2, CI_func), CI_Deaths_pd_tot_Combined)
#colnames(CI_Deaths_pd_Combined) <- c(age_cats, "total")

# RSV-LRTI attributable asthma at 6 years
CI_att_no_u <- CI_func(att_no_u)
CI_att_mAb_u <- CI_func(att_mAb_u)
CI_att_rsvPreF_u <- CI_func(att_rsvPreF_u)
CI_att_Combined_u <- CI_func(att_Combined_u)

CI_att_df <- cbind(CI_att_no_u, CI_att_mAb_u, CI_att_rsvPreF_u, CI_att_Combined_u)
colnames(CI_att_df) <- c("no intervention", "mAb", "rsvPreF", "Combined")

# RSV-LRTI attributable asthma per 10,000 at 6 years
CI_att_no_pr_u <- CI_func(att_no_pr_u)
CI_att_mAb_pr_u <- CI_func(att_mAb_pr_u)
CI_att_rsvPreF_pr_u <- CI_func(att_rsvPreF_pr_u)
CI_att_Combined_pr_u <- CI_func(att_Combined_pr_u)

CI_att_pr_df <- cbind(CI_att_no_pr_u, CI_att_mAb_pr_u, CI_att_rsvPreF_pr_u, CI_att_Combined_pr_u)
colnames(CI_att_pr_df) <- c("no intervention", "mAb", "rsvPreF", "Combined")

# RSV-LRTI attributable asthma percent decrease from status quo
CI_att_mAb_pd_u <- CI_func(att_mAb_pd_u)
CI_att_rsvPreF_pd_u <- CI_func(att_rsvPreF_pd_u)
CI_att_Combined_pd_u <- CI_func(att_Combined_pd_u)

CI_att_pd_df <- cbind(CI_att_mAb_pd_u, CI_att_rsvPreF_pd_u, CI_att_Combined_pd_u)
colnames(CI_att_pd_df) <- c("mAb", "rsvPreF", "Combined")

# total with asthma
CI_tot_asth_no_u <- CI_func(tot_asth_no_u)
CI_tot_asth_mAb_u <- CI_func(tot_asth_mAb_u)
CI_tot_asth_rsvPreF_u <- CI_func(tot_asth_rsvPreF_u)
CI_tot_asth_Combined_u <- CI_func(tot_asth_Combined_u)

CI_tot_asth_df <- cbind(CI_tot_asth_no_u, CI_tot_asth_mAb_u, CI_tot_asth_rsvPreF_u, CI_tot_asth_Combined_u)
colnames(CI_tot_asth_df) <- c("no intervention", "mAb", "rsvPreF", "Combined")

# total asthma per 10,000 population
CI_tot_asth_no_pr_u <- CI_func(tot_asth_no_pr_u)
CI_tot_asth_mAb_pr_u <- CI_func(tot_asth_mAb_pr_u)
CI_tot_asth_rsvPreF_pr_u <- CI_func(tot_asth_rsvPreF_pr_u)
CI_tot_asth_Combined_pr_u <- CI_func(tot_asth_Combined_pr_u)

CI_tot_asth_pr_df <- cbind(CI_tot_asth_no_pr_u, CI_tot_asth_mAb_pr_u, CI_tot_asth_rsvPreF_pr_u, CI_tot_asth_Combined_pr_u)
colnames(CI_tot_asth_pr_df) <- c("no intervention", "mAb", "rsvPreF", "Combined")

# total ashtma percent decrease from status quo
CI_tot_asth_mAb_pd_u <- CI_func(tot_asth_mAb_pd_u)
CI_tot_asth_rsvPreF_pd_u <- CI_func(tot_asth_rsvPreF_pd_u)
CI_tot_asth_Combined_pd_u <- CI_func(tot_asth_Combined_pd_u)

CI_tot_asth_pd_df <- cbind(CI_tot_asth_mAb_pd_u, CI_tot_asth_rsvPreF_pd_u, CI_tot_asth_Combined_pd_u)
colnames(CI_tot_asth_pd_df) <- c("mAb", "rsvPreF", "Combined")

# Total recurrent wheeze/ asthma if all RSV-LRTI were prevented
CI_all_rsv_prev_u <- CI_func(all_rsv_prev_u)
CI_all_rsv_prev_pr_u <- CI_func(all_rsv_prev_pr_u)
CI_all_rsv_prev_pd_u <- CI_func(all_rsv_prev_pd_u)

CI_all_rsv_df <- cbind(CI_all_rsv_prev_u, CI_all_rsv_prev_pr_u, CI_all_rsv_prev_pd_u)
colnames(CI_all_rsv_df) <- c("cases at 6 yrs", "prevalence per 10,000 at 6 yrs", "percent decrease from status quo")
