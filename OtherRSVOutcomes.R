#Other RSV-LRTI associated illness

#Delinating into 6mo/12mo intervals for outcome purposes
Hosps_no_05_df <- rowSums(hosps_no_u_df[3:8])
Hosps_mAb_05_df <- rowSums(hosps_mAb_u_df[,3:8])
Hosps_rsvPreF_05_df <- rowSums(hosps_rsvPreF_u_df[,3:8])

Hosps_no_611_df <- rowSums(hosps_no_u_df[9:14])
Hosps_mAb_611_df <- rowSums(hosps_mAb_u_df[,9:14])
Hosps_rsvPreF_611_df <- rowSums(hosps_rsvPreF_u_df[,9:14])

#Differences in trials
mAb_dif_05 <- Hosps_no_05_df - Hosps_mAb_05_df
rsvPreF_dif_05 <- Hosps_no_05_df - Hosps_rsvPreF_05_df 

mAb_dif_611 <- Hosps_no_611_df - Hosps_mAb_611_df
rsvPreF_dif_611 <- Hosps_no_611_df - Hosps_rsvPreF_611_df 

#Pneumonia/OM/Abx increase mAb
pnincrease_mAb <- mean(rr_pn05_u*mAb_dif_05+rr_pn611_u*mAb_dif_611)
OMincrease_mAb <- mean(rr_OM05_u*mAb_dif_05+rr_OM611_u*mAb_dif_611)
abincrease_mAb <- mean(rr_ab05_u*mAb_dif_05+rr_ab611_u*mAb_dif_611)

#Pneumonia/OM/Abx increase mAb
pnincrease_rsvPreF <- mean(rr_pn05_u*rsvPreF_dif_05+rr_pn611_u*rsvPreF_dif_611)
OMincrease_rsvPreF <- mean(rr_OM05_u*rsvPreF_dif_05+rr_OM611_u*rsvPreF_dif_611)
abincrease_rsvPreF <- mean(rr_ab05_u*rsvPreF_dif_05+rr_ab611_u*rsvPreF_dif_611)
