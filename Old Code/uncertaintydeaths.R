#Uncertainty Deaths
source("ImportHuttonData.R")
source("PARAMS_asthma.R")
source("asthmafunctionsMali.R")
#ADOPTED FROM Hospitalizations
# Transform data structure to be rows = trials, columns = age in months
unique_ages <- unique(Deaths_u_df$Age)
Deaths_age_no <- Deaths_no_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(Deaths_age_no) <- paste0(names(Deaths_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- Deaths_no_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  Deaths_age_no <- cbind(Deaths_age_no, 
                        temp)
}

Deaths_age_mAb <- Deaths_mAb_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(Deaths_age_mAb) <- paste0(names(Deaths_age_mAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- Deaths_mAb_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  Deaths_age_mAb <- cbind(Deaths_age_mAb, 
                         temp2)
}

Deaths_age_rsvPreF <- Deaths_rsvPreF_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(Deaths_age_rsvPreF) <- paste0(names(Deaths_age_rsvPreF), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- Deaths_rsvPreF_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  Deaths_age_rsvPreF <- cbind(Deaths_age_rsvPreF, 
                             temp3)
}

Deaths_age_Combined <- Deaths_Combined_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(Deaths_age_Combined) <- paste0(names(Deaths_age_Combined), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- Deaths_Combined_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  Deaths_age_Combined <- cbind(Deaths_age_Combined, 
                              temp3)
}

# Bin to age categories
age_cats <- c("0-<6", "6-<12")

Deaths_no_agebin <- cbind(rowSums(Deaths_age_no[, 1:6]), rowSums(Deaths_age_no[, 7:12]))
colnames(Deaths_no_agebin) <- age_cats

Deaths_mAb_agebin <- cbind(rowSums(Deaths_age_mAb[, 1:6]), rowSums(Deaths_age_mAb[, 7:12]))
colnames(Deaths_mAb_agebin) <- age_cats

Deaths_rsvPreF_agebin <- cbind(rowSums(Deaths_age_rsvPreF[, 1:6]), rowSums(Deaths_age_rsvPreF[, 7:12]))
colnames(Deaths_rsvPreF_agebin) <- age_cats

Deaths_Combined_agebin <- cbind(rowSums(Deaths_age_Combined[, 1:6]), rowSums(Deaths_age_Combined[, 7:12]))
colnames(Deaths_Combined_agebin) <- age_cats

# total Deaths
Deaths_tot_no <- rowSums(Deaths_age_no)
Deaths_tot_mAb <- rowSums(Deaths_age_mAb)
Deaths_tot_rsvPreF <- rowSums(Deaths_age_rsvPreF)
Deaths_tot_Combined <- rowSums(Deaths_age_Combined)

# total Deaths percent decrease from status quo
Deaths_pd_tot_mAb <- (Deaths_tot_no - Deaths_tot_mAb) / Deaths_tot_no * 100
Deaths_pd_tot_rsvPreF <- (Deaths_tot_no - Deaths_tot_rsvPreF) / Deaths_tot_no * 100
Deaths_pd_tot_Combined <- (Deaths_tot_no - Deaths_tot_Combined) / Deaths_tot_no * 100

# Deaths percent decrease from status quo, with age bins
Deaths_pd_mAb <- (Deaths_no_agebin - Deaths_mAb_agebin) / Deaths_no_agebin * 100
Deaths_pd_rsvPreF <- (Deaths_no_agebin - Deaths_rsvPreF_agebin) / Deaths_no_agebin * 100
Deaths_pd_Combined <- (Deaths_no_agebin - Deaths_Combined_agebin) / Deaths_no_agebin * 100

## point estimate calculations

sum(Deaths_no_df[1:6,4])
sum(Deaths_no_df[7:12,4])

sum(Deaths_mAb_df[1:6,4])
sum(Deaths_mAb_df[7:12,4])

sum(Deaths_rsvPreF_df[1:6,4])
sum(Deaths_rsvPreF_df[7:12,4])

sum(Deaths_Combined_df[1:6,4])
sum(Deaths_Combined_df[7:12,4])
