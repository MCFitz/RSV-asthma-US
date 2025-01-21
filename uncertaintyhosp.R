# USA Asthma Project: hospitalizations - adopted from Mali

################################################################################

hdatahosp <- read.csv("healthoutcomeshosp.csv")

# Extract just hospitalizations
hosps_u_df <- hdatahosp%>%filter(Metric == "Hospitalizations")

hosps_no_u_df <- hosps_u_df%>%filter(Intervention == "no intervention")
hosps_mAb_u_df <- hosps_u_df%>%filter(Intervention == "Nirsevimab")
hosps_rsvPreF_u_df <- hosps_u_df%>%filter(Intervention == "RSVpreF")
hosps_Combined_u_df <- hosps_u_df%>%filter(Intervention == "Combined")

# Transform data structure to be rows = trials, columns = age in months
unique_ages <- unique(hosps_u_df$Age)

hosps_age_no <- hosps_no_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_no) <- paste0(names(hosps_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- hosps_no_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  hosps_age_no <- cbind(hosps_age_no, 
                        temp)
}


hosps_age_mAb <- hosps_mAb_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_mAb) <- paste0(names(hosps_age_mAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- hosps_mAb_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  hosps_age_mAb <- cbind(hosps_age_mAb, 
                          temp2)
}


hosps_age_rsvPreF <- hosps_rsvPreF_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_rsvPreF) <- paste0(names(hosps_age_rsvPreF), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- hosps_rsvPreF_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  hosps_age_rsvPreF <- cbind(hosps_age_rsvPreF, 
                               temp3)
}

hosps_age_Combined <- hosps_Combined_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(hosps_age_Combined) <- paste0(names(hosps_age_Combined), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- hosps_Combined_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  hosps_age_Combined <- cbind(hosps_age_Combined, 
                             temp3)
}

# Bin to age categories
age_cats <- c("0-<6", "6-<12")

hosps_no_agebin <- cbind(rowSums(hosps_age_no[, 1:6]), rowSums(hosps_age_no[, 7:12]))
colnames(hosps_no_agebin) <- age_cats

hosps_mAb_agebin <- cbind(rowSums(hosps_age_mAb[, 1:6]), rowSums(hosps_age_mAb[, 7:12]))
colnames(hosps_mAb_agebin) <- age_cats

hosps_rsvPreF_agebin <- cbind(rowSums(hosps_age_rsvPreF[, 1:6]), rowSums(hosps_age_rsvPreF[, 7:12]))
colnames(hosps_rsvPreF_agebin) <- age_cats

hosps_Combined_agebin <- cbind(rowSums(hosps_age_Combined[, 1:6]), rowSums(hosps_age_Combined[, 7:12]))
colnames(hosps_Combined_agebin) <- age_cats

# total hosps
hosps_tot_no <- rowSums(hosps_age_no)
hosps_tot_mAb <- rowSums(hosps_age_mAb)
hosps_tot_rsvPreF <- rowSums(hosps_age_rsvPreF)
hosps_tot_Combined <- rowSums(hosps_age_Combined)

# total hosps percent decrease from status quo
hosps_pd_tot_mAb <- (hosps_tot_no - hosps_tot_mAb) / hosps_tot_no * 100
hosps_pd_tot_rsvPreF <- (hosps_tot_no - hosps_tot_rsvPreF) / hosps_tot_no * 100
hosps_pd_tot_Combined <- (hosps_tot_no - hosps_tot_Combined) / hosps_tot_no * 100

# hosps percent decrease from status quo, with age bins
hosps_pd_mAb <- (hosps_no_agebin - hosps_mAb_agebin) / hosps_no_agebin * 100
hosps_pd_rsvPreF <- (hosps_no_agebin - hosps_rsvPreF_agebin) / hosps_no_agebin * 100
hosps_pd_Combined <- (hosps_no_agebin - hosps_Combined_agebin) / hosps_no_agebin * 100

## point estimate calculations
hosps_df <- hdata%>%filter(Metric == "Hospitalizations")
hosps_no_df <- hosps_df%>%filter(Intervention == "no intervention")
hosps_mAb_df <- hosps_df%>%filter(Intervention == "Nirsevimab")
hosps_rsvPreF_df <- hosps_df%>%filter(Intervention == "RSVpreF")
hosps_Combined_df <- hosps_df%>%filter(Intervention == "Combined")

sum(hosps_no_df[1:6,4])
sum(hosps_no_df[7:12,4])

sum(hosps_mAb_df[1:6,4])
sum(hosps_mAb_df[7:12,4])

sum(hosps_rsvPreF_df[1:6,4])
sum(hosps_rsvPreF_df[7:12,4])

sum(hosps_Combined_df[1:6,4])
sum(hosps_Combined_df[7:12,4])
