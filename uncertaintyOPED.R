#Uncertainty ED/OP

# NEEDS TO BE ADOPTED FROM Hospitalizations

hdata <- read.csv("healthoutcomestest.csv")

# Extract just OP and ED
OP_u_df <- hdata%>%filter(Metric == "Outpatient")
ED_u_df <- hdata%>%filter(Metric == "ED")

OP_no_u_df <- OP_u_df%>%filter(Intervention == "no intervention")
OP_mAb_u_df <- OP_u_df%>%filter(Intervention == "Nirsevimab")
OP_rsvPreF_u_df <- OP_u_df%>%filter(Intervention == "RSVpreF")
OP_Combined_u_df <- OP_u_df%>%filter(Intervention == "Combined")

ED_no_u_df <- ED_u_df%>%filter(Intervention == "no intervention")
ED_mAb_u_df <- ED_u_df%>%filter(Intervention == "Nirsevimab")
ED_rsvPreF_u_df <- ED_u_df%>%filter(Intervention == "RSVpreF")
ED_Combined_u_df <- ED_u_df%>%filter(Intervention == "Combined")

# Transform data structure to be rows = trials, columns = age in months
unique_ages <- unique(OP_u_df$Age)

OP_age_no <- OP_no_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(OP_age_no) <- paste0(names(OP_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- OP_no_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  OP_age_no <- cbind(OP_age_no, 
                         temp)
}

OP_age_mAb <- OP_mAb_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(OP_age_mAb) <- paste0(names(OP_age_mAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- OP_mAb_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  OP_age_mAb <- cbind(OP_age_mAb, 
                          temp2)
}


OP_age_rsvPreF <- OP_rsvPreF_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(OP_age_rsvPreF) <- paste0(names(OP_age_rsvPreF), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- OP_rsvPreF_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  OP_age_rsvPreF <- cbind(OP_age_rsvPreF, 
                              temp3)
}

OP_age_Combined <- OP_Combined_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(OP_age_Combined) <- paste0(names(OP_age_Combined), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- OP_Combined_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  OP_age_Combined <- cbind(OP_age_Combined, 
                               temp3)
}

ED_age_no <- ED_no_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(ED_age_no) <- paste0(names(ED_age_no), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp <- ED_no_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp) <- paste0(names(temp), "_age", unique_ages[idx])
  ED_age_no <- cbind(ED_age_no, 
                     temp)
}

ED_age_mAb <- ED_mAb_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(ED_age_mAb) <- paste0(names(ED_age_mAb), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp2 <- OP_mAb_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp2) <- paste0(names(temp2), "_age", unique_ages[idx])
  ED_age_mAb <- cbind(ED_age_mAb, 
                      temp2)
}


ED_age_rsvPreF <- ED_rsvPreF_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(ED_age_rsvPreF) <- paste0(names(ED_age_rsvPreF), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- ED_rsvPreF_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  ED_age_rsvPreF <- cbind(ED_age_rsvPreF, 
                          temp3)
}

ED_age_Combined <- ED_Combined_u_df %>% 
  filter(Age == unique_ages[1]) %>% 
  select(value)
names(ED_age_Combined) <- paste0(names(ED_age_Combined), "_age", unique_ages[1])
for (idx in 2:length(unique_ages)) {
  temp3 <- ED_Combined_u_df %>% 
    filter(Age == unique_ages[idx]) %>% 
    select(value)
  names(temp3) <- paste0(names(temp3), "_age", unique_ages[idx])
  ED_age_Combined <- cbind(ED_age_Combined, 
                           temp3)
}

# Bin to age categories
age_cats <- c("0-<6", "6-<12")

OP_no_agebin <- cbind(rowSums(OP_age_no[, 1:6]), rowSums(OP_age_no[, 7:12]))
colnames(OP_no_agebin) <- age_cats

OP_mAb_agebin <- cbind(rowSums(OP_age_mAb[, 1:6]), rowSums(OP_age_mAb[, 7:12]))
colnames(OP_mAb_agebin) <- age_cats

OP_rsvPreF_agebin <- cbind(rowSums(OP_age_rsvPreF[, 1:6]), rowSums(OP_age_rsvPreF[, 7:12]))
colnames(OP_rsvPreF_agebin) <- age_cats

OP_Combined_agebin <- cbind(rowSums(OP_age_Combined[, 1:6]), rowSums(OP_age_Combined[, 7:12]))
colnames(OP_Combined_agebin) <- age_cats

ED_no_agebin <- cbind(rowSums(ED_age_no[, 1:6]), rowSums(ED_age_no[, 7:12]))
colnames(ED_no_agebin) <- age_cats

ED_mAb_agebin <- cbind(rowSums(ED_age_mAb[, 1:6]), rowSums(ED_age_mAb[, 7:12]))
colnames(ED_mAb_agebin) <- age_cats

ED_rsvPreF_agebin <- cbind(rowSums(ED_age_rsvPreF[, 1:6]), rowSums(ED_age_rsvPreF[, 7:12]))
colnames(Deaths_rsvPreF_agebin) <- age_cats

ED_Combined_agebin <- cbind(rowSums(ED_age_Combined[, 1:6]), rowSums(ED_age_Combined[, 7:12]))
colnames(ED_Combined_agebin) <- age_cats

# total OP/ED visits
OP_tot_no <- rowSums(OP_age_no)
OP_tot_mAb <- rowSums(OP_age_mAb)
OP_tot_rsvPreF <- rowSums(OP_age_rsvPreF)
OP_tot_Combined <- rowSums(OP_age_Combined)

ED_tot_no <- rowSums(ED_age_no)
ED_tot_mAb <- rowSums(ED_age_mAb)
ED_tot_rsvPreF <- rowSums(ED_age_rsvPreF)
ED_tot_Combined <- rowSums(ED_age_Combined)

# total OP/ED visits percent decrease from status quo
OP_pd_tot_mAb <- (OP_tot_no - OP_tot_mAb) / OP_tot_no * 100
OP_pd_tot_rsvPreF <- (OP_tot_no - OP_tot_rsvPreF) / OP_tot_no * 100
OP_pd_tot_Combined <- (OP_tot_no - OP_tot_Combined) / OP_tot_no * 100

ED_pd_tot_mAb <- (ED_tot_no - ED_tot_mAb) / ED_tot_no * 100
ED_pd_tot_rsvPreF <- (ED_tot_no - ED_tot_rsvPreF) / ED_tot_no * 100
ED_pd_tot_Combined <- (ED_tot_no - ED_tot_Combined) / ED_tot_no * 100

# OP/ED visits percent decrease from status quo, with age bins
OP_pd_mAb <- (OP_no_agebin - OP_mAb_agebin) / OP_no_agebin * 100
OP_pd_rsvPreF <- (OP_no_agebin -OP_rsvPreF_agebin) / OP_no_agebin * 100
OP_pd_Combined <- (OP_no_agebin - OP_Combined_agebin) / OP_no_agebin * 100

ED_pd_mAb <- (ED_no_agebin - ED_mAb_agebin) / ED_no_agebin * 100
ED_pd_rsvPreF <- (ED_no_agebin - ED_rsvPreF_agebin) / ED_no_agebin * 100
ED_pd_Combined <- (ED_no_agebin - ED_Combined_agebin) / ED_no_agebin * 100

## point estimate calculations
OP_df <- hdata%>%filter(Metric == "Outpatient")
OP_no_df <- OP_df%>%filter(Intervention == "no intervention")
OP_mAb_df <- OP_df%>%filter(Intervention == "Nirsevimab")
OP_rsvPreF_df <- OP_df%>%filter(Intervention == "RSVpreF")
OP_Combined_df <- OP_df%>%filter(Intervention == "Combined")

ED_df <- hdata%>%filter(Metric == "ED")
ED_no_df <- ED_df%>%filter(Intervention == "no intervention")
ED_mAb_df <- ED_df%>%filter(Intervention == "Nirsevimab")
ED_rsvPreF_df <- ED_df%>%filter(Intervention == "RSVpreF")
ED_Combined_df <- ED_df%>%filter(Intervention == "Combined")

sum(OP_no_df[1:6,4])
sum(OP_no_df[7:12,4])

sum(ED_no_df[1:6,4])
sum(ED_no_df[7:12,4])

sum(OP_mAb_df[1:6,4])
sum(OP_mAb_df[7:12,4])

sum(ED_mAb_df[1:6,4])
sum(ED_mAb_df[7:12,4])

sum(OP_rsvPreF_df[1:6,4])
sum(OP_rsvPreF_df[7:12,4])

sum(ED_rsvPreF_df[1:6,4])
sum(ED_rsvPreF_df[7:12,4])

sum(OP_Combined_df[1:6,4])
sum(OP_Combined_df[7:12,4])

sum(ED_Combined_df[1:6,4])
sum(ED_Combined_df[7:12,4])

#Combining data (ED + OP)
OPED_age_no <- ED_age_no + OP_age_no
OPED_age_mAb <- ED_age_mAb + OP_age_mAb
OPED_age_rsvPreF <- ED_age_rsvPreF + OP_age_rsvPreF
OPED_age_Combined <- ED_age_Combined + OP_age_Combined
