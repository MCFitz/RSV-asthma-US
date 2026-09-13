# Created by Meagan Fitzpatrick and Ian Galbreath

# -----------------------------
# Creating RSV incidence DF
# -----------------------------

library(tidyverse)
library(readxl)

# Outpatient/ED/Hospitalization RSV-LRTI outcomes and CI w/ corresponding PE - 10,000 samples, original waning curve
hdataCI <- read_excel("healthoutcomesnewsamples.xlsx")

# Uncertainty DFs
# -----------------------------
# OP
OP_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Outpatient")
OP_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Outpatient")
OP_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Outpatient")
#Summing rows
OP_no_u_1st <- rowSums(OP_no_u_df[3:14])
OP_mAb_u_1st <- rowSums(OP_mAb_u_df[,3:14])
OP_rsvPreF_u_1st <- rowSums(OP_rsvPreF_u_df[,3:14])

# ED
ED_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Emergency Department")
ED_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Emergency Department")
ED_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Emergency Department")
#Summing rows
ED_no_u_1st <- rowSums(ED_no_u_df[3:14])
ED_mAb_u_1st <- rowSums(ED_mAb_u_df[,3:14])
ED_rsvPreF_u_1st <- rowSums(ED_rsvPreF_u_df[,3:14])

# hospitalizations
hosps_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Hospitalizations")
hosps_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Hospitalizations")
hosps_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Hospitalizations")
#Summing rows
Hosps_no_u_1st <- rowSums(hosps_no_u_df[3:14])
Hosps_mAb_u_1st <- rowSums(hosps_mAb_u_df[,3:14])
Hosps_rsvPreF_u_1st <- rowSums(hosps_rsvPreF_u_df[,3:14])

# Point estimate DF
# -----------------------------
OP_no_df <- hdataCI%>%filter(Intervention == "Natural History Outpatient PE")
OP_mAb_df <- hdataCI%>%filter(Intervention == "Nirsevimab Outpatient PE")
OP_rsvPreF_df <- hdataCI%>%filter(Intervention == "RSVpreF Outpatient PE")
ED_no_df <- hdataCI%>%filter(Intervention == "Natural History Emergency Department PE")
ED_mAb_df <- hdataCI%>%filter(Intervention == "Nirsevimab Emergency Department PE")
ED_rsvPreF_df <- hdataCI%>%filter(Intervention == "RSVpreF Emergency Department PE")
hosps_no_df <- hdataCI%>%filter(Intervention == "Natural History Hospitalizations PE")
hosps_mAb_df <- hdataCI%>%filter(Intervention == "Nirsevimab Hospitalizations PE")
hosps_rsvPreF_df <- hdataCI%>%filter(Intervention == "RSVpreF Hospitalizations PE")

#converting PE data yearly data
OP_no_PE_1st <- rowSums(OP_no_df[3:14])
OP_mAb_PE_1st <- rowSums(OP_mAb_df[,3:14])
OP_rsvPreF_PE_1st <- rowSums(OP_rsvPreF_df[,3:14])

ED_no_PE_1st <- rowSums(ED_no_df[3:14])
ED_mAb_PE_1st <- rowSums(ED_mAb_df[,3:14])
ED_rsvPreF_PE_1st <- rowSums(ED_rsvPreF_df[,3:14])

Hosps_no_PE_1st <- rowSums(hosps_no_df[3:14])
Hosps_mAb_PE_1st <- rowSums(hosps_mAb_df[,3:14])
Hosps_rsvPreF_PE_1st <- rowSums(hosps_rsvPreF_df[,3:14])


##----------------------------------------------------------
# Outpatient/ED/Hospitalization RSV-LRTI outcomes and CI w/ corresponding PE - 10,000 samples, adjusted waning curve
##----------------------------------------------------------
hdataCI_adjc <- read_excel("healthoutcomesnewcurve.xlsx")

# Uncertainty DFs
# -----------------------------
# OP
OP_no_u_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Natural History Outpatient")
OP_mAb_u_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Nirsevimab Outpatient")
#Summing rows OP
OP_no_u_adjc <- rowSums(OP_no_u_df_adjc[3:14])
OP_mAb_u_adjc <- rowSums(OP_mAb_u_df_adjc[,3:14])

# ED
ED_no_u_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Natural History Emergency Department")
ED_mAb_u_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Nirsevimab Emergency Department")
#Summing rows
ED_no_u_adjc <- rowSums(ED_no_u_df_adjc[3:14])
ED_mAb_u_adjc <- rowSums(ED_mAb_u_df_adjc[,3:14])

# hospitalizations
hosps_no_u_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Natural History Hospitalizations")
hosps_mAb_u_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Nirsevimab Hospitalizations")
#Summing rows 
Hosps_no_u_adjc <- rowSums(hosps_no_u_df_adjc[3:14])
Hosps_mAb_u_adjc <- rowSums(hosps_mAb_u_df_adjc[,3:14])

# Point estimate DF
# -----------------------------
OP_no_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Natural History Outpatient PE")
OP_mAb_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Nirsevimab Outpatient PE")
ED_no_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Natural History Emergency Department PE")
ED_mAb_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Nirsevimab Emergency Department PE")
hosps_no_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Natural History Hospitalizations PE")
hosps_mAb_df_adjc <- hdataCI_adjc%>%filter(Intervention == "Nirsevimab Hospitalizations PE")

#converting PE data yearly data
OP_no_PE_adjc <- rowSums(OP_no_df_adjc[3:14])
OP_mAb_PE_adjc <- rowSums(OP_mAb_df_adjc[,3:14])
ED_no_PE_adjc <- rowSums(ED_no_df_adjc[3:14])
ED_mAb_PE_adjc <- rowSums(ED_mAb_df_adjc[,3:14])
Hosps_no_PE_adjc <- rowSums(hosps_no_df_adjc[3:14])
Hosps_mAb_PE_adjc <- rowSums(hosps_mAb_df_adjc[,3:14])

##-------------------------------------
## Outpatient/ED/Hospitalization RSV-LRTI outcomes and CI for 1 year w/ corresponding PE - 10,000 samples, normal waning curve, 90% coverage estimate
##-------------------------------------
hdataCI_adjcov <- read_excel("healthoutcomesnewcov.xlsx")

# Uncertainty DFs
# -----------------------------
# OP
OP_no_u_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Natural History Outpatient")
OP_mAb_u_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Nirsevimab Outpatient")
#Summing rows
OP_no_u_adjcov <- rowSums(OP_no_u_df_adjcov[3:14])
OP_mAb_u_adjcov <- rowSums(OP_mAb_u_df_adjcov[,3:14])

# ED
ED_no_u_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Natural History Emergency Department")
ED_mAb_u_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Nirsevimab Emergency Department")
#Summing row
ED_no_u_adjcov <- rowSums(ED_no_u_df_adjcov[3:14])
ED_mAb_u_adjcov <- rowSums(ED_mAb_u_df_adjcov[,3:14])

# hospitalizations
hosps_no_u_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Natural History Hospitalizations")
hosps_mAb_u_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Nirsevimab Hospitalizations")
#Summing rows
Hosps_no_u_adjcov <- rowSums(hosps_no_u_df_adjcov[3:14])
Hosps_mAb_u_adjcov <- rowSums(hosps_mAb_u_df_adjcov[,3:14])

# Point estimate DF
# -----------------------------
OP_no_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Natural History Outpatient PE")
OP_mAb_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Nirsevimab Outpatient PE")
ED_no_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Natural History Emergency Department PE")
ED_mAb_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Nirsevimab Emergency Department PE")
hosps_no_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Natural History Hospitalizations PE")
hosps_mAb_df_adjcov <- hdataCI_adjcov%>%filter(Intervention == "Nirsevimab Hospitalizations PE")

#converting PE data yearly data
OP_no_PE_adjcov <- rowSums(OP_no_df_adjcov[3:14])
OP_mAb_PE_adjcov <- rowSums(OP_mAb_df_adjcov[,3:14])
ED_no_PE_adjcov <- rowSums(ED_no_df_adjcov[3:14])
ED_mAb_PE_adjcov <- rowSums(ED_mAb_df_adjcov[,3:14])
Hosps_no_PE_adjcov <- rowSums(hosps_no_df_adjcov[3:14])
Hosps_mAb_PE_adjcov <- rowSums(hosps_mAb_df_adjcov[,3:14])

# OLD ANALYSIS - Outpatient/ED/Hospitalization RSV-LRTI outcomes and CI w/ corresponding PE - 1000 samples, original waning curve
hdataCI_old <- read_excel("healthoutcomescomplete.xlsx")

# Uncertainty DFs
# -----------------------------
# OP
OP_no_u_old_df <- hdataCI_old%>%filter(Intervention == "Natural History Outpatient")
OP_mAb_u_old_df <- hdataCI_old%>%filter(Intervention == "Nirsevimab Outpatient")
OP_rsvPreF_u_old_df <- hdataCI_old%>%filter(Intervention == "RSVpreF Outpatient")
#Summing rows
OP_no_u_1st_old <- rowSums(OP_no_u_old_df[3:14])
OP_mAb_u_1st_old <- rowSums(OP_mAb_u_old_df[,3:14])
OP_rsvPreF_u_1st_old <- rowSums(OP_rsvPreF_u_old_df[,3:14])

# ED
ED_no_u_old_df <- hdataCI_old%>%filter(Intervention == "Natural History Emergency Department")
ED_mAb_u_old_df <- hdataCI_old%>%filter(Intervention == "Nirsevimab Emergency Department")
ED_rsvPreF_u_old_df <- hdataCI_old%>%filter(Intervention == "RSVpreF Emergency Department")
#Summing rows
ED_no_u_old_1st <- rowSums(ED_no_u_old_df[3:14])
ED_mAb_u_old_1st <- rowSums(ED_mAb_u_old_df[,3:14])
ED_rsvPreF_u_old_1st <- rowSums(ED_rsvPreF_u_old_df[,3:14])

# hospitalizations
hosps_no_u_old_df <- hdataCI_old%>%filter(Intervention == "Natural History Hospitalizations")
hosps_mAb_u_old_df <- hdataCI_old%>%filter(Intervention == "Nirsevimab Hospitalizations")
hosps_rsvPreF_u_old_df <- hdataCI_old%>%filter(Intervention == "RSVpreF Hospitalizations")
#Summing rows
Hosps_no_u_old_1st <- rowSums(hosps_no_u_old_df[3:14])
Hosps_mAb_u_old_1st <- rowSums(hosps_mAb_u_old_df[,3:14])
Hosps_rsvPreF_u_old_1st <- rowSums(hosps_rsvPreF_u_old_df[,3:14])

# Point estimate DF
# -----------------------------
OP_no_old_df <- hdataCI_old%>%filter(Intervention == "Natural History Outpatient PE")
OP_mAb_old_df <- hdataCI_old%>%filter(Intervention == "Nirsevimab Outpatient PE")
OP_rsvPreF_old_df <- hdataCI_old%>%filter(Intervention == "RSVpreF Outpatient PE")
ED_no_old_df <- hdataCI_old%>%filter(Intervention == "Natural History Emergency Department PE")
ED_mAb_old_df <- hdataCI_old%>%filter(Intervention == "Nirsevimab Emergency Department PE")
ED_rsvPreF_old_df <- hdataCI_old%>%filter(Intervention == "RSVpreF Emergency Department PE")
hosps_no_old_df <- hdataCI_old%>%filter(Intervention == "Natural History Hospitalizations PE")
hosps_mAb_old_df <- hdataCI_old%>%filter(Intervention == "Nirsevimab Hospitalizations PE")
hosps_rsvPreF_old_df <- hdataCI_old%>%filter(Intervention == "RSVpreF Hospitalizations PE")

#converting PE data yearly data
OP_no_PE_old_1st <- rowSums(OP_no_old_df[3:14])
OP_mAb_PE_old_1st <- rowSums(OP_mAb_old_df[,3:14])
OP_rsvPreF_PE_old_1st <- rowSums(OP_rsvPreF_old_df[,3:14])

ED_no_PE_old_1st <- rowSums(ED_no_old_df[3:14])
ED_mAb_PE_old_1st <- rowSums(ED_mAb_old_df[,3:14])
ED_rsvPreF_PE_old_1st <- rowSums(ED_rsvPreF_old_df[,3:14])

Hosps_no_PE_old_1st <- rowSums(hosps_no_old_df[3:14])
Hosps_mAb_PE_old_1st <- rowSums(hosps_mAb_old_df[,3:14])
Hosps_rsvPreF_PE_old_1st <- rowSums(hosps_rsvPreF_old_df[,3:14])


# -----------------------------
# CALCULATING TOTAL EVENTS AND PATIENT LEVEL DATA - ORIGINAL WANING CURVE AND 10000 SAMPLES
# -----------------------------
source("adjustmentformultipleepisode.R")

#Uncertainty
#----------------------
#Outpatient episodes 
quantile(OP_no_u_1st, probs = c(0.025, 0.975))
quantile(OP_mAb_u_1st, probs = c(0.025, 0.975))
quantile(OP_rsvPreF_u_1st, probs = c(0.025, 0.975))
#difference
quantile(OP_no_u_1st-OP_mAb_u_1st, probs = c(0.025, 0.975))
quantile(OP_no_u_1st-OP_rsvPreF_u_1st, probs = c(0.025, 0.975))
#% reduction
quantile((OP_no_u_1st-OP_mAb_u_1st)/OP_no_u_1st*100, probs = c(0.025, 0.975))
quantile((OP_no_u_1st-OP_rsvPreF_u_1st)/OP_no_u_1st*100, probs = c(0.025, 0.975))

#ED episodes
quantile(ED_no_u_1st, probs = c(0.025, 0.975))
quantile(ED_mAb_u_1st, probs = c(0.025, 0.975))
quantile(ED_rsvPreF_u_1st, probs = c(0.025, 0.975))
#difference
quantile(ED_no_u_1st-ED_mAb_u_1st, probs = c(0.025, 0.975))
quantile(ED_no_u_1st-ED_rsvPreF_u_1st, probs = c(0.025, 0.975))
#% reduction
quantile((ED_no_u_1st-ED_mAb_u_1st)/ED_no_u_1st*100, probs = c(0.025, 0.975))
quantile((ED_no_u_1st-ED_rsvPreF_u_1st)/ED_no_u_1st*100, probs = c(0.025, 0.975))

#Hosp episodes 
quantile(Hosps_no_u_1st, probs = c(0.025, 0.975))
quantile(Hosps_mAb_u_1st, probs = c(0.025, 0.975))
quantile(Hosps_rsvPreF_u_1st, probs = c(0.025, 0.975))
#difference
quantile(Hosps_no_u_1st-Hosps_mAb_u_1st, probs = c(0.025, 0.975))
quantile(Hosps_no_u_1st-Hosps_rsvPreF_u_1st, probs = c(0.025, 0.975))
#% reduction
quantile((Hosps_no_u_1st-Hosps_mAb_u_1st)/Hosps_no_u_1st*100, probs = c(0.025, 0.975))
quantile((Hosps_no_u_1st-Hosps_rsvPreF_u_1st)/Hosps_no_u_1st*100, probs = c(0.025, 0.975))

#total episodes per scenario
quantile(OP_no_u_1st+ED_no_u_1st+Hosps_no_u_1st, probs = c(0.025, 0.975))
quantile(OP_mAb_u_1st+ED_mAb_u_1st+Hosps_mAb_u_1st, probs = c(0.025, 0.975))
quantile(OP_rsvPreF_u_1st+ED_rsvPreF_u_1st+Hosps_rsvPreF_u_1st, probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_1st+ED_no_u_1st+Hosps_no_u_1st)-(OP_mAb_u_1st+ED_mAb_u_1st+Hosps_mAb_u_1st), probs = c(0.025, 0.975))
quantile((OP_no_u_1st+ED_no_u_1st+Hosps_no_u_1st)-(OP_rsvPreF_u_1st+ED_rsvPreF_u_1st+Hosps_rsvPreF_u_1st), probs = c(0.025, 0.975))
#% reduction
quantile(((OP_no_u_1st+ED_no_u_1st+Hosps_no_u_1st)-(OP_mAb_u_1st+ED_mAb_u_1st+Hosps_mAb_u_1st))/(OP_no_u_1st+ED_no_u_1st+Hosps_no_u_1st)*100, probs = c(0.025, 0.975))
quantile(((OP_no_u_1st+ED_no_u_1st+Hosps_no_u_1st)-(OP_rsvPreF_u_1st+ED_rsvPreF_u_1st+Hosps_rsvPreF_u_1st))/(OP_no_u_1st+ED_no_u_1st+Hosps_no_u_1st)*100, probs = c(0.025, 0.975))


#first year of life patient level events
quantile((OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a), probs = c(0.025, 0.975))
quantile((OP_mAb_u_1st_a+ED_mAb_u_1st_a+Hosps_mAb_u_1st_a), probs = c(0.025, 0.975))
quantile((OP_rsvPreF_u_1st_a+ED_rsvPreF_u_1st_a+Hosps_rsvPreF_u_1st_a), probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a)-(OP_mAb_u_1st_a+ED_mAb_u_1st_a+Hosps_mAb_u_1st_a), probs = c(0.025, 0.975))
quantile((OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a)-(OP_rsvPreF_u_1st_a+ED_rsvPreF_u_1st_a+Hosps_rsvPreF_u_1st_a), probs = c(0.025, 0.975))
#% reduction
quantile(((OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a)-(OP_mAb_u_1st_a+ED_mAb_u_1st_a+Hosps_mAb_u_1st_a))/(OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a)*100, probs = c(0.025, 0.975))
quantile(((OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a)-(OP_rsvPreF_u_1st_a+ED_rsvPreF_u_1st_a+Hosps_rsvPreF_u_1st_a))/(OP_no_u_1st_a+ED_no_u_1st_a+Hosps_no_u_1st_a)*100, probs = c(0.025, 0.975))

#Point estimate
#----------------------
#Outpatient episodes
OP_no_PE_1st
OP_mAb_PE_1st
OP_rsvPreF_PE_1st
#difference
OP_no_PE_1st-OP_mAb_PE_1st
OP_no_PE_1st-OP_rsvPreF_PE_1st
#%difference
(OP_no_PE_1st-OP_mAb_PE_1st)/OP_no_PE_1st*100
(OP_no_PE_1st-OP_rsvPreF_PE_1st)/OP_no_PE_1st*100

#ED episodes alone
ED_no_PE_1st
ED_mAb_PE_1st
ED_rsvPreF_PE_1st
#difference
ED_no_PE_1st-ED_mAb_PE_1st
ED_no_PE_1st-ED_rsvPreF_PE_1st
#% difference
(ED_no_PE_1st-ED_mAb_PE_1st)/ED_no_PE_1st*100
(ED_no_PE_1st-ED_rsvPreF_PE_1st)/ED_no_PE_1st*100

#Hosp episodes alone
Hosps_no_PE_1st
Hosps_mAb_PE_1st
Hosps_rsvPreF_PE_1st
#difference
Hosps_no_PE_1st-Hosps_mAb_PE_1st
Hosps_no_PE_1st-Hosps_rsvPreF_PE_1st
#% reduction
(Hosps_no_PE_1st-Hosps_mAb_PE_1st)/Hosps_no_PE_1st*100
(Hosps_no_PE_1st-Hosps_rsvPreF_PE_1st)/Hosps_no_PE_1st*100

#total episodes per year
OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st
OP_mAb_PE_1st+ED_mAb_PE_1st+Hosps_mAb_PE_1st
OP_rsvPreF_PE_1st+ED_rsvPreF_PE_1st+Hosps_rsvPreF_PE_1st
#difference
(OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)-(OP_mAb_PE_1st+ED_mAb_PE_1st+Hosps_mAb_PE_1st)
(OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)-(OP_rsvPreF_PE_1st+ED_rsvPreF_PE_1st+Hosps_rsvPreF_PE_1st)
#% reduction
((OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)-(OP_mAb_PE_1st+ED_mAb_PE_1st+Hosps_mAb_PE_1st))/(OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)*100
((OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)-(OP_rsvPreF_PE_1st+ED_rsvPreF_PE_1st+Hosps_rsvPreF_PE_1st))/(OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)*100


#first/second year of life totals patient level events w/ combined totals - adjusted
OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a
OP_rsvPreF_PE_1st_a+ED_rsvPreF_PE_1st_a+Hosps_rsvPreF_PE_1st_a
#difference
(OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)-(OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a)
(OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)-(OP_rsvPreF_PE_1st_a+ED_rsvPreF_PE_1st_a+Hosps_rsvPreF_PE_1st_a)
#% reduction
((OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)-(OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a))/(OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)*100
((OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)-(OP_rsvPreF_PE_1st_a+ED_rsvPreF_PE_1st_a+Hosps_rsvPreF_PE_1st_a))/(OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)*100

# -----------------------------
# CALCULATING TOTAL EVENTS AND PATIENT LEVEL DATA - ORIGINAL WANING CURVE AND 1,000 - OLD ANALYSIS

#Uncertainty
#----------------------
#Outpatient episodes 
quantile(OP_no_u_old_1st, probs = c(0.025, 0.975))
quantile(OP_mAb_u_old_1st, probs = c(0.025, 0.975))
quantile(OP_rsvPreF_u_old_1st, probs = c(0.025, 0.975))
#difference
quantile(OP_no_u_old_1st-OP_mAb_u_old_1st, probs = c(0.025, 0.975))
quantile(OP_no_u_old_1st-OP_rsvPreF_u_old_1st, probs = c(0.025, 0.975))
#% reduction
quantile((OP_no_u_old_1st-OP_mAb_u_old_1st)/OP_no_u_old_1st*100, probs = c(0.025, 0.975))
quantile((OP_no_u_old_1st-OP_rsvPreF_u_old_1st)/OP_no_u_old_1st*100, probs = c(0.025, 0.975))

#ED episodes
quantile(ED_no_u_old_1st, probs = c(0.025, 0.975))
quantile(ED_mAb_u_old_1st, probs = c(0.025, 0.975))
quantile(ED_rsvPreF_u_old_1st, probs = c(0.025, 0.975))
#difference
quantile(ED_no_u_old_1st-ED_mAb_u_old_1st, probs = c(0.025, 0.975))
quantile(ED_no_u_old_1st-ED_rsvPreF_u_old_1st, probs = c(0.025, 0.975))
#% reduction
quantile((ED_no_u_old_1st-ED_mAb_u_old_1st)/ED_no_u_old_1st*100, probs = c(0.025, 0.975))
quantile((ED_no_u_old_1st-ED_rsvPreF_u_old_1st)/ED_no_u_old_1st*100, probs = c(0.025, 0.975))

#Hosp episodes 
quantile(Hosps_no_u_old_1st, probs = c(0.025, 0.975))
quantile(Hosps_mAb_u_old_1st, probs = c(0.025, 0.975))
quantile(Hosps_rsvPreF_u_old_1st, probs = c(0.025, 0.975))
#difference
quantile(Hosps_no_u_old_1st-Hosps_mAb_u_old_1st, probs = c(0.025, 0.975))
quantile(Hosps_no_u_old_1st-Hosps_rsvPreF_u_old_1st, probs = c(0.025, 0.975))
#% reduction
quantile((Hosps_no_u_old_1st-Hosps_mAb_u_old_1st)/Hosps_no_u_old_1st*100, probs = c(0.025, 0.975))
quantile((Hosps_no_u_old_1st-Hosps_rsvPreF_u_old_1st)/Hosps_no_u_old_1st*100, probs = c(0.025, 0.975))

#total episodes per scenario
quantile(OP_no_u_old_1st+ED_no_u_old_1st+Hosps_no_u_old_1st, probs = c(0.025, 0.975))
quantile(OP_mAb_u_old_1st+ED_mAb_u_old_1st+Hosps_mAb_u_old_1st, probs = c(0.025, 0.975))
quantile(OP_rsvPreF_u_old_1st+ED_rsvPreF_u_old_1st+Hosps_rsvPreF_u_old_1st, probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_old_1st+ED_no_u_old_1st+Hosps_no_u_old_1st)-(OP_mAb_u_old_1st+ED_mAb_u_old_1st+Hosps_mAb_u_old_1st), probs = c(0.025, 0.975))
quantile((OP_no_u_old_1st+ED_no_u_old_1st+Hosps_no_u_old_1st)-(OP_rsvPreF_u_old_1st+ED_rsvPreF_u_old_1st+Hosps_rsvPreF_u_old_1st), probs = c(0.025, 0.975))
#% reduction
quantile(((OP_no_u_old_1st+ED_no_u_old_1st+Hosps_no_u_old_1st)-(OP_mAb_u_old_1st+ED_mAb_u_old_1st+Hosps_mAb_u_old_1st))/(OP_no_u_old_1st+ED_no_u_1st+Hosps_no_u_old_1st)*100, probs = c(0.025, 0.975))
quantile(((OP_no_u_old_1st+ED_no_u_old_1st+Hosps_no_u_old_1st)-(OP_rsvPreF_u_old_1st+ED_rsvPreF_u_old_1st+Hosps_rsvPreF_u_old_1st))/(OP_no_u_old_1st+ED_no_u_old_1st+Hosps_no_u_old_1st)*100, probs = c(0.025, 0.975))


#first year of life patient level events
quantile((OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a), probs = c(0.025, 0.975))
quantile((OP_mAb_u_old_1st_a+ED_mAb_u_old_1st_a+Hosps_mAb_u_old_1st_a), probs = c(0.025, 0.975))
quantile((OP_rsvPreF_u_old_1st_a+ED_rsvPreF_u_old_1st_a+Hosps_rsvPreF_u_old_1st_a), probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a)-(OP_mAb_u_old_1st_a+ED_mAb_u_old_1st_a+Hosps_mAb_u_old_1st_a), probs = c(0.025, 0.975))
quantile((OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a)-(OP_rsvPreF_u_old_1st_a+ED_rsvPreF_u_old_1st_a+Hosps_rsvPreF_u_old_1st_a), probs = c(0.025, 0.975))
#% reduction
quantile(((OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a)-(OP_mAb_u_old_1st_a+ED_mAb_u_old_1st_a+Hosps_mAb_u_old_1st_a))/(OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a)*100, probs = c(0.025, 0.975))
quantile(((OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a)-(OP_rsvPreF_u_old_1st_a+ED_rsvPreF_u_old_1st_a+Hosps_rsvPreF_u_old_1st_a))/(OP_no_u_old_1st_a+ED_no_u_old_1st_a+Hosps_no_u_old_1st_a)*100, probs = c(0.025, 0.975))

#Point estimate
#----------------------
#Outpatient episodes
OP_no_PE_old_1st
OP_mAb_PE_old_1st
OP_rsvPreF_PE_old_1st
#difference
OP_no_PE_old_1st-OP_mAb_PE_old_1st
OP_no_PE_old_1st-OP_rsvPreF_PE_old_1st
#%difference
(OP_no_PE_old_1st-OP_mAb_PE_old_1st)/OP_no_PE_old_1st*100
(OP_no_PE_old_1st-OP_rsvPreF_PE_old_1st)/OP_no_PE_old_1st*100

#ED episodes alone
ED_no_PE_old_1st
ED_mAb_PE_old_1st
ED_rsvPreF_PE_old_1st
#difference
ED_no_PE_old_1st-ED_mAb_PE_old_1st
ED_no_PE_old_1st-ED_rsvPreF_PE_old_1st
#% difference
(ED_no_PE_old_1st-ED_mAb_PE_old_1st)/ED_no_PE_old_1st*100
(ED_no_P_oldE_1st-ED_rsvPreF_PE_old_1st)/ED_no_PE_old_1st*100

#Hosp episodes alone
Hosps_no_PE_old_1st
Hosps_mAb_PE_old_1st
Hosps_rsvPreF_PE_old_1st
#difference
Hosps_no_PE_old_1st-Hosps_mAb_PE_old_1st
Hosps_no_PE_old_1st-Hosps_rsvPreF_PE_old_1st
#% reduction
(Hosps_no_PE_old_1st-Hosps_mAb_PE_old_1st)/Hosps_no_PE_old_1st*100
(Hosps_no_PE_old_1st-Hosps_rsvPreF_PE_old_1st)/Hosps_no_PE_old_1st*100

#total episodes per year
OP_no_PE_old_1st+ED_no_PE_old_1st+Hosps_no_PE_old_1st
OP_mAb_PE_old_1st+ED_mAb_PE_old_1st+Hosps_mAb_PE_old_1st
OP_rsvPreF_PE_old_1st+ED_rsvPreF_PE_old_1st+Hosps_rsvPreF_PE_old_1st
#difference
(OP_no_PE_old_1st+ED_no_PE_old_1st+Hosps_no_PE_old_1st)-(OP_mAb_PE_old_1st+ED_mAb_PE_old_1st+Hosps_mAb_PE_old_1st)
(OP_no_PE_old_1st+ED_no_PE_old_1st+Hosps_no_PE_old_1st)-(OP_rsvPreF_PE_old_1st+ED_rsvPreF_PE_old_1st+Hosps_rsvPreF_PE_old_1st)
#% reduction
((OP_no_PE_old_1st+ED_no_PE_old_1st+Hosps_no_PE_old_1st)-(OP_mAb_PE_old_1st+ED_mAb_PE_old_1st+Hosps_mAb_PE_old_1st))/(OP_no_PE_old_1st+ED_no_PE_old_1st+Hosps_no_PE_old_1st)*100
((OP_no_PE_old_1st+ED_no_PE_old_1st+Hosps_no_PE_old_1st)-(OP_rsvPreF_PE_old_1st+ED_rsvPreF_PE_old_1st+Hosps_rsvPreF_PE_old_1st))/(OP_no_PE_old_1st+ED_no_PE_old_1st+Hosps_no_PE_old_1st)*100


#first/second year of life totals patient level events w/ combined totals - adjusted
OP_no_PE_old_1st_a+ED_no_PE_old_1st_a+Hosps_no_PE_old_1st_a
OP_mAb_PE_old_1st_a+ED_mAb_PE_old_1st_a+Hosps_mAb_PE_old_1st_a
OP_rsvPreF_PE_old_1st_a+ED_rsvPreF_PE_old_1st_a+Hosps_rsvPreF_PE_old_1st_a
#difference
(OP_no_PE_old_1st_a+ED_no_PE_old_1st_a+Hosps_no_PE_old_1st_a)-(OP_mAb_PE_old_1st_a+ED_mAb_PE_old_1st_a+Hosps_mAb_PE_old_1st_a)
(OP_no_PE_old_1st_a+ED_no_PE_old_1st_a+Hosps_no_PE_old_1st_a)-(OP_rsvPreF_PE_old_1st_a+ED_rsvPreF_PE_old_1st_a+Hosps_rsvPreF_PE_old_1st_a)
#% reduction
((OP_no_PE_old_1st_a+ED_no_PE_old_1st_a+Hosps_no_PE_old_1st_a)-(OP_mAb_PE_old_1st_a+ED_mAb_PE_old_1st_a+Hosps_mAb_PE_old_1st_a))/(OP_no_PE_old_1st_a+ED_no_PE_old_1st_a+Hosps_no_PE_old_1st_a)*100
((OP_no_PE_old_1st_a+ED_no_PE_old_1st_a+Hosps_no_PE_old_1st_a)-(OP_rsvPreF_PE_old_1st_a+ED_rsvPreF_PE_old_1st_a+Hosps_rsvPreF_PE_old_1st_a))/(OP_no_PE_old_1st_a+ED_no_PE_old_1st_a+Hosps_no_PE_old_1st_a)*100


# -----------------------------
# CALCULATING TOTAL EVENTS AND PATIENT LEVEL DATA - ORIGINAL WEANING CURVE AND 10000 SAMPLES - Specific definition
# -----------------------------

#Uncertainty
#----------------------

#first year of life patient level events
quantile((OP_no_u_1st_a_spec+ED_no_u_1st_a_spec+Hosps_no_u_1st_a_spec), probs = c(0.025, 0.975))
quantile((OP_mAb_u_1st_a_spec+ED_mAb_u_1st_a_spec+Hosps_mAb_u_1st_a_spec), probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_1st_a_spec+ED_no_u_1st_a_spec+Hosps_no_u_1st_a_spec)-(OP_mAb_u_1st_a_spec+ED_mAb_u_1st_a_spec+Hosps_mAb_u_1st_a_spec), probs = c(0.025, 0.975))
#%difference
quantile(((OP_no_u_1st_a_spec+ED_no_u_1st_a_spec+Hosps_no_u_1st_a_spec)-(OP_mAb_u_1st_a_spec+ED_mAb_u_1st_a_spec+Hosps_mAb_u_1st_a_spec))/(OP_no_u_1st_a_spec+ED_no_u_1st_a_spec+Hosps_no_u_1st_a_spec)*100, probs = c(0.025, 0.975))

#Point estimate
#----------------------

#first/second year of life totals patient level events w/ combined totals - adjusted
OP_no_PE_1st_a_spec+ED_no_PE_1st_a_spec+Hosps_no_PE_1st_a_spec
OP_mAb_PE_1st_a_spec+ED_mAb_PE_1st_a_spec+Hosps_mAb_PE_1st_a_spec
#difference
(OP_no_PE_1st_a_spec+ED_no_PE_1st_a_spec+Hosps_no_PE_1st_a_spec)-(OP_mAb_PE_1st_a_spec+ED_mAb_PE_1st_a_spec+Hosps_mAb_PE_1st_a_spec)
#% difference
((OP_no_PE_1st_a_spec+ED_no_PE_1st_a_spec+Hosps_no_PE_1st_a_spec)-(OP_mAb_PE_1st_a_spec+ED_mAb_PE_1st_a_spec+Hosps_mAb_PE_1st_a_spec))/(OP_no_PE_1st_a_spec+ED_no_PE_1st_a_spec+Hosps_no_PE_1st_a_spec)*100

# -----------------------------
# CALCULATING TOTAL EVENTS AND PATIENT LEVEL DATA - ADJUSTED WEANING CURVE AND 10,000 SAMPLES
# -----------------------------

#Uncertainty
#----------------------
#Outpatient episodes 
quantile(OP_no_u_adjc, probs = c(0.025, 0.975))
quantile(OP_mAb_u_adjc, probs = c(0.025, 0.975))
#difference
quantile(OP_no_u_adjc-OP_mAb_u_adjc, probs = c(0.025, 0.975))
#%difference
quantile((OP_no_u_adjc-OP_mAb_u_adjc)/OP_no_u_adjc*100, probs = c(0.025, 0.975))

#ED episodes
quantile(ED_no_u_adjc, probs = c(0.025, 0.975))
quantile(ED_mAb_u_adjc, probs = c(0.025, 0.975))
#difference
quantile(ED_no_u_adjc-ED_mAb_u_adjc, probs = c(0.025, 0.975))
#%difference
quantile((ED_no_u_adjc-ED_mAb_u_adjc)/ED_no_u_adjc*100, probs = c(0.025, 0.975))

#Hosp episodes 
quantile(Hosps_no_u_adjc, probs = c(0.025, 0.975))
quantile(Hosps_mAb_u_adjc, probs = c(0.025, 0.975))
#difference 
quantile(Hosps_no_u_adjc-Hosps_mAb_u_adjc, probs = c(0.025, 0.975))
#%difference
quantile((Hosps_no_u_adjc-Hosps_mAb_u_adjc)/Hosps_no_u_adjc*100, probs = c(0.025, 0.975))

#total episodes per scenario
quantile(OP_no_u_adjc+ED_no_u_adjc+Hosps_no_u_adjc, probs = c(0.025, 0.975))
quantile(OP_mAb_u_adjc+ED_mAb_u_adjc+Hosps_mAb_u_adjc, probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_adjc+ED_no_u_adjc+Hosps_no_u_adjc)-(OP_mAb_u_adjc+ED_mAb_u_adjc+Hosps_mAb_u_adjc), probs = c(0.025, 0.975))
#%difference
quantile(((OP_no_u_adjc+ED_no_u_adjc+Hosps_no_u_adjc)-(OP_mAb_u_adjc+ED_mAb_u_adjc+Hosps_mAb_u_adjc))/(OP_no_u_adjc+ED_no_u_adjc+Hosps_no_u_adjc)*100, probs = c(0.025, 0.975))

#first year of life patient level events
quantile((OP_no_u_adjc_a+ED_no_u_adjc_a+Hosps_no_u_adjc_a), probs = c(0.025, 0.975))
quantile((OP_mAb_u_adjc_a+ED_mAb_u_adjc_a+Hosps_mAb_u_adjc_a), probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_adjc_a+ED_no_u_adjc_a+Hosps_no_u_adjc_a)-(OP_mAb_u_adjc_a+ED_mAb_u_adjc_a+Hosps_mAb_u_adjc_a), probs = c(0.025, 0.975))
#%difference
quantile(((OP_no_u_adjc_a+ED_no_u_adjc_a+Hosps_no_u_adjc_a)-(OP_mAb_u_adjc_a+ED_mAb_u_adjc_a+Hosps_mAb_u_adjc_a))/(OP_no_u_adjc_a+ED_no_u_adjc_a+Hosps_no_u_adjc_a)*100, probs = c(0.025, 0.975))

#Point estimate
#----------------------
#Outpatient episodes
OP_no_PE_adjc
OP_mAb_PE_adjc
#difference
OP_no_PE_adjc-OP_mAb_PE_adjc
#%difference
(OP_no_PE_adjc-OP_mAb_PE_adjc)/OP_no_PE_adjc*100

#ED episodes alone
ED_no_PE_adjc
ED_mAb_PE_adjc
#difference
ED_no_PE_adjc-ED_mAb_PE_adjc
#%difference
(ED_no_PE_adjc-ED_mAb_PE_adjc)/ED_no_PE_adjc*100

#Hosp episodes alone
Hosps_no_PE_adjc
Hosps_mAb_PE_adjc
#difference
Hosps_no_PE_adjc-Hosps_mAb_PE_adjc
#%difference
(Hosps_no_PE_adjc-Hosps_mAb_PE_adjc)/Hosps_no_PE_adjc*100

#total episodes per year
OP_no_PE_adjc+ED_no_PE_adjc+Hosps_no_PE_adjc
OP_mAb_PE_adjc+ED_mAb_PE_adjc+Hosps_mAb_PE_adjc
#difference
(OP_no_PE_adjc+ED_no_PE_adjc+Hosps_no_PE_adjc)-(OP_mAb_PE_adjc+ED_mAb_PE_adjc+Hosps_mAb_PE_adjc)
#%difference
((OP_no_PE_adjc+ED_no_PE_adjc+Hosps_no_PE_adjc)-(OP_mAb_PE_adjc+ED_mAb_PE_adjc+Hosps_mAb_PE_adjc))/(OP_no_PE_adjc+ED_no_PE_adjc+Hosps_no_PE_adjc)*100

#first/second year of life totals patient level events w/ combined totals - adjusted
OP_no_PE_adjc_a+ED_no_PE_adjc_a+Hosps_no_PE_adjc_a
OP_mAb_PE_adjc_a+ED_mAb_PE_adjc_a+Hosps_mAb_PE_adjc_a
#difference
(OP_no_PE_adjc_a+ED_no_PE_adjc_a+Hosps_no_PE_adjc_a)-(OP_mAb_PE_adjc_a+ED_mAb_PE_adjc_a+Hosps_mAb_PE_adjc_a)
#%difference
((OP_no_PE_adjc_a+ED_no_PE_adjc_a+Hosps_no_PE_adjc_a)-(OP_mAb_PE_adjc_a+ED_mAb_PE_adjc_a+Hosps_mAb_PE_adjc_a))/(OP_no_PE_adjc_a+ED_no_PE_adjc_a+Hosps_no_PE_adjc_a)*100

# -----------------------------
# CALCULATING TOTAL EVENTS AND PATIENT LEVEL DATA - ADJUSTED COVERAGE ESTIMATE 
# -----------------------------

#Uncertainty
#----------------------
#Outpatient episodes 
quantile(OP_no_u_adjcov, probs = c(0.025, 0.975))
quantile(OP_mAb_u_adjcov, probs = c(0.025, 0.975))
#difference
quantile(OP_no_u_adjcov-OP_mAb_u_adjcov, probs = c(0.025, 0.975))
#%difference
quantile((OP_no_u_adjcov-OP_mAb_u_adjcov)/OP_no_u_adjcov*100, probs = c(0.025, 0.975))

#ED episodes
quantile(ED_no_u_adjcov, probs = c(0.025, 0.975))
quantile(ED_mAb_u_adjcov, probs = c(0.025, 0.975))
#difference
quantile(ED_no_u_adjcov-ED_mAb_u_adjcov, probs = c(0.025, 0.975))
#%difference
quantile((ED_no_u_adjcov-ED_mAb_u_adjcov)/ED_no_u_adjcov*100, probs = c(0.025, 0.975))

#Hosp episodes 
quantile(Hosps_no_u_adjcov, probs = c(0.025, 0.975))
quantile(Hosps_mAb_u_adjcov, probs = c(0.025, 0.975))
#difference
quantile(Hosps_no_u_adjcov-Hosps_mAb_u_adjcov, probs = c(0.025, 0.975))
#%difference
quantile((Hosps_no_u_adjcov-Hosps_mAb_u_adjcov)/Hosps_no_u_adjcov*100, probs = c(0.025, 0.975))

#total episodes per scenario
quantile(OP_no_u_adjcov+ED_no_u_adjcov+Hosps_no_u_adjcov, probs = c(0.025, 0.975))
quantile(OP_mAb_u_adjcov+ED_mAb_u_adjcov+Hosps_mAb_u_adjcov, probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_adjcov+ED_no_u_adjcov+Hosps_no_u_adjcov)-(OP_mAb_u_adjcov+ED_mAb_u_adjcov+Hosps_mAb_u_adjcov), probs = c(0.025, 0.975))
#%difference
quantile(((OP_no_u_adjcov+ED_no_u_adjcov+Hosps_no_u_adjcov)-(OP_mAb_u_adjcov+ED_mAb_u_adjcov+Hosps_mAb_u_adjcov))/(OP_no_u_adjcov+ED_no_u_adjcov+Hosps_no_u_adjcov)*100, probs = c(0.025, 0.975))

#first year of life patient level events
quantile((OP_no_u_adjcov_a+ED_no_u_adjcov_a+Hosps_no_u_adjcov_a), probs = c(0.025, 0.975))
quantile((OP_mAb_u_adjcov_a+ED_mAb_u_adjcov_a+Hosps_mAb_u_adjcov_a), probs = c(0.025, 0.975))
#difference
quantile((OP_no_u_adjcov_a+ED_no_u_adjcov_a+Hosps_no_u_adjcov_a)-(OP_mAb_u_adjcov_a+ED_mAb_u_adjcov_a+Hosps_mAb_u_adjcov_a), probs = c(0.025, 0.975))
#%difference
quantile(((OP_no_u_adjcov_a+ED_no_u_adjcov_a+Hosps_no_u_adjcov_a)-(OP_mAb_u_adjcov_a+ED_mAb_u_adjcov_a+Hosps_mAb_u_adjcov_a))/(OP_no_u_adjcov_a+ED_no_u_adjcov_a+Hosps_no_u_adjcov_a)*100, probs = c(0.025, 0.975))

#Point estimate
#----------------------
#Outpatient episodes
OP_no_PE_adjcov
OP_mAb_PE_adjcov
#difference
OP_no_PE_adjcov-OP_mAb_PE_adjcov
#%difference
(OP_no_PE_adjcov-OP_mAb_PE_adjcov)/OP_no_PE_adjcov*100

#ED episodes alone
ED_no_PE_adjcov
ED_mAb_PE_adjcov
#difference
ED_no_PE_adjcov-ED_mAb_PE_adjcov
#%difference
(ED_no_PE_adjcov-ED_mAb_PE_adjcov)/ED_no_PE_adjcov*100

#Hosp episodes alone
Hosps_no_PE_adjcov
Hosps_mAb_PE_adjcov
#difference
Hosps_no_PE_adjcov-Hosps_mAb_PE_adjcov
#%difference
(Hosps_no_PE_adjcov-Hosps_mAb_PE_adjcov)/Hosps_no_PE_adjcov*100

#total episodes per year
OP_no_PE_adjcov+ED_no_PE_adjcov+Hosps_no_PE_adjcov
OP_mAb_PE_adjcov+ED_mAb_PE_adjcov+Hosps_mAb_PE_adjcov
#difference
(OP_no_PE_adjcov+ED_no_PE_adjcov+Hosps_no_PE_adjcov)-(OP_mAb_PE_adjcov+ED_mAb_PE_adjcov+Hosps_mAb_PE_adjcov)
#%difference
((OP_no_PE_adjcov+ED_no_PE_adjcov+Hosps_no_PE_adjcov)-(OP_mAb_PE_adjcov+ED_mAb_PE_adjcov+Hosps_mAb_PE_adjcov))/(OP_no_PE_adjcov+ED_no_PE_adjcov+Hosps_no_PE_adjcov)*100

#first/second year of life totals patient level events w/ combined totals - adjusted
OP_no_PE_adjcov_a+ED_no_PE_adjcov_a+Hosps_no_PE_adjcov_a
OP_mAb_PE_adjcov_a+ED_mAb_PE_adjcov_a+Hosps_mAb_PE_adjcov_a
#difference
(OP_no_PE_adjcov_a+ED_no_PE_adjcov_a+Hosps_no_PE_adjcov_a)-(OP_mAb_PE_adjcov_a+ED_mAb_PE_adjcov_a+Hosps_mAb_PE_adjcov_a)
#%difference
((OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)-(OP_mAb_PE_adjcov_a+ED_mAb_PE_adjcov_a+Hosps_mAb_PE_adjcov_a))/(OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)*100


#EXTRA/OLD CODE
# -----------------------------
# 3RD YEAR OF LIFE DATA
# -----------------------------
#Uncertainty
#OP_no_df_3rd <- rowSums(OP_no_u_df[27:38])
#OP_mAb_df_3rd <- rowSums(OP_mAb_u_df[,27:38])
#OP_rsvPreF_df_3rd <- rowSums(OP_rsvPreF_u_df[,27:38])
#ED_no_df_3rd <- rowSums(ED_no_u_df[27:38])
#ED_mAb_df_3rd <- rowSums(ED_mAb_u_df[,27:38])
#ED_rsvPreF_df_3rd <- rowSums(ED_rsvPreF_u_df[,27:38])
#Hosps_no_df_3rd <- rowSums(hosps_no_u_df[27:38])
#Hosps_mAb_df_3rd <- rowSums(hosps_mAb_u_df[,27:38])
#Hosps_rsvPreF_df_3rd <- rowSums(hosps_rsvPreF_u_df[,27:38])

#PE
#OP_no_PE_3rd <- rowSums(OP_no_df[27:38])
#OP_mAb_PE_3rd <- rowSums(OP_mAb_df[,27:38])
#OP_rsvPreF_PE_3rd <- rowSums(OP_rsvPreF_df[,27:38])
#ED_no_PE_3rd <- rowSums(ED_no_df[27:38])
#ED_mAb_PE_3rd <- rowSums(ED_mAb_df[,27:38])
#ED_rsvPreF_PE_3rd <- rowSums(ED_rsvPreF_df[,27:38])
#Hosps_no_PE_3rd <- rowSums(hosps_no_df[27:38])
#Hosps_mAb_PE_3rd <- rowSums(hosps_mAb_df[,27:38])
#Hosps_rsvPreF_PE_3rd <- rowSums(hosps_rsvPreF_df[,27:38])

# -----------------------------
# Prior extraction
# -----------------------------

#hdata <- read.csv("Health_outcomes_USAMichpaper.csv")
# Outpatient/ED/Hosp episodes: Extracting outpatient/ED/Hosp episodes of RSV-LRTI into individualized DF
#Outpatient_df <- hdata%>%filter(Metric == "Outpatient")
#ED_df <- hdata%>%filter(Metric == "ED")
#hosps_df <- hdata%>%filter(Metric == "Hospitalizations")
#Outpatient/ED episodes/Hospitalization: Delineate RSV-LRTI events by intervention   
#Outpatient_no_df <- Outpatient_df%>%filter(Intervention == "no intervention")
#Outpatient_mAb_df <- Outpatient_df%>%filter(Intervention == "Nirsevimab")
#Outpatient_rsvPreF_df <- Outpatient_df%>%filter(Intervention == "RSVpreF")
#Outpatient_Combined_df <- Outpatient_df%>%filter(Intervention == "Combined")
#ED_no_df <- ED_df%>%filter(Intervention == "no intervention")
#ED_mAb_df <- ED_df%>%filter(Intervention == "Nirsevimab")
#ED_rsvPreF_df <- ED_df%>%filter(Intervention == "RSVpreF")
#ED_Combined_df <- ED_df%>%filter(Intervention == "Combined")
#Hosps_no_df <- hosps_df%>%filter(Intervention == "no intervention")
#Hosps_mAb_df <- hosps_df%>%filter(Intervention == "Nirsevimab")
#Hosps_rsvPreF_df <- hosps_df%>%filter(Intervention == "RSVpreF")
#Hosps_Combined_df <- hosps_df%>%filter(Intervention == "Combined")

#Death alone extraction - From original DF
#Death episodes alone: sum point estimates across years and ages
#RSV_Deaths_df <- hdata%>%filter(Metric == "Deaths")
#RSV_Deaths_no_df <- Deaths_df%>%filter(Intervention == "no intervention")
#RSV_Deaths_mAb_df <- Deaths_df%>%filter(Intervention == "Nirsevimab")
#RSV_Deaths_rsvPreF_df <- Deaths_df%>%filter(Intervention == "RSVpreF")
#RSV_Deaths_Combined_df <- Deaths_df%>%filter(Intervention == "Combined")
#deaths episodes alone: sum point estimates across years and ages - From OLD/Original DF
#RSV_Deaths_no <- sum(RSV_Deaths_no_df[1:12,4:15])
#RSV_Deaths_mAb <- sum(RSV_Deaths_mAb_df[1:12,4:15])
#RSV_Deaths_rsvPreF <- sum(RSV_Deaths_rsvPreF_df[1:12,4:15])
#RSV_Deaths_Combined <- sum(RSV_Deaths_Combined_df[1:12,4:15])

# Extract just Deaths - not provided by dataset, non-contributory to results from asthma
#Deaths_u_df <- hdata%>%filter(Metric == "Deaths")
#Deaths_no_u_df <- hosps_u_df%>%filter(Intervention == "no intervention")
#Deaths_mAb_u_df <- hosps_u_df%>%filter(Intervention == "Nirsevimab")
#Deaths_rsvPreF_u_df <- hosps_u_df%>%filter(Intervention == "RSVpreF")
#Deaths_Combined_u_df <- hosps_u_df%>%filter(Intervention == "Combined")
