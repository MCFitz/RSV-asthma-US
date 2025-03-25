# Pull in point estimates from Hutton/Creating dataframes
# As reported in Hutton et al. - description of events of RSV delinated 
# initially by LRTI/URTI and differential outcomes based on intervention. 
# also with events delinated by Outpatient visits, ED vists, 
# hospitalizations, and deaths. Used initially to describe cost 
# effectiveness of intervention strategies and averted RSV at 1 y/o.

library(tidyverse)
library(readxl)

hdata <- read.csv("Health_outcomes_USAMichpaper.csv")
hdatahospCI <- read_excel("Healthoutcomes.xlsx")

trials <- 1000

################################################################################
#Point estimate work

#Combined Masterscript
# Outpatient/ED episodes: Extracting just outpatient/ED episodes of RSV-LRTI into individualized DF
Outpatient_df <- hdata%>%filter(Metric == "Outpatient")
ED_df <- hdata%>%filter(Metric == "ED")
#Outpatient/ED episodes: Delineate RSV-LRTI events by intervention   
Outpatient_no_df <- Outpatient_df%>%filter(Intervention == "no intervention")
Outpatient_mAb_df <- Outpatient_df%>%filter(Intervention == "Nirsevimab")
Outpatient_rsvPreF_df <- Outpatient_df%>%filter(Intervention == "RSVpreF")
Outpatient_Combined_df <- Outpatient_df%>%filter(Intervention == "Combined")
ED_no_df <- ED_df%>%filter(Intervention == "no intervention")
ED_mAb_df <- ED_df%>%filter(Intervention == "Nirsevimab")
ED_rsvPreF_df <- ED_df%>%filter(Intervention == "RSVpreF")
ED_Combined_df <- ED_df%>%filter(Intervention == "Combined")
#Outpatient episodes: sum point estimates 
num_OP_ED_no <- sum(Outpatient_no_df[1:12,4:15])+sum(ED_no_df[1:12,4:15])
num_OP_ED_mAb <- sum(Outpatient_mAb_df[1:12,4:15])+sum(ED_mAb_df[1:12,4:15])
num_OP_ED_rsvPreF <- sum(Outpatient_rsvPreF_df[1:12,4:15])+sum(ED_rsvPreF_df[1:12,4:15])
num_OP_ED_combined <- sum(Outpatient_Combined_df[1:12,4:15])+sum(ED_Combined_df[1:12,4:15])

# Outpatient alone masterscript
# Outpatient episodes alone: sum point estimates across years and ages
# Replicate for upper and lower CI
RSV_no <- sum(Outpatient_no_df[1:12,4:15])
RSV_mAb <- sum(Outpatient_mAb_df[1:12,4:15])
RSV_mat <- sum(Outpatient_rsvPreF_df[1:12,4:15])
RSV_com <- sum(Outpatient_Combined_df[1:12,4:15])

#Hosps
# sum cases by intervention type
# Hospitalizations episodes: Extracting just outpatient episodes of RSV-LRTI
hosps_df <- hdata%>%filter(Metric == "Hospitalizations")

#Hospitalizations episodes: Delineate RSV-LRTI events by intervention   
tot_Hosps_no <- hosps_df%>%filter(Intervention == "no intervention")
tot_Hosps_mAb <- hosps_df%>%filter(Intervention == "Nirsevimab")
tot_Hosps_rsvPreF <- hosps_df%>%filter(Intervention == "RSVpreF")
#tot_Hosps_Combined <- hosps_df%>%filter(Intervention == "Combined")

#Hospitalization episodes: sum point estimates 
num_Hosps_no <- sum(tot_Hosps_no[1:12,4:15])
num_Hosps_mAb <- sum(tot_Hosps_mAb[1:12,4:15])
num_Hosps_rsvPreF <- sum(tot_Hosps_rsvPreF[1:12,4:15])
#num_Hosps_combined <- sum(tot_Hosps_Combined[1:12,4:15])

#Deaths
Deaths_df <- hdata%>%filter(Metric == "Deaths")
Deaths_no_df <- Deaths_df%>%filter(Intervention == "no intervention")
Deaths_mAb_df <- Deaths_df%>%filter(Intervention == "Nirsevimab")
Deaths_rsvPreF_df <- Deaths_df%>%filter(Intervention == "RSVpreF")
Deaths_Combined_df <- Deaths_df%>%filter(Intervention == "Combined")

################################################################################
#Uncertainty work

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

# Extract just hospitalizations
hosps_u_df <- hdatahospCI
hosps_no_u_df <- hosps_u_df%>%filter(Intervention == "Natural History")
hosps_mAb_u_df <- hosps_u_df%>%filter(Intervention == "Nirsevimab")
hosps_rsvPreF_u_df <- hosps_u_df%>%filter(Intervention == "RSVpreF")
hosps_Combined_u_df <- hosps_u_df%>%filter(Intervention == "Combined")

# Extract just Deaths
Deaths_u_df <- hdata%>%filter(Metric == "Deaths")
Deaths_no_u_df <- hosps_u_df%>%filter(Intervention == "no intervention")
Deaths_mAb_u_df <- hosps_u_df%>%filter(Intervention == "Nirsevimab")
Deaths_rsvPreF_u_df <- hosps_u_df%>%filter(Intervention == "RSVpreF")
Deaths_Combined_u_df <- hosps_u_df%>%filter(Intervention == "Combined")