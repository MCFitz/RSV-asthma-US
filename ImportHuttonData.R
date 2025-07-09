# Pull in point estimates and uncertainty data from Hutton and creating dataframes
# As reported in Hutton et al. - description of events of RSV delinated 
# initially by LRTI/URTI and differential outcomes based on intervention. 
# also with events delinated by Outpatient visits, ED vists, 
# Hospitalizations, and deaths. Used initially to describe cost 
# effectiveness of intervention strategies and averted RSV at 1 y/o.

library(tidyverse)
library(readxl)

hdata <- read.csv("Health_outcomes_USAMichpaper.csv")
hdataCI <- read_excel("Healthoutcomescomplete.xlsx")

trials <- 1000

################################################################################
#Point estimate work

#Combined Masterscript
# Outpatient/ED/Hosp episodes: Extracting outpatient/ED/Hosp episodes of RSV-LRTI into individualized DF
Outpatient_df <- hdata%>%filter(Metric == "Outpatient")
ED_df <- hdata%>%filter(Metric == "ED")
hosps_df <- hdata%>%filter(Metric == "Hospitalizations")

#Outpatient/ED episodes/Hospitalization: Delineate RSV-LRTI events by intervention   
Outpatient_no_df <- Outpatient_df%>%filter(Intervention == "no intervention")
Outpatient_mAb_df <- Outpatient_df%>%filter(Intervention == "Nirsevimab")
Outpatient_rsvPreF_df <- Outpatient_df%>%filter(Intervention == "RSVpreF")
Outpatient_Combined_df <- Outpatient_df%>%filter(Intervention == "Combined")
ED_no_df <- ED_df%>%filter(Intervention == "no intervention")
ED_mAb_df <- ED_df%>%filter(Intervention == "Nirsevimab")
ED_rsvPreF_df <- ED_df%>%filter(Intervention == "RSVpreF")
ED_Combined_df <- ED_df%>%filter(Intervention == "Combined")
Hosps_no_df <- hosps_df%>%filter(Intervention == "no intervention")
Hosps_mAb_df <- hosps_df%>%filter(Intervention == "Nirsevimab")
Hosps_rsvPreF_df <- hosps_df%>%filter(Intervention == "RSVpreF")
Hosps_Combined_df <- hosps_df%>%filter(Intervention == "Combined")

#Outpatient episodes: sum point estimates 
num_OP_ED_Hosp_no <- sum(Outpatient_no_df[1:12,4:15])+sum(ED_no_df[1:12,4:15])+sum(Hosps_no_df[1:12,4:15])
num_OP_ED_Hosp_mAb <- sum(Outpatient_mAb_df[1:12,4:15])+sum(ED_mAb_df[1:12,4:15])+sum(Hosps_mAb_df[1:12,4:15])
num_OP_ED_Hosp_rsvPreF <- sum(Outpatient_rsvPreF_df[1:12,4:15])+sum(ED_rsvPreF_df[1:12,4:15])+sum(Hosps_rsvPreF_df[1:12,4:15])
num_OP_ED_Hosp_Combined <- sum(Outpatient_Combined_df[1:12,4:15])+sum(ED_Combined_df[1:12,4:15])+sum(Hosps_Combined_df[1:12,4:15])

# Outpatient alone masterscript
# Outpatient episodes alone: sum point estimates across years and ages
# Replicate for upper and lower CI
RSV_no <- sum(Outpatient_no_df[1:12,4:15])
RSV_mAb <- sum(Outpatient_mAb_df[1:12,4:15])
RSV_rsvPreF <- sum(Outpatient_rsvPreF_df[1:12,4:15])
RSV_Combined <- sum(Outpatient_Combined_df[1:12,4:15])

#ED alone 
#ED episodes alone: sum point estimates across years and ages
RSV_ED_no <- sum(ED_no_df[1:12,4:15])
RSV_ED_mAb <- sum(ED_mAb_df[1:12,4:15])
RSV_ED_rsvPreF <- sum(ED_rsvPreF_df[1:12,4:15])
RSV_ED_Combined <- sum(ED_Combined_df[1:12,4:15])

#hosp alone 
#hosp episodes alone: sum point estimates across years and ages
RSV_Hosps_no <- sum(Hosps_no_df[1:12,4:15])
RSV_Hosps_mAb <- sum(Hosps_mAb_df[1:12,4:15])
RSV_Hosps_rsvPreF <- sum(Hosps_rsvPreF_df[1:12,4:15])
RSV_Hosps_Combined <- sum(Hosps_Combined_df[1:12,4:15])

#Death alone extraction
#Death episodes alone: sum point estimates across years and ages
RSV_Deaths_df <- hdata%>%filter(Metric == "Deaths")
RSV_Deaths_no_df <- Deaths_df%>%filter(Intervention == "no intervention")
RSV_Deaths_mAb_df <- Deaths_df%>%filter(Intervention == "Nirsevimab")
RSV_Deaths_rsvPreF_df <- Deaths_df%>%filter(Intervention == "RSVpreF")
RSV_Deaths_Combined_df <- Deaths_df%>%filter(Intervention == "Combined")

#deaths episodes alone: sum point estimates across years and ages
RSV_Deaths_no <- sum(RSV_Deaths_no_df[1:12,4:15])
RSV_Deaths_mAb <- sum(RSV_Deaths_mAb_df[1:12,4:15])
RSV_Deaths_rsvPreF <- sum(RSV_Deaths_rsvPreF_df[1:12,4:15])
RSV_Deaths_Combined <- sum(RSV_Deaths_Combined_df[1:12,4:15])

################################################################################
#Uncertainty work - QUESTION: Do these represent just LRTI or LRTI+URTI?

# Extract OP
OP_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Outpatient")
OP_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Outpatient")
OP_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Outpatient")
#OP_Combined_u_df <- hdataCI%>%filter(Intervention == "Combined Outpatient")
##Summing rows OP
OP_no_df <- rowSums(OP_no_u_df[3:14])
OP_mAb_df <- rowSums(OP_mAb_u_df[,3:14])
OP_rsvPreF_df <- rowSums(OP_rsvPreF_u_df[,3:14])

# Extract ED
ED_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Emergency Department")
ED_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Emergency Department")
ED_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Emergency Department")
#ED_Combined_u_df <- hdataCI%>%filter(Intervention == "Combined Emergency Department")
##Summing rows OP
ED_no_df <- rowSums(ED_no_u_df[3:14])
ED_mAb_df <- rowSums(ED_mAb_u_df[,3:14])
ED_rsvPreF_df <- rowSums(ED_rsvPreF_u_df[,3:14])

# Extract hospitalizations
hosps_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Hospitalizations")
hosps_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Hospitalizations")
hosps_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Hospitalizations")
#hosps_Combined_u_df <- hdataCI%>%filter(Intervention == "Combined Hospitalizations")

##Summing rows hospitalizations
Hosps_no_df <- rowSums(hosps_no_u_df[3:14])
Hosps_mAb_df <- rowSums(hosps_mAb_u_df[,3:14])
Hosps_rsvPreF_df <- rowSums(hosps_rsvPreF_u_df[,3:14])

# Extract just Deaths - not provided by dataset, non-contributory to results from asthma
#Deaths_u_df <- hdata%>%filter(Metric == "Deaths")
#Deaths_no_u_df <- hosps_u_df%>%filter(Intervention == "no intervention")
#Deaths_mAb_u_df <- hosps_u_df%>%filter(Intervention == "Nirsevimab")
#Deaths_rsvPreF_u_df <- hosps_u_df%>%filter(Intervention == "RSVpreF")
#Deaths_Combined_u_df <- hosps_u_df%>%filter(Intervention == "Combined")