# Master Script for RSV-asthma-US analysis
# Created by Meagan Fitzpatrick and Ian Galbreath

library(tidyverse)

# Set number of trials
trials <- 1000

# Import data from Hutton
source("ImportHuttonData.R")
source("PARAMS_asthma.R")

# sum cases by intervention type
# assuming all recorded patients are mutually exclusive
# IAN_ADD please check if this is what Hutton assumed
# except hospitalization and death?
# subtract deaths from hospitalizations first?
# Outpatient episodes: Extracting just outpatient episodes of RSV-LRTI
Outpatient_df <- hdata%>%filter(Metric == "Outpatient")

#Outpatient episodes: Delineate RSV-LRTI events by intervention   
Outpatient_no_df <- Outpatient_df%>%filter(Intervention == "no intervention")
Outpatient_mAb_df <- Outpatient_df%>%filter(Intervention == "Nirsevimab")
Outpatient_rsvPreF_df <- Outpatient_df%>%filter(Intervention == "RSVpreF")
Outpatient_Combined_df <- Outpatient_df%>%filter(Intervention == "Combined")

# Outpatient episodes: sum point estimates across years and ages
# Replicate for upper and lower CI
RSV_no <- sum(Outpatient_no_df[1:12,4:15])
RSV_mAb <- sum(Outpatient_mAb_df[1:12,4:15])
RSV_mat <- sum(Outpatient_rsvPreF_df[1:12,4:15])
RSV_com <- sum(Outpatient_Combined_df[1:12,4:15])

