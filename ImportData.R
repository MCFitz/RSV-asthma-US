# Created by Meagan Fitzpatrick and Ian Galbreath
# Adapted code from Ortiz et al 2023 JACI Global

# -----------------------------
# Creating RSV incidence DF
# -----------------------------

library(tidyverse)
library(readxl)

# Data supplied by Hutton team - ED/Hospitalization/Outpatient outcomes and CI - methods from Pediatrics manuscript 2024
hdataCI <- read_excel("Healthoutcomescomplete.xlsx")

# Uncertainty DFs
# -----------------------------
# OP
OP_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Outpatient")
OP_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Outpatient")
OP_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Outpatient")
##Summing rows OP stratified by year of life to 3 years
OP_no_df_1st <- rowSums(OP_no_u_df[3:14])
OP_mAb_df_1st <- rowSums(OP_mAb_u_df[,3:14])
OP_rsvPreF_df_1st <- rowSums(OP_rsvPreF_u_df[,3:14])
OP_no_df_2nd <- rowSums(OP_no_u_df[15:26])
OP_mAb_df_2nd <- rowSums(OP_mAb_u_df[,15:26])
OP_rsvPreF_df_2nd <- rowSums(OP_rsvPreF_u_df[,15:26])
# ED
ED_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Emergency Department")
ED_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Emergency Department")
ED_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Emergency Department")
##Summing rows ED stratified by year of life to 3 years
ED_no_df_1st <- rowSums(ED_no_u_df[3:14])
ED_mAb_df_1st <- rowSums(ED_mAb_u_df[,3:14])
ED_rsvPreF_df_1st <- rowSums(ED_rsvPreF_u_df[,3:14])
ED_no_df_2nd <- rowSums(ED_no_u_df[15:26])
ED_mAb_df_2nd <- rowSums(ED_mAb_u_df[,15:26])
ED_rsvPreF_df_2nd <- rowSums(ED_rsvPreF_u_df[,15:26])
# hospitalizations
hosps_no_u_df <- hdataCI%>%filter(Intervention == "Natural History Hospitalizations")
hosps_mAb_u_df <- hdataCI%>%filter(Intervention == "Nirsevimab Hospitalizations")
hosps_rsvPreF_u_df <- hdataCI%>%filter(Intervention == "RSVpreF Hospitalizations")
##Summing rows hospitalizations stratified by year of life to 3 years
Hosps_no_df_1st <- rowSums(hosps_no_u_df[3:14])
Hosps_mAb_df_1st <- rowSums(hosps_mAb_u_df[,3:14])
Hosps_rsvPreF_df_1st <- rowSums(hosps_rsvPreF_u_df[,3:14])
Hosps_no_df_2nd <- rowSums(hosps_no_u_df[15:26])
Hosps_mAb_df_2nd <- rowSums(hosps_mAb_u_df[,15:26])
Hosps_rsvPreF_df_2nd <- rowSums(hosps_rsvPreF_u_df[,15:26])

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
#converting uncertainty data to PE by scenario, year of life, and healthcare setting
OP_no_PE_1st <- rowSums(OP_no_df[3:14])
OP_mAb_PE_1st <- rowSums(OP_mAb_df[,3:14])
OP_rsvPreF_PE_1st <- rowSums(OP_rsvPreF_df[,3:14])
OP_no_PE_2nd <- rowSums(OP_no_df[15:26])
OP_mAb_PE_2nd <- rowSums(OP_mAb_df[,15:26])
OP_rsvPreF_PE_2nd <- rowSums(OP_rsvPreF_df[,15:26])
ED_no_PE_1st <- rowSums(ED_no_df[3:14])
ED_mAb_PE_1st <- rowSums(ED_mAb_df[,3:14])
ED_rsvPreF_PE_1st <- rowSums(ED_rsvPreF_df[,3:14])
ED_no_PE_2nd <- rowSums(ED_no_df[15:26])
ED_mAb_PE_2nd <- rowSums(ED_mAb_df[,15:26])
ED_rsvPreF_PE_2nd <- rowSums(ED_rsvPreF_df[,15:26])
Hosps_no_PE_1st <- rowSums(hosps_no_df[3:14])
Hosps_mAb_PE_1st <- rowSums(hosps_mAb_df[,3:14])
Hosps_rsvPreF_PE_1st <- rowSums(hosps_rsvPreF_df[,3:14])
Hosps_no_PE_2nd <- rowSums(hosps_no_df[15:26])
Hosps_mAb_PE_2nd <- rowSums(hosps_mAb_df[,15:26])
Hosps_rsvPreF_PE_2nd <- rowSums(hosps_rsvPreF_df[,15:26])

# -----------------------------
# CALCULATING TOTAL EVENTS AND PATIENT LEVEL DATA
# -----------------------------
source("adjustmentformultipleepisode.R")

#Uncertainty
#----------------------
#Outpatient episodes alone: sum point estimates across years and ages
OP_no_df_1st+OP_no_df_2nd
quantile(OP_no_df_1st+OP_no_df_2nd, probs = c(0.05, 0.95))
quantile(OP_no_df_1st, probs = c(0.05, 0.95))
quantile(OP_no_df_2nd, probs = c(0.05, 0.95))
OP_mAb_df_1st+OP_mAb_df_2nd
quantile(OP_mAb_df_1st+OP_mAb_df_2nd, probs = c(0.05, 0.95))
quantile(OP_mAb_df_1st, probs = c(0.05, 0.95))
quantile(OP_mAb_df_2nd, probs = c(0.05, 0.95))
OP_rsvPreF_df_1st+OP_rsvPreF_df_2nd
quantile((OP_no_df_1st-OP_mAb_df_1st)/OP_no_df_1st*100, probs = c(0.05, 0.95))
#ED episodes alone: sum point estimates across years and ages
ED_no_df_1st+ED_no_df_2nd
quantile(ED_no_df_1st+ED_no_df_2nd, probs = c(0.05, 0.95))
quantile(ED_no_df_1st, probs = c(0.05, 0.95))
quantile(ED_no_df_2nd, probs = c(0.05, 0.95))
ED_mAb_df_1st+ED_mAb_df_2nd
quantile(ED_mAb_df_1st+ED_mAb_df_2nd, probs = c(0.05, 0.95))
quantile(ED_mAb_df_1st, probs = c(0.05, 0.95))
quantile(ED_mAb_df_2nd, probs = c(0.05, 0.95))
ED_rsvPreF_df_1st+ED_rsvPreF_df_2nd
quantile((ED_no_df_1st-ED_mAb_df_1st)/ED_no_df_1st*100, probs = c(0.05, 0.95))
#hosp episodes alone: sum point estimates across years and ages
Hosps_no_df_1st+Hosps_no_df_2nd
quantile(Hosps_no_df_1st+Hosps_no_df_2nd, probs = c(0.05, 0.95))
quantile(Hosps_no_df_1st, probs = c(0.05, 0.95))
quantile(Hosps_no_df_2nd, probs = c(0.05, 0.95))
Hosps_mAb_df_1st+Hosps_mAb_df_2nd
quantile(Hosps_mAb_df_1st+Hosps_mAb_df_2nd, probs = c(0.05, 0.95))
quantile(Hosps_mAb_df_1st, probs = c(0.05, 0.95))
quantile(Hosps_mAb_df_2nd, probs = c(0.05, 0.95))
Hosps_rsvPreF_df_1st+Hosps_rsvPreF_df_2nd
quantile((Hosps_no_df_1st-Hosps_mAb_df_1st)/Hosps_no_df_1st*100, probs = c(0.05, 0.95))

#total episodes per scenario
OP_no_df_1st+OP_no_df_2nd+ED_no_df_1st+ED_no_df_2nd+Hosps_no_df_1st+Hosps_no_df_2nd
quantile(OP_no_df_1st+OP_no_df_2nd+ED_no_df_1st+ED_no_df_2nd+Hosps_no_df_1st+Hosps_no_df_2nd, probs = c(0.05, 0.95))
quantile(OP_no_df_1st+ED_no_df_1st+Hosps_no_df_1st, probs = c(0.05, 0.95))
quantile(OP_no_df_2nd+ED_no_df_2nd+Hosps_no_df_2nd, probs = c(0.05, 0.95))
OP_mAb_df_1st+OP_mAb_df_2nd+ED_mAb_df_1st+ED_mAb_df_2nd+Hosps_mAb_df_1st+Hosps_mAb_df_2nd
quantile(OP_mAb_df_1st+OP_mAb_df_2nd+ED_mAb_df_1st+ED_mAb_df_2nd+Hosps_mAb_df_1st+Hosps_mAb_df_2nd, probs = c(0.05, 0.95))
quantile(OP_mAb_df_1st+ED_mAb_df_1st+Hosps_mAb_df_1st, probs = c(0.05, 0.95))
quantile(OP_mAb_df_2nd+ED_mAb_df_2nd+Hosps_mAb_df_2nd, probs = c(0.05, 0.95))
quantile(((OP_no_df_1st+ED_no_df_1st+Hosps_no_df_1st)-(OP_mAb_df_1st+ED_mAb_df_1st+Hosps_mAb_df_1st))/(OP_no_df_1st+ED_no_df_1st+Hosps_no_df_1st)*100, probs = c(0.05, 0.95))

#Point estimate
#----------------------
#Outpatient episodes alone: sum point estimates across years and ages
OP_no_PE_1st+OP_no_PE_2nd
OP_mAb_PE_1st+OP_mAb_PE_2nd
OP_rsvPreF_PE_1st+OP_rsvPreF_PE_2nd
(OP_no_PE_1st-OP_mAb_PE_1st)/OP_no_PE_1st*100
#ED episodes alone: sum point estimates across years and ages
ED_no_PE_1st+ED_no_PE_2nd
ED_mAb_PE_1st+ED_mAb_PE_2nd
ED_rsvPreF_PE_1st+ED_rsvPreF_PE_2nd
(ED_no_PE_1st-ED_mAb_PE_1st)/ED_no_PE_1st*100
#hosp episodes alone: sum point estimates across years and ages
Hosps_no_PE_1st+Hosps_no_PE_2nd
Hosps_mAb_PE_1st+Hosps_mAb_PE_2nd
Hosps_rsvPreF_PE_1st+Hosps_rsvPreF_PE_2nd
(Hosps_no_PE_1st-Hosps_mAb_PE_1st)/Hosps_no_PE_1st*100
#total episodes per year
OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st
OP_mAb_PE_1st+ED_mAb_PE_1st+Hosps_mAb_PE_1st
OP_no_PE_1st+OP_no_PE_2nd+ED_no_PE_1st+ED_no_PE_2nd+Hosps_no_PE_1st+Hosps_no_PE_2nd
OP_mAb_PE_1st+OP_mAb_PE_2nd+ED_mAb_PE_1st+ED_mAb_PE_2nd+Hosps_mAb_PE_1st+Hosps_mAb_PE_2nd
((OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)-(OP_mAb_PE_1st+ED_mAb_PE_1st+Hosps_mAb_PE_1st))/(OP_no_PE_1st+ED_no_PE_1st+Hosps_no_PE_1st)*100
#first/second year of life totals patient level events w/ combined totals - adjusted
OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a
OP_no_PE_2nd_a+ED_no_PE_2nd_a+Hosps_no_PE_2nd_a
OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a+OP_no_PE_2nd_a+ED_no_PE_2nd_a+Hosps_no_PE_2nd_a
OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a
OP_mAb_PE_2nd_a+ED_mAb_PE_2nd_a+Hosps_mAb_PE_2nd_a
OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a+OP_mAb_PE_2nd_a+ED_mAb_PE_2nd_a+Hosps_mAb_PE_2nd_a
((OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)-(OP_mAb_PE_1st_a+ED_mAb_PE_1st_a+Hosps_mAb_PE_1st_a))/(OP_no_PE_1st_a+ED_no_PE_1st_a+Hosps_no_PE_1st_a)*100

quantile(((OP_no_df_1st_a+ED_no_df_1st_a+Hosps_no_df_1st_a)-(OP_mAb_df_1st_a+ED_mAb_df_1st_a+Hosps_mAb_df_1st_a))/(OP_no_df_1st_a+ED_no_df_1st_a+Hosps_no_df_1st_a)*100, probs = c(0.05, 0.95))

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
# Prior extraction from former health outcomes document
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

