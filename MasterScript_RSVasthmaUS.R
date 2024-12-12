# Master Script for RSV-asthma-US analysis
# Created by Meagan Fitzpatrick and Ian Galbreath

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