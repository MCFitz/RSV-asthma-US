# RSV-asthma-US
#Analysis of the impact of RSV preventative interventions on US asthma burden

#ImportData: Takes the healthoutcomescomplete.csv (Hutton's full output including 1000 trials) and divides into scenario based data and calculates total amount of episodes. Feeds into the adjustment for multiple episodes. Has third year of life data but relegated to extra code at bottom of the script
#AdjustmentForMultipleEpisodes: Takes the Gatenberg data and applies this to the import data to make adjusted datasets/point estimates in order to determine patient level data
#ParamsAsthma: more input variables for our model (total population size, asthma prevalanence ect)
#asthmafunctions: defines the necessary functions which will need to be performed in the UncertaintyCombined/Pointestimate code
#UncertaintyCombined/Pointestimate: Performs the mali cacluations on the adjusted data sets. Also performs the adjustment of the OR to the RR. With a sensitivity analysis at the bottom which will not be published
#OtherRSVOutcomes: Potential for adding other infection based outcomes, currently a work in progress
#Figures: Workbook for figures in the paper
#RSVMultipleEpisodeWorkbook: excel doc which I used to calculate the odds of having multiple episodes for the same infection during the first season of RSV
#Healthoutcomes: Original 1st year dataset supplied by Hutton
#Healthoutcomescomplete: Dataset we are currently using for results - includes PE and 2/3rd year of life data

#Logistical discussions regarding data - 
# Re adjustment for mortality: given the source data does not adjust and the affect of infant mortality in US is low, we've decided against adjusting for mortality. We will still use RR of asthma at 5/6 years given some concerns about wheezing related illness and dx of asthma earlier than 5 y/o
# Re mutual exclusivity of Hutton data: not mutually exclusive (ie hospitalization and ED appointment not exclusive, also importantly ED appointment and outpatient appointment not exclusive) as their outcomes were economic outcomes. 
# Adjustment of aOR to RR: discussion with Justin and Steve - ultimately to reduce bias in this paper and to better approximate the risk of asthma/wheeze after RSV
# Use of 2nd year of life data: better approximate the risk of RSV on asthma and wheeze (although there is a lower risk of catching RSV in the second year we think this will better capture the full burden of disease)
