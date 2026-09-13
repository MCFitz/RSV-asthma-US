# RSV-asthma-US
#Analysis of the impact of RSV preventative interventions on US asthma burden

#ImportData: Takes the RSV-LRTI incidence full output including 1,000 trials and 10,000 for the different sensitivity analysis and primary analysis and divides into scenario based data and calculates total amount of episodes. Feeds into the adjustment for multiple episodes. Has second and third year of life data but relegated to extra code at bottom of the script
#AdjustmentForMultipleEpisodes: Takes the Gatenberg data and applies this to the import data to make adjusted datasets/point estimates in order to determine patient level data
#ParamsAsthma: more input variables for our model (total population size, asthma prevalence, variety of OR ect)
#asthmafunctions: defines the necessary functions which will need to be performed in the Uncertainty/Combined/Pointestimate code
#UncertaintyCombined/Pointestimate: Performs the mali cacluations on the adjusted data sets. Also performs the adjustment of the OR to the RR. With a sensitivity analysis at the bottom which will not be published
#OtherRSVOutcomes: Old code - relegated to potential future analysis - Potential for adding additional infection based outcomes
#Figures: old code - relegated to annals of workspace. Workbook for figures in the paper
#RSVMultipleEpisodeWorkbook: excel doc which I used to calculate the odds of having multiple episodes for the same infection during the first season of RSV - has both sensitive and specific definition
#Healthoutcomes: Original 1st year dataset for old primary analysis, only 1,000 samples
#Healthoutcomescomplete: Dataset we are currently using for results - includes PE and 2/3rd year of life data
#PAF: old code - relegated to future analysis comparing our calculated PAF to abreo paper PAD
#Healthoutcomesnewcurve: RSV LRTI outcomes under different waning curve for sensitivity analysis
#Healthoutcomesnewsamples:  RSV LRTI outcomes under original waning curve, 10,000 samples
#Healthoutcomesnewcov:  RSV LRTI outcomes under original waning curve, 10,000 samples, 90% coverage for sensitivity analysis
#my_data.csv: output used for some direct output to excel when working 
#tornadoplot.R: creating tornado plot - using PE and doing a uni-variable analysis to determine which variable contributes the most to the width of our UI
