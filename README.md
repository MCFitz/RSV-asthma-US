# RSV-asthma-US
#Analysis of the impact of RSV preventative interventions on US asthma burden

#MASTERSCRIPTALT_RSVasthmaUS - Point estimate calculation of outpatient RSV associated LRTI related asthma outcome using Dr. Huttons original data from Health_outcomes_USAMichpaper (data stratified by intervention, setting, month of the year, age in months)
#MASTERSCRIPTALT_HOSP - Point estimate calculation of hospitalizations RSV associated LRTI related asthma outcome using Dr. Huttons original data from Health_outcomes_USAMichpaper (data stratified by intervention, setting, month of the year, age in months)
#MASTERSCRIPTALT_ED_OP - Point estimate calculation of summed OP + ED RSV associated LRTI related asthma outcomes (trying to capture all encounters of RSV/LRTI) using Dr. Huttons original data from Health_outcomes_USAMichpaper (data stratified by intervention, setting, month of the year, age in months)

#Uncertaintyhosp.r - uncertainty calculations of hospitalization RSV associated LRTI related asthma (uses trials from Dr. Hutton healthoutcomeshosp)
#ImportHuttonData.r - manipulation of excel data frames from Hutton (ie delinating intervention, setting, ect) for both uncertainty and point estimate calculations. Additionally for original data (delinated by month of the year), sums the columns.
#Params_asthma.r - derived from the parent Mali paper, integrates US outcomes/ecologic data and RR of asthma given RSV LRTI in different circumstances (hospitalization vs LRTI overall). 
#asthmafunctionsMali.r - functions used throughout the paper - derived from the Mali paper
#asthmacredibleintervals.r - creates CI for the uncertainty data
#uncertaintyOPED.r - uncertainty calculations of outpatient + emergency department visits RSV associated LRTI related asthma (uses original dataset)
#uncertaintydeaths.r - uncertainty calculations of death from RSV associated LRTI (uses original dataset)

#health_outcomes_USAMichpaper.csv - Original dataset provided by Dr. Hutton - delinates point estimate of RSV associated outcomes (Outpatient/ED/Hospitalization/Death) delianted by intervention (Combined, Nirsevimab, prematernal vaccine) by patient age and year of the month
#healthoutcomestest.csv - health_outcomes_USAMichpaper.csv dataset but rows summed across month 
#healthoutcomeshosp.csv - Dataset provided to us by Dr. Hutton after requesting hospitalization data and trials of date - provided mean of 1000 trials and "binned data" meaning total data per age in months not delinated by month of the year
#healthoutcomeshospCI.csv - healthoutcomeshosp.csv dataset but provided with CI by Dr. Hutton after request
#healthoutcomes.csv - 1000 trials of each intervention for hospitalization associated RSV LRTI (excluding combined as this was discussed to be an unlikely/unrealistic scenaria) - delinated by age in months

#Logistical discussions regarding data - 
# Re adjustment for mortality: given the source data does not adjust and the affect of infant mortality in US is low, we've decided against adjusting for mortality. We will still use RR of asthma at 5/6 years given some concerns about wheezing related illness and dx of asthma earlier than 5 y/o
# Re mutual exclusivity of Hutton data: not mutually exclusive (ie hospitalization and ED appointment not exclusive, also importantly ED appointment and outpatient appointment not exclusive) as their outcomes were economic outcomes. 
# Re using hospitalizations vs OP + ED: decision to use hospitalization associated RSV-LRTI as there is a higher association/relationship between more severe RSV-LRTI infection to asthma
# Re using hospitalization rate of asthma vs Brunwasser paper: as above, there is a higher associated risk from Dr. Hartert's papers in association of RSV-LRTI associated asthma from hospitalization. 