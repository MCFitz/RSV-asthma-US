# Mali Asthma Project Functions

################################################################################

# function to calculate 95% credible intervals
CI_func <- function(dist) {
  c(quantile(dist, probs = 0.05), quantile(dist, probs = 0.95))
}

# function for adjusting number of RSV-LRTI to account for all cause mortality
# out to 6 years
mort_adj_func <- function(LRTI, U5, U9){
  U5_adj <- LRTI - (LRTI * 2/5 * U5)
  U6_adj <- U5_adj - (U5_adj * 1/5 * U9)
} # will need to figure out where LRTI is

# function to calculate the prevalence of asthma/wheeze among those w/o RSV-LRTI
prev_no_rsv_func <- function(prev_tot, pop_tot, rr_w,
                             population_rsv, population_no_rsv){
  prev_no_rsv <- prev_tot * pop_tot / 
    ((rr_w * population_rsv) + population_no_rsv)
  prev_no_rsv
}

# function to calculate asthma among those without RSV-LRTI
asth_no_rsv_func <- function(n, r_asth){
  num_asth_no_rsv <- n * r_asth
  num_asth_no_rsv
}

# function to calculate asthma among those with RSV-LRTI
asth_rsv_func <- function(n, r_asth, rr){
  num_asth_rsv <- n * r_asth * rr
  num_asth_rsv
}

# function to calculate total asthma cases
tot_asth_func <- function(asth_no, asth_rsv){
  tot_asth <- asth_no + asth_rsv
  tot_asth
}

# function to calculate asthma among those w/ RSVLRTI had they not been infected
asth_rsv_null_func <- asth_no_rsv_func

# function to calculate RSV-LRTI attributable asthma
asth_rsv_att_func <- function(asth_rsv, asth_rsv_null){
  asth_rsv_att <- asth_rsv - asth_rsv_null
  asth_rsv_att
}
