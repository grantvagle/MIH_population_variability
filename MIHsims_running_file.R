### RUNNING FILE ###
library(dplyr)
library(reshape2)
library(mobsim)

source("MIHsims_functions_file.R")


######## MAIN SIMULATIONS #########

# fix linear relationshps among 18, 36, 54 sites
sites_18 = data.frame(Productivity = seq(500, 6450, 350), Diversity = c(3:20))
sites_36 = rbind(sites_18, sites_18) # doubles number of sites
sites_54 = rbind(sites_18,sites_18,sites_18) # triples the number of sites

# parameters - small demo run, just 10 sims per set of params
numsims = 10
numyears = 50
taxonvar_arr = c(0.01, 0.10, 0.20, 0.30, 0.40, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00)
sample.windows = c(1,2,5,10,40) 
cv_abund = 4
r_ = 1
proportion = 1
site_list = list('18' = sites_18, '36' = sites_36, '54' = sites_54)


# parameters - long run 1000 sims
# numsims = 1000 
# numyears = 50
# taxonvar_arr = c(0.01, 0.10, 0.20, 0.30, 0.40, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00)
# sample.windows = c(1,2,3,4,5,7,10,15,20,30,40) 
# cv_abund = 4
# r_ = 1
# proportion = 1
# site_list = list('18' = sites_18, '36' = sites_36, '54' = sites_54)


# run simulations
main_df = do_sims(fixed_rel = 'linear', 
                  site_list = site_list, 
                  taxonvar_arr = taxonvar_arr, 
                  sample.windows = sample.windows, 
                  numyears = numyears, 
                  numsims = numsims, 
                  cv_abund = cv_abund, 
                  proportion = proportion, 
                  r_ = r_ )

write.csv(main_df, file = "main_sim_results.csv", row.names = FALSE)




######## NON-CONSECUTIVE SAMPLING ########

# parameters (additional to above)
sample.windows.years = list(c(1), c(1,2), c(1:3), c(1:4), c(1:5), c(1:10), c(1,3), c(1,4), c(1,5), c(1,3,5), c(1,5,10), c(1,4,7,10))


# run simulations
nonconsec_df = do_nonconsec_sims(fixed_rel = 'linear', 
                                 site_list = site_list, 
                                 taxonvar_arr = taxonvar_arr, 
                                 sample.windows.years = sample.windows.years, 
                                 numyears = numyears, 
                                 numsims = numsims, 
                                 cv_abund = cv_abund, 
                                 proportion = proportion, 
                                 r_ = r_ )
write.csv(nonconsec_df, file = "nonconsec_sim_results.csv", row.names = FALSE)




######## RANDOM FIXED RELATIONSHIP #######
# all parameters same as main sims

# run simulations

randrel_df = do_sims(fixed_rel = 'random', 
                  site_list = site_list, 
                  taxonvar_arr = taxonvar_arr, 
                  sample.windows = sample.windows, 
                  numyears = numyears, 
                  numsims = numsims, 
                  cv_abund = cv_abund, 
                  proportion = proportion, 
                  r_ = r_ )
write.csv(randrel_df, file = "randrel_sim_results.csv", row.names = FALSE)












