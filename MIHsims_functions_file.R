### FUNCTIONS FILE ###


###### SHARED FUNCTIONS ######

RickerYears = function(N_0, r, K, PV, numyears) {
  # N_0 = initial population
  # r = average log intrinsic growth rate
  # K = carrying capacity
  # PV = population variability (variance of Normal distribution to choose growth rate from)
  # numyears = number of years for which to simulate abundance
  # output: 1-dimensional array of abundance over time
  
  A = array(c(NA), dim = c(1,numyears))
  R = array(c(NA), dim = c(1,numyears))
  A[1] = N_0
  R[1] = rnorm(1,mean = r*(1 - A[1]/K), sd = sqrt(PV))
  
  for (y in 2:numyears){
    A[y] = A[y-1] * exp(R[y-1])
    if (A[y] < 2) {A[y] = 2} # prevent extinction due to stochasticity
    R[y] = rnorm(1,mean = r*(1 - A[y]/K), sd = sqrt(PV))
  }
  
  return(A)
}

LogNorm = function(diversity, productivity, fixed_rel, proportion = 1, cv_abund, sad_type='lnorm', fix_s_sim=TRUE){
  # diversity = number of species among which to divide abundance
  # productivity = productivity of the site
  # rel = type of relationship between average community abundance and diversity ('linear' or 'random')
  # proportion = how many individuals per unit of productivity
  # cv_abund = steepness of the Log-Normal distribution
  # sad_type = type of distribution to use
  # fix_s_sim = fixes (or doesn't) the total abundance exactly, possibly altering distribution to do so
  # output: array of initial abundances for each species
  
  if(fixed_rel=='linear'){comm_abund = proportion*productivity} #linear fixed rel
  if(fixed_rel=='random'){comm_abund = sample(x = c(proportion*500,proportion*6450), size = 1)} #random fixed rel
  sadsim = sim_sad(s_pool = diversity, n_sim = comm_abund, sad_type = sad_type, sad_coef = list("cv_abund" = cv_abund), fix_s_sim = fix_s_sim)
  
  # retrieve array of abunds from sad object
  init_arr = array(c(NA), dim=length(sadsim))
  for (i in 1:length(sadsim)){
    init_arr[i] = sadsim[[i]]
  }
  
  #return the array of abunds
  return(init_arr)
}

TaxonAbund = function(site.table, fixed_rel, r_ = 1, proportion = 1, taxonvar = 1, numyears, cv_abund){
  # site.table = table of sites with productivity and diversity values
  # r_ = average log intrinsic growth rate for Ricker model
  # taxonvar = population variability for Ricker model
  # numyears = number of years of simulated abundance
  # cv_abund = steepness of log-normal distribution of abundance among species
  # output: array of community abundances across sites (rows) and over numyears years (columns)
  
  compabund = array(c(NA), dim = c(nrow(site.table),numyears))
  
  for (site in 1:nrow(site.table)){
    
    div = site.table$Diversity[site]
    prod = site.table$Productivity[site]
    lognormres = LogNorm(diversity =div, productivity = prod, fixed_rel = fixed_rel, proportion = proportion, cv_abund = cv_abund, sad_type = 'lnorm', fix_s_sim = TRUE)
    abundarr = array(c(NA), dim = c(div, numyears))
    
    # generating the time series of abundances for a single site
    for (sp in 1:length(lognormres)){
      ave = lognormres[sp]
      abundarr[sp,] = RickerYears(N_0 = ave, r = r_, K = ave, PV = taxonvar, numyears = numyears)
      
    }
    compabund[site,] = colSums(abundarr)
    rm(abundarr)
    
  }
  return(compabund)
}

do_sims = function(fixed_rel, site_list, taxonvar_arr, sample.windows, numyears, numsims, cv_abund = 4, proportion = 1, r_ = 1){
  # description: 
  # for each site table, for each population variability value, for numsims number of simulations, 
  # simulate variable community abundance then sample it for each sample.window and 
  # do linear regression of diversity on mean abundance in sampled years, 
  # then save in the data.frame
  
  # fixed_rel = type of fixed relationship between average community abundance and diversity
  # site_list = list of data frames of sites with productivity and diversity values
  # taxonvar_arr = array of population variability values to loop through
  # sample.windows = array of sample lengths (number of sampled years) to loop through
  # numyears = number of years for simulated abundance
  # numsims = number of sims to run for each site.table, PV, and sample window
  # cv_abund = steepness of log-normal distribution for splitting community abundance among species at a site
  # proportion = number of community individuals per unit of productivity
  # r_ = average log intrinsic growth rate for Ricker model
  
  # output: data.frame with model results across all parameter values
  
  
  
  full_df = data.frame("PV" = NA,
                       "Slope" = NA,
                       "Sample.Length" = NA,
                       "SimNum" = NA,
                       "P_value" = NA,
                       "R_squared" = NA,
                       "Std_error" = NA,
                       "p_val_bin" = NA,
                       "SiteNum" = NA
  )
  
  for(sn in 1:length(site_list)){
    site.table = site_list[[sn]]
    sitenum = nrow(site.table)
    
    for (pv in 1:length(taxonvar_arr)){
      taxonvar = taxonvar_arr[pv]
      
      for (sim in 1:numsims){
        
        # generate community abundance over time
        abund = data.frame(TaxonAbund(site.table = site.table, fixed_rel = fixed_rel, r_ = r_, proportion = proportion, taxonvar = taxonvar, numyears = numyears, cv_abund = cv_abund))
        
        # reformat
        colnames(abund) <- c(1:numyears)
        abund$Diversity = site.table$Diversity
        abund = abund %>%
          melt(id.vars = 'Diversity', variable.name = 'Year', value.name = 'Abundance')
        
        for (wind in 1:length(sample.windows)){
          samplen = sample.windows[wind]
          
          # subset abundances
          sub_ab = subset(abund, Year %in% c(10 + 1:samplen))
          
          ### model with means ###
          mean_ab = sub_ab %>%
            group_by(Diversity) %>%
            summarise(mean_abund = mean(Abundance))
          
          # normalize
          div_mean_norm = (mean_ab$Diversity - min(mean_ab$Diversity)) / (max(mean_ab$Diversity) - min(mean_ab$Diversity))
          samp_mean_norm = (mean_ab$mean_abund - min(mean_ab$mean_abund)) / (max(mean_ab$mean_abund) - min(mean_ab$mean_abund))
          
          # regression
          mod_mean = lm(div_mean_norm~samp_mean_norm)
          rm(div_mean_norm,samp_mean_norm)
          
          # regression values
          slope = mod_mean$coefficients[2]
          pval = summary(mod_mean)$coefficients[8]
          rsq = summary(mod_mean)$r.squared
          stderr = summary(mod_mean)$coefficients[4]
          if(pval>0.05){pbin = 0} else {pbin = 1}
          
          # save in data frame
          newrow_means = data.frame("PV" = taxonvar,
                                    "Slope" = slope,
                                    "Sample.Length" = samplen,
                                    "SimNum" = sim,
                                    "P_value" = pval,
                                    "R_squared" = rsq,
                                    "Std_error" = stderr,
                                    "p_val_bin" = pbin,
                                    "SiteNum" = sitenum
          )
          full_df = rbind(full_df, newrow_means)
        }
      }
    }
  }
  rownames(full_df) <- NULL
  return(full_df[-1,])
}

###### MAIN SIMULATIONS ######

  #all functions are shared





###### NON-CONSECUTIVE SAMPLING ######

do_nonconsec_sims = function(fixed_rel = 'linear', site_list, taxonvar_arr, sample.windows.years, numyears, numsims, cv_abund = 4, proportion = 1, r_ = 1){
  # description: 
  # for each site table, for each population variability value, for numsims number of simulations, 
  # simulate variable community abundance then sample it for each sample.window and 
  # do linear regression of diversity on mean abundance in sampled years, 
  # then save in the data.frame
  
  # fixed_rel = type of fixed relationship between average community abundance and diversity
  # site_list = list of data frames of sites with productivity and diversity values
  # taxonvar_arr = array of population variability values to loop through
  # sample.windows = array of sample lengths (number of sampled years) to loop through
  # numyears = number of years for simulated abundance
  # numsims = number of sims to run for each site.table, PV, and sample window
  # cv_abund = steepness of log-normal distribution for splitting community abundance among species at a site
  # proportion = number of community individuals per unit of productivity
  # r_ = average log intrinsic growth rate for Ricker model
  
  # output: data.frame with model results across all parameter values
  
  
  
  full_df = data.frame("PV" = NA,
                       "Slope" = NA,
                       "Sample.Years" = NA, #this line specific to non-consecutive sampling
                       "SimNum" = NA,
                       "P_value" = NA,
                       "R_squared" = NA,
                       "Std_error" = NA,
                       "p_val_bin" = NA,
                       "SiteNum" = NA
  )
  
  for(sn in 1:length(site_list)){
    site.table = site_list[[sn]]
    sitenum = nrow(site.table)
    
    for (pv in 1:length(taxonvar_arr)){
      taxonvar = taxonvar_arr[pv]
      
      for (sim in 1:numsims){
        
        # generate community abundance over time
        abund = data.frame(TaxonAbund(fixed_rel = fixed_rel, site.table = site.table, r_ = r_, proportion = proportion, taxonvar = taxonvar, numyears = numyears, cv_abund = cv_abund))
        
        # reformat
        colnames(abund) <- c(1:numyears)
        abund$Diversity = site.table$Diversity
        abund = abund %>%
          melt(id.vars = 'Diversity', variable.name = 'Year', value.name = 'Abundance')
        
        for (wind in 1:length(sample.windows.years)){
          samplen = sample.windows.years[[wind]] #this line specific to non-consecutive sampling
          
          # subset abundances
          sub_ab = subset(abund, Year %in% c(10 + samplen)) #this line specific to non-consecutive sampling
          
          ### model with means ###
          mean_ab = sub_ab %>%
            group_by(Diversity) %>%
            summarise(mean_abund = mean(Abundance))
          
          # normalize
          div_mean_norm = (mean_ab$Diversity - min(mean_ab$Diversity)) / (max(mean_ab$Diversity) - min(mean_ab$Diversity))
          samp_mean_norm = (mean_ab$mean_abund - min(mean_ab$mean_abund)) / (max(mean_ab$mean_abund) - min(mean_ab$mean_abund))
          
          # regression
          mod_mean = lm(div_mean_norm~samp_mean_norm)
          rm(div_mean_norm,samp_mean_norm)
          
          # regression values
          slope = mod_mean$coefficients[2]
          pval = summary(mod_mean)$coefficients[8]
          rsq = summary(mod_mean)$r.squared
          stderr = summary(mod_mean)$coefficients[4]
          if(pval>0.05){pbin = 0} else {pbin = 1}
          
          # save in data frame
          newrow_means = data.frame("PV" = taxonvar,
                                    "Slope" = slope,
                                    "Sample.Years" = paste(samplen, collapse="_"), #this line specific to non-consec
                                    "SimNum" = sim,
                                    "P_value" = pval,
                                    "R_squared" = rsq,
                                    "Std_error" = stderr,
                                    "p_val_bin" = pbin,
                                    "SiteNum" = sitenum
          )
          full_df = rbind(full_df, newrow_means)
        }
      }
    }
  }
  rownames(full_df) <- NULL
  return(full_df[-1,])
}


###### RANDOM FIXED RELATIONSHIP ######

  #all functions are shared




###### VARYING COMMUNITY EVENNESS ######

do_sims_lnorm_range = function(fixed_rel, site_list, taxonvar_arr, sample.windows, numyears, numsims, cv_abund = 4, proportion = 1, r_ = 1){
  # description: 
  # for each site table, for each population variability value, for numsims number of simulations, 
  # simulate variable community abundance then sample it for each sample.window and 
  # do linear regression of diversity on mean abundance in sampled years, 
  # then save in the data.frame
  
  # fixed_rel = type of fixed relationship between average community abundance and diversity
  # site_list = list of data frames of sites with productivity and diversity values
  # taxonvar_arr = array of population variability values to loop through
  # sample.windows = array of sample lengths (number of sampled years) to loop through
  # numyears = number of years for simulated abundance
  # numsims = number of sims to run for each site.table, PV, and sample window
  # cv_abund = steepness of log-normal distribution for splitting community abundance among species at a site
  # proportion = number of community individuals per unit of productivity
  # r_ = average log intrinsic growth rate for Ricker model
  
  # output: list with two data.frames:
  #         data.frame with model results across all parameter values
  #         data.frame with log-normal distributions used in each simulation
  
  
  
  full_df = data.frame("PV" = NA,
                       "Slope" = NA,
                       "Sample.Length" = NA,
                       "SimNum" = NA,
                       "P_value" = NA,
                       "R_squared" = NA,
                       "Std_error" = NA,
                       "p_val_bin" = NA,
                       "SiteNum" = NA
  )
  
  lognorm_df_fin = data.frame("SiteNum" = NA, 
                              "PV" = NA, 
                              "SimNum" = NA, 
                              "CV_abund" = NA, 
                              "Sp_no" = NA, 
                              "Diversity" = NA, 
                              "Init_abund" = NA)
  
  for(sn in 1:length(site_list)){
    site.table = site_list[[sn]]
    sitenum = nrow(site.table)
    
    for (pv in 1:length(taxonvar_arr)){
      taxonvar = taxonvar_arr[pv]
      
      for (sim in 1:numsims){
        
        # generate community abundance over time
        tax_abund = TaxonAbund_(site.table = site.table, fixed_rel = fixed_rel, r_ = r_, proportion = proportion, taxonvar = taxonvar, numyears = numyears, cv_abund = cv_abund)
        abund = data.frame(tax_abund[[1]])
        
        #save lognorm distributions
        ln_df = tax_abund[[2]]
        ln_df$SimNum = rep(sim)
        
        lognorm_df_fin = rbind(lognorm_df_fin, ln_df)
        
        # reformat
        colnames(abund) <- c(1:numyears)
        abund$Diversity = site.table$Diversity
        abund = abund %>%
          melt(id.vars = 'Diversity', variable.name = 'Year', value.name = 'Abundance')
        
        for (wind in 1:length(sample.windows)){
          samplen = sample.windows[wind]
          
          # subset abundances
          sub_ab = subset(abund, Year %in% c(10 + 1:samplen))
          
          ### model with means ###
          mean_ab = sub_ab %>%
            group_by(Diversity) %>%
            summarise(mean_abund = mean(Abundance))
          
          # normalize
          div_mean_norm = (mean_ab$Diversity - min(mean_ab$Diversity)) / (max(mean_ab$Diversity) - min(mean_ab$Diversity))
          samp_mean_norm = (mean_ab$mean_abund - min(mean_ab$mean_abund)) / (max(mean_ab$mean_abund) - min(mean_ab$mean_abund))
          
          # regression
          mod_mean = lm(div_mean_norm~samp_mean_norm)
          rm(div_mean_norm,samp_mean_norm)
          
          # regression values
          slope = mod_mean$coefficients[2]
          pval = summary(mod_mean)$coefficients[8]
          rsq = summary(mod_mean)$r.squared
          stderr = summary(mod_mean)$coefficients[4]
          if(pval>0.05){pbin = 0} else {pbin = 1}
          
          # save in data frame
          newrow_means = data.frame("PV" = taxonvar,
                                    "Slope" = slope,
                                    "Sample.Length" = samplen,
                                    "SimNum" = sim,
                                    "P_value" = pval,
                                    "R_squared" = rsq,
                                    "Std_error" = stderr,
                                    "p_val_bin" = pbin,
                                    "SiteNum" = sitenum
          )
          full_df = rbind(full_df, newrow_means)
        }
      }
    }
  }
  rownames(full_df) <- NULL
  return(list(full_df[-1,], lognorm_df_fin[-1,]))
}

# different from above (notice _)
# returns both the community abundance over time and the log-normal distribution
# used at each site. This function is only used by do_sims_lnorm_range
TaxonAbund_ = function(site.table, fixed_rel, r_ = 1, proportion = 1, taxonvar = 1, numyears, cv_abund){
  # site.table = table of sites with productivity and diversity values
  # r_ = average log intrinsic growth rate for Ricker model
  # taxonvar = population variability for Ricker model
  # numyears = number of years of simulated abundance
  # cv_abund = steepness of log-normal distribution of abundance among species
  
  
  compabund = array(c(NA), dim = c(nrow(site.table),numyears))
  
  lognorm_df = data.frame("Sp_no" = NA, "Diversity" = NA, "Init_abund" = NA, "SiteNum" = NA, "PV" = NA, "CV_abund" = NA)
  
  for (site in 1:nrow(site.table)){
    
    div = site.table$Diversity[site]
    prod = site.table$Productivity[site]
    lognormres = LogNorm(diversity =div, productivity = prod, fixed_rel = fixed_rel, proportion = proportion, cv_abund = cv_abund, sad_type = 'lnorm', fix_s_sim = TRUE)
    abundarr = array(c(NA), dim = c(div, numyears))
    
    #save lognormres
    new_df = data.frame("Sp_no" = 1:div, 
                        "Diversity" = rep(div), 
                        "Init_abund" = lognormres, 
                        "SiteNum" = rep(nrow(site.table)), 
                        "PV" = rep(taxonvar), 
                        "CV_abund" = rep(cv_abund))
    
    lognorm_df = rbind(lognorm_df, new_df)
    
    # generating the time series of abundances for a single site
    for (sp in 1:length(lognormres)){
      ave = lognormres[sp]
      abundarr[sp,] = RickerYears(N_0 = ave, r = r_, K = ave, PV = taxonvar, numyears = numyears)
      
    }
    compabund[site,] = colSums(abundarr)
    rm(abundarr)
    
  }
  return(list(compabund, lognorm_df[-1,]))
}






###### HIGHER DIVERSITY SIMULATIONS ######
  
  #all functions are shared




















