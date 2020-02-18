### PLOTTING FILE ###
library(ggplot2)
library(dplyr)

###### MAIN SIMS ######
main_df = read.csv('main_sim_results.csv')

# detected slopes
plot_samps = c(1,2,5,10,40)     # which sample lengths you want to plot

main_df %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  ggplot(aes(x = PV, y = Slope, group = PV, colour = as.factor(p_val_bin))) +
    geom_jitter(show.legend = FALSE) +
    facet_grid(site_num_char ~ samp_len_char)+
    geom_abline(intercept = 1, slope = 0) +
    theme_bw() +
    scale_color_manual(values = c('red', 'black')) +
    xlab("Population Variability") +
    ylab("Detected Slope") 
  

# proportion significant

main_df %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(Sample.Length, PV, SiteNum, samp_len_char, site_num_char) %>%
  summarise(prop_sig = mean(p_val_bin)) %>%
  ggplot(aes(x = PV, y = prop_sig, group = Sample.Length, colour = Sample.Length)) +
    geom_line() +
    facet_grid(site_num_char ~ .) +
    theme_bw() +
    xlab('Population Variability') +
    ylab('Proportion Significant') 


# median r-squared

main_df %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(PV, Sample.Length, SiteNum, samp_len_char, site_num_char) %>%
  summarise(median_rsq = median(R_squared)) %>%
  ggplot() +
    geom_line(aes(x = PV, y = median_rsq, group = Sample.Length, colour = Sample.Length)) +
    theme_bw() +
    xlab('Population Variability') +
    ylab(expression('Median R'^2)) +
    ylim(c(0,1)) +
    facet_grid(site_num_char ~ .)








###### NON-CONSECUTIVE SAMPLING ######
nonconsec_df = read.csv("nonconsec_sim_results.csv")

one_year_samps = c('1')
two_year_samps = c('1_2', '1_3', '1_4', '1_5')
three_year_samps = c('1_2_3', '1_3_5', '1_5_10')
four_year_samps = c('1_2_3_4', '1_4_7_10')
five_year_samps = c('1_2_3_4_5')
ten_year_samps = c('1_2_3_4_5_6_7_8_9_10')


# proportion significant

nonconsec_df %>%
  mutate(num_years = case_when(
    Sample.Years %in% one_year_samps ~ '1',
    Sample.Years %in% two_year_samps ~ '2',
    Sample.Years %in% three_year_samps ~ '3',
    Sample.Years %in% four_year_samps ~ '4',
    Sample.Years %in% five_year_samps ~ '5',
    Sample.Years %in% ten_year_samps ~ '10'
  )) %>%
  group_by(Sample.Years, PV, SiteNum, num_years) %>%
  summarise(prop_sig = mean(p_val_bin)) %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(num_years_char = as.factor(paste0(num_years, ' year'))) %>%
  mutate(num_years_char = factor(num_years_char, levels = c('10 year', '5 year', '4 year', '3 year', '2 year', '1 year'))) %>%
  ggplot(aes(x = PV, y = prop_sig, group = Sample.Years, colour = num_years_char)) +
    geom_line() +
    facet_grid(site_num_char ~ .) +
    theme_bw() +
    xlab("Population Variability") +
    ylab("Proportion Significant") +
    labs(color = "Number of Years") +
    scale_color_grey(start = 0.1, end = 0.7)



# median r-squared
nonconsec_df %>%
  mutate(num_years = case_when(
    Sample.Years %in% one_year_samps ~ '1',
    Sample.Years %in% two_year_samps ~ '2',
    Sample.Years %in% three_year_samps ~ '3',
    Sample.Years %in% four_year_samps ~ '4',
    Sample.Years %in% five_year_samps ~ '5',
    Sample.Years %in% ten_year_samps ~ '10'
  )) %>%
  group_by(Sample.Years, PV, SiteNum, num_years) %>%
  summarise(medrsq = median(R_squared)) %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(num_years_char = as.factor(paste0(num_years, ' year'))) %>%
  mutate(num_years_char = factor(num_years_char, levels = c('10 year', '5 year', '4 year', '3 year', '2 year', '1 year'))) %>%
  ggplot(aes(x = PV, y = medrsq, group = Sample.Years, colour = num_years_char)) +
    geom_line() +
    facet_grid(site_num_char ~ .) +
    theme_bw() +
    xlab("Population Variability") +
    ylab(expression('Median R'^2)) +
    labs(color = "Number of Years") +
    scale_color_grey(start = 0.1, end = 0.7)









###### RANDOM FIXED RELATIONSHIP ######
randrel_df = read.csv("randrel_sim_results.csv")
plot_samps = c(1,2,5,10,40) # which sample lengths you want to plot

# slopes
randrel_df %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  ggplot() +
    geom_jitter(aes(x = PV, y = Slope, group = PV, colour = as.factor(p_val_bin)), show.legend = FALSE)+
    facet_grid(site_num_char ~ samp_len_char)+
    geom_abline(intercept = 1, slope = 0) +
    theme_bw() +
    scale_color_manual(values = c('red', 'black')) +
    xlab("Population Variability") +
    ylab("Detected Slope")



# proportion significant
randrel_df %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(PV, Sample.Length, SiteNum, site_num_char) %>%
  summarise(propsig = mean(p_val_bin)) %>%
  ggplot(aes(x = PV, y = propsig, group = Sample.Length, colour = Sample.Length)) +
    geom_line() +
    facet_grid(site_num_char ~ .) +
    theme_bw() +
    xlab("Population Variability") +
    ylab("Proportion Significant") +
    labs(color = "Number of Years") +
    ylim(c(0,1))

# median r-squared

randrel_df %>%
  mutate(site_num_char = as.factor(case_when(
    SiteNum == 18 ~ '18 sites',
    SiteNum == 36 ~ '36 sites',
    SiteNum == 54 ~ '54 sites'
  ))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(PV, Sample.Length, SiteNum, site_num_char) %>%
  summarise(median_rsq = median(R_squared)) %>%
  ggplot(aes(x = PV, y = median_rsq, group = Sample.Length, colour = Sample.Length)) +
    geom_line() +
    facet_grid(site_num_char ~ .) +
    theme_bw() +
    xlab("Population Variability") +
    ylab(expression("Median R"^2)) +
    labs(color = "Number of Years") +
    ylim(c(0,1))



###### VARYING COMMUNITY EVENNESS ######

cv1 = read.csv("cv1_results.csv") %>% cbind('cv_abund' = rep(1))
cv4 = read.csv("cv4_results.csv") %>% cbind('cv_abund' = rep(4))
cv8 = read.csv("cv8_results.csv") %>% cbind('cv_abund' = rep(8))

cv_all = rbind(cv1, cv4, cv8)

plot_samps = c(1,2,5,10,40) #sample durations to be plotted

# slope
cv_all %>%
  mutate(cv_abund_char = as.factor(paste0('cv_abund=',cv_abund))) %>%
  mutate(cv_abund_char = factor(cv_abund_char, levels = c('cv_abund=1', 'cv_abund=4', 'cv_abund=8'))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  ggplot(aes(x = PV, y = Slope, group = PV, colour = as.factor(p_val_bin))) +
  geom_jitter(show.legend = FALSE) +
  facet_grid(cv_abund_char ~ samp_len_char)+
  geom_abline(intercept = 1, slope = 0) +
  theme_bw() +
  scale_color_manual(values = c('red', 'black')) +
  xlab("Population Variability") +
  ylab("Detected Slope") 

#proportion significant
cv_all %>%
  mutate(cv_abund_char = as.factor(paste0('cv_abund=',cv_abund))) %>%
  mutate(cv_abund_char = factor(cv_abund_char, levels = c('cv_abund=1', 'cv_abund=4', 'cv_abund=8'))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(Sample.Length, PV, SiteNum, samp_len_char, cv_abund_char) %>%
  summarise(prop_sig = mean(p_val_bin)) %>%
  ggplot(aes(x = PV, y = prop_sig, group = Sample.Length, colour = Sample.Length)) +
  geom_line() +
  facet_grid(cv_abund_char ~ .) +
  theme_bw() +
  xlab('Population Variability') +
  ylab('Proportion Significant')

#median r-squared
cv_all %>% 
  mutate(cv_abund_char = as.factor(paste0('cv_abund=',cv_abund))) %>%
  mutate(cv_abund_char = factor(cv_abund_char, levels = c('cv_abund=1', 'cv_abund=4', 'cv_abund=8'))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(Sample.Length, PV, SiteNum, samp_len_char, cv_abund_char) %>%
  summarise(median_rsq = median(R_squared)) %>%
  ggplot() +
  geom_line(aes(x = PV, y = median_rsq, group = Sample.Length, colour = Sample.Length)) +
  theme_bw() +
  xlab('Population Variability') +
  ylab(expression('Median R'^2)) +
  ylim(c(0,1)) +
  facet_grid(cv_abund_char ~ .)


# looking at the log-normal distributions
cv1_ln = read.csv("cv1_log_norm_results.csv")
cv4_ln = read.csv("cv4_log_norm_results.csv")
cv8_ln = read.csv("cv8_log_norm_results.csv")

cv_all_ln = rbind(cv1_ln, cv4_ln, cv8_ln)

sim_nums_to_keep = sample(1:max(cv_all_ln$SimNum), size = 10, replace = FALSE) #only show random sample for cleanliness
pv_show = 0.01 # choose a PV value to show the log-norm plots for (doesn't matter since log-norm distribution does not depend on PV just needs to be chosen)
div_show = c(20) # choose the diversity (aka which site) you want to see the log-norm distribution for

cv_all_ln %>%
  mutate(cv_abund_char = as.factor(paste0('cv_abund=',CV_abund))) %>%
  mutate(cv_abund_char = factor(cv_abund_char, levels = c('cv_abund=1', 'cv_abund=4', 'cv_abund=8'))) %>%
  filter(PV==pv_show, Diversity %in% div_show, SimNum %in% sim_nums_to_keep) %>%
  ggplot() +
  geom_line(aes(x = Sp_no, y = Init_abund, group = SimNum)) +
  facet_grid(cv_abund_char ~ .) +
  xlab("Species rank") +
  ylab("Initial abundance") +
  theme_bw()





###### HIGHER DIVERSITY SIMULATIONS ######

div20 = read.csv("div20_results.csv")
div100 = read.csv("div100_results.csv")
div500 = read.csv("div500_results.csv")

div_all = rbind(div20, div100, div500)

plot_samps = c(1,2,5,10,40) #sample durations to be plotted

#slopes
div_all %>%
  mutate(max_div_char = as.factor(paste0('Max. richness=',max_div))) %>%
  mutate(max_div_char = factor(max_div_char, levels = c('Max. richness=20', 'Max. richness=100', 'Max. richness=500'))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  ggplot(aes(x = PV, y = Slope, group = PV, colour = as.factor(p_val_bin))) +
  geom_jitter(show.legend = FALSE) +
  facet_grid(max_div_char ~ samp_len_char)+
  geom_abline(intercept = 1, slope = 0) +
  theme_bw() +
  scale_color_manual(values = c('red', 'black')) +
  xlab("Population Variability") +
  ylab("Detected Slope")


#proportion significant
div_all %>%
  mutate(max_div_char = as.factor(paste0('Max. richness=',max_div))) %>%
  mutate(max_div_char = factor(max_div_char, levels = c('Max. richness=20', 'Max. richness=100', 'Max. richness=500'))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(Sample.Length, PV, SiteNum, samp_len_char, max_div_char) %>%
  summarise(prop_sig = mean(p_val_bin)) %>%
  ggplot(aes(x = PV, y = prop_sig, group = Sample.Length, colour = Sample.Length)) +
  geom_line() +
  facet_grid(max_div_char ~ .) +
  theme_bw() +
  xlab('Population Variability') +
  ylab('Proportion Significant')

#median r-squared
div_all %>%
  mutate(max_div_char = as.factor(paste0('Max. richness=',max_div))) %>%
  mutate(max_div_char = factor(max_div_char, levels = c('Max. richness=20', 'Max. richness=100', 'Max. richness=500'))) %>%
  mutate(samp_len_char = as.factor(paste0(Sample.Length, ' year'))) %>%
  mutate(samp_len_char = factor(samp_len_char, levels = c('1 year', '2 year', '3 year', '4 year', '5 year', '7 year', '10 year', '15 year', '20 year', '30 year', '40 year'))) %>%
  filter(Sample.Length %in% plot_samps) %>%
  group_by(Sample.Length, PV, SiteNum, samp_len_char, max_div_char) %>%
  summarise(median_rsq = median(R_squared)) %>%
  ggplot() +
  geom_line(aes(x = PV, y = median_rsq, group = Sample.Length, colour = Sample.Length)) +
  theme_bw() +
  xlab('Population Variability') +
  ylab(expression('Median R'^2)) +
  ylim(c(0,1)) +
  facet_grid(max_div_char ~ .)



