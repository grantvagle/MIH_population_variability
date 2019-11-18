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




