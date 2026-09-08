################################################################################
################################## Table E.2 ###################################
################################################################################

################################################################################
################## Prepare data set for DAC and MDB creditors ##################
################################################################################

# Call data set
data_df_dac_mdbs <- read_excel("Analysis/master_data_dac_MDBs_only.xlsx") # use data set with only DAC and MDBs

# Create log of GDP p.c. variable
data_df_dac_mdbs <- data_df_dac_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(log_gdp_pc = log(gdp_pc_const_usd + 0.0001)) %>%
  ungroup()

# Create variables of interest
data_df_dac_mdbs <- data_df_dac_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    share_trad_cred = 
      if_else(total_longterm_ppg_debt > 0, 
              round(longterm_trad_cred_debt / total_longterm_ppg_debt, 3), 0)
    ) %>%
  ungroup()  
  

# Normalize fx_perc_change (min-max, within-country)
data_df_dac_mdbs <- data_df_dac_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    c_min = suppressWarnings(min(fx_perc_change, na.rm = TRUE)), 
    c_max = suppressWarnings(max(fx_perc_change, na.rm = TRUE)),
    
    fx_perc_change_norm = case_when(
      is.na(fx_perc_change) ~ NA_real_,         
      is.infinite(c_min) ~ NA_real_,            
      c_min == c_max ~ 0,                       
      TRUE ~ (fx_perc_change - c_min) / (c_max - c_min)
    )
  ) %>%
  dplyr::select(-c_min, -c_max) %>%
  ungroup()

# Create lagged variables
data_df_dac_mdbs <- data_df_dac_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    ihme_health_exp_gdp_lag1 = dplyr::lag(ihme_health_exp_gdp, 1),
    owid_educ_exp_perc_gdp_lag1 = dplyr::lag(owid_educ_exp_perc_gdp, 1),
    share_trad_cred_lag1 = dplyr::lag(share_trad_cred, 1),
    share_trad_cred_lag2 = dplyr::lag(share_trad_cred, 2),
    log_gdp_pc_lag1 = dplyr::lag(log_gdp_pc, 1),
    dep_ratio_lag1 = dplyr::lag(dep_ratio, 1),
    urb_pop_lag1 = dplyr::lag(urb_pop, 1),
    trade_openness_pwt_lag1 = dplyr::lag(trade_openness_pwt, 1),
    gov_bal_lag1 = dplyr::lag(gov_bal_perc_gdp, 1),
    oda_perc_gni_lag1 = dplyr::lag(oda_perc_gni, 1),
    fx_perc_change_norm_lag1 = dplyr::lag(fx_perc_change_norm, 1),
    infl_avg_lag1 = dplyr::lag(infl_avg, 1),
    war_lag1 = as.factor(dplyr::lag(war, 1)),
    political_rights_lag1 = as.factor(dplyr::lag(political_rights, 1)),
    cap_acc_openness_lag1 = dplyr::lag(cap_acc_openness, 1),
    imf_active_lag1 = as.factor(dplyr::lag(imf_active, 1)),
    gdp_growth_lag1 = dplyr::lag(gdp_growth, 1),
    avg_int_rate_lag1 = dplyr::lag(avg_int_rate, 1),
    us_int_rate_lag2 = dplyr::lag(us_int_rate, 2)
    ) %>%
  ungroup()

# Create first part of IV: rolling average of traditional creditor share up to t-2
data_df_dac_mdbs <- data_df_dac_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    share_trad_cred_run_avg_lag2 = slide_index_dbl(
      .x = share_trad_cred, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = Inf, 
      .after = -2
    )
  ) %>%
  mutate(
    iv_interaction = share_trad_cred_run_avg_lag2*us_int_rate_lag2
    ) %>%
  ungroup()


# Define samples to be used 
data_df_dac_mdbs <- data_df_dac_mdbs %>%
  left_join(inc_groups, by = c("year" = "Year", "debtor_country" = "Code"))

data_df_dac_mdbs <- data_df_dac_mdbs %>% 
  mutate(is_constant_sample_health = 
           complete.cases(across(all_of(main_vars_health))),
         is_constant_sample_educ = 
           complete.cases(across(all_of(main_vars_educ)))) 

data_df_dac_mdbs_mic <- data_df_dac_mdbs %>%
  group_by(debtor_country) %>%
  mutate(
    low_inc_share = mean(inc_status[year >= 2013] == "Low-income countries", na.rm = TRUE)
  ) %>%
  filter(low_inc_share <= 0.5) %>%
  ungroup() 


################################################################################
############### Prepare data set for Paris Club and MDB creditors ##############
################################################################################

# Call data set
data_df_pc_mdbs <- read_excel("Analysis/master_data_pc_MDBs.xlsx") # use data set with only PC and MDBs

# Create log of GDP p.c. variable
data_df_pc_mdbs <- data_df_pc_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(log_gdp_pc = log(gdp_pc_const_usd + 0.0001)) %>%
  ungroup()

# Create variables of interest
data_df_pc_mdbs <- data_df_pc_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    share_trad_cred = 
      if_else(total_longterm_ppg_debt > 0, 
              round(longterm_trad_cred_debt / total_longterm_ppg_debt, 3), 0)
  ) %>%
  ungroup()  


# Normalize fx_perc_change (min-max, within-country)
data_df_pc_mdbs <- data_df_pc_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    c_min = suppressWarnings(min(fx_perc_change, na.rm = TRUE)), 
    c_max = suppressWarnings(max(fx_perc_change, na.rm = TRUE)),
    
    fx_perc_change_norm = case_when(
      is.na(fx_perc_change) ~ NA_real_,         
      is.infinite(c_min) ~ NA_real_,            
      c_min == c_max ~ 0,                       
      TRUE ~ (fx_perc_change - c_min) / (c_max - c_min)
    )
  ) %>%
  dplyr::select(-c_min, -c_max) %>%
  ungroup()

# Create lagged variables
data_df_pc_mdbs <- data_df_pc_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    ihme_health_exp_gdp_lag1 = dplyr::lag(ihme_health_exp_gdp, 1),
    owid_educ_exp_perc_gdp_lag1 = dplyr::lag(owid_educ_exp_perc_gdp, 1),
    share_trad_cred_lag1 = dplyr::lag(share_trad_cred, 1),
    share_trad_cred_lag2 = dplyr::lag(share_trad_cred, 2),
    log_gdp_pc_lag1 = dplyr::lag(log_gdp_pc, 1),
    dep_ratio_lag1 = dplyr::lag(dep_ratio, 1),
    urb_pop_lag1 = dplyr::lag(urb_pop, 1),
    trade_openness_pwt_lag1 = dplyr::lag(trade_openness_pwt, 1),
    gov_bal_lag1 = dplyr::lag(gov_bal_perc_gdp, 1),
    oda_perc_gni_lag1 = dplyr::lag(oda_perc_gni, 1),
    fx_perc_change_norm_lag1 = dplyr::lag(fx_perc_change_norm, 1),
    infl_avg_lag1 = dplyr::lag(infl_avg, 1),
    war_lag1 = as.factor(dplyr::lag(war, 1)),
    political_rights_lag1 = as.factor(dplyr::lag(political_rights, 1)),
    cap_acc_openness_lag1 = dplyr::lag(cap_acc_openness, 1),
    imf_active_lag1 = as.factor(dplyr::lag(imf_active, 1)),
    gdp_growth_lag1 = dplyr::lag(gdp_growth, 1),
    avg_int_rate_lag1 = dplyr::lag(avg_int_rate, 1),
    us_int_rate_lag2 = dplyr::lag(us_int_rate, 2)
  ) %>%
  ungroup()

# Create first part of IV: rolling average of traditional creditor share up to t-2
data_df_pc_mdbs <- data_df_pc_mdbs %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    share_trad_cred_run_avg_lag2 = slide_index_dbl(
      .x = share_trad_cred, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = Inf, 
      .after = -2
    )
  ) %>%
  mutate(
    iv_interaction = share_trad_cred_run_avg_lag2*us_int_rate_lag2
  ) %>%
  ungroup()


## Define samples to be used 
data_df_pc_mdbs <- data_df_pc_mdbs %>%
  left_join(inc_groups, by = c("year" = "Year", "debtor_country" = "Code"))

data_df_pc_mdbs <- data_df_pc_mdbs %>% 
  mutate(is_constant_sample_health = 
           complete.cases(across(all_of(main_vars_health))),
         is_constant_sample_educ = 
           complete.cases(across(all_of(main_vars_educ)))) 

data_df_pc_mdbs_mic <- data_df_pc_mdbs %>%
  group_by(debtor_country) %>%
  mutate(
    low_inc_share = mean(inc_status[year >= 2013] == "Low-income countries", na.rm = TRUE)
  ) %>%
  filter(low_inc_share <= 0.5) %>%
  ungroup() 


################################################################################
############################## Repeat IV analyses ##############################
################################################################################

data_df_dac_mdbs_mic_plm <- pdata.frame(data_df_dac_mdbs_mic, index = c("debtor_country", "year"))
data_df_pc_mdbs_mic_plm <- pdata.frame(data_df_pc_mdbs_mic, index = c("debtor_country", "year"))

est_iv1.19 = feols(ihme_health_exp_gdp ~ # DAC + MDBs only
                     share_trad_cred_run_avg_lag2 +
                     log_gdp_pc_lag1 + 
                     gdp_growth_lag1 +
                     cap_acc_openness_lag1 +
                     dep_ratio_lag1 + 
                     urb_pop_lag1 +
                     political_rights_lag1 +
                     war_lag1 + 
                     trade_openness_pwt_lag1 + 
                     fx_perc_change_norm_lag1 +
                     infl_avg_lag1 | 
                     debtor_country + year | 
                     share_trad_cred_lag1 ~ 
                     iv_interaction, 
                   data = data_df_dac_mdbs_mic_plm, 
                   subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.19 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     log_gdp_pc_lag1 + 
                     gdp_growth_lag1 +
                     cap_acc_openness_lag1 +
                     dep_ratio_lag1 + 
                     urb_pop_lag1 +
                     political_rights_lag1 +
                     war_lag1 + 
                     trade_openness_pwt_lag1 + 
                     fx_perc_change_norm_lag1 +
                     infl_avg_lag1 |
                     debtor_country + year | 
                     share_trad_cred_lag1 ~ 
                     iv_interaction, 
                   data = data_df_dac_mdbs_mic_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv1.20 = feols(ihme_health_exp_gdp ~ # Paris Club + MDBs only
                     share_trad_cred_run_avg_lag2 +
                     log_gdp_pc_lag1 + 
                     gdp_growth_lag1 +
                     cap_acc_openness_lag1 +
                     dep_ratio_lag1 + 
                     urb_pop_lag1 +
                     political_rights_lag1 +
                     war_lag1 + 
                     trade_openness_pwt_lag1 + 
                     fx_perc_change_norm_lag1 +
                     infl_avg_lag1 | 
                     debtor_country + year | 
                     share_trad_cred_lag1 ~ 
                     iv_interaction, 
                   data = data_df_pc_mdbs_mic_plm, 
                   subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.20 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     log_gdp_pc_lag1 + 
                     gdp_growth_lag1 +
                     cap_acc_openness_lag1 +
                     dep_ratio_lag1 + 
                     urb_pop_lag1 +
                     political_rights_lag1 +
                     war_lag1 + 
                     trade_openness_pwt_lag1 + 
                     fx_perc_change_norm_lag1 +
                     infl_avg_lag1 |
                     debtor_country + year | 
                     share_trad_cred_lag1 ~ 
                     iv_interaction, 
                   data = data_df_pc_mdbs_mic_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_iv1.19, est_iv1.20, est_iv2.19, est_iv2.20, 
       keep = "%fit_share_trad_cred_lag1",
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share"),
       tex = TRUE,
       title = "Baseline IV results, borrowing from DAC members and traditional MDBs and from members of the Paris Club and traditional MDBs, middle-income countries", 
       label = "tab:results_baseline_iv_overall_dacs+mdbs_only_other",
       file = "Analysis/MIC_results/IV_tables/iv_robustness_alt_def_loc.tex", 
       replace = TRUE)
