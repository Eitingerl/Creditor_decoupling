################################################################################
############################ Table B.3: Reduced form ###########################
################################################################################

est_iv1.4 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     us_int_rate_lag2 +
                     iv_interaction +
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
                     debtor_country + year, 
                   data = data_df_plm, subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.4 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     us_int_rate_lag2 +
                     iv_interaction +
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
                     debtor_country + year, 
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_iv1.4, est_iv2.4,
       fitstat = ~ n + ar2 + bic,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share",
                "share_trad_cred_run_avg_lag2" = "Hist. reliance on LD",
                "log_gdp_pc_lag1" = "Log(GDP p.c.)",
                "gdp_growth_lag1" = "GDP growth",
                "iv_interaction" =
                  "Hist. reliance on LD x FFR",
                "dep_ratio_lag1" = "Dependency ratio",
                "urb_pop_lag1" = "Urbanization rate",
                "trade_openness_pwt_lag1" = "Trade openness",
                "fx_perc_change_norm_lag1" = "Perc. change in FX rate",
                "political_rights_lag12" = "Political rights = 2",
                "political_rights_lag13" = "Political rights = 3",
                "political_rights_lag14" = "Political rights = 4",
                "political_rights_lag15" = "Political rights = 5",
                "political_rights_lag16" = "Political rights = 6",
                "political_rights_lag17" = "Political rights = 7",
                "cap_acc_openness_lag1" = "Capital account openness",
                "war_lag11" = "War",
                "infl_avg_lag1" = "Average inflation rate"),
       title = "IV specification reduced form, middle-income countries",
       label = "tab:IV_reduced_form_other",
       tex = TRUE,  
       file = "Analysis/MIC_results/IV_tables/IV_reduced_form.tex", 
       replace = TRUE)


################################################################################
############### Table B.5: Time breaks around MDGs, MDRI and SDGs ##############
################################################################################

est_iv1.5 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     I(share_trad_cred_run_avg_lag2*MDG) +
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
                     share_trad_cred_lag1 + I(share_trad_cred_lag1*MDG) ~ 
                     iv_interaction +
                     I(share_trad_cred_run_avg_lag2*us_int_rate_lag2*MDG), 
                   data = data_df_plm, subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv1.6 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     I(share_trad_cred_run_avg_lag2*SDG) +
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
                     share_trad_cred_lag1 + I(share_trad_cred_lag1*SDG) ~ 
                     iv_interaction +
                     I(share_trad_cred_run_avg_lag2*us_int_rate_lag2*SDG), 
                   data = data_df_plm, subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv1.7 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     I(share_trad_cred_run_avg_lag2*MDRI) +
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
                     share_trad_cred_lag1 + I(share_trad_cred_lag1*MDRI) ~ 
                     iv_interaction +
                     I(share_trad_cred_run_avg_lag2*us_int_rate_lag2*MDRI), 
                   data = data_df_plm, subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.5 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     I(share_trad_cred_run_avg_lag2*MDG) +
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
                     share_trad_cred_lag1 + I(share_trad_cred_lag1*MDG) ~ 
                     iv_interaction +
                     I(share_trad_cred_run_avg_lag2*us_int_rate_lag2*MDG),
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.6 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     I(share_trad_cred_run_avg_lag2*SDG) +
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
                     share_trad_cred_lag1 + I(share_trad_cred_lag1*SDG) ~ 
                     iv_interaction +
                     I(share_trad_cred_run_avg_lag2*us_int_rate_lag2*SDG),
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.7 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     I(share_trad_cred_run_avg_lag2*MDRI) +
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
                     share_trad_cred_lag1 + I(share_trad_cred_lag1*MDRI) ~ 
                     iv_interaction +
                     I(share_trad_cred_run_avg_lag2*us_int_rate_lag2*MDRI),
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_iv1.5, est_iv1.6, est_iv1.7, est_iv2.5, est_iv2.6, est_iv2.7,
       keep = c("%fit_share_trad_cred_lag1", 
                "%fit_I\\(share_trad_cred_lag1\\*MDG\\)",
                "%fit_I\\(share_trad_cred_lag1\\*SDG\\)",
                "%fit_I\\(share_trad_cred_lag1\\*MDRI\\)"), 
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share",
                "I(share_trad_cred_lag1 * MDG)" = "LD share after MDG",
                "I(share_trad_cred_lag1 * SDG)" = "LD share after SDG",
                "I(share_trad_cred_lag1 * MDRI)" = "LD share after MDRI"),
       tex = TRUE,
       title = "Potential time breaks, middle-income countries",
       label = "tab:results_iv_time_breaks_other",
       file = "Analysis/MIC_results/IV_tables/results_iv_time_breaks.tex", 
       replace = TRUE)


################################################################################
###################### Table B.6: Private creditor share #######################
################################################################################

est_iv1.8 = feols(ihme_health_exp_gdp ~ 
                     share_prvt_cred_run_avg_lag2 +
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
                     share_prvt_cred_lag1 ~ 
                     iv_interaction_prvt, 
                   data = data_df_plm, subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.8 = feols(owid_educ_exp_perc_gdp ~ 
                     share_prvt_cred_run_avg_lag2 +
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
                     share_prvt_cred_lag1 ~ 
                     iv_interaction_prvt, 
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)


ar_prvt_health <- AR_test( 
  data = subset(data_df_plm, is_constant_sample_health),
  Y = "ihme_health_exp_gdp",
  D = "share_prvt_cred_lag1",
  Z = "iv_interaction_prvt",
  controls = c("share_prvt_cred_run_avg_lag2", "log_gdp_pc_lag1", 
               "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
               "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
               "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
               "infl_avg_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)

ar_prvt_educ <- AR_test( 
  data = subset(data_df_plm, is_constant_sample_educ),
  Y = "owid_educ_exp_perc_gdp",
  D = "share_prvt_cred_lag1",
  Z = "iv_interaction_prvt",
  controls = c("share_prvt_cred_run_avg_lag2", "log_gdp_pc_lag1", 
               "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
               "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
               "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
               "infl_avg_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)

ar_prvt_custom_rows <- list(
  "AR F-Statistic" = c(
    round(ar_prvt_health$Fstat["F"], 2),
    round(ar_prvt_educ$Fstat["F"], 2)
  ),
  "AR p-value" = c(
    round(ar_prvt_health$Fstat["p"], 3),
    round(ar_prvt_educ$Fstat["p"], 3)
  ),
  "AR Robust 95% CI" = c(
    ar_prvt_health$ci.print,
    ar_prvt_educ$ci.print
  )
)

etable(est_iv1.8, est_iv2.8, 
       keep = "%share_prvt_cred_lag1", 
       extralines = ar_prvt_custom_rows,
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_prvt_cred_lag1" = "Private creditor share"
       ),
       tex = TRUE,
       title = "IV results for private creditor share, middle-income countries",
       label = "tab:results_baseline_iv_overall_prvt_cred_other",
       file = "Analysis/MIC_results/IV_tables/results_baseline_iv_overall_table_prvt_cred.tex", 
       replace = TRUE)


################################################################################
############# Figure E.1: Randomize instrument values in 1st stage #############
################################################################################

# Subset the data first to save time inside the loop
data_perm_h <- data_df_plm %>% 
  filter(is_constant_sample_health == TRUE) %>%
  mutate(share_calc = share_trad_cred_run_avg_lag2)

data_perm_e <- data_df_plm %>% 
  filter(is_constant_sample_educ == TRUE) %>%
  mutate(share_calc = share_trad_cred_run_avg_lag2)

# Extract unique years and corresponding FFR
unique_shifts_h <- data_perm_h %>%
  distinct(year, us_int_rate_lag2)

unique_shifts_e <- data_perm_e %>%
  distinct(year, us_int_rate_lag2)

# Prepare for the loop
n_perms <- 1000 # set number of repetitions
perm_coefs_h <- numeric(n_perms) 
perm_coefs_e <- numeric(n_perms) 
true_coef_h <- numeric(1)
true_coef_e <- numeric(1)        

set.seed(545) 

# Permutation loop
for(i in 1:n_perms) {
  
  # Randomly shuffle the FFR across years and store in new column
  shuffled_shifts_h <- unique_shifts_h %>%
    mutate(shuffled_rate_h = sample(us_int_rate_lag2, replace = FALSE)) %>%
    dplyr::select(year, shuffled_rate_h) # only keep year and new random FFR values
  
  shuffled_shifts_e <- unique_shifts_e %>%
    mutate(shuffled_rate_e = sample(us_int_rate_lag2, replace = FALSE)) %>%
    dplyr::select(year, shuffled_rate_e)
  
  # Merge back into the panel and rebuild the IV
  temp_data_h <- data_perm_h %>%
    left_join(shuffled_shifts_h, by = "year") %>%
    mutate(iv_shuffled_h = share_calc * shuffled_rate_h)
  
  temp_data_e <- data_perm_e %>%
    left_join(shuffled_shifts_e, by = "year") %>%
    mutate(iv_shuffled_e = share_calc * shuffled_rate_e)
  
  # Re-run model with shuffled instrument
  est_h <- feols(share_trad_cred_lag1 ~ 
                   iv_shuffled_h +
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
                   debtor_country + year, 
                 data = temp_data_h, 
                 panel.id = ~debtor_country + year, cluster = ~debtor_country,
                 notes = FALSE) # Suppress notes to speed up the loop
  
  est_e <- feols(share_trad_cred_lag1 ~ 
                   iv_shuffled_e +
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
                   debtor_country + year, 
                 data = temp_data_e, 
                 panel.id = ~debtor_country + year, cluster = ~debtor_country,
                 notes = FALSE)
  
  # Extract and store 1st-stage coefficient on instrument
  perm_coefs_h[i] <- coef(est_h)["iv_shuffled_h"]
  perm_coefs_e[i] <- coef(est_e)["iv_shuffled_e"]
}

# Get true baseline 1st-stage coefficient for comparison
baseline_est_h <- feols(share_trad_cred_lag1 ~ 
                          iv_interaction +
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
                          debtor_country + year,
                        data = data_perm_h, cluster = ~debtor_country)

true_coef_h <- coef(baseline_est_h)["iv_interaction"]

baseline_est_e <- feols(share_trad_cred_lag1 ~ 
                          iv_interaction +
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
                          debtor_country + year,
                        data = data_perm_e, cluster = ~debtor_country)

true_coef_e <- coef(baseline_est_e)["iv_interaction"]

# Plot the Distribution
hist_data_h <- data.frame(coefs_h = perm_coefs_h)
hist_data_e <- data.frame(coefs_e = perm_coefs_e)

ggplot(hist_data_h, aes(x = coefs_h)) +
  geom_density(alpha = 0.7, color = "darkblue") +
  geom_vline(xintercept = true_coef_h, color = "red", linewidth = 0.7) +
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  labs(x = "Coefficient on instrument", y = "Frequency")
ggsave("Analysis/MIC_results/Figures/iv_shuffle_health.png", width = 10, height = 8)

ggplot(hist_data_e, aes(x = coefs_e)) +
  geom_density(alpha = 0.7, color = "darkblue") +
  geom_vline(xintercept = true_coef_e, color = "red", linewidth = 0.7) +
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  labs(x = "Coefficient on instrument", y = "Frequency")
ggsave("Analysis/MIC_results/Figures/iv_shuffle_educ.png", width = 10, height = 8)


################################################################################
########### Table E.1: Legacy debt share in commitments (debt flows) ###########
################################################################################
est_iv1.9 = feols(ihme_health_exp_gdp ~ 
                    share_trad_crd_com_run_avg_lag2 +
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
                    share_trad_cred_commit_lag1 ~ 
                    iv_interaction_commit, 
                  data = data_df_plm, subset = ~ is_constant_sample_health,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)
  
est_iv2.9 = feols(owid_educ_exp_perc_gdp ~ 
                    share_trad_crd_com_run_avg_lag2 +
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
                    share_trad_cred_commit_lag1 ~ 
                    iv_interaction_commit, 
                  data = data_df_plm, subset = ~ is_constant_sample_educ,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_iv1.9, est_iv2.9, 
       keep = "%fit_share_trad_cred_commit_lag1",
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_commit_lag1" = "LD share"),
       tex = TRUE,
       title = "IV results with share of commitments from legacy creditors, middle-income countries", 
       label = "tab:iv_commit_other",
       file = "Analysis/MIC_results/IV_tables/IV_robustness_commit.tex", 
       replace = TRUE)


################################################################################
## Table E.3: Interact oil price and glob. GDP growth & Exp. p.c. as outcome ##
################################################################################

# Expenditure per capita as outcome
est_iv1.10 = feols(log(ihme_health_exp_pc_ppp + 0.0000001) ~ 
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
                  data = data_df_plm, subset = ~ is_constant_sample_health,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.10 = feols(log(owid_educ_exp_pc_ppp + 0.0000001) ~ 
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
                  data = data_df_plm, subset = ~ is_constant_sample_educ,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

# Interact share part with oil price and glob. GDP growth
est_iv1.11 = feols(ihme_health_exp_gdp ~ 
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
                    infl_avg_lag1 +
                    I(share_trad_cred_run_avg_lag2*oil_price_lag1) +
                    I(share_trad_cred_run_avg_lag2*gdp_growth_glob_lag1) | 
                    debtor_country + year | 
                    share_trad_cred_lag1 ~ 
                    iv_interaction, 
                  data = data_df_plm, subset = ~ is_constant_sample_health,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.11 = feols(owid_educ_exp_perc_gdp ~ 
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
                    infl_avg_lag1 +
                    I(share_trad_cred_run_avg_lag2*oil_price_lag1) +
                    I(share_trad_cred_run_avg_lag2*gdp_growth_glob_lag1) | 
                    debtor_country + year | 
                    share_trad_cred_lag1 ~ 
                    iv_interaction, 
                  data = data_df_plm, subset = ~ is_constant_sample_educ,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)


etable(est_iv1.10, est_iv1.11, est_iv2.10, est_iv2.11, 
       keep = c("%fit_share_trad_cred_lag1", 
                "%I\\(share_trad_cred_run_avg_lag2\\*oil_price_lag1\\)",
                "%I\\(share_trad_cred_run_avg_lag2\\*gdp_growth_glob_lag1\\)"
       ),
       fitstat = ~ n + ar2 + bic + ivwald, 
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "log(ihme_health_exp_pc_ppp+0.0000001)" = 
                  "Log(Health spending p.c., PPP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "log(owid_educ_exp_pc_ppp+0.0000001)" = 
                  "Log(Education spending p.c., PPP)",
                "share_trad_cred_lag1" = "LD share",
                "I(share_trad_cred_run_avg_lag2*oil_price_lag1)" =
                  "Hist. reliance on LD x Oil price",
                "I(share_trad_cred_run_avg_lag2*gdp_growth_glob_lag1)" =
                  "Hist. reliance on LD x Global GDP growth"),
       title = "IV specification robustness check: alternative specifications, middle-income countries",
       label = "tab:IV_robustness_alt_specifications_other",
       tex = TRUE,  
       file = "Analysis/MIC_results/IV_tables/IV_robustness_alt_specifications.tex", 
       replace = TRUE)


################################################################################
######################## Table E.5: Alternative samples ########################
################################################################################

# Drop crisis years 
est_iv1.12 = feols(ihme_health_exp_gdp ~ 
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
                  data = data_df_plm, subset = ~ is_constant_sample_health & 
                    !year %in% c(2011, 2020:2022), # drop crisis years
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.12 = feols(owid_educ_exp_perc_gdp ~ 
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
                  data = data_df_plm, subset = ~ is_constant_sample_educ & 
                    !year %in% c(2011, 2020:2022),
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)


# Drop B(R)I(C)S countries
data_df_plm_rob <- data_df_plm %>% 
  filter(!debtor_country %in% c("BRA", "IND", "ZAF")) # China already excluded and Russia missing

est_iv1.13 = feols(ihme_health_exp_gdp ~ 
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
                  data = data_df_plm_rob, subset = ~ is_constant_sample_health,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.13 = feols(owid_educ_exp_perc_gdp ~ 
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
                  data = data_df_plm_rob, subset = ~ is_constant_sample_educ,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_iv1.12, est_iv1.13, est_iv2.12, est_iv2.13,
       keep = "%fit_share_trad_cred_lag1",
       fitstat = ~ n + ar2 + bic + ivwald, 
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share"),
       title = "IV specification robustness check: alternative samples, middle-income countries",
       label = "tab:IV_robustness_sample_changes_other",
       tex = TRUE, 
       file = "Analysis/MIC_results/IV_tables/IV_robustness_sample_changes.tex", 
       replace = TRUE)


################################################################################
################ Table E.6: Only upper-middle-income countries #################
################################################################################

data_df_plm_umic <- pdata.frame(data_df_umic, index = c("debtor_country", "year"))

est_iv1.14 = feols(ihme_health_exp_gdp ~ 
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
                  data = data_df_plm_umic, subset = ~ is_constant_sample_health,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.14 = feols(owid_educ_exp_perc_gdp ~ 
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
                  data = data_df_plm_umic, subset = ~ is_constant_sample_educ,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_iv1.14, est_iv2.14,
       keep = "%fit_share_trad_cred_lag1", 
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share"),
       tex = TRUE,
       title = "Baseline IV results, upper-middle-income countries",
       label = "tab:iv_robustness_umics_only",
       file = "Analysis/MIC_results/IV_tables/IV_robustness_UMICs_only.tex", 
       replace = TRUE)


################################################################################
###### Table E.7: Time-invariant share variable & Condensed rolling average ####
################################################################################

# Time-invariant share variable
est_iv1.15 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_avg + # time-invariant - to address autocorrelation in FFR, gets absorbed by FE
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
                     iv_invar_share_interaction,
                   data = data_df_plm, subset = ~ is_constant_sample_health,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

ar_rob_invar_share_health <- AR_test( 
  data = subset(data_df_plm, is_constant_sample_health),
  Y = "ihme_health_exp_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_invar_share_interaction",
  controls = c("share_trad_cred_run_avg_lag2", "log_gdp_pc_lag1", 
               "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
               "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
               "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
               "infl_avg_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)

est_iv2.15 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_avg +
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
                     iv_invar_share_interaction, 
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

ar_rob_invar_share_educ <- AR_test( 
  data = subset(data_df_plm, is_constant_sample_educ),
  Y = "owid_educ_exp_perc_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_invar_share_interaction",
  controls = c("share_trad_cred_run_avg_lag2", "log_gdp_pc_lag1", 
               "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
               "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
               "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
               "infl_avg_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)


# Condensed share part (rolling average)
est_iv1.16 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_run_avg_cond +
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
                     I(share_trad_cred_run_avg_cond*us_int_rate_lag2), 
                   data = data_df_plm, subset = ~ is_constant_sample_health, 
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.16 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_cond +
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
                     I(share_trad_cred_run_avg_cond*us_int_rate_lag2),
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

ar_custom_rows_invar_share <- list(
  "AR F-Statistic" = c(
    round(ar_rob_invar_share_health$Fstat["F"], 2), 
    "-",
    round(ar_rob_invar_share_educ$Fstat["F"], 2), 
    "-"
  ),
  "AR p-value" = c(
    round(ar_rob_invar_share_health$Fstat["p"], 3), 
    "-",
    round(ar_rob_invar_share_educ$Fstat["p"], 3), 
    "-"
  ),
  "AR Robust 95% CI" = c(
    ar_rob_invar_share_health$ci.print, 
    "-",
    ar_rob_invar_share_educ$ci.print, 
    "-"
  )
)

etable(est_iv1.15, est_iv1.16, est_iv2.15, est_iv2.16,
       keep = c("%share_trad_cred_run_avg_cond", 
                "%fit_share_trad_cred_lag1"), 
       extralines = ar_custom_rows_invar_share,
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share",
                "share_trad_cred_run_avg_cond" = 
                  "Avg. reliance on LD, recent past"),
       tex = TRUE,
       title = "IV results with different specifications of the share part of the IV, middle-income countries",
       label = "tab:results_iv_alt_share_other",
       file = "Analysis/MIC_results/IV_tables/results_iv_alt_share.tex", 
       replace = TRUE)


################################################################################
################## Table E.8: Include longer instrumented lags #################
################################################################################

est_iv1.17 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     share_trad_cred_run_avg_lag3 +
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
                     share_trad_cred_lag1 + share_trad_cred_lag2 ~ 
                     iv_interaction + 
                     I(share_trad_cred_run_avg_lag3*us_int_rate_lag3), 
                   data = data_df_plm, subset = ~ is_constant_sample_health, 
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.17 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     share_trad_cred_run_avg_lag3 +
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
                     share_trad_cred_lag1 + share_trad_cred_lag2 ~ 
                     iv_interaction + 
                     I(share_trad_cred_run_avg_lag3*us_int_rate_lag3),
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv1.18 = feols(ihme_health_exp_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     share_trad_cred_run_avg_lag5 +
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
                     share_trad_cred_lag1 + share_trad_cred_lag4 ~ 
                     iv_interaction + 
                     I(share_trad_cred_run_avg_lag5*us_int_rate_lag5), 
                   data = data_df_plm, subset = ~ is_constant_sample_health, 
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

est_iv2.18 = feols(owid_educ_exp_perc_gdp ~ 
                     share_trad_cred_run_avg_lag2 +
                     share_trad_cred_run_avg_lag5 +
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
                     share_trad_cred_lag1 + share_trad_cred_lag4 ~ 
                     iv_interaction + 
                     I(share_trad_cred_run_avg_lag5*us_int_rate_lag5),
                   data = data_df_plm, subset = ~ is_constant_sample_educ,
                   panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_iv1.17, est_iv1.18, est_iv2.17, est_iv2.18,
       keep = c("%fit_share_trad_cred_lag1", "%fit_share_trad_cred_lag2", 
                "%fit_share_trad_cred_lag4"), 
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share",
                "share_trad_cred_lag2" = "LD share (lag = 2)",
                "share_trad_cred_lag4" = "LD share (lag = 4)"),
       tex = TRUE,
       title = "IV results with instrumented longer lags, middle-income countries",
       label = "tab:results_iv_instr_long_lags_other",
       file = "Analysis/MIC_results/IV_tables/results_iv_instr_long_lags.tex", 
       replace = TRUE)


################################################################################
########### Tables E.9 and E.10: Test interval where IV model holds ############
################################################################################
data_df_plm_1perc <- data_df_plm %>%
  filter(us_int_rate_lag2 < quantile(us_int_rate_lag2, 0.99, na.rm = TRUE))

data_df_plm_5perc <- data_df_plm %>%
  filter(us_int_rate_lag2 < quantile(us_int_rate_lag2, 0.95, na.rm = TRUE))

data_df_plm_10perc <- data_df_plm %>%
  filter(us_int_rate_lag2 < quantile(us_int_rate_lag2, 0.9, na.rm = TRUE))

# Estimate on the 1% trimmed data
est_1perc_health <- feols(ihme_health_exp_gdp ~ 
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
                          data = data_df_plm_1perc, 
                          subset = ~ is_constant_sample_health,
                          panel.id = ~debtor_country + year, 
                          cluster = ~debtor_country)

est_1perc_educ <- feols(owid_educ_exp_perc_gdp ~ 
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
                        data = data_df_plm_1perc, 
                        subset = ~ is_constant_sample_educ,
                        panel.id = ~debtor_country + year, 
                        cluster = ~debtor_country)

# 5% trimmed data
est_5perc_health <- feols(ihme_health_exp_gdp ~ 
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
                          data = data_df_plm_5perc, 
                          subset = ~ is_constant_sample_health,
                          panel.id = ~debtor_country + year, 
                          cluster = ~debtor_country)

est_5perc_educ <- feols(owid_educ_exp_perc_gdp ~ 
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
                          share_trad_cred_lag1 ~ 
                          iv_interaction,
                        data = data_df_plm_5perc, 
                        subset = ~ is_constant_sample_educ,
                        panel.id = ~debtor_country + year, 
                        cluster = ~debtor_country)

# 10% trimmed data
est_10perc_health <- feols(ihme_health_exp_gdp ~ 
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
                           data = data_df_plm_10perc, 
                           subset = ~ is_constant_sample_health,
                           panel.id = ~debtor_country + year, 
                           cluster = ~debtor_country)

est_10perc_educ <- feols(owid_educ_exp_perc_gdp ~ 
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
                         data = data_df_plm_10perc, 
                         subset = ~ is_constant_sample_educ,
                         panel.id = ~debtor_country + year, 
                         cluster = ~debtor_country)

# Print to Latex
etable(est_iv1.1, est_1perc_health, est_5perc_health, est_10perc_health,
       headers = c("Baseline", "Trim top 1%", "Trim top 5%", "Trim top 10%"),
       keep = "%share_trad_cred_lag1", 
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share"),
       tex = TRUE,
       title = "IV results without extreme instrument values, health spending, middle-income countries",
       label = "tab:results_iv_trim_top_health_other",
       file = "Analysis/MIC_results/IV_tables/results_iv_trim_top_health.tex", 
       replace = TRUE)

etable(est_iv2.1, est_1perc_educ, est_5perc_educ, est_10perc_educ,
       headers = c("Baseline", "Trim top 1%", "Trim top 5%", "Trim top 10%"),
       keep = "%share_trad_cred_lag1", 
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share"),
       tex = TRUE,
       title = "IV results without extreme instrument values, education spending, middle-income countries",
       label = "tab:results_iv_trim_top_educ_other",
       file = "Analysis/MIC_results/IV_tables/results_iv_trim_top_educ.tex", 
       replace = TRUE)


################################################################################
#################### Table E.11: Unrelated outcome variable ####################
################################################################################

################################ Health sample #################################
est_mil_exp_h = feols(mil_exp ~ 
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
                        I(share_trad_cred_run_avg_lag2*us_int_rate_lag2), 
                      data = data_df_plm, subset = ~ is_constant_sample_health,
                      panel.id = ~debtor_country + year, cluster = ~debtor_country)

############################### Education sample ###############################
est_mil_exp_e = feols(mil_exp ~ 
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
                        I(share_trad_cred_run_avg_lag2*us_int_rate_lag2), 
                      data = data_df_plm, subset = ~ is_constant_sample_educ,
                      panel.id = ~debtor_country + year, cluster = ~debtor_country)

etable(est_mil_exp_h, est_mil_exp_e,
       headers = c("Health sample", "Education sample"),
       keep = "%share_trad_cred_lag1", 
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = TRUE, 
       digits = "r3",
       dict = c("mil_exp" = "Military spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share"),
       tex = TRUE,
       title = "IV results for military spending, middle-income countries",
       label = "tab:results_iv_unrel_outcome_other",
       file = "Analysis/MIC_results/IV_tables/results_iv_unrel_outcome.tex", 
       replace = TRUE)


################################################################################
################### Table E.12: Time series reg. with NW SE ####################
################################################################################

############################## Health expenditure ##############################
### Compress data into time series (weighted by country exposure shares; see Borusyak et al. (2022))
df_y_h <- subset(data_df, is_constant_sample_health == TRUE) # need to subset first (run into trouble later)

model_y_h <- feols(ihme_health_exp_gdp ~ # residualize outcome
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
                   debtor_country + year, 
                 data = df_y_h,
                 panel.id = ~debtor_country + year)
df_y_h <- df_y_h[obs(model_y_h), ]
df_y_h$Y_residual <- resid(model_y_h)
true_panel_df_h <- df.residual(model_y_h) # adjust degrees of freedom for results later

model_x_h <- feols(share_trad_cred_lag1 ~ # residualize explanatory variable
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
                   debtor_country + year, 
                 data = df_y_h,
                 panel.id = ~debtor_country + year)
df_y_h$X_residual <- resid(model_x_h)

ts_df_h <- df_y_h %>%
  group_by(year) %>%
  summarize(
    Y_t = sum(share_trad_cred_run_avg_lag2*Y_residual)/sum(share_trad_cred_run_avg_lag2),
    X_t = sum(share_trad_cred_run_avg_lag2*X_residual)/sum(share_trad_cred_run_avg_lag2),
    FFR_t = first(us_int_rate_lag2),
    weights_t = sum(share_trad_cred_run_avg_lag2)
  ) %>%
  ungroup()

# Run IV regression on time series
ts_iv_h <- ivreg(Y_t ~ X_t - 1 | FFR_t - 1, weights = weights_t, data = ts_df_h) # remove intercept

# Obtain Variance-Covariance Matrix using Newey-West standard errors
nw_vcov_h <- NeweyWest(ts_iv_h, lag = 2, # since us_int_rate_lag2 follows AR(0,1,2) process
                       prewhite = FALSE, adjust = TRUE) 
test_h <- coeftest(ts_iv_h, vcov = nw_vcov_h, df = true_panel_df_h)

############################ Education expenditure #############################
df_y_e <- subset(data_df, is_constant_sample_educ == TRUE) 

model_y_e <- feols(owid_educ_exp_perc_gdp ~ 
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
                     debtor_country + year, 
                   data = df_y_e, 
                   panel.id = ~debtor_country + year)
df_y_e <- df_y_e[obs(model_y_e), ]
df_y_e$Y_residual <- resid(model_y_e)
true_panel_df_e <- df.residual(model_y_e) 

model_x_e <- feols(share_trad_cred_lag1 ~ 
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
                     debtor_country + year, 
                   data = df_y_e, 
                   panel.id = ~debtor_country + year)
df_y_e$X_residual <- resid(model_x_e)

ts_df_e <- df_y_e %>%
  group_by(year) %>%
  summarize(
    Y_t = sum(share_trad_cred_run_avg_lag2*Y_residual)/sum(share_trad_cred_run_avg_lag2),
    X_t = sum(share_trad_cred_run_avg_lag2*X_residual)/sum(share_trad_cred_run_avg_lag2),
    FFR_t = first(us_int_rate_lag2),
    weights_t = sum(share_trad_cred_run_avg_lag2)
  )

ts_iv_e <- ivreg(Y_t ~ X_t - 1 | FFR_t - 1, weights = weights_t, data = ts_df_e)

nw_vcov_e <- NeweyWest(ts_iv_e, lag = 2, prewhite = FALSE, adjust = TRUE)
test_e <- coeftest(ts_iv_e, vcov = nw_vcov_e, df = true_panel_df_e)

# For table output
robustness_models_ts <- list(
  "Health Spending" = ts_iv_h,
  "Education Spending" = ts_iv_e
)

robust_vcovs <- list(nw_vcov_h, nw_vcov_e)

modelsummary(robustness_models_ts,
             vcov = robust_vcovs,
             output = "Analysis/MIC_results/IV_tables/IV_robustness_NW_ts.tex", # Change to "robustness_table.tex" to save as file
             stars = TRUE,
             coef_rename = c("X_t" = "Share of LD (Residualized)"))
