################################################################################
######################### Table 3: Mediation analysis ##########################
################################################################################
data_df_plm$imf_active_lag1 <- as.numeric(as.character(data_df_plm$imf_active_lag1))

# Set up the Wild Cluster Bootstrap Function
wild_cluster_joint_mediation <- function(med_m1, med_m2, med_m3, med_m4,
                                         med_y, data_plm, cluster_var, 
                                         treatment_var, m1_var, m2_var, m3_var, 
                                         m4_var, reps=1000) {
  
  # Prepare for Bootstrap - extract coefficients and residuals from "null" models (i.e., the original fitted models)
  data_plm$res_m1 <- residuals(med_m1); data_plm$fit_m1 <- fitted(med_m1)
  data_plm$res_m2 <- residuals(med_m2); data_plm$fit_m2 <- fitted(med_m2)
  data_plm$res_m3 <- residuals(med_m3); data_plm$fit_m3 <- fitted(med_m3)
  data_plm$res_m4 <- residuals(med_m4); data_plm$fit_m4 <- fitted(med_m4)
  data_plm$res_y  <- residuals(med_y);  data_plm$fit_y  <- fitted(med_y)
  
  clusters <- unique(data_plm[[cluster_var]])
  results <- matrix(NA, nrow=reps, ncol=11)
  colnames(results) <- c("ab1", "ab2", "ab3", "ab4", "c_prime", 
                         "total_indirect", "total", 
                         "a1", "a2", "a3", "a4")
  
  controls <- paste("share_trad_cred_lag1 + log_gdp_pc_lag1 + gdp_growth_lag1 +",
                    "cap_acc_openness_lag1 + dep_ratio_lag1 +  urb_pop_lag1 +", 
                    "political_rights_lag1 + war_lag1 + trade_openness_pwt_lag1 +", 
                    "fx_perc_change_norm_lag1 + infl_avg_lag1") 
  
  pb <- txtProgressBar(min = 0, max = reps, style = 3) # for presentation of progress
  
  for(i in 1:reps) {
    w_map <- sample(c(-1, 1), size = length(clusters), replace = TRUE) # Rademacher weights for standard errors (cluster being countries)
    names(w_map) <- clusters
    w <- w_map[as.character(data_plm[[cluster_var]])] # mapping random weights back to original data frame length (by cluster)
    
    # Create bootstrapped (synthetic) outcomes based on original fitted values + residuals weighted by random weights
    data_plm$Y_m1_b <- data_plm$fit_m1 + (data_plm$res_m1 * w) # add residual weighted by random weight (+1 or -1)
    data_plm$Y_m2_b <- data_plm$fit_m2 + (data_plm$res_m2 * w)
    data_plm$Y_m3_b <- data_plm$fit_m3 + (data_plm$res_m3 * w)
    data_plm$Y_m4_b <- data_plm$fit_m4 + (data_plm$res_m4 * w)
    data_plm$Y_y_b  <- data_plm$fit_y  + (data_plm$res_y  * w)
    
    try({
      # Path A Models (one for each mediator)
      mod_m1 <- feols(as.formula(paste("Y_m1_b ~", controls, 
                                       "| debtor_country + year")), data=data_plm)
      mod_m2 <- feols(as.formula(paste("Y_m2_b ~", controls, 
                                       "| debtor_country + year")), data=data_plm)
      mod_m3 <- feols(as.formula(paste("Y_m3_b ~", controls, 
                                       "| debtor_country + year")), data=data_plm)
      mod_m4 <- feols(as.formula(paste("Y_m4_b ~", controls, 
                                       "| debtor_country + year")), data=data_plm)
      
      # Path B & C' Model (all mediators included) 
      fml_y <- as.formula(paste("Y_y_b ~", m1_var, "+", m2_var, "+", m3_var, "+", 
                                m4_var, "+",
                                controls, 
                                "| debtor_country + year"))
      mod_y <- feols(fml_y, data=data_plm)
      
      # Extract Coefficients
      a1 <- coef(mod_m1)[treatment_var]; a2 <- coef(mod_m2)[treatment_var]; 
      a3 <- coef(mod_m3)[treatment_var]; a4 <- coef(mod_m4)[treatment_var]; 
      b1 <- coef(mod_y)[m1_var]; b2 <- coef(mod_y)[m2_var]; 
      b3 <- coef(mod_y)[m3_var]; b4 <- coef(mod_y)[m4_var]; 
      c_p <- coef(mod_y)[treatment_var]
      
      # Calculate Indirect Effects
      ab1 <- a1 * b1; ab2 <- a2 * b2; ab3 <- a3 * b3; ab4 <- a4 * b4
      total_ind <- ab1 + ab2 + ab3 + ab4 # total indirect effect is sum of all four paths (VanderWeele & Vansteelandt, 2014)
      
      results[i, ] <- c(ab1, ab2, ab3, ab4,  
                        c_p, total_ind, (total_ind + c_p), 
                        a1, a2, a3, a4)
    }, silent = TRUE)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(results)
}

# Helper function to obtain results from bootstrap (95%-CI)
get_stats <- function(x) {
  c(Estimate = median(x, na.rm=TRUE), 
    `2.5%`   = quantile(x, 0.025, na.rm=TRUE), 
    `97.5%`  = quantile(x, 0.975, na.rm=TRUE))
}

############################### Health expenditure #############################
# Use data set with no NAs in explanatory vars and no singletons 
data_mediation_health_plm <- data_df_plm %>%
  filter(is_constant_sample_health == TRUE) %>%
  filter(!is.na(gov_bal_lag1))

has_singletons <- TRUE # tackle problem of iterative singletons (bootstrap breaks down in the presence of singletons)

while (has_singletons) {
  initial_rows <- nrow(data_mediation_health_plm)
  
  data_mediation_health_plm <- data_mediation_health_plm %>%
    group_by(debtor_country) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    group_by(year) %>%
    filter(n() >= 2) %>%
    ungroup()
  
  # If the row count didn't shrink, the cascading has stopped
  if (nrow(data_mediation_health_plm) == initial_rows) {
    has_singletons <- FALSE
  }
}

# Fit the mediation models 
med_m1_h <- feols(avg_int_rate_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_health_plm,
                  panel.id = ~debtor_country + year)

med_m2_h <- feols(oda_perc_gni_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_health_plm,
                  panel.id = ~debtor_country + year)

med_m3_h <- feols(imf_active_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_health_plm,
                  panel.id = ~debtor_country + year)

med_m4_h <- feols(gov_bal_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_health_plm,
                  panel.id = ~debtor_country + year)

med_y_h <- feols(ihme_health_exp_gdp ~ 
                   share_trad_cred_lag1 +
                   avg_int_rate_lag1 +
                   oda_perc_gni_lag1 +
                   imf_active_lag1 +
                   gov_bal_lag1 +
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
                 data = data_mediation_health_plm,
                 panel.id = ~debtor_country + year)


# Actual execution of the function
set.seed(467) # for reproducibility
mediation_boot_results_h <- wild_cluster_joint_mediation(
  med_m1        = med_m1_h, 
  med_m2        = med_m2_h, 
  med_m3        = med_m3_h,
  med_m4        = med_m4_h,
  med_y         = med_y_h, 
  data_plm      = data_mediation_health_plm, 
  cluster_var   = "debtor_country", 
  treatment_var = "share_trad_cred_lag1", 
  m1_var        = "avg_int_rate_lag1", 
  m2_var        = "oda_perc_gni_lag1", 
  m3_var        = "imf_active_lag1", 
  m4_var        = "gov_bal_lag1", 
  reps          = 1000
)

# Extracts stats from results matrix
summary_table_health <- as.data.frame(t(apply(mediation_boot_results_h, 2, 
                                              get_stats)))
print(xtable(summary_table_health, 
             caption = "Joint mediation results, health spending,",
             digits = 3), 
      type = "latex", 
      file = "Mediation_tables/joint_mediation_results_health.tex")

# Calculate the proportion that is mediated
prop_med_h <- mediation_boot_results_h[, "total_indirect"] / 
  mediation_boot_results_h[, "total"]

# Get the summary stats
prop_summary_h <- c(
  Median = median(prop_med_h, na.rm = TRUE),
  Lower_CI = quantile(prop_med_h, 0.025, na.rm = TRUE),
  Upper_CI = quantile(prop_med_h, 0.975, na.rm = TRUE)
)

print(round(prop_summary_h, 3))


############################# Education expenditure ############################
# Use data set with no NAs in explanatory vars and no singletons 
data_mediation_educ_plm <- data_df_plm %>%
  filter(is_constant_sample_educ == TRUE) %>%
  filter(!is.na(gov_bal_lag1))

has_singletons <- TRUE # reset indicator

while (has_singletons) {
  initial_rows <- nrow(data_mediation_educ_plm)
  
  data_mediation_educ_plm <- data_mediation_educ_plm %>%
    group_by(debtor_country) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    group_by(year) %>%
    filter(n() >= 2) %>%
    ungroup()
  
  # If the row count didn't shrink, the cascading has stopped
  if (nrow(data_mediation_educ_plm) == initial_rows) {
    has_singletons <- FALSE
  }
}

# Fit mediation models
med_m1_e <- feols(avg_int_rate_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_educ_plm,
                  panel.id = ~debtor_country + year)

med_m2_e <- feols(oda_perc_gni_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_educ_plm,
                  panel.id = ~debtor_country + year)

med_m3_e <- feols(imf_active_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_educ_plm,
                  panel.id = ~debtor_country + year)

med_m4_e <- feols(gov_bal_lag1 ~ 
                    share_trad_cred_lag1 +
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
                  data = data_mediation_educ_plm,
                  panel.id = ~debtor_country + year)

med_y_e <- feols(owid_educ_exp_perc_gdp ~ 
                   share_trad_cred_lag1 +
                   avg_int_rate_lag1 +
                   oda_perc_gni_lag1 +
                   imf_active_lag1 +
                   gov_bal_lag1 +
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
                 data = data_mediation_educ_plm,
                 panel.id = ~debtor_country + year)

# Actual execution of the function
set.seed(467)
mediation_boot_results_e <- wild_cluster_joint_mediation(
  med_m1        = med_m1_e, 
  med_m2        = med_m2_e, 
  med_m3        = med_m3_e,
  med_m4        = med_m4_e,
  med_y         = med_y_e, 
  data_plm      = data_mediation_educ_plm, 
  cluster_var   = "debtor_country", 
  treatment_var = "share_trad_cred_lag1", 
  m1_var        = "avg_int_rate_lag1", 
  m2_var        = "oda_perc_gni_lag1", 
  m3_var        = "imf_active_lag1", 
  m4_var        = "gov_bal_lag1", 
  reps          = 1000
)

# Apply to the results matrix
summary_table_educ <- as.data.frame(t(apply(mediation_boot_results_e, 2, 
                                            get_stats)))
print(xtable(summary_table_educ, 
             caption = "Joint mediation results, education spending,",
             digits = 3), 
      type = "latex", 
      file = "Mediation_tables/joint_mediation_results_educ.tex")

# Calculate the proportion that is mediated
prop_med_e <- mediation_boot_results_e[, "total_indirect"] / 
  mediation_boot_results_e[, "total"]

# Get the summary stats
prop_summary_e <- c(
  Median = median(prop_med_e, na.rm = TRUE),
  Lower_CI = quantile(prop_med_e, 0.025, na.rm = TRUE),
  Upper_CI = quantile(prop_med_e, 0.975, na.rm = TRUE)
)

print(round(prop_summary_e, 3))
