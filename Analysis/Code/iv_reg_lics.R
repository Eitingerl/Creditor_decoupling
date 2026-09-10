################################################################################
################ Tables 1 and 2, B.1: Baseline IV specification ################
################################################################################
dir.create("Analysis/LIC_results/IV_tables", recursive = TRUE, showWarnings = FALSE)

############################## Health expenditure ##############################
### Full set of controls
est_iv1.1 = feols(ihme_health_exp_gdp ~ 
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
                    debtor_country + year | # fixed effects
                    share_trad_cred_lag1 ~ 
                    iv_interaction, # IV specification
                  data = data_df_plm, subset = ~ is_constant_sample_health,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

### Restricted set of controls to minimize threat of "bad controls"
est_iv1.2 = feols(ihme_health_exp_gdp ~ 
                    share_trad_cred_run_avg_lag2 +
                    dep_ratio_lag1 + 
                    urb_pop_lag1 +
                    political_rights_lag1 +
                    war_lag1 |
                    debtor_country + year | 
                    share_trad_cred_lag1 ~ 
                    iv_interaction, 
                  data = data_df_plm, subset = ~ is_constant_sample_health,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

############################ Education expenditure #############################
### Full set of controls
est_iv2.1 = feols(owid_educ_exp_perc_gdp ~ 
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

### Restricted set of controls
est_iv2.2 = feols(owid_educ_exp_perc_gdp ~ 
                    share_trad_cred_run_avg_lag2 +
                    dep_ratio_lag1 + 
                    urb_pop_lag1 +
                    political_rights_lag1 +
                    war_lag1 | 
                    debtor_country + year | 
                    share_trad_cred_lag1 ~ 
                    iv_interaction,
                  data = data_df_plm, subset = ~ is_constant_sample_educ,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

############################### Write table ####################################
# For Table B.1
etable(est_iv1.1, est_iv1.2, est_iv2.1, est_iv2.2, # first stage 
       stage = 1,
       keep = "%iv_interaction",
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = list("Country-year FE" = ".*"), 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "iv_interaction" =
                  "Hist. reliance on LD x FFR",
                "ar2" = "Adjusted R$^2$", "ivwald" = "K-P F-statistic"),
       tabular = "*",
       style.tex = style.tex(depvar.title = "Dependent Variables:",
                             model.title = "Model:",
                             model.format = "(%s)"),
       notes = "Country-clustered standard errors are in parentheses. Stars indicate significance at *** $p < 0.001$; ** $p < 0.01$; * $p < 0.05$; and + $p < 0.1$. Controls include \\textit{log(GDP p.c.)}, \\textit{GDP growth}, \\textit{capital account openness}, \\textit{dependency ratio}, \\textit{urbanization rate}, \\textit{political rights}, \\textit{occurrence of war}, \\textit{trade openness}, \\textit{change in the foreign exchange rate}, \\textit{average inflation}, and \\textit{historical reliance on LD}.",
       tex = TRUE,
       title = "Baseline IV specification, first-stage results, low-income countries", 
       label = "tab:results_baseline_iv_overall_1st-stage_lics",
       file = "Analysis/LIC_results/IV_tables/results_baseline_iv_overall_1st-stage.tex", 
       replace = TRUE)

### Now include AR-test for health spending
ar_results_full_ctrl_health <- AR_test( # Anderson-Rubin test for restricted controls
  data = subset(data_df_plm, is_constant_sample_health),
  Y = "ihme_health_exp_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_interaction",
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

ar_results_rest_ctrl_health <- AR_test( # Anderson-Rubin test for full controls
  data = subset(data_df_plm, is_constant_sample_health),
  Y = "ihme_health_exp_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_interaction",
  controls = c("share_trad_cred_run_avg_lag2", "dep_ratio_lag1", "urb_pop_lag1",
               "political_rights_lag1", "war_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)

### Now include AR-test for education spending
ar_results_full_ctrl_educ <- AR_test( # Anderson-Rubin test for restricted controls
  data = subset(data_df_plm, is_constant_sample_educ),
  Y = "owid_educ_exp_perc_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_interaction",
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

ar_results_rest_ctrl_educ <- AR_test( # Anderson-Rubin test for full controls
  data = subset(data_df_plm, is_constant_sample_educ),
  Y = "owid_educ_exp_perc_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_interaction",
  controls = c("share_trad_cred_run_avg_lag2", "dep_ratio_lag1", "urb_pop_lag1",
               "political_rights_lag1", "war_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)

ar_custom_rows <- list(
  "AR F-Statistic" = c(
    round(ar_results_full_ctrl_health$Fstat["F"], 2),
    round(ar_results_rest_ctrl_health$Fstat["F"], 2), 
    round(ar_results_full_ctrl_educ$Fstat["F"], 2),
    round(ar_results_rest_ctrl_educ$Fstat["F"], 2)
  ),
  "AR p-value" = c(
    round(ar_results_full_ctrl_health$Fstat["p"], 3),
    round(ar_results_rest_ctrl_health$Fstat["p"], 3), 
    round(ar_results_full_ctrl_educ$Fstat["p"], 3),
    round(ar_results_rest_ctrl_educ$Fstat["p"], 3)
  ),
  "AR Robust 95% CI" = c(
    ar_results_full_ctrl_health$ci.print,
    ar_results_rest_ctrl_health$ci.print, 
    ar_results_full_ctrl_educ$ci.print,
    ar_results_rest_ctrl_educ$ci.print
  )
)

# For Tables 1 and 2
etable(est_iv1.1, est_iv1.2, est_iv2.1, est_iv2.2, # second stage
       extralines = ar_custom_rows,
       fitstat = ~ n + ar2 + bic + ivwald,
       fixef.group = list("Country-year FE" = ".*"), 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share",
                "share_trad_cred_run_avg_lag2" = 
                  "Hist. reliance on LD",
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
                "infl_avg_lag1" = "Average inflation rate",
                "avg_int_rate_lag1" = "Average interest rate",
                "ar2" = "Adjusted R$^2$", "ivwald" = "K-P F-statistic"),
       tabular = "*",
       style.tex = style.tex(depvar.title = "Dependent Variables:",
                             model.title = "Model:",
                             model.format = "(%s)"),
       title = "Baseline IV results, low-income countries",
       label = "tab:results_baseline_iv_overall_lics",
       tex = TRUE, 
       file = "Analysis/LIC_results/IV_tables/results_baseline_iv_overall_w_ar.tex", 
       replace = TRUE)


################################################################################
############### Tables B.2 and B.4: Include mediators as controls ##############
################################################################################

est_iv1.3 = feols(ihme_health_exp_gdp ~ 
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
                    avg_int_rate_lag1 +
                    oda_perc_gni_lag1 +
                    imf_active_lag1 +
                    gov_bal_lag1 |
                    debtor_country + year | 
                    share_trad_cred_lag1 ~ 
                    iv_interaction, 
                  data = data_df_plm, subset = ~ is_constant_sample_health, 
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

ar_results_med_health <- AR_test( 
  data = subset(data_df_plm, is_constant_sample_health),
  Y = "ihme_health_exp_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_interaction",
  controls = c("share_trad_cred_run_avg_lag2", "log_gdp_pc_lag1", 
               "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
               "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
               "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
               "infl_avg_lag1", "avg_int_rate_lag1", "oda_perc_gni_lag1",
               "imf_active_lag1", "gov_bal_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)

est_iv2.3 = feols(owid_educ_exp_perc_gdp ~ 
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
                    avg_int_rate_lag1 +
                    oda_perc_gni_lag1 +
                    imf_active_lag1 +
                    gov_bal_lag1 |
                    debtor_country + year | 
                    share_trad_cred_lag1 ~ 
                    iv_interaction, 
                  data = data_df_plm, subset = ~ is_constant_sample_educ,
                  panel.id = ~debtor_country + year, cluster = ~debtor_country)

ar_results_med_educ <- AR_test( # Anderson-Rubin test for restricted controls
  data = subset(data_df_plm, is_constant_sample_educ),
  Y = "owid_educ_exp_perc_gdp",
  D = "share_trad_cred_lag1",
  Z = "iv_interaction",
  controls = c("share_trad_cred_run_avg_lag2", "log_gdp_pc_lag1", 
               "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
               "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
               "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
               "infl_avg_lag1", "avg_int_rate_lag1", "oda_perc_gni_lag1",
               "imf_active_lag1", "gov_bal_lag1"),
  FE = c("debtor_country", "year"),
  cl = "debtor_country",
  CI = TRUE,
  alpha = 0.05
)

ar_app_custom_row <- list(
  "AR F-Statistic" = c(
    round(ar_results_full_ctrl_health$Fstat["F"], 2),
    round(ar_results_rest_ctrl_health$Fstat["F"], 2), 
    round(ar_results_med_health$Fstat["F"], 2), 
    round(ar_results_full_ctrl_educ$Fstat["F"], 2),
    round(ar_results_rest_ctrl_educ$Fstat["F"], 2),
    round(ar_results_med_educ$Fstat["F"], 2)
  ),
  "AR p-value" = c(
    round(ar_results_full_ctrl_health$Fstat["p"], 3),
    round(ar_results_rest_ctrl_health$Fstat["p"], 3),
    round(ar_results_med_health$Fstat["p"], 3),
    round(ar_results_full_ctrl_educ$Fstat["p"], 3),
    round(ar_results_rest_ctrl_educ$Fstat["p"], 3),
    round(ar_results_med_educ$Fstat["p"], 3)
  ),
  "AR Robust 95% CI" = c(
    ar_results_full_ctrl_health$ci.print,
    ar_results_rest_ctrl_health$ci.print, 
    ar_results_med_health$ci.print, 
    ar_results_full_ctrl_educ$ci.print,
    ar_results_rest_ctrl_educ$ci.print,
    ar_results_med_educ$ci.print
  )
)

etable(est_iv1.1, est_iv1.2, est_iv1.3, est_iv2.1, est_iv2.2, est_iv2.3,
       extralines = ar_app_custom_row,
       fitstat = ~ n + ar2 + bic + ivwald, 
       fixef.group = list("Country-year FE" = ".*"), 
       digits = "r3",
       dict = c("ihme_health_exp_gdp" = "Health spending (share of GDP)",
                "owid_educ_exp_perc_gdp" = "Education spending (share of GDP)",
                "share_trad_cred_lag1" = "LD share",
                "share_trad_cred_run_avg_lag2" = 
                  "Hist. reliance on LD",
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
                "gov_bal_lag1" = "Government balance",
                "imf_active_lag11" = "Active IMF program",
                "oda_perc_gni_lag1" = "ODA",
                "infl_avg_lag1" = "Average inflation rate",
                "avg_int_rate_lag1" = "Average interest rate",
                "ar2" = "Adjusted R$^2$", "ivwald" = "K-P F-statistic"),
       tabular = "*",
       style.tex = style.tex(depvar.title = "Dependent Variables:",
                             model.title = "Model:",
                             model.format = "(%s)"),
       title = "Baseline IV results, low-income countries",
       label = "tab:app_results_baseline_iv_overall_lics",
       tex = TRUE, 
       file = "Analysis/LIC_results/IV_tables/IV_all_ctrls_appendix.tex", 
       replace = TRUE)
