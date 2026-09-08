################################################################################
####################### Tables D.2-5: Summary statistics #######################
################################################################################

all_vars_h <- c("ihme_health_exp_gdp",  
                "share_trad_cred_lag1", "share_prvt_cred_lag1","log_gdp_pc_lag1", 
                "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
                "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
                "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
                "infl_avg_lag1", "oda_perc_gni_lag1", "imf_active_lag1", 
                "avg_int_rate_lag1")

all_vars_e <- c("owid_educ_exp_perc_gdp", 
                "share_trad_cred_lag1", "share_prvt_cred_lag1","log_gdp_pc_lag1", 
                "gdp_growth_lag1", "cap_acc_openness_lag1", "dep_ratio_lag1",
                "urb_pop_lag1", "political_rights_lag1", "war_lag1", 
                "trade_openness_pwt_lag1", "fx_perc_change_norm_lag1",
                "infl_avg_lag1", "oda_perc_gni_lag1", "imf_active_lag1", 
                "avg_int_rate_lag1")


data_df_plm_health <- subset(data_df_plm, is_constant_sample_health == TRUE) %>%
  group_by(debtor_country) %>% # drop singletons
  filter(n() > 1) %>% 
  ungroup() %>%
  group_by(year) %>%
  filter(n() >1) %>%
  ungroup()

data_df_plm_educ <- subset(data_df_plm, is_constant_sample_educ == TRUE) %>%
  group_by(debtor_country) %>% 
  filter(n() > 1) %>% 
  ungroup() %>%
  group_by(year) %>%
  filter(n() >1) %>%
  ungroup()


df_health_clean <- as.data.frame(data_df_plm_health)[ , all_vars_h]
df_health_clean$war_lag1 <- 
  as.numeric(as.character(df_health_clean$war_lag1))
df_health_clean$imf_active_lag1 <- 
  as.numeric(as.character(df_health_clean$imf_active_lag1))

colnames(df_health_clean) <- c("Health expenditure (share of GDP)", 
                               "LD share",
                               "Private creditor share",
                               "Log GDP per capita",
                               "GDP growth",
                               "Capital account openness", 
                               "Dependency ratio",
                               "Urban population (share of total)", 
                               "Political rights", 
                               "War", 
                               "Trade (share of GDP)", 
                               "FX rate perc. change (normalized)",
                               "Average inflation rate", 
                               "Official Development Assistance (share of GNI)", 
                               "Active IMF program", 
                               "Average interest rate")

datasummary(All(df_health_clean) ~ N + Mean + SD + Min + Max, 
            data = df_health_clean,
            title = "Summary statistics, health spending, middle-income countries",
            notes = "The statistics of explanatory variables refer to their one-year lagged version which is used in the models.",
            output = "Analysis/MIC_results/General_tables/summary_table_health_exp.tex")


df_educ_clean <- as.data.frame(data_df_plm_educ)[ , all_vars_e]
df_educ_clean$war_lag1 <- 
  as.numeric(as.character(df_educ_clean$war_lag1))
df_educ_clean$imf_active_lag1 <- 
  as.numeric(as.character(df_educ_clean$imf_active_lag1))

colnames(df_educ_clean) <- c("Education expenditure (share of GDP)",
                             "LD share",
                             "Private creditor share",
                             "Log GDP per capita",
                             "GDP growth",
                             "Capital account openness", 
                             "Dependency ratio",
                             "Urban population (share of total)", 
                             "Political rights", 
                             "War", 
                             "Trade (share of GDP)", 
                             "FX rate perc. change (normalized)",
                             "Average inflation rate", 
                             "Official Development Assistance (share of GNI)", 
                             "Active IMF program", 
                             "Average interest rate")


datasummary(All(df_educ_clean) ~ N + Mean + SD + Min + Max, 
            data = df_educ_clean,
            title = "Summary statistics, education spending, middle-income countries",
            notes = "The statistics of explanatory variables refer to their one-year lagged version which is used in the models.",
            output = "Analysis/MIC_results/General_tables/summary_table_educ_exp.tex")
