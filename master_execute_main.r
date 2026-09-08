################################################################################
############################### 1. Basic set-up ################################
################################################################################
rm(list = ls())

library(tidyverse)
library(haven)
library(sjPlot)
library(plm)
library(lme4)
library(car)
library(readxl)
library(writexl)
library(fixest)
library(forecast)
library(tseries)
library(stats)
library(sandwich)
library(lmtest)
library(AER)
library(stargazer)
library(ggplot2)
library(naniar)
library(tseries)
library(nortest)
library(slider)
library(countrycode)
library(psych)
library(psychTools)
library(xtable)
library(modelsummary)
library(ivDiag)
library(factoextra)
library(NbClust)
library(compositions)
library(strucchange)
library(mediation)
library(multilevel)
library(bda)
library(xtable)
options(scipen = 999)

# set significance asterisks globally
setFixest_etable(signif.code = c("***" = 0.001, "**" = 0.01, 
                                 "*" = 0.05, "+" = 0.1))


################################################################################
################## 2. Create independent variable of interest ##################
################################################################################

# Call data set
data_df <- read_excel("Analysis/master_data.xlsx") # use data set with expanded set of traditional creditors

# Create log of GDP p.c. variable
data_df <- data_df %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(log_gdp_pc = log(gdp_pc_const_usd + 0.0001)) %>%
  ungroup()

# Create variables of interest: proportion of debt owed to traditional creditors & private creditors, and education spending p.c., and fractionalization IV
data_df <- data_df %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    share_trad_cred = # compute share of legacy official creditors
      if_else(total_longterm_ppg_debt > 0, 
              round(longterm_trad_cred_debt / total_longterm_ppg_debt, 3), 0)
    ) %>%
  mutate(
    share_prvt_cred = # compute share of private creditors
      if_else(total_longterm_ppg_debt > 0, 
              round(prvt_cred_ppg_debt / total_longterm_ppg_debt, 3), 0)
  ) %>%
  mutate(
    share_new_off_cred = # compute share of new official creditors
      if_else(total_longterm_ppg_debt > 0, 
              round((official_cred_ppg_debt - longterm_trad_cred_debt) / 
                      total_longterm_ppg_debt, 3), 0)
  ) %>%
  mutate(
    share_trad_cred_commit = # share of legacy official creditors in debt flows (robustness)
      if_else(total_commit > 0, 
              round(commitments_trad_cred / total_commit, 3), 0)
  ) %>%
  mutate(owid_educ_exp_pc_ppp = owid_educ_exp_tot / total_pop) %>% # alt. outcome for robustness (IHME provides the equivalent for health spending)
 ungroup()  
  

# Normalize fx_perc_change (min-max, within-country)
data_df <- data_df %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    c_min = suppressWarnings(min(fx_perc_change, na.rm = TRUE)), # temporary columns to reduce calculation load
    c_max = suppressWarnings(max(fx_perc_change, na.rm = TRUE)),
    
    fx_perc_change_norm = case_when(
      is.na(fx_perc_change) ~ NA_real_,         # Preserve original NAs
      is.infinite(c_min) ~ NA_real_,            # Handle countries with 100% missing data
      c_min == c_max ~ 0,                       # Fix the division-by-zero issue
      TRUE ~ (fx_perc_change - c_min) / (c_max - c_min)
    )
  ) %>%
  dplyr::select(-c_min, -c_max) %>%
  ungroup()

# Create dummy variables for structural breaks
data_df <- data_df %>%
  mutate(MDG = case_when(
    year < 2000 ~ 0, # year of adoption of the Millennium Declaration
    TRUE ~ 1
  )) %>%
  mutate(MDRI = case_when( 
    year < 2005 ~ 0, # year of the Multilateral Debt Relief Initiative
    TRUE ~ 1
  )) %>%
  mutate(SDG = case_when(
    year < 2015 ~ 0, # year of adoption of the 2030 Agenda for Sustainable Development
    TRUE ~ 1
  ))

################################################################################
############################## 3. Variable set-up ##############################
################################################################################

# Create lagged variables
data_df <- data_df %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    ihme_health_exp_gdp_lag1 = dplyr::lag(ihme_health_exp_gdp, 1),
    owid_educ_exp_perc_gdp_lag1 = dplyr::lag(owid_educ_exp_perc_gdp, 1),
    share_trad_cred_lag1 = dplyr::lag(share_trad_cred, 1),
    share_trad_cred_lag2 = dplyr::lag(share_trad_cred, 2), # for mediation analysis
    share_trad_cred_lag4 = dplyr::lag(share_trad_cred, 4),
    share_prvt_cred_lag1 = dplyr::lag(share_prvt_cred, 1),
    share_new_off_cred_lag1 = dplyr::lag(share_new_off_cred, 1),
    share_trad_cred_commit_lag1 = dplyr::lag(share_trad_cred_commit, 1),
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
    us_int_rate_lag2 = dplyr::lag(us_int_rate, 2),
    us_int_rate_lag3 = dplyr::lag(us_int_rate, 3),
    us_int_rate_lag5 = dplyr::lag(us_int_rate, 5),
    avg_int_rate_lag1 = dplyr::lag(avg_int_rate, 1),
    oil_price_lag1 = dplyr::lag(oil_price, 1),
    gdp_growth_glob_lag1 = dplyr::lag(gdp_growth_glob, 1)
    ) %>%
  ungroup()

# Create first part of IV: rolling average of traditional creditor share up to t-2
data_df <- data_df %>%
  arrange(debtor_country, year) %>%
  group_by(debtor_country) %>%
  mutate(
    # .before = Inf means "from the very start of the data"
    # .after = -2 means "stop 2 years before the current row"
    share_trad_cred_run_avg_lag2 = slide_index_dbl(
      .x = share_trad_cred, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = Inf, 
      .after = -2
    )
  ) %>%
  mutate(
    share_prvt_cred_run_avg_lag2 = slide_index_dbl(
      .x = share_prvt_cred, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = Inf, 
      .after = -2
    )
  ) %>%
  mutate(
    share_trad_crd_com_run_avg_lag2 = slide_index_dbl(
      .x = share_trad_cred_commit, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = Inf, 
      .after = -2
    )
  ) %>%
  mutate(
    share_trad_cred_run_avg_lag3 = slide_index_dbl( # for robustness check with 3-year lag
      .x = share_trad_cred, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = Inf, 
      .after = -3
    )
  ) %>%
  mutate(
    share_trad_cred_run_avg_lag5 = slide_index_dbl( # for robustness check with 5-year lag
      .x = share_trad_cred, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = Inf, 
      .after = -5
    )
  ) %>%
  mutate(
    share_trad_cred_run_avg_cond = slide_index_dbl( # for robustness check with condensed share
      .x = share_trad_cred, 
      .i = year, 
      .f = ~mean(.x, na.rm = TRUE),
      .before = 5, 
      .after = -2
    )
  ) %>%
  mutate(
    share_trad_cred_org = first(share_trad_cred)
  ) %>%
  mutate(share_trad_cred_avg = mean(share_trad_cred, na.rm = TRUE)) %>%
  
  # Create IV interaction term
  mutate(
    iv_interaction = share_trad_cred_run_avg_lag2*us_int_rate_lag2,
    iv_invar_share_interaction = I(share_trad_cred_avg*us_int_rate_lag2),
    iv_interaction_prvt = I(share_prvt_cred_run_avg_lag2*us_int_rate_lag2),
    iv_interaction_commit = share_trad_crd_com_run_avg_lag2*us_int_rate_lag2
  ) %>%
  
  # Create interaction terms for robustness check in Stata
  mutate(
    share_trad_cred_lag1_sq = I(share_trad_cred_lag1*share_trad_cred_lag1)
  ) %>%
  ungroup()


################################################################################
######################### 4. Define samples to be used #########################
################################################################################
# Create different data sets (comprising different countries)
## Load data set from Our World in Data on World Bank country classifications 
## (see article https://ourworldindata.org/world-bank-income-groups-explained)
inc_groups <- read_csv("Construct_dataset/Downloaded_data/world-bank-income-groups.csv")

# get countries and their codes that were classified as low- or lower-middle-income at some point
inc_groups <- inc_groups %>%
  rename(inc_status = `World Bank's income classification`) %>%
  dplyr::select(-Entity)

data_df <- data_df %>%
  left_join(inc_groups, by = c("year" = "Year", "debtor_country" = "Code"))


# Create logical vector to be used in sample subsetting so that models with full and restricted set of controls run on same sample
main_vars <- c(
  "share_trad_cred", "share_trad_cred_lag1", "share_trad_cred_lag2", 
  "log_gdp_pc_lag1", 
  "gdp_growth_lag1",
  "dep_ratio_lag1", 
  "urb_pop_lag1", 
  "trade_openness_pwt_lag1", 
  "fx_perc_change_norm_lag1", 
  "political_rights_lag1", 
  "cap_acc_openness_lag1", 
  "infl_avg_lag1",
  "war_lag1", 
  "gov_bal_lag1",
  "imf_active_lag1",
  "oda_perc_gni_lag1", 
  "avg_int_rate_lag1",
  "share_trad_cred_run_avg_lag2", 
  "us_int_rate_lag2"
)

main_vars_health <- c(main_vars, "ihme_health_exp_gdp") # do not include exp p.c. - may unduly limit sample size, esp. for education (just a robustness check)
                      
main_vars_educ <- c(main_vars, "owid_educ_exp_perc_gdp") 
                    

data_df <- data_df %>% 
  mutate(is_constant_sample_health = 
           complete.cases(across(all_of(main_vars_health))),
         is_constant_sample_educ = 
           complete.cases(across(all_of(main_vars_educ)))) # create separate variable for education expenditure DV because it has more missing values than health expenditure


# create data set with only low-income countries (classified as low-income in at least 50% of the years from 2013-2023)
data_df_lic <- data_df %>%
  group_by(debtor_country) %>%
  mutate(
    low_inc_share = mean(inc_status[year >= 2013] == "Low-income countries", na.rm = TRUE)
    ) %>%
  filter(low_inc_share > 0.5) %>%
  ungroup()

# create data set with only non-low-income (middle-income) countries
data_df_mic <- data_df %>%
  group_by(debtor_country) %>%
  mutate(
    low_inc_share = mean(inc_status[year >= 2013] == "Low-income countries", na.rm = TRUE)
  ) %>%
  filter(low_inc_share <= 0.5) %>%
  ungroup() 

# write to dta for later robustness check
mic_stata <- data_df_mic %>%
  dplyr::select(-c(`Country Code`, `Country Name`)) %>%
  write_dta("Analysis/data_mic.dta")

# create data set with only upper-middle-income countries
data_df_umic <- data_df %>%
  group_by(debtor_country) %>%
  mutate(
    umic_inc_share = mean(inc_status[year >= 2013] == "Upper-middle-income countries", na.rm = TRUE)
  ) %>%
  filter(umic_inc_share > 0.5) %>%
  ungroup()

# Pre-analysis
source("Analysis/Code/pre-analysis_overall.R") # data exploration and visualization for the full sample


################################################################################
############################## 6. Execute scripts ##############################
################################################################################

################################## LICs first ##################################
setwd("Analysis/LIC_results/")

data_df <- data_df_lic
source("Analysis/Code/pre-analysis_sample.R")

data_df_plm <- pdata.frame(data_df_lic, index = c("debtor_country", "year"))

source("Analysis/Code/summary_stats.R") # summary statistics table

source("Analysis/Code/iv_reg.R") 
source("Analysis/Code/mediation.R") 


################################### Now MICs ###################################
setwd("Analysis/MIC_results/")

data_df <- data_df_mic
source("Analysis/Code/pre-analysis_sample.R")

data_df_plm <- pdata.frame(data_df_mic, index = c("debtor_country", "year"))

source("Analysis/Code/summary_stats.R") 

source("Analysis/Code/iv_reg.R") 
source("Analysis/Code/robustness.R") 
source("Analysis/Code/iv_reg_diff_def_legacy_cred.R")
source("Analysis/Code/mediation.R") 
