****************************************************************
***************** Table E.4: Different models ******************
****************************************************************

use "Analysis/data_mic.dta", clear

encode debtor_country, gen(country)

*** Fractional probit model with IV
/// Health spending with restricted set of controls
ivfprobit ihme_health_exp_gdp share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 i.political_rights_lag1 war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 i.country i.year (share_trad_cred_lag1 = iv_interaction) if is_constant_sample_health == 1, vce(cluster country)

estadd local nobs=string(e(N))
estadd local countryyrfe "Yes"
estimates store est_iv_1_20

/// Education spending with restricted set of controls
ivfprobit owid_educ_exp_perc_gdp share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 i.political_rights_lag1 war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 i.country i.year (share_trad_cred_lag1 = iv_interaction) if is_constant_sample_educ == 1, vce(cluster country)

estadd local nobs=string(e(N))
estadd local countryyrfe "Yes"
estimates store est_iv_2_20


*** Control function approach with squared IV by hand, following Dreher and Langlotz's (2020) code
xtset country year

/// Create dummy vars for factor vars (needed for xtivreg2)
cap drop yr*
cap drop pr*
tab year, gen(yr)
tab political_rights_lag1, gen(pr)

/// Health spending with restricted set of controls
set more off
cap program drop CFAboot_health
program CFAboot_health, eclass
    cap drop locresidual
    tempname b

xtivreg2 ihme_health_exp_gdp share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 pr* war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 (share_trad_cred_lag1=iv_interaction) yr* if is_constant_sample_health == 1, fe robust cluster(new_country) first

reg share_trad_cred_lag1 iv_interaction share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 pr* war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 yr* i.new_country if e(sample), robust cluster(new_country)

predict locresidual if e(sample), resid

xtreg ihme_health_exp_gdp share_trad_cred_lag1 share_trad_cred_lag1_sq locresidual share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 pr* war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 yr* if e(sample), fe robust cluster(new_country)

			matrix `b' = e(b)
			ereturn post `b'
	end

preserve
keep if is_constant_sample_health == 1 
cap drop new_country
gen new_country = country
xtset new_country year
bootstrap _b, reps(500) seed(454) cluster(country) idcluster(new_country) : CFAboot_health

estadd local nobs=string(e(N))
estadd local countryyrfe "Yes"
est store est_iv_1_21_boot
restore 
xtset country year

/// Education spending with restricted set of controls			
set more off
cap program drop CFAboot_educ
program CFAboot_educ, eclass
    cap drop locresidual
    tempname b

xtivreg2 owid_educ_exp_perc_gdp share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 pr* war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 (share_trad_cred_lag1=iv_interaction) yr* if is_constant_sample_educ == 1, fe robust cluster(new_country) first

reg share_trad_cred_lag1 iv_interaction share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 pr* war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 yr* i.new_country if e(sample), robust cluster(new_country)

predict locresidual if e(sample), resid

xtreg owid_educ_exp_perc_gdp share_trad_cred_lag1 share_trad_cred_lag1_sq locresidual share_trad_cred_run_avg_lag2 log_gdp_pc_lag1 gdp_growth_lag1 cap_acc_openness_lag1 dep_ratio_lag1 urb_pop_lag1 pr* war_lag1 trade_openness_pwt_lag1 fx_perc_change_norm_lag1 infl_avg_lag1 yr* if e(sample), fe robust cluster(new_country)

			matrix `b' = e(b)
			ereturn post `b'
	end

preserve
keep if is_constant_sample_educ == 1 
cap drop new_country
gen new_country = country
xtset new_country year
bootstrap _b, reps(500) seed(454) cluster(country) idcluster(new_country) : CFAboot_educ

estadd local nobs=string(e(N))
estadd local countryyrfe "Yes"
est store est_iv_2_21_boot

restore
xtset country year


*** Print everything to Latex
esttab est_iv_1_20 est_iv_1_21_boot est_iv_2_20 est_iv_2_21_boot ///
using "Analysis/MIC_results/IV_tables/IV_cfa+probit.tex", replace ///
keep(share_trad_cred_lag1 share_trad_cred_lag1_sq) ///
coeflabels(share_trad_cred_lag1 "LD share" share_trad_cred_lag1_sq "LD share squared") ///
cells(b(star fmt(3)) se(par fmt(3))) ///
star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
stats(nobs countryyrfe, labels("N observations" "Country-year FE")) ///
mtitles("Health spending (frac. probit)" "Health spending (CFA)" "Education spending (frac. probit)" "Education spending (CFA)") ///
title("Robustness check: IV model with different specifications, \label{tab:iv_robustness_cfa+probit_}") ///
addnotes("For the fractional probit model, country-clustered standard errors are in parentheses. For the CFA, bootstrapped standard errors (rep = 500) are in parentheses. Stars indicate significance at *** p < 0.001; ** p < 0.01; * p < 0.05; and + p < 0.10.") ///
booktabs fragment nolines