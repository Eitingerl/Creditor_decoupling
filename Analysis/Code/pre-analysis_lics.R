################################################################################
############ Figure A.IV: IV vs. explanatory and outcome variables #############
################################################################################
dir.create("Analysis/LIC_results/General_tables", recursive = TRUE, showWarnings = FALSE)
dir.create("Analysis/LIC_results/Figures", recursive = TRUE, showWarnings = FALSE)

data_df_iv <- data_df %>%
  group_by(year) %>%
  filter(year >= 1995) %>% 
  summarize(
    avg_health_exp = mean(ihme_health_exp_gdp, na.rm = TRUE),
    avg_educ_exp = mean(owid_educ_exp_perc_gdp, na.rm = TRUE),
    avg_share_trad_cred = mean(share_trad_cred, na.rm = TRUE),
    us_int_rate_lag2 = first(us_int_rate_lag2)) %>%
  ungroup()

ggplot(data_df_iv, aes(x = us_int_rate_lag2, y = avg_share_trad_cred)) +
  geom_point(alpha = 0.6) +
  geom_text(aes(label = year), vjust = -1, hjust = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "US federal funds rate", 
       y = "Average share of legacy debt") +
  theme_minimal(base_size = 22) +
  theme(text = element_text(size = 22))
ggsave("Analysis/LIC_results/Figures/iv_vs_avg_share_trad_cred.png", width = 12, height = 8)


################################################################################
############### Figures A.V and A.VI: Trends in IV vs. outcomes ################
################################################################################

# Health expenditure 
us_int_rate_global <- data_df %>%
  group_by(year) %>%
  filter(year >= 1995) %>% 
  summarise(value = mean(us_int_rate_lag2, na.rm = TRUE), .groups = "drop") %>%
  mutate(metric = "US federal funds rate")

health_exp_quart <- data_df %>%
  group_by(year) %>%
  filter(year >= 1995) %>%
  mutate(
    # Using ntile(x, 2) creates 2 equal-sized groups (above and below median)
    cred_group = ntile(share_trad_cred_run_avg_lag2, 2)
  ) %>%
  group_by(year, cred_group) %>%
  summarise(value = mean(ihme_health_exp_gdp, na.rm = TRUE), .groups = "drop") %>%
  mutate(metric = paste0("Q", cred_group)) %>%
  dplyr::select(year, metric, value)

iv_plot1_data <- bind_rows(health_exp_quart, us_int_rate_global) %>%
  filter(metric != "QNA") %>% # Remove the QNA category in metric
  filter(!is.na(value)) # Remove NaN

ggplot(iv_plot1_data, aes(x = year, y = value, color = metric, 
                          group = metric, linetype = metric)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("US federal funds rate" = "black", 
              "Q1" = "steelblue", 
              "Q2" = "firebrick"), 
    labels = c("US interest rates" = "US federal funds rate", 
              "Q1" = 
                "Avg. health exp: Below median\nhistorical LD share", 
              "Q2" = 
                "Avg. health exp: Above median\nhistorical LD share") 
    ) +
  scale_linetype_manual(
    values = c("US federal funds rate" = "solid", 
              "Q1" = "dashed", 
              "Q2" = "dashed"), 
    labels = c("US interest rates" = "US federal funds rate", 
              "Q1" = 
                "Avg. health exp: Below median\nhistorical LD share", 
              "Q2" = 
                "Avg. health exp: Above median\nhistorical LD share") 
    ) +
  labs(
    x = "Year", y = "Share of GDP",
    color = "", linetype = ""
  ) +
  theme_minimal(base_size = 22) + 
  theme(
    legend.position = "bottom",
    legend.key.spacing.y = unit(0.6,"cm"),
    element_text(size = 22)
  )
ggsave("Analysis/LIC_results/Figures/iv_health_exp_trends.png", width = 12, height = 8)

# Education expenditure
educ_exp_quart <- data_df %>%
  group_by(year) %>%
  filter(year >= 1995) %>%
  filter(!is.na(share_trad_cred_run_avg_lag2)) %>%
  mutate(
    cred_group = ntile(share_trad_cred_run_avg_lag2, 2)
  ) %>%
  group_by(year, cred_group) %>%
  summarise(value = mean(owid_educ_exp_perc_gdp, na.rm = TRUE), .groups = "drop") %>%
  mutate(metric = paste0("Q", cred_group)) %>%
  dplyr::select(year, metric, value)

iv_plot2_data <- bind_rows(educ_exp_quart, us_int_rate_global) %>%
filter(metric != "QNA") %>% 
  filter(!is.na(value)) 

ggplot(iv_plot2_data, aes(x = year, y = value, color = metric, linetype = metric)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("US federal funds rate" = "black", 
              "Q1" = "steelblue", 
              "Q2" = "firebrick"), 
    labels = c("US interest rates" = "US federal funds rate", 
               "Q1" =
                 "Avg. education exp: Below\nmedian historical LD share", 
               "Q2" = 
                 "Avg. education exp: Above\nmedian historical LD share") 
    ) +
  scale_linetype_manual(
    values = c("US federal funds rate" = "solid", 
              "Q1" = "dashed", 
              "Q2" = "dashed"), 
    labels = c("US interest rates" = "US federal funds rate", 
              "Q1" =
                "Avg. education exp: Below\nmedian historical LD share", 
              "Q2" = 
                "Avg. education exp: Above\nmedian historical LD share") 
    ) +
  labs(
    x = "Year", y = "Share of GDP",
    color = "", linetype = ""
  ) +
  theme_minimal(base_size = 22) + # Large base font for readability
  theme(
    legend.position = "bottom",
    legend.key.spacing.y = unit(0.6,"cm"),
    element_text(size = 22)
  )
ggsave("Analysis/LIC_results/Figures/iv_educ_exp_trends.png", width = 12, height = 8)


################################################################################
############### Tables D.6 and D.7: Correlation and distribution ###############
################################################################################

corr_vars <- c(
  "share_trad_cred_lag1", 
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
  "imf_active_lag1",
  "oda_perc_gni_lag1", 
  "avg_int_rate_lag1", 
  "share_trad_cred_run_avg_lag2", 
  "us_int_rate_lag2"
)

cor_matrix <- data_df %>% # correlation matrix with health expenditure 
  filter(year >= 1995) %>%
  dplyr::select(all_of(corr_vars)) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>% # force all to numeric 
  tidyr::drop_na() 

colnames(cor_matrix) <- c(
  "LD share",
  "Log GDP per capita",
  "GDP growth",
  "Dependency ratio", 
  "Urbanization rate", 
  "Trade openness", 
  "FX rate perc. change", 
  "Political rights", 
  "Capital account openness", 
  "Average inflation rate",
  "War", 
  "Active IMF program",
  "ODA", 
  "Average interest rate", 
  "Hist. reliance on LD", 
  "FFR (2-year lag)"             
)

datasummary_correlation(cor_matrix,
                        fmt = 3, # This replaces round(3)
                        title = "Correlation matrix of independent variables, low-income countries",
                        output = "Analysis/LIC_results/General_tables/correlation_ind_vars_table.tex", 
                        replace = TRUE,
          silent = FALSE)
