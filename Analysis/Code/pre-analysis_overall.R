################################################################################
######################## Figure A.I: Evolution of debt #########################
################################################################################
dir.create("Analysis/General_figures", recursive = TRUE, showWarnings = FALSE)

# Total external debt 
ids_totals <- read_excel("Construct_dataset/Downloaded_data/IDS_totals_w_China.xlsx", range = cell_rows(1:9), 
                         na = c("", ".."))

ids_totals <- ids_totals %>%
  dplyr::select(-c(`Country Code`, `Counterpart-Area Name`, 
            `Counterpart-Area Code`, `Series Code`))

long_ids_total <- ids_totals %>%
  rename(creditor_type = `Series Name`,
         country = `Country Name`) %>%
  pivot_longer(!c(creditor_type, country), names_to = "year", values_to = "debt") %>%
  mutate(year = as.numeric(str_sub(year, 1, 4)),
         debt = debt/1e9) %>%
  mutate(creditor_type = case_when(
    creditor_type == "PPG, commercial banks (DOD, current US$)" ~ "Comm. banks",
    creditor_type == "PPG, multilateral (DOD, current US$)" ~ "Multilateral",
    creditor_type == "PPG, bonds (DOD, current US$)" ~ "Bonds",
    creditor_type == "PPG, bilateral (DOD, current US$)" ~ "Bilateral"
  )) 

wide_ids <- long_ids_total %>%
  mutate(debt = replace_na(debt, 0)) %>% # fill 0 for NAs for China (up to 1980)
  pivot_wider(names_from = country, values_from = debt, values_fill = 0) %>%
  mutate(debt_wo_China = `Low & middle income` - `China`)

wide_ids$creditor_type <- factor(wide_ids$creditor_type, 
                                 levels=c("Bonds", "Comm. banks", 
                                          "Bilateral", "Multilateral"))

ggplot(wide_ids, aes(x=year, y=debt_wo_China, fill=creditor_type)) + 
  geom_area() +
  labs(x = "Year", y = "PPG debt in billion USD (excl. China)", fill = "Creditor type") +
  scale_fill_manual(values = c("darkgray", "orange", "blue", "red")) +
  theme_minimal(base_size = 22) +
  theme(text = element_text(size = 22))
ggsave("Analysis/General_figures/ids_debt_totals_wo_chn.png", width = 10, height = 8)

# Now by creditor type shares 
wide_ids_total_test <- wide_ids %>%
  group_by(year, creditor_type) %>%
  summarise(n_lmics = sum(`Low & middle income`),
            n_wo_chn = sum(debt_wo_China),
            .groups = "drop_last") %>%
  mutate(percentage_wo_chn = n_wo_chn / sum(n_wo_chn)) %>%
  ungroup() 

wide_ids_total_test$creditor_type <- factor(wide_ids_total_test$creditor_type, 
                                            levels=c("Bonds", 
                                                     "Comm. banks", 
                                                     "Bilateral", 
                                                     "Multilateral"))

ggplot(wide_ids_total_test, aes(x=year, y=percentage_wo_chn, fill=creditor_type)) +
  geom_area() +
  labs(x = "Year", y = "Share of PPG debt (excl. China)", fill = "Creditor type") +
  scale_fill_manual(values = c("darkgray", "orange", "blue", "red")) +
  theme_minimal(base_size = 22) +
  theme(text = element_text(size = 22))
ggsave("Analysis/General_figures/ids_debt_shares_wo_chn.png", width = 10, height = 8)


################################################################################
########################## World maps of key variables #########################
################################################################################
world_coordinates <- map_data("world") %>%
  filter(region != "Antarctica")

world_map <- world_coordinates %>%
  mutate(debtor_country = countrycode(region, "country.name", "iso3c")) # get ISO-3 country codes

data_df_95 <- data_df %>% # for year 1995 - health and educ exp. too patchy then
  filter(year == 1995) %>%
  dplyr::select(debtor_country, ihme_health_exp_gdp, owid_educ_exp_perc_gdp,
         share_trad_cred) 

data_df_23 <- data_df %>% # for year 2022
  filter(year == 2023) %>%
  dplyr::select(debtor_country, ihme_health_exp_gdp, owid_educ_exp_perc_gdp, 
         share_trad_cred)

map_comb_95 <- left_join(world_map, data_df_95, by = "debtor_country")
map_comb_23 <- left_join(world_map, data_df_23, by = "debtor_country")


# Figure 1
ggplot(map_comb_95, aes(x = long, y = lat, fill = debtor_country, 
                        group = group)) +
  geom_polygon(aes(fill = share_trad_cred), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", direction = -1,
                       breaks = c(0.25, 0.50, 0.75)) +
  labs(fill = "Legacy debt share in 1995") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        aspect.ratio = 1/2,
        text = element_text(size = 18))
ggsave("Analysis/General_figures/map_share_trad_cred_1995.png", width = 12, height = 8)

ggplot(map_comb_23, aes(x = long, y = lat, fill = debtor_country, 
                        group = group)) +
  geom_polygon(aes(fill = share_trad_cred), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", direction = -1,
                       breaks = c(0.25, 0.50, 0.75)) +
  labs(fill = "Legacy debt share in 2023") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        aspect.ratio = 1/2,
        text = element_text(size = 18))
ggsave("Analysis/General_figures/map_share_trad_cred_2023.png", width = 12, height = 8)

# Figure A.II
ggplot(map_comb_95, aes(x = long, y = lat, fill = debtor_country, 
                        group = group)) +
  geom_polygon(aes(fill = ihme_health_exp_gdp), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", direction = -1,
                       breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08)) +
  labs(fill = "Government health exp. (share of GDP) in 1995") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        aspect.ratio = 1/2,
        text = element_text(size = 18))
ggsave("Analysis/General_figures/map_health_exp_1995.png", width = 12, height = 8)

ggplot(map_comb_23, aes(x = long, y = lat, fill = debtor_country, 
                        group = group)) +
  geom_polygon(aes(fill = ihme_health_exp_gdp), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", direction = -1,
                       breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08)) +
  labs(fill = "Government health exp. (share of GDP) in 2023") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        aspect.ratio = 1/2,
        text = element_text(size = 18))
ggsave("Analysis/General_figures/map_health_exp_2023.png", width = 12, height = 8)

# Figure A.III
ggplot(map_comb_95, aes(x = long, y = lat, fill = debtor_country, 
                        group = group)) +
  geom_polygon(aes(fill = owid_educ_exp_perc_gdp), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", direction = -1,
                       breaks = c(0.02, 0.04, 0.06, 0.08)) +
  labs(fill = "Government education exp. (share of GDP) in 1995") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        aspect.ratio = 1/2,
        text = element_text(size = 18))
ggsave("Analysis/General_figures/map_educ_exp_1995.png", width = 12, height = 8)

ggplot(map_comb_23, aes(x = long, y = lat, fill = debtor_country, 
                        group = group)) +
  geom_polygon(aes(fill = owid_educ_exp_perc_gdp), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", direction = -1,
                       breaks = c(0.02, 0.04, 0.06, 0.08, 0.1)) +
  labs(fill = "Government education exp. (share of GDP) in 2023") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        aspect.ratio = 1/2,
        text = element_text(size = 18))
ggsave("Analysis/General_figures/map_educ_exp_2023.png", width = 12, height = 8)

