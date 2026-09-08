################################################################################
############################### 1. Basic set-up ################################
################################################################################
rm(list = ls())

library(wbstats)
library(httr)
library(jsonlite)
library(tidyverse)
library(WDI)
library(plotly)
library(writexl)
library(readxl)
library(readr)
library(stringr)
library(stringdist)
library(zoo)
library(haven)
library(purrr)
library(countrycode)


################################################################################
#################### 2. Specify debtors of interest (LMICs) ####################
################################################################################

## Load data set from Our World in Data on World Bank country classifications 
## (see article https://ourworldindata.org/world-bank-income-groups-explained)
inc_groups <- read_csv("Construct_dataset/Downloaded_data/world-bank-income-groups.csv")

# get countries and their codes that were classified as low- or lower-middle-income at some point
lmics <- inc_groups %>%
  group_by(Entity) %>%
  filter(Year >= 2013) %>% # look at last 10 years of period under observation
  filter(any(`World Bank's income classification` != # any indicates unstable status
               "High-income countries")) %>% 
  distinct(Entity, .keep_all = TRUE) %>%
  select(Entity, Code) %>%
  filter(Code != "CHN") %>% # exclude China
  ungroup()


lmics$Code[lmics$Code == "OWID_KOS"] <- "XKX" # amend country code for Kosovo

# create vector of LMIC debtors
debtor_countries <- c(lmics$Code)


################################################################################
############################ 3. Specify creditors  #############################
################################################################################

## again using World Bank Github for API code 
## (https://worldbank.github.io/debt-data/creditor-composition/creditor-data-r.html)

# the creditor entity is "World" if we don't want to disaggregate
world_creditor <- "WLD" # found on https://github.com/worldbank/debt-data/blob/master1/api-guide/location-codes.csv
  
# create vectors of key traditional creditors and low-income debtors 
trad_cred <- c("Australia", "Austria", "Belgium", "Canada", "Czechia", # regular DAC members by 2022
               "Denmark", "Finland", "France", "Germany, Fed. Rep. of", 
               "Greece", "Hungary", "Iceland", "Ireland",  "Italy", "Japan", 
               "Korea, Republic of", "Luxembourg", "Netherlands", "New Zealand", 
               "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", 
               "Spain", "Sweden", "Switzerland", "United Kingdom", "United States",
               "African Dev. Bank", "Asian Dev. Bank", # legacy MDBs
               "Inter-American Dev. Bank", 
               "European Bank for Reconstruction and Dev. (EBRD)",
               "World Bank-IDA", "World Bank-IBRD", "World Bank-MIGA", 
               "International Finance Corporation",
               "European Union", "European Investment Bank", "Council of Europe",
               "European Coal and Steel Community (ECSC)",
               "European Development Fund (EDF)",
               "European Economic Community (EEC)",
               "European Free Trade Association (EFTA)", "European Relief Fund",
               "European Social Fund (ESF)", "EUROFIMA",
               "Nordic Development Fund", "Nordic Environment Finance Corporation (NEFCO)",
               "Nordic Investment Bank", "Food and Agriculture Organization (FAO)",
               "UN-Children's Fund (UNICEF)", "UN-Development Fund for Women (UNIFEM)",
               "UN-Development Programme (UNDP)", "UN-Educ., Scientific and Cultural Org. (UNESCO)",
               "UN-High Commissioner for Refugees (UNHCR)",
               "UN-Population Fund (UNFPA)", "UN-Regular Programme of Technical Assistance",
               "UN-Regular Programme of Technical Coop. (RPTC)",
               "UN-Relief and Works Agency (UNRWA)",
               "UN-Office on Drugs and Crime (UNDCP)", "UN-Fund for Human Rights",
               "UN-Environment Programme (UNEP)", "UN-UNETPSA",
               "UN-Industrial Development Organization (UNIDO)",
               "UN-General Assembly (UNGA)", "UN-INSTRAW", 
               "UN-Fund for Drug Abuse Control (UNFDAC)",
               "UN-World Food Programme (WFP)", "UN-World Meteorological Organization",
               "UN-World Intellectual Property Organization",
               "World Health Organization", "World Trade Organization",
               "International Labour Organization (ILO)", "International Fund for Agricultural Dev.",
               "Global Environment Facility", "Montreal Protocol Fund")

# Get API codes for creditor entities (https://worldbank.github.io/debt-data/api-guide/ids-api-guide-r-1.html)
# Make request to World Bank API
clocationRequest <- GET(url = "http://api.worldbank.org/v2/sources/6/counterpart-area?per_page=300&format=JSON")
clocationResponse <- content(clocationRequest, as = "text", encoding = "UTF-8")

# Parse the JSON content and convert it to a data frame.
clocationsJSON <- fromJSON(clocationResponse, flatten = TRUE)

# Create a dataframe with the location codes and names
clocationList <- clocationsJSON[["source"]][["concept"]][[1]][["variable"]]%>%
  data.frame() %>%
  mutate(value = str_trim(as.character(value))) # trim whitespace

# Create vector of traditional creditor identifiers (necessary for later API calls)
trad_cred_ids <- clocationList %>%
  mutate(value = as.character(value)) %>% 
  mutate(value = trimws(value, which = "right")) %>%
  filter(value %in% trad_cred) %>%
  select(id) %>%
  pull()

# Get ID of IMF - want to omit that later
imf_creditor <- clocationList %>%
  mutate(value = as.character(value)) %>% 
  mutate(value = trimws(value, which = "right")) %>%
  filter(value == "International Monetary Fund") %>%
  select(id) %>%
  pull()

################################################################################
######################### 4. General API call set-up  ##########################
################################################################################

# Specify time frame
years <- seq(1990, 2023) # Generate a sequence of numbers from 1990 to 2023 (want to have data for lags)
years_with_prefix <- paste0("yr", years) # Add the "yr" prefix to each year (necessary for API call)
time <- paste(years_with_prefix, collapse = ";") # Collapse the vector into a single string separated by semicolons

# Specify the structure of the API call to be used later
url <- "http://api.worldbank.org/v2/sources/6/country/" #International Debt Statistics have id 6
end <- "?format=json&per_page=500"

# Create function that will parse the JSON data returned by the API call 
getData_aggregate <- function(JSON, debtor, series) {
  if (inherits(JSON, "list") && !is.null(JSON[["source"]])) { # Check the class of the first element (if it is a JSON list and if it contains the "source" element)
    listLen <- length(JSON[["source"]][["data"]][["value"]]) # Successful response is a list
    if (listLen == 0) {
      return(NULL) # Return NULL if no data is found in the JSON response
    }
    
    df <- data.frame( # Specify how to populate the empty data set with the retrieved results
      year = rep(0, listLen),
      debtorCountry = rep(debtor, listLen),
      series = rep(series, listLen),
      data = rep(0, listLen),
      stringsAsFactors = FALSE
    )
    
    for(k in 1:listLen) {
      year <- JSON[["source"]][["data"]][["variable"]][[k]][[3]][[2]] # access data according to specific JSON structure
      value <- JSON[["source"]][["data"]][["value"]][[k]]
      
      df[k, "year"] <- as.numeric(year)
      df[k, "data"] <- as.numeric(value) # make sure this is numeric to keep decimal places
    }
    return(df)
  } else { # If the response is an error (e.g., XML), simply return NULL
    return(NULL)
  }
}

################################################################################
########################## 5. Obtain PPG debt totals  ##########################
################################################################################

# Specify relevant series
series1 <- c(
  "DT.DOD.DPPG.CD", # total long-term external public and publicly guaranteed debt stock (should be >= sum of others)
  "DT.DOD.OFFT.CD", # public and publicly guaranteed debt by official creditors
  "DT.DOD.PRVT.CD", # public and publicly guaranteed debt by private creditors
  "DT.DOD.PCBK.CD", # public and publicly guaranteed debt by commercial banks
  "DT.DOD.PBND.CD", # public and publicly guaranteed debt by bondholders
  "DT.COM.DPPG.CD", # loan commitments, public and publicly guaranteed
  "DT.COM.OFFT.CD", # loan commitments from official creditors
  "DT.COM.PRVT.CD", # loan commitments from private creditors
  "DT.DIS.DPPG.CD" # disbursements on long-term external debt, public and publicly guaranteed
)

# Initialize an empty data frame to store all results
total_data <- data.frame()

# Outer loop for each loan term series
for(series in series1) {
  
  # Inner loop for each debtor country
  for(i in 1:length(debtor_countries)) {
    current_debtor <- debtor_countries[i]
    
    # Construct the URL for the current debtor and series
    path <- paste(url, current_debtor, "/series/", series,
                  "/counterpart-area/", world_creditor,
                  "/time/", time, end, sep="")
    
    # Getting the data from the API
    customRequest <- GET(url = path)
    customResponse <- content(customRequest, as = "text", encoding = "UTF-8")
    cleanedResponse <- str_replace(customResponse, "^\uFEFF", "") # to avoid byte-order-mark warning
    
    customJSON <- tryCatch(
      fromJSON(cleanedResponse, flatten = TRUE),
      error = function(e) {
        message("Error parsing JSON for debtor: ", current_debtor, 
                " and series: ", series, ". Skipping.")
        return(NULL)
      }
    )
    
    # Plugging the data into the parsing function and binding
    new_data1 <- getData_aggregate(customJSON, current_debtor, series)
    
    if (!is.null(new_data1)) {
      total_data <- rbind(total_data, new_data1)
    }
  }
}

# Clean data set and clarify column names
wide_total_data <- total_data %>% # create separate column for each of the terms and their values
  distinct(.keep_all = TRUE) %>% # remove duplicate rows
  pivot_wider(names_from = series, values_from = data)

wide_final_data_ppg_debt_total <- wide_total_data %>%
  rename(year = year, debtor_country = debtorCountry, 
         total_longterm_ppg_debt = DT.DOD.DPPG.CD, 
         official_cred_ppg_debt = DT.DOD.OFFT.CD,
         prvt_cred_ppg_debt = DT.DOD.PRVT.CD,
         banks_ppg_debt = DT.DOD.PCBK.CD, 
         bonds_ppg_debt = DT.DOD.PBND.CD,
         total_commit = DT.COM.DPPG.CD,
         official_commit = DT.COM.OFFT.CD,
         prvt_commit = DT.COM.PRVT.CD,
         total_disburse_ppg = DT.DIS.DPPG.CD)

# Write as Excel file
write_xlsx(wide_final_data_ppg_debt_total, "Construct_dataset/Main/final_data_ppg_debt_total_dac.xlsx") 

# Calculate the sum of the debt held by different creditor types
wide_final_data_ppg_debt_total <- wide_final_data_ppg_debt_total %>%
  mutate(
    # Create a new column with the predicted total
    total_debt_predicted = 
      official_cred_ppg_debt + prvt_cred_ppg_debt
  )

# Calculate the difference and filter for substantial discrepancies
discrepancies_near <- wide_final_data_ppg_debt_total %>%
  mutate(
    # Calculate the absolute difference between the actual and predicted totals
    debt_difference = total_longterm_ppg_debt - total_debt_predicted
  ) %>%
  filter(abs(debt_difference) > 1) # test whether the components indeed add up to total debt (allowing for $1 rounding errors)

# View the results
# If this data frame is empty, the identity holds within the tolerance level
print(discrepancies_near$debt_difference) # passes check 


################################################################################
############# 6. Retrieve debt and terms disaggregated by creditor #############
################################################################################

# Create new parsing function that accounts for different creditors
getData_single_series <- function(JSON, debtor, creditor, series_name) {
  if (inherits(JSON, "list") && !is.null(JSON[["source"]])) { 
    listLen <- length(JSON[["source"]][["data"]][["value"]]) 
    if (listLen == 0) return(NULL) 
    
    df <- data.frame( 
      year = rep(0, listLen),
      creditorEntity = rep(creditor, listLen),
      debtorCountry = rep(debtor, listLen),
      series = rep(series_name, listLen), 
      data = rep(0, listLen),
      stringsAsFactors = FALSE
    )
    
    for(k in 1:listLen) {
      year <- JSON[["source"]][["data"]][["variable"]][[k]][[3]][[2]] 
      data <- JSON[["source"]][["data"]][["value"]][[k]]
      df[k, "year"] <- as.numeric(year)
      df[k, "data"] <- as.numeric(data)
    }
    return(df)
  } else {
    return(NULL)
  }
}

# Series on debt stock
series_stock <- "DT.DOD.OFFT.CD" # long-term public and publicly guaranteed debt from official creditors
data_stock <- data.frame()

# Loop
for(i in seq_along(debtor_countries)) {
  current_debtor <- debtor_countries[i]
  
  for(j in seq_along(trad_cred_ids)) {
    current_creditor <- trad_cred_ids[j]
    
    path <- paste(url, current_debtor, "/series/", series_stock,
                  "/counterpart-area/", current_creditor,
                  "/time/", time, end, sep="")
    
    customRequest <- GET(url = path)
    customResponse <- content(customRequest, as = "text", encoding = "UTF-8")
    
    customJSON <- tryCatch(fromJSON(customResponse, flatten = TRUE), error = function(e) NULL)
    
    new_data <- getData_single_series(customJSON, current_debtor, current_creditor, series_stock)
    if (!is.null(new_data)) data_stock <- rbind(data_stock, new_data)
  }
}

# Save and clear memory
write.csv(data_stock, "Construct_dataset/Main/debt_stock_official.csv", row.names = FALSE)
rm(data_stock) # Frees up RAM

# Series on commitments
series_commit <- "DT.COM.OFFT.CD" # loan commitments from official creditors
data_commit <- data.frame()

for(i in seq_along(debtor_countries)) {
  current_debtor <- debtor_countries[i]
  
  for(j in seq_along(trad_cred_ids)) {
    current_creditor <- trad_cred_ids[j]
    
    path <- paste(url, current_debtor, "/series/", series_commit,
                  "/counterpart-area/", current_creditor,
                  "/time/", time, end, sep="")
    
    customRequest <- GET(url = path)
    customResponse <- content(customRequest, as = "text", encoding = "UTF-8")
    
    customJSON <- tryCatch(fromJSON(customResponse, flatten = TRUE), error = function(e) NULL)
    
    new_data <- getData_single_series(customJSON, current_debtor, current_creditor, series_commit)
    if (!is.null(new_data)) data_commit <- rbind(data_commit, new_data)
  }
}

write.csv(data_commit, "Construct_dataset/Main/debt_commitments.csv", row.names = FALSE)
rm(data_commit)

# Series on interest rates
series_interest <- "DT.INR.DPPG" # average interest rate on new external debt commitments (%)
data_interest <- data.frame()

for(i in seq_along(debtor_countries)) {
  current_debtor <- debtor_countries[i]
  
  for(j in seq_along(trad_cred_ids)) {
    current_creditor <- trad_cred_ids[j]
    
    path <- paste(url, current_debtor, "/series/", series_interest,
                  "/counterpart-area/", current_creditor,
                  "/time/", time, end, sep="")
    
    customRequest <- GET(url = path)
    customResponse <- content(customRequest, as = "text", encoding = "UTF-8")
    
    customJSON <- tryCatch(fromJSON(customResponse, flatten = TRUE), error = function(e) NULL)
    
    new_data <- getData_single_series(customJSON, current_debtor, current_creditor, series_interest)
    if (!is.null(new_data)) data_interest <- rbind(data_interest, new_data)
  }
}

write.csv(data_interest, "Construct_dataset/Main/debt_interest_rates.csv", row.names = FALSE)
rm(data_interest) 


# Load the saved CSVs
stock_df <- read_csv("Construct_dataset/Main/debt_stock_official.csv")
commit_df <- read_csv("Construct_dataset/Main/debt_commitments.csv")
int_df   <- read_csv("Construct_dataset/Main/debt_interest_rates.csv")

# Aggregate sums (stock, commitments and disbursements)
agg_stock <- stock_df %>%
  group_by(debtorCountry, year) %>%
  summarise(longterm_trad_cred_debt = sum(data, na.rm = TRUE), .groups = 'drop')

agg_commit <- commit_df %>%
  group_by(debtorCountry, year) %>%
  summarise(commitments_trad_cred = sum(data, na.rm = TRUE), .groups = 'drop')

# Aggregate averages (interest rate)
agg_int <- int_df %>%
  group_by(debtorCountry, year) %>%
  summarise(avg_interest_rate_trad = mean(data, na.rm = TRUE), .groups = 'drop') %>%
  mutate(avg_interest_rate_trad = ifelse(is.nan(avg_interest_rate_trad), NA, 
                                         avg_interest_rate_trad))

# Merge everything into one wide panel data set
list_of_dfs <- list(agg_stock, agg_commit, agg_int) # put dfs into one list first

# Merge them all by country and year
final_panel_dac_df <- list_of_dfs %>%
  reduce(full_join, by = c("debtorCountry", "year")) %>%
  rename(debtor_country = debtorCountry) %>%
  arrange(debtor_country, year)

write_xlsx(final_panel_dac_df, "Construct_dataset/Main/debt_data_dac_creditors.xlsx") # save as Excel file


################################################################################
############### 7. Retrieve loan terms for mediation analysis  #################
################################################################################

# Specify series (see WB 2024 Debt Report for selection of indicators)
loan_terms_series <- "DT.INR.DPPG" # average interest rate on new external debt commitments (%)
                     
# Initialize empty data frame
final_data_loan_terms <- data.frame()

# Outer loop for each loan term series
for(series in loan_terms_series) {
  
  # Inner loop for each debtor country
  for(i in 1:length(debtor_countries)) {
    current_debtor <- debtor_countries[i]
    
    # Construct the URL for the current debtor and series
    path <- paste(url, current_debtor, "/series/", series,
                  "/counterpart-area/", world_creditor,
                  "/time/", time, end, sep="")
    
    # Getting the data from the API
    customRequest <- GET(url = path)
    customResponse <- content(customRequest, as = "text", encoding = "UTF-8")
    cleanedResponse <- str_replace(customResponse, "^\uFEFF", "") # to avoid byte-order-mark warning
    
    customJSON <- tryCatch(
      fromJSON(cleanedResponse, flatten = TRUE),
      error = function(e) {
        message("Error parsing JSON for debtor: ", current_debtor, 
                " and series: ", series, ". Skipping.")
        return(NULL)
      }
    )
    
    # Plugging the data into the parsing function and binding
    new_data4 <- getData_aggregate(customJSON, current_debtor, series)
    
    if (!is.null(new_data4)) {
      final_data_loan_terms <- rbind(final_data_loan_terms, new_data4)
    }
  }
}

# Clean data set and clarify column names
wide_loan_terms_data <- final_data_loan_terms %>% # create separate column for each of the terms and their values
  pivot_wider(names_from = series, values_from = data) 

wide_loan_terms_data <- wide_loan_terms_data %>%
  rename(debtor_country = debtorCountry, 
         avg_int_rate = DT.INR.DPPG) # clarify column names

write_xlsx(wide_loan_terms_data, "Construct_dataset/Main/loan_terms_data.xlsx")


################################################################################
######################## 8. Retrieve control variables #########################
################################################################################

# Specify series 
controls_series <- "DT.DOD.DECT.GN.ZS" # external debt stocks as % of GNI

# Initialize empty data frame
final_data_control <- data.frame()

# Outer loop for each loan term series
for(series in controls_series) {
  
  # Inner loop for each debtor country
  for(i in 1:length(debtor_countries)) {
    current_debtor <- debtor_countries[i]
    
    # Construct the URL for the current debtor and series
    path <- paste(url, current_debtor, "/series/", series,
                  "/counterpart-area/", world_creditor,
                  "/time/", time, end, sep="")
    
    # Getting the data from the API
    customRequest <- GET(url = path)
    customResponse <- content(customRequest, as = "text", encoding = "UTF-8")
    cleanedResponse <- str_replace(customResponse, "^\uFEFF", "") # to avoid byte-order-mark warning
    
    customJSON <- tryCatch(
      fromJSON(cleanedResponse, flatten = TRUE),
      error = function(e) {
        message("Error parsing JSON for debtor: ", current_debtor, 
                " and series: ", series, ". Skipping.")
        return(NULL)
      }
    )
    
    # Plugging the data into the parsing function and binding
    new_data5 <- getData_aggregate(customJSON, current_debtor, series)
    
    if (!is.null(new_data5)) {
      final_data_control <- rbind(final_data_control, new_data5)
    }
  }
}

# Clean data set and clarify column names
wide_controls_data <- final_data_control %>% # create separate column for each of the terms and their values
  pivot_wider(names_from = series, values_from = data) 

wide_controls_data <- wide_controls_data %>% 
  rename(debtor_country = debtorCountry,
  ext_debt_perc_gni = DT.DOD.DECT.GN.ZS) # clarify column names

write_xlsx(wide_controls_data, "Construct_dataset/Main/controls_data_dac.xlsx")


################################################################################
###################### 9. Merge all the debt data sets  ########################
################################################################################

df_debt <- wide_final_data_ppg_debt_total %>%
  full_join(final_panel_dac_df, by = c("year", "debtor_country")) %>%
  full_join(wide_loan_terms_data, by = c("year", "debtor_country")) #%>%
  #full_join(wide_controls_data_full, by = c("year", "debtor_country")) 

write_xlsx(df_debt, "Construct_dataset/Main/debt_data_dac.xlsx")                    


################################################################################
################## 10. Now retrieve development indicators  ####################
################################################################################

# Specify series (see World Development Indicators)
dev_indicators <- c("NY.GDP.PCAP.KD", # control variable: GDP p.c. in constant 2015 USD
                    "SP.POP.DPND", # control variable: age dependency ratio as % of working-age population
                    "SP.URB.TOTL.IN.ZS", # control variable: urban population, % of total population
                    "DT.ODA.ODAT.GN.ZS", # control variable: net ODA as % of GNI
                    "NY.GDP.DEFL.KD.ZG", # control variable: inflation as % growth of GDP deflator
                    "NY.GDP.MKTP.KD.ZG", # control variable: annual GDP growth in %
                    "SP.POP.TOTL", # control variable: population size
                    "MS.MIL.XPND.GD.ZS") # robustness check: military spending (% of GDP)
                    
                  
# Retrieve data - can use WDI package here
dev_ind_data = WDI(indicator=dev_indicators, 
                   country=debtor_countries, 
                   start=1990, 
                   end=2023) 

# Drop irrelevant columns
dev_ind_data <- dev_ind_data %>%
  select(-iso2c)

# Rename columns
dev_ind_data <- dev_ind_data %>% 
  rename(full_country_name = country,
  debtor_country = iso3c,
  gdp_pc_const_usd = NY.GDP.PCAP.KD,
  dep_ratio = SP.POP.DPND, 
  urb_pop = SP.URB.TOTL.IN.ZS,
  oda_perc_gni = DT.ODA.ODAT.GN.ZS,
  growth_gdp_defl = NY.GDP.DEFL.KD.ZG,
  gdp_growth = NY.GDP.MKTP.KD.ZG,
  total_pop = SP.POP.TOTL,
  mil_exp = MS.MIL.XPND.GD.ZS)

write_xlsx(dev_ind_data, "Construct_dataset/Main/dev_ind_data_dac.xlsx")


################################################################################
################### 11. Get variables from other data sets  ####################
################################################################################

############################### 11.1: IHME data ################################
health_spend_df <- read_csv("Construct_dataset/Downloaded_data/IHME_health_spending_data.csv") # covers more years than WB

health_spend_df <- health_spend_df %>%
  select(c(location_id, iso3, year, 
           ghes_per_gdp_mean, # gov health spending as % of GDP
           ghes_per_cap_mean, # gov health spending per person
           ghes_per_cap_ppp_mean)) # gov health spending per person in PPP terms

ihme_df <- health_spend_df %>% 
  rename(debtor_country = iso3, 
         ihme_health_exp_gdp = ghes_per_gdp_mean,
         ihme_health_exp_pc = ghes_per_cap_mean,
         ihme_health_exp_pc_ppp = ghes_per_cap_ppp_mean)

all_var_df <- dev_ind_data %>%
  left_join(ihme_df, by = c("debtor_country", "year")) # keep all the obs from dev_ind_data


######################### 11.2: Our World in Data data #########################
educ_exp <- read_csv("Construct_dataset/Downloaded_data/total-government-expenditure-on-education-gdp.csv")

educ_exp <- educ_exp %>%
  select(-Entity) %>%
  filter(Year >= 1990) %>%
  rename(debtor_country = Code,
         year = Year,
         owid_educ_exp_perc_gdp = `Public spending on education as a share of GDP (historical and recent)`) %>%
  mutate(owid_educ_exp_perc_gdp = owid_educ_exp_perc_gdp/100) # convert from % to proportion

tot_educ_exp <- read_csv("Construct_dataset/Downloaded_data/total-government-spending-on-education.csv")

tot_educ_exp <- tot_educ_exp %>%
  select(-Entity) %>%
  filter(Year >= 1990) %>%
  rename(debtor_country = Code,
         year = Year,
         owid_educ_exp_tot = `Total across all levels`) 

educ_data <- educ_exp %>%
  full_join(tot_educ_exp, by = c("debtor_country", "year")) 

all_var_df <- all_var_df %>%
  left_join(educ_data, by = c("debtor_country", "year")) 


################################ 11.3: IMF data ################################
# IMF programs - come in two distinct datasets from the IMF, so need to merge them first before creating the dummy variable for program participation.
imf_mona_programs_old <- read_csv("Construct_dataset/Downloaded_data/IMF_Mona_agreements.csv") %>%
  select(c(`Country Code`, `Country Name`, `Approval Year`, `Initial End Year`,
           `Arrangement Number`)) %>%
  mutate(`Country Code` = as.numeric(`Country Code`)) # to ensure same class as in new dataset

imf_mona_programs_new <- read_excel("Construct_dataset/Downloaded_data/IMF_Mona_agreements_new.xlsx") %>%
  select(c(`Country Code`, `Country Name`, `Approval Year`, `Initial End Year`,
           `Arrangement Number`)) %>%
  mutate(`Country Code` = as.numeric(`Country Code`))

imf_mona_programs <- bind_rows(imf_mona_programs_old, imf_mona_programs_new) %>%
  distinct(`Country Code`, `Approval Year`, `Arrangement Number`, .keep_all = TRUE) # remove duplicates (some arrangements appear in both datasets)

imf_mona_programs <- imf_mona_programs %>%
  rowwise() %>% 
  # Create a list of years from start to end
  mutate(year = list(seq(`Approval Year`, `Initial End Year`))) %>% 
  unnest(year) %>% 
  mutate(imf_active = 1) %>% 
  # Keep only unique country-year combinations (to deal with overlapps) 
  distinct(`Country Code`, `Country Name`, year, imf_active)

imf_mona_data <- imf_mona_programs %>%
  mutate(debtor_country = countrycode(`Country Code`, origin = "imf", 
                                      destination = "iso3c")) %>% # convert country coding
  mutate(debtor_country = case_when(
    `Country Code` == 965 ~ "SRB", # manually deal with cases countrycode can't handle
    `Country Code` == 967 ~ "XKX",
    TRUE ~ debtor_country
  ))

all_var_df <- all_var_df %>%
  left_join(imf_mona_data, 
            by = c("debtor_country", "year")) %>%
 mutate(imf_active = replace_na(imf_active, 0)) # convert NAs to 0 for a clean dummy variable

# Exchange rates
fx_rate_df <- read_csv("Construct_dataset/Downloaded_data/IMF_exchange_rate.csv")

fx_rate_df <- fx_rate_df %>%
  group_by(COUNTRY) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    fx_perc_change = # create new variable with change in exchange rates in %
      ((OBS_VALUE - dplyr::lag(OBS_VALUE, 1)) / dplyr::lag(OBS_VALUE, 1)) * 100
    ) %>%
  select(c(COUNTRY, TIME_PERIOD, OBS_VALUE, fx_perc_change)) %>%
  ungroup()

fx_rate_df <- fx_rate_df %>%
  rename(full_country_name = COUNTRY, 
  year = TIME_PERIOD,
  fx_rate = OBS_VALUE)

# Government balance
gov_bal_df <- read_csv("Construct_dataset/Downloaded_data/IMF_gov_balance_debt_to_gdp.csv")

gov_bal_df <- gov_bal_df %>%
  group_by(COUNTRY) %>%
  arrange(TIME_PERIOD) %>%
  select(-c(FREQUENCY, SCALE)) %>%
  ungroup() %>%
  rename(TIME = TIME_PERIOD, VALUE = OBS_VALUE)

gov_bal_df_wide <- gov_bal_df %>%
  pivot_wider(names_from = "INDICATOR",
    values_from = "VALUE")

gov_bal_df_wide <- gov_bal_df_wide %>%
  rename(full_country_name = COUNTRY, 
  year = TIME,
  gov_bal_perc_gdp = `Net lending (+) / net borrowing (-), General government, Percent of GDP`, 
  debt_perc_gdp = `Gross debt, General government, Percent of GDP`)

# Inflation and current account balance
infl_curr_acc <- read_csv("Construct_dataset/Downloaded_data/IMF_inflation_curr_acc_bal.csv")

infl_curr_acc_df <- infl_curr_acc %>%
  group_by(COUNTRY.ID) %>%
  arrange(TIME_PERIOD) %>%
  select(-c(COUNTRY, FREQUENCY, FREQUENCY.ID, SCALE, SCALE.ID, INDICATOR)) %>%
  ungroup() %>%
  rename(TIME = TIME_PERIOD, VALUE = OBS_VALUE)

infl_curr_acc_df_wide <- infl_curr_acc_df %>%
  pivot_wider(names_from = "INDICATOR.ID",
              values_from = "VALUE")

infl_curr_acc_df_wide <- infl_curr_acc_df_wide %>%
  rename(debtor_country = COUNTRY.ID, 
         year = TIME,
         curr_acc_bal_perc_gdp = `BCA_NGDPD`, 
         infl_avg = `PCPIPCH`,
         infl_eop = `PCPIEPCH`)


# Merge the three IMF data sets
imf_data <- fx_rate_df %>%
  full_join(gov_bal_df_wide, by = c("full_country_name", "year")) 

# Case of Yemen (unified in 1990): use data from North Yemen (Arab Republic) for earlier years - WB doesn't have data on South
# save data from Yemen, Arab Republic as Yemen, Republic of
imf_data$full_country_name[imf_data$full_country_name=="Yemen Arab Republic"] <-
  "Yemen, Republic of"

# Now address issue of country names: get unique country names from both data frames
names_source <- unique(all_var_df$full_country_name)  
names_target <- unique(imf_data$full_country_name)

# Initialize a list to store the best match for each source name
best_matches <- list()

for (i in 1:length(names_source)) {
  source_name <- names_source[i]
  
  # Calculate the Jaccard distance between the source name and all target names
  # Jaccard is good for names with slight differences (e.g., "Republic of..." vs. "Rep. of...")
  distances <- stringdist(source_name, names_target, method = "jaccard")
  
  # Find the index of the closest match (minimum distance)
  min_index <- which.min(distances)
  
  # Store the match and the distance
  best_matches[[i]] <- data.frame(
    all_var_name = source_name,
    imf_data_name = names_target[min_index],
    distance = distances[min_index],
    stringsAsFactors = FALSE
  )
}

# Combine results into a mapping table
name_map_df <- do.call(rbind, best_matches) %>%
  filter(distance < 0.1) %>% # Set tolerance threshold (near 0 means near-perfect match - turned out necessary)
  select(imf_data_name, all_var_name)

# Check for country names that haven't been matched
unmatched_countries1 <- # first in what will be the master data frame
  unique(all_var_df$full_country_name[!all_var_df$full_country_name %in% 
                                        name_map_df$all_var_name])

unmatched_countries2 <- # then in the to-be-joined data frame
  unique(imf_data$full_country_name[!imf_data$full_country_name %in% 
                                        name_map_df$imf_data_name])

# Fix those instances manually
manual_fixes <- data.frame(
  imf_data_name = c("Afghanistan, Islamic Republic of", NA, # no American Samoa in IMF data
                    "Armenia, Republic of", 
                   "Azerbaijan, Republic of", "Belarus, Republic of", 
                   #"China, People's Republic of", 
                   "Comoros, Union of the",
                   "Congo, Democratic Republic of the", "Congo, Republic of", 
                   "Croatia, Republic of", 
                   "Egypt, Arab Republic of, Arab Rep.", 
                   "Equatorial Guinea, Republic of",
                   "Eritrea, The State of", "Eswatini, Kingdom of", 
                   "Ethiopia, The Federal Democratic Republic of",
                   "Fiji, Republic of", 
                   "Iran, Islamic Republic of", 
                   "Kazakhstan, Republic of", 
                   NA, # no North Korea in IMF data
                   "Kosovo, Republic of", "Lao People's Democratic Republic", 
                   "Lesotho, Kingdom of", 
                   "Madagascar, Republic of", 
                   "Marshall Islands, Republic of the", 
                   "Mauritania, Islamic Republic of",
                   "Micronesia, Federated States of", "Moldova, Republic of", 
                   "Mozambique, Republic of", "Nauru, Republic of",
                   "North Macedonia, Republic of", "Palau, Republic of",
                   "São Tomé and Príncipe, Democratic Republic of", 
                   "Serbia, Republic of", 
                   "Somalia",
                   "South Sudan, Republic of", "Tajikistan, Republic of", 
                   "Tanzania, United Republic of", 
                   "Timor-Leste, Democratic Republic of", 
                   "Türkiye, Republic of",
                   "Uzbekistan, Republic of", 
                   "Venezuela, República Bolivariana de", 
                   "Vietnam", 
                   NA, # no West Bank and Gaza in IMF data
                   "Yemen, Republic of"),
  all_var_name = unmatched_countries1,
  stringsAsFactors = FALSE
)

# Append to name map
name_map_df <- bind_rows(
  name_map_df,
  manual_fixes
) %>%
  distinct(all_var_name, .keep_all = TRUE)

# Join IMF data to name map
imf_data_mapped <- imf_data %>%
  left_join(
    name_map_df,
    by = c("full_country_name" = "imf_data_name") 
  )

# First left join for data on inflation and curr. account balance (ISO codes available)
all_var_df <- all_var_df %>%
  left_join(
    infl_curr_acc_df_wide,
    by = (c("debtor_country", "year"))
  )

# Final left join to master data frame
all_var_df <- all_var_df %>%
  left_join(
    imf_data_mapped,
    by = c("full_country_name" = "all_var_name", "year" = "year")
  ) %>%
  select(-full_country_name.y)


######################## 10.4: Capital account openness ########################
cap_acc_open <- read_excel("Construct_dataset/Downloaded_data/Chinn-Ito_index.xls") #kaopen is the index; ka_open the normalized index

cap_acc_open <- cap_acc_open %>%
  select(c(ccode, year, kaopen))

cap_acc_open <- cap_acc_open %>% 
  rename(debtor_country = ccode, cap_acc_openness = kaopen)

all_var_df <- all_var_df %>%
  left_join(
    cap_acc_open,
    by = c("debtor_country", "year")
  )


####################### 11.5: Trade openness from Penn WT ######################
pwt <- read_dta("Construct_dataset/Downloaded_data/pwt110_trade_detail.dta") # Penn World Tables

pwt <- pwt %>%
  group_by(countrycode) %>%
  filter(year >= 1990) %>%
  mutate(
    trade_openness_pwt = abs(csh_x) + abs(csh_m) # construct measure of X + M as share of real GDP in current prices
  ) %>%
  select(c(countrycode, year, trade_openness_pwt)) %>%
  rename(debtor_country = countrycode) %>%
  ungroup() 

all_var_df <- all_var_df %>%
  left_join(
    pwt,
    by = c("debtor_country", "year")
  )


########################## 11.6: Level of democracy ############################
democr_raw <- read_excel("Construct_dataset/Downloaded_data/Freedom_House.xlsx", 
                         skip = 1, col_names = FALSE)

year_row <- as.character(democr_raw[1, ]) 
year_row <- na.locf(year_row) # to fill vector positions with years
year_row <- substr(year_row, nchar(year_row) - 3, nchar(year_row)) # eliminate more complex time intervals in the 1980s

metric_row <- as.character(democr_raw[2, ])

keep_cols <- c(
  TRUE,
  metric_row[-1] != "Status"
)

democr_filtered <- democr_raw[-c(1, 2), keep_cols]

header_vector <- paste0(
  metric_row[keep_cols], 
  "_", 
  year_row[keep_cols]
)

# Clean the first header, which should be the country name (e.g., 'NA_NA' becomes 'Country')
header_vector[1] <- "full_country_name"

# Apply the new headers
colnames(democr_filtered) <- header_vector

# Now case of Yemen: replace empty cells for "Yemen" with values for North Yemen until 1989
north_yemen_idx <- which(democr_filtered$full_country_name == "Yemen, N.")

democr_filtered_clean <- democr_filtered %>%
  mutate(across(
    .cols = PR_1972:CL_1989, # select the exact range of columns
    .fns = ~ case_when( # Conditional function for each selected column
      # Condition: If the current row is the target ("Yemen")
      full_country_name == "Yemen" ~ # Extract value from the "Yemen, N." row for the current column 
        democr_filtered[[north_yemen_idx, cur_column()]],
      TRUE ~ . # Otherwise, keep the original value
    )
  )) %>%
  mutate(across(
    .cols = c(PR_1972, CL_1972),
    .fns = ~ case_when(
      full_country_name == "South Africa" & str_detect(., "\\d+\\(\\d+\\)") ~
        as.character(str_extract(., "(?<=\\()\\d+(?=\\))")), # Capture the digits inside the brackets
      TRUE ~ as.character(.) # Otherwise, keep the original value
    )
  ))


# Reshape to long format
democr_filtered_long <- democr_filtered %>%
  pivot_longer(cols = starts_with(c("PR_", "CL_")),
               names_to = c(".value", "year"),
               names_sep = "_") %>%
  mutate(year = as.numeric(year),
         PR = as.numeric(PR),
         CL = as.numeric(CL)) 

democr_filtered_long <- democr_filtered_long %>% 
  rename(political_rights = PR, civil_liberties = CL)

# Now figure out country names (only given in full here, no ISO-3 codes)
# Get unique country names from both data frames
names_source2 <- unique(all_var_df$full_country_name)  
names_target2 <- unique(democr_filtered_long$full_country_name)

# Initialize a list to store the best match for each source name
best_matches2 <- list()

for (i in 1:length(names_source2)) {
  source_name2 <- names_source2[i]
  
  distances <- stringdist(source_name2, names_target2, method = "jaccard")
  
  # Find the index of the closest match (minimum distance)
  min_index <- which.min(distances)
  
  # Store the match and the distance
  best_matches2[[i]] <- data.frame(
    all_var_name2 = source_name2,
    democr_filt_name = names_target2[min_index],
    distance = distances[min_index],
    stringsAsFactors = FALSE
  )
}

# Combine results into a mapping table
name_map_df2 <- do.call(rbind, best_matches2) %>%
  filter(distance < 0.1) %>% # Set a tolerance threshold 
  select(democr_filt_name, all_var_name2)

# Check for country names that haven't been matched
unmatched_countries3 <- # first in what will be the master data frame
  unique(all_var_df$full_country_name[!all_var_df$full_country_name %in% 
                                        name_map_df2$all_var_name2])

unmatched_countries4 <- # then in the to-be-joined data frame
  unique(democr_filtered_long$full_country_name[!democr_filtered_long$full_country_name 
                                                %in% 
                                                  name_map_df2$democr_filt_name])

# Fix those instances manually
manual_fixes2 <- data.frame(
  democr_filt_name = c(NA, 
    "Congo (Kinshasa)", "Congo (Brazzaville)", "Egypt", "Iran", "North Korea",
    "Kyrgyzstan", "Laos", "Micronesia", "Nauru", "Russia",
    "Somalia", "Syria", "Turkey", "Venezuela",
    "Vietnam", NA, "Yemen"),
  all_var_name2 = unmatched_countries3,
  stringsAsFactors = FALSE
)

# Append to name map
name_map_df2 <- bind_rows(
  name_map_df2,
  manual_fixes2
) %>%
  distinct(all_var_name2, .keep_all = TRUE)

# Join Freedom House data to name map
democr_filt_mapped <- democr_filtered_long %>%
  left_join(
    name_map_df2,
    by = c("full_country_name" = "democr_filt_name") 
  )

# Reverse code
democr_filt_mapped <- democr_filt_mapped %>%
  mutate(
    political_rights = 8 - political_rights,
    civil_liberties = 8 - civil_liberties
  )

# Final left join to master data frame
all_var_df <- all_var_df %>%
  left_join(
    democr_filt_mapped,
    by = c("full_country_name" = "all_var_name2", "year" = "year")
  ) %>%
  select(-full_country_name.y)


########################### 11.7: Occurrence of war ############################
war_df <- read_excel("Construct_dataset/Downloaded_data/War_Uppsala.xlsx") # careful: only countries with conflicts show up - need to record 0 for NAs at the end

war_df <- war_df %>% # drop irrelevant columns
  select(c(location, year, intensity_level)) # no true missing values here because intensity_level has no NAs

war_df_long <- war_df %>%
  separate_rows( # split the 'location' column by comma and create new rows
    location, 
    sep = ","
  ) %>%
  mutate( 
    country = str_trim(location) # create new category country and remove leading/trailing spaces
  ) %>%
  group_by(country, year) %>% # group the data by the unique identifier keys
  summarise( # summarize to get only one row per unique country-year
    max_intensity = max(intensity_level, na.rm = TRUE), #find the maximum intensity level observed for this country-year
    war = if_else(max_intensity >= 2, 1, 0), # "war" variable: 1 if max_intensity is 2 or higher (more than 1,000 casualties), 0 otherwise
    .groups = 'drop' 
  ) %>%
  select(country, year, war) # drop irrelevant columns

war_df_long <- war_df_long %>%
  rename(full_country_name = country)

# Now deal with country names
# Get unique country names from both data frames
names_source3 <- unique(all_var_df$full_country_name)  
names_target3 <- unique(war_df_long$full_country_name)

# Initialize a list to store the best match for each source name
best_matches3 <- list()

for (i in 1:length(names_source3)) {
  source_name3 <- names_source3[i]
  
  distances <- stringdist(source_name3, names_target3, method = "jaccard")
  
  # Find the index of the closest match (minimum distance)
  min_index <- which.min(distances)
  
  # Store the match and the distance
  best_matches3[[i]] <- data.frame(
    all_var_name3 = source_name3,
    war_df_name = names_target3[min_index],
    distance = distances[min_index],
    stringsAsFactors = FALSE
  )
}

# Combine results into a mapping table
name_map_df3 <- do.call(rbind, best_matches3) %>%
  filter(distance < 0.1) %>% # Set a tolerance threshold 
  select(war_df_name, all_var_name3)

# Check for country names that haven't been matched
unmatched_countries5 <- # first in what will be the master data frame
  unique(all_var_df$full_country_name[!all_var_df$full_country_name %in% 
                                        name_map_df3$all_var_name3])

unmatched_countries6 <- # then in the to-be-joined data frame
  unique(war_df_long$full_country_name[!war_df_long$full_country_name %in% 
                                                  name_map_df3$war_df_name])

# Fix those instances manually
manual_fixes3 <- data.frame(
  war_df_name = c(NA, NA, NA, NA, NA, "Bosnia-Herzegovina", NA, 
                  NA, NA, NA, 
                  "Cambodia (Kampuchea)", "DR Congo (Zaire)", "Congo", 
                  "Ivory Coast", NA, 
                  "Egypt", NA, NA, NA, 
                  "Gambia", NA, 
                  "Iran", NA, NA, NA, 
                  "North Korea", NA, "Kyrgyzstan", "Laos", 
                  "Madagascar (Malagasy)", 
                  NA, NA, NA, NA, NA, NA, NA, 
                  "Myanmar (Burma)",
                  NA, NA, NA, "Russia (Soviet Union)", 
                  NA, NA, "Serbia (Yugoslavia)", NA, NA, 
                  "Somalia",
                  NA, NA, 
                  "Syria", NA, NA, "Turkey",
                  NA, NA, NA, "Venezuela", 
                  "Vietnam (North Vietnam)", 
                  NA, "Yemen (North Yemen)", NA, "Zimbabwe (Rhodesia)"),
  all_var_name3 = unmatched_countries5,
  stringsAsFactors = FALSE
)

# Append to name map
name_map_df3 <- bind_rows(
  name_map_df3,
  manual_fixes3
) %>%
  distinct(all_var_name3, .keep_all = TRUE)

# Join Freedom House data to name map
war_df_mapped <- war_df_long %>%
  left_join(
    name_map_df3,
    by = c("full_country_name" = "war_df_name") 
  )

# Final left join to master data frame
all_var_df <- all_var_df %>%
  left_join(
    war_df_mapped,
    by = c("full_country_name" = "all_var_name3", "year" = "year")
  ) %>%
  select(-full_country_name.y) %>%
  mutate(
    war = replace_na(war, 0) # treat missing values as no recorded conflict (either because there was none or the country didn't yet exist)
  )

######################### 11.8: Brent crude oil price ##########################
oil_price_df <- read_csv("Construct_dataset/Downloaded_data/Europe_Brent_Spot_Price_FOB.csv", skip = 4) 

oil_price_df <- oil_price_df %>%
  rename(oil_price = `Europe Brent Spot Price FOB Dollars per Barrel`,
         year = Year)

all_var_df <- all_var_df %>%
  left_join(
    oil_price_df, by = "year"
  )

########################### 11.9: Global GDP growth ############################
gdp_growth_glob = WDI(indicator="NY.GDP.MKTP.KD.ZG",
                      country="WLD", 
                      start=1990, 
                      end=2023) 

# Drop irrelevant columns
gdp_growth_glob <- gdp_growth_glob %>%
  select(-c(country, iso2c, iso3c)) %>%
  rename(gdp_growth_glob = NY.GDP.MKTP.KD.ZG)

all_var_df <- all_var_df %>%
  left_join(
    gdp_growth_glob, by = "year"
  )
  

################# 11.10: Instrument: Effective US interest rate ################
eff_us_int_rate <- read_csv("Construct_dataset/Downloaded_data/FEDFUNDS.csv")
eff_us_int_rate <- eff_us_int_rate %>%
  mutate(
    year = as.numeric(str_sub(observation_date, 1, 4))
  ) %>%
  rename(us_int_rate = FEDFUNDS) %>%
  arrange(year) %>%
  mutate(
    mov_avg_us_int_rate = zoo::rollmean(us_int_rate, k = 3, 
                                        fill = NA, align = "right") # 3-year moving average
  ) %>%
  filter(year >= 1990) %>%
  select(-observation_date)

all_var_df <- all_var_df %>%
  left_join(
    eff_us_int_rate,
    by = "year"
  )


################################################################################
############################## 12. Overall join ################################
################################################################################
master_df <- df_debt %>%
  left_join( # only keep countries that also appear in the debt data
    all_var_df,
    by = c("debtor_country", "year")
  ) 

# address problematic scaling in World Bank data
percentage_vars <- c( # relevant variables whose values are not denoted in decimal form
  "avg_int_rate",
  "avg_interest_rate_trad",
  "oda_perc_gni", 
  "gov_bal_perc_gdp", 
  "dep_ratio", 
  "urb_pop", 
  "fx_perc_change", 
  "gdp_growth",
  "gdp_growth_glob",
  "infl_avg",
  "infl_eop",
  "us_int_rate",
  "mov_avg_us_int_rate",
  "mil_exp"
)

# Loop through each variable and apply the logic
for (var in percentage_vars) {
  master_df[[var]] <- master_df[[var]] / 100
  }


# write file into new analysis folder
write_xlsx(master_df, "Analysis/master_data.xlsx")  
