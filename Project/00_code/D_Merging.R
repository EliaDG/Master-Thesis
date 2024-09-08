getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
ardeco <- read_csv("02_intermediary-input/ardeco_dataset.csv")
eurostat <- read_csv("02_intermediary-input/eurostat_dataset.csv") %>% 
  select(-Name)
extra <- read_csv("02_intermediary-input/extra_dataset.csv")
cherry <- read_csv("02_intermediary-input/GDP-POP_dataset.csv")
cream <- read_csv("02_intermediary-input/labour.csv")

#Merging ------
data_EU <- full_join(ardeco, eurostat) %>%
  mutate(inv_rate = GFCF_EUR / GVA_EUR,
         GVA_agriculture = (GVA_NACE_A)/GVA_EUR,
         GVA_industry = (`GVA_NACE_B-E`)/GVA_EUR,
         GVA_construction = (GVA_NACE_F)/GVA_EUR,
         #GVA_primary = (GVA_NACE_A+`GVA_NACE_B-E`+GVA_NACE_F)/GVA_EUR,
         GVA_services = (`GVA_NACE_G-I`+GVA_NACE_J+GVA_NACE_K+`GVA_NACE_L-M-N`)/GVA_EUR,
         GVA_public = (`GVA_NACE_O-Q`+`GVA_NACE_R-U`)/GVA_EUR) %>%
  select(NUTS, Name, Year, Employment_abs, GFCF_EUR,  Migration_abs, Labor_Productivity_abs,
         Unemployment_abs, Wage_EUR, 33:45) %>% 
  rename(Labor_Prodx = Labor_Productivity_abs)

data_EXTRA <- extra %>%
  mutate(inv_rate = GFCF_NCU / GVA_NCU,
         GVA_agriculture = (GVA_NACE_A)/GVA_EUR,
         GVA_industry = (`GVA_NACE_B-E`)/GVA_EUR,
         GVA_construction = (GVA_NACE_F)/GVA_EUR,
         #GVA_primary = (GVA_NACE_A+`GVA_NACE_B-E`+GVA_NACE_F)/GVA_EUR,
         GVA_services = (`GVA_NACE_G-I`+GVA_NACE_J+GVA_NACE_K+`GVA_NACE_L-M-N`)/GVA_EUR,
         GVA_public = (`GVA_NACE_O-Q`+`GVA_NACE_R-U`)/GVA_EUR) %>% 
  select(NUTS, Name, Year, Employment_abs, 29:46) %>% 
  select(-GFCF_NCU)
setdiff(colnames(data_EXTRA), colnames(data_EU))
setdiff(colnames(data_EU), colnames(data_EXTRA))

dataset <- full_join(data_EU, data_EXTRA) %>%
  full_join(cherry) %>% 
  full_join(cream) %>% 
  arrange(NUTS, Year) %>% 
  mutate(Country = as.character(sapply(NUTS, mapping_nuts)),
         Employment_rate = Employment_abs / Population_abs,
         Unemployment_rate = Unemployment_abs / Population_abs,
         Migration_rate = Migration_abs/Population_abs,
         GFCF_share = if_else(NUTS %in% c("BA00", "MD00", "XK00"), GFCF_share, GFCF_EUR / GDP_EUR),
         Labor_Prodx = if_else(NUTS %in% c("BA00", "MD00", "XK00"), GDP_EUR/Employment_abs, Labor_Prodx)) %>% 
  select(NUTS, Name, Country, Year, GDP_growth, GDP_capita, everything()) %>% 
  select(-c(GFCF_EUR, Migration_abs, Unemployment_abs, GDP_EUR)) %>% 
  rename(GDP_EUR = GDP_EUR_to_keep) %>% 
  as.data.frame() %>%
  filter(!NUTS %in% c("AL01", "AL02", "AL03", "HR02", "HR05", "HR06", "HU11", 
                      "HU12", "LT01", "LT02", "UKI3", "UKI4", "UKI5" , "UKI6", 
                      "UKI7", "IE01", "IE02", "UKM2", "UKM3", "FI20"),
         Year %in% 2008:2019)

#SAVING
write.csv(dataset, file = here("02_intermediary-input", "dataset-merged.csv"), row.names = FALSE)
