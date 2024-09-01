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
         GVA_primary = (GVA_NACE_A+`GVA_NACE_B-E`+GVA_NACE_F)/GVA_EUR,
         GVA_services = (`GVA_NACE_G-I`+GVA_NACE_J+GVA_NACE_K+`GVA_NACE_L-M-N`)/GVA_EUR,
         GVA_public = (`GVA_NACE_O-Q`+`GVA_NACE_R-U`)/GVA_EUR) %>%
  select(NUTS, Name, Year, Employment_abs, GFCF_EUR,  Migration_abs, Labor_Productivity_abs,
         Unemployment_abs, Wage_EUR, 33:43)

data_EXTRA <- extra %>%
  mutate(inv_rate = GFCF_NCU / GVA_NCU,
         GVA_primary = (GVA_NACE_A+`GVA_NACE_B-E`+GVA_NACE_F)/GVA_EUR,
         GVA_services = (`GVA_NACE_G-I`+GVA_NACE_J+GVA_NACE_K+`GVA_NACE_L-M-N`)/GVA_EUR,
         GVA_public = (`GVA_NACE_O-Q`+`GVA_NACE_R-U`)/GVA_EUR) %>% 
  select(NUTS, Name, Year, Employment_abs, 29:44) %>% 
  select(-GFCF_NCU)
setdiff(colnames(data_EXTRA), colnames(data_EU))
setdiff(colnames(data_EU), colnames(data_EXTRA))

dataset <- full_join(data_EU, data_EXTRA) %>%
  full_join(cherry) %>% 
  full_join(cream) %>% 
  arrange(NUTS, Year) %>% 
  mutate(Country = as.character(sapply(NUTS, mapping_nuts)),
         # Employment_rate = Employment_abs / Population_abs, #Decided to rely on direct % to avoid mistakes
         # Unemployment_rate = Unemployment_abs / Population_abs,
         Migration_rate = Migration_abs/Population_abs,
         GFCF_share = if_else(NUTS %in% c("BA00", "MD00", "XK00"), GFCF_share, GFCF_EUR / GDP_EUR),
         Labor_Productivity_abs = if_else(NUTS %in% c("BA00", "MD00", "XK00"), GDP_EUR/Employment_abs, Labor_Productivity_abs)) %>% 
  select(NUTS, Name, Country, Year, GDP_growth, GDP_capita, GDP_lag, everything()) %>% 
  select(-c(GFCF_EUR, Migration_abs, Unemployment_abs))

#SAVING
write.csv(dataset, file = here("02_intermediary-input", "dataset-merged.csv"), row.names = FALSE)

#Appendix
# Prodx_A = GVA_NACE_A / EMP_NACE_A,
#      `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
#      Prodx_F = GVA_NACE_F / EMP_NACE_F,
#      `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
#      Prodx_J = GVA_NACE_J / EMP_NACE_J,
#      Prodx_K = GVA_NACE_K / EMP_NACE_K,
#      `Prodx_L-M-N` = `GVA_NACE_L-M-N` / `EMP_NACE_L-M-N`,
#      `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
#      `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
#      GVA_A = GVA_NACE_A/GVA_EUR,
#      `GVA_B-E` = `GVA_NACE_B-E`/GVA_EUR,
#      GVA_F = GVA_NACE_F/GVA_EUR,
#      `GVA_G-I` = `GVA_NACE_G-I`/GVA_EUR,
#      GVA_J = GVA_NACE_J/GVA_EUR,
#      GVA_K = GVA_NACE_K/GVA_EUR,
#      `GVA_L-M-N` = `GVA_NACE_L-M-N`/GVA_EUR,
#      `GVA_O-Q` =`GVA_NACE_O-Q`/GVA_EUR,
#      `GVA_R-U`= `GVA_NACE_R-U`/GVA_EUR,
