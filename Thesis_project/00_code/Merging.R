getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
ardeco <- read_csv("02_intermediary-input/ardeco_dataset.csv")
eurostat <- read_csv("02_intermediary-input/eurostat_dataset.csv") %>% 
  select(-Name)
extra <- read_csv("02_intermediary-input/extra_dataset.csv")

data_EU <- full_join(ardeco, eurostat) %>%
  mutate(Labor_force_abs = Employment_abs + Unemployment_abs,
         Employment_rate = Employment_abs / Population_abs,
         Unemployment_rate = Unemployment_abs / Population_abs,
         Prodx_A = GVA_NACE_A / EMP_NACE_A,
         `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
         Prodx_F = GVA_NACE_F / EMP_NACE_F,
         `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
         Prodx_J = GVA_NACE_J / EMP_NACE_J,
         Prodx_K = GVA_NACE_K / EMP_NACE_K,
         `Prodx_L-M-N` = `GVA_NACE_L-M-N` / `EMP_NACE_L-M-N`,
         `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
         `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
         inv_rate = GFCF_EUR / GVA_EUR,
         GFCF_share = GFCF_EUR/GDP_EUR,
         GVA_A = GVA_NACE_A/GVA_EUR,
         `GVA_B-E` = `GVA_NACE_B-E`/GVA_EUR,
         GVA_F = GVA_NACE_F/GVA_EUR,
         `GVA_G-I` = `GVA_NACE_G-I`/GVA_EUR,
         GVA_J = GVA_NACE_J/GVA_EUR,
         GVA_K = GVA_NACE_K/GVA_EUR,
         `GVA_L-M-N` = `GVA_NACE_L-M-N`/GVA_EUR,
         `GVA_O-Q` =`GVA_NACE_O-Q`/GVA_EUR,
         `GVA_R-U`= `GVA_NACE_R-U`/GVA_EUR) %>%
  select(1:5, Migration_abs, Population_abs, Pop_growth, Labor_Productivity_abs, Wage_EUR, 38:69)

data_EXTRA <- extra %>%
  mutate(Labor_force_abs = Employment_abs + Unemployment_abs,
         Employment_rate = Employment_abs / Population_abs,
         Unemployment_rate = Unemployment_abs / Population_abs,
         Prodx_A = GVA_NACE_A / EMP_NACE_A,
         `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
         Prodx_F = GVA_NACE_F / EMP_NACE_F,
         `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
         Prodx_J = GVA_NACE_J / EMP_NACE_J,
         Prodx_K = GVA_NACE_K / EMP_NACE_K,
         `Prodx_L-M-N` = `GVA_NACE_L-M-N` / `EMP_NACE_L-M-N`,
         `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
         `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
         inv_rate = GFCF_NCU / GVA_NCU,
         Labor_Productivity_abs = GDP_EUR/Employment_abs,
         GVA_A = GVA_NACE_A/GVA_EUR,
         `GVA_B-E` = `GVA_NACE_B-E`/GVA_EUR,
         GVA_F = GVA_NACE_F/GVA_EUR,
         `GVA_G-I` = `GVA_NACE_G-I`/GVA_EUR,
         GVA_J = GVA_NACE_J/GVA_EUR,
         GVA_K = GVA_NACE_K/GVA_EUR,
         `GVA_L-M-N` = `GVA_NACE_L-M-N`/GVA_EUR,
         `GVA_O-Q` =`GVA_NACE_O-Q`/GVA_EUR,
         `GVA_R-U`= `GVA_NACE_R-U`/GVA_EUR,
         #For Kosovo proxy:
         Pop_edu_1 = if_else(NUTS == "XK00", LF_edu_1_share/100, Pop_edu_1),
         Pop_edu_2 = if_else(NUTS == "XK00", LF_edu_2_share/100, Pop_edu_2),
         Pop_edu_3 = if_else(NUTS == "XK00", LF_edu_3_share/100, Pop_edu_3)) %>% 
  select(1:4, 30:33, Wage_EUR, 37:73) %>% 
  select(-starts_with("LF_edu_"), -GFCF_NCU)
setdiff(colnames(data_EXTRA), colnames(data_EU))
setdiff(colnames(data_EU), colnames(data_EXTRA))

dataset <- full_join(data_EU, data_EXTRA) %>%
  arrange(NUTS, Year) %>% 
  mutate(Country = sapply(NUTS, mapping_nuts)) %>% 
  select(Country, NUTS, Name, Year, GDP_growth, everything())

#SAVING
write.csv(dataset, file = here("02_intermediary-input", "dataset-merged.csv"), row.names = FALSE)
