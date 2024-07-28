getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
ardeco <- read_csv("02_intermediary-input/ardeco_dataset.csv")
eurostat <- read_csv("02_intermediary-input/eurostat_dataset.csv") %>% 
  select(-Name)
extra <- read_csv("02_intermediary-input/extra_dataset.csv")
#shapefiles <- st_read(dsn = "02_intermediary-input/geometries.shp", options = "ENCODING=ISO-8859-1")

data_EU <- full_join(ardeco, eurostat) %>%
  mutate(Labor_force_abs = Employment_abs + Unemployment_abs,
         Employment_rate = Employment_abs / Population_abs,
         Unemployment_rate = Unemployment_abs / Population_abs,
         Unemp_edu_1_rate = Unempl_edu_1 / Labor_force_abs,
         Unemp_edu_2_rate = Unempl_edu_2 / Labor_force_abs,
         Unemp_edu_3_rate = Unempl_edu_3 / Labor_force_abs,
         Emp_edu_1_rate = Empl_edu_1 / Labor_force_abs,
         Emp_edu_2_rate = Empl_edu_2 / Labor_force_abs,
         Emp_edu_3_rate = Empl_edu_3 / Labor_force_abs,
         GDP_capita = GDP_EUR / Population_abs,
         Prodx_A = GVA_NACE_A / EMP_NACE_A,
         `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
         Prodx_F = GVA_NACE_F / EMP_NACE_F,
         `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
         Prodx_J = GVA_NACE_J / EMP_NACE_J,
         Prodx_K = GVA_NACE_K / EMP_NACE_K,
         Prodx_L = GVA_NACE_L / EMP_NACE_L,
         `Prodx_M-N` = `GVA_NACE_M-N` / `EMP_NACE_M-N`,
         `Prodx_L-M-N` = `GVA_NACE_L-M-N` / `EMP_NACE_L-M-N`,
         `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
         `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
         inv_rate = GFCF_EUR / GVA_EUR,
         GFCF_share = GFCF_EUR/GDP_EUR,
         # for Albania proxy:
         Labor_force_edu_1_AL_abs = if_else(NUTS == "AL00", Labor_force_abs * A__LF_edu_1_share, NA_real_), 
         Labor_force_edu_2_AL_abs = if_else(NUTS == "AL00", Labor_force_abs * A__LF_edu_2_share, NA_real_),
         Labor_force_edu_3_AL_abs = if_else(NUTS == "AL00", Labor_force_abs * A__LF_edu_3_share, NA_real_),
         Unempl_edu_1_AL_abs = if_else(NUTS == "AL00", Labor_force_edu_1_AL_abs * A__Unempl_edu_1_share, NA_real_),
         Unempl_edu_2_AL_abs = if_else(NUTS == "AL00", Labor_force_edu_2_AL_abs * A__Unempl_edu_2_share, NA_real_),
         Unempl_edu_3_AL_abs = if_else(NUTS == "AL00", Labor_force_edu_3_AL_abs * A__Unempl_edu_3_share, NA_real_),
         Unemp_edu_1_rate.AL = if_else(NUTS == "AL00", Unempl_edu_1_AL_abs / Labor_force_abs, NA_real_),
         Unemp_edu_2_rate.AL = if_else(NUTS == "AL00", Unempl_edu_2_AL_abs / Labor_force_abs, NA_real_),
         Unemp_edu_3_rate.AL = if_else(NUTS == "AL00", Unempl_edu_3_AL_abs / Labor_force_abs, NA_real_),
         Empl_edu_1_AL_abs = if_else(NUTS == "AL00", Labor_force_edu_1_AL_abs - Unempl_edu_1_AL_abs, NA_real_),
         Empl_edu_2_AL_abs = if_else(NUTS == "AL00", Labor_force_edu_2_AL_abs - Unempl_edu_2_AL_abs, NA_real_),
         Empl_edu_3_AL_abs = if_else(NUTS == "AL00", Labor_force_edu_2_AL_abs - Unempl_edu_3_AL_abs, NA_real_),
         Emp_edu_1_rate.AL = if_else(NUTS == "AL00", Empl_edu_1_AL_abs / Labor_force_abs, NA_real_),
         Emp_edu_2_rate.AL = if_else(NUTS == "AL00", Empl_edu_2_AL_abs / Labor_force_abs, NA_real_),
         Emp_edu_3_rate.AL = if_else(NUTS == "AL00", Empl_edu_3_AL_abs / Labor_force_abs, NA_real_),
         Unemp_edu_1_rate = coalesce(Unemp_edu_1_rate, Unemp_edu_1_rate.AL),
         Unemp_edu_2_rate = coalesce(Unemp_edu_2_rate, Unemp_edu_2_rate.AL),
         Unemp_edu_3_rate = coalesce(Unemp_edu_3_rate, Unemp_edu_3_rate.AL),
         Emp_edu_1_rate = coalesce(Emp_edu_1_rate, Emp_edu_1_rate.AL),
         Emp_edu_2_rate = coalesce(Emp_edu_2_rate, Emp_edu_2_rate.AL),
         Emp_edu_3_rate = coalesce(Emp_edu_3_rate, Emp_edu_3_rate.AL)) %>%
  select(1:5, Migration_abs, Population_abs, Labor_Productivity_abs, Wage_EUR, Wage_NCU, GDP_growth, Activity_rate, 51:79)

data_EXTRA <- extra %>%
  mutate(Labor_force_abs = Employment_abs + Unemployment_abs,
         Employment_rate = Employment_abs / Population_abs,
         Unemployment_rate = Unemployment_abs / Population_abs,
         Unemp_edu_1_rate = Unempl_edu_1 / Labor_force_abs,
         Unemp_edu_2_rate = Unempl_edu_2 / Labor_force_abs,
         Unemp_edu_3_rate = Unempl_edu_3 / Labor_force_abs,
         Emp_edu_1_rate = Empl_edu_1 / Labor_force_abs,
         Emp_edu_2_rate = Empl_edu_2 / Labor_force_abs,
         Emp_edu_3_rate = Empl_edu_3 / Labor_force_abs,
         GDP_capita = GDP_EUR / Population_abs,
         Prodx_A = GVA_NACE_A / EMP_NACE_A,
         `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
         Prodx_F = GVA_NACE_F / EMP_NACE_F,
         `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
         Prodx_J = GVA_NACE_J / EMP_NACE_J,
         Prodx_K = GVA_NACE_K / EMP_NACE_K,
         Prodx_L = GVA_NACE_L / EMP_NACE_L,
         `Prodx_M-N` = `GVA_NACE_M-N` / `EMP_NACE_M-N`,
         `Prodx_L-M-N` = `GVA_NACE_L-M-N` / `EMP_NACE_L-M-N`,
         `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
         `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
         inv_rate = GFCF_NCU / GVA_NCU,
         Labor_Productivity_abs = GDP_EUR/Employment_abs,
         # for BIH proxy:
         Labor_force_edu_1_abs = if_else(NUTS == "BA00", Labor_force_abs*(A__LF_edu_1_share), NA_real_), 
         Labor_force_edu_2_abs = if_else(NUTS == "BA00", Labor_force_abs*(A__LF_edu_2_share), NA_real_),
         Labor_force_edu_3_abs = if_else(NUTS == "BA00", Labor_force_abs*(A__LF_edu_2_share), NA_real_),
         Unempl_edu_1_abs = if_else(NUTS == "BA00", Labor_force_edu_1_abs*A__Unempl_edu_1_share, NA_real_),
         Unempl_edu_2_abs = if_else(NUTS == "BA00", Labor_force_edu_2_abs*A__Unempl_edu_2_share, NA_real_),
         Unempl_edu_3_abs = if_else(NUTS == "BA00", Labor_force_edu_3_abs*A__Unempl_edu_3_share, NA_real_),
         Unemp_edu_1_rate.BA = if_else(NUTS == "BA00", Unempl_edu_1_abs/ Labor_force_abs, NA_real_),
         Unemp_edu_2_rate.BA = if_else(NUTS == "BA00", Unempl_edu_2_abs/ Labor_force_abs, NA_real_),
         Unemp_edu_3_rate.BA = if_else(NUTS == "BA00", Unempl_edu_3_abs/ Labor_force_abs, NA_real_),
         Empl_edu_1_BA_abs = if_else(NUTS == "BA00", Labor_force_edu_1_abs - Unempl_edu_1_abs, NA_real_),
         Empl_edu_2_BA_abs = if_else(NUTS == "BA00", Labor_force_edu_2_abs - Unempl_edu_2_abs, NA_real_),
         Empl_edu_3_BA_abs = if_else(NUTS == "BA00", Labor_force_edu_2_abs - Unempl_edu_3_abs, NA_real_),
         Emp_edu_1_rate.BA = if_else(NUTS == "BA00", Empl_edu_1_BA_abs / Labor_force_abs, NA_real_),
         Emp_edu_2_rate.BA = if_else(NUTS == "BA00", Empl_edu_2_BA_abs / Labor_force_abs, NA_real_),
         Emp_edu_3_rate.BA = if_else(NUTS == "BA00", Empl_edu_3_BA_abs / Labor_force_abs, NA_real_),
         Unemp_edu_1_rate = coalesce(Unemp_edu_1_rate, Unemp_edu_1_rate.BA),
         Unemp_edu_2_rate = coalesce(Unemp_edu_2_rate, Unemp_edu_2_rate.BA),
         Unemp_edu_3_rate = coalesce(Unemp_edu_3_rate, Unemp_edu_3_rate.BA),
         Emp_edu_1_rate = coalesce(Emp_edu_1_rate, Emp_edu_1_rate.BA),
         Emp_edu_2_rate = coalesce(Emp_edu_2_rate, Emp_edu_2_rate.BA),
         Emp_edu_3_rate = coalesce(Emp_edu_3_rate, Emp_edu_3_rate.BA),
         #For Kosovo proxy:
         Pop_edu_1 = if_else(NUTS == "XK00", A__LF_edu_1_share, Pop_edu_1),
         Pop_edu_2 = if_else(NUTS == "XK00", A__LF_edu_2_share, Pop_edu_2),
         Pop_edu_3 = if_else(NUTS == "XK00", A__LF_edu_3_share, Pop_edu_3)) %>% 
  select(1:3, Employment_abs, GDP_EUR, Population_abs, GDP_growth, Activity_rate, Wage_EUR, Wage_NCU, NEET_share, Migration_abs, GFCF_share, 52:79)

dataset <- full_join(data_EU, data_EXTRA) %>%
  arrange(NUTS, Year) %>% 
  mutate(Country = sapply(NUTS, mapping_nuts)) %>% 
  select(Country, NUTS, Name, Year, GDP_growth, everything()) %>% 
  select(-c(Wage_NCU,Unemp_edu_1_rate,Unemp_edu_2_rate,Unemp_edu_3_rate))

#SAVING
write.csv(dataset, file = here("02_intermediary-input", "dataset-merged.csv"), row.names = FALSE)
