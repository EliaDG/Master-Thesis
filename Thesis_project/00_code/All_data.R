getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
ardeco <- read_csv("02_intermediary-input/ardeco_dataset.csv")
eurostat <- read_csv("02_intermediary-input/eurostat_dataset.csv")

# nuts_ardeco <- unique(ardeco$NUTS)
# nuts_eurostat <- unique(eurostat$NUTS)
# setdiff(nuts_eurostat, nuts_ardeco)
# setdiff(nuts_ardeco, nuts_eurostat)

eu_data <- ardeco %>%
  left_join(eurostat, by = c("NUTS", "Year")) %>% 
  mutate(Prodx_A = GVA_NACE_A / EMP_NACE_A,
         `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
         Prodx_F = GVA_NACE_F / EMP_NACE_F,
         `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
         Prodx_J = GVA_NACE_J / EMP_NACE_J,
         Prodx_K = GVA_NACE_K / EMP_NACE_K,
         Prodx_L = GVA_NACE_L / EMP_NACE_L,
         `Prodx_M-N` = `GVA_NACE_M-N` / `EMP_NACE_M-N`,
         `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
         `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
         gdpc = GDP_abs / Population_abs,
         Migration_rate = Migration_abs / Population_abs,
         Labor_force = Employment_abs + Unemployment_abs,
         emp_rate = Employment_abs / Population_abs,
         unemp_rate = Unemployment_abs / Population_abs,
         Edu_1 = Pop_edu_1 / Population_abs,
         Edu_2 = Pop_edu_2 / Population_abs,
         Edu_3 = Pop_edu_3 / Population_abs,
         LFS_1 = Empl_edu_1 / Labor_force,
         LFS_2 = Empl_edu_2 / Labor_force,
         LFS_3 = Empl_edu_3 / Labor_force,
         ULFS_1 = Unempl_edu_1 / Labor_force,
         ULFS_2 = Unempl_edu_2 / Labor_force,
         ULFS_3 = Unempl_edu_3 / Labor_force,
         Labor_force_edu_1 = (Empl_edu_1+Unempl_edu_1)/Labor_force, #for Albania trick
         Labor_force_edu_2 = (Empl_edu_2+Unempl_edu_2)/Labor_force,
         Labor_force_edu_3 = (Empl_edu_3+Unempl_edu_3)/Labor_force,
         inve_rate = GFCF_abs / GVA_abs) %>%
  select(1:3, 62:70)
