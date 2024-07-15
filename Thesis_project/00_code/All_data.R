getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
ardeco <- read_csv("02_intermediary-input/ardeco_dataset.csv")
eurostat <- read_csv("02_intermediary-input/eurostat_dataset.csv") %>% 
  select(-Name)
extra <- read_csv("02_intermediary-input/extra_dataset.csv")

# nuts_ardeco <- unique(ardeco$NUTS)
# nuts_eurostat <- unique(eurostat$NUTS)
# setdiff(nuts_eurostat, nuts_ardeco)
# setdiff(nuts_ardeco, nuts_eurostat)

data_EU <- ardeco %>%
  full_join(eurostat)

data_A <- data_EU %>%
  mutate(Labor_force = Employment_abs + Unemployment_abs,
         emp_rate = Employment_abs / Population_abs,
         unemp_rate = Unemployment_abs / Population_abs,
         ULFS_1 = Unempl_edu_1 / Labor_force,
         ULFS_2 = Unempl_edu_2 / Labor_force,
         ULFS_3 = Unempl_edu_3 / Labor_force,
         LFS_1 = Empl_edu_1 / Labor_force,
         LFS_2 = Empl_edu_2 / Labor_force,
         LFS_3 = Empl_edu_3 / Labor_force,
         GDP_capita = GDP_EUR / Population_abs,
         Prodx_A = GVA_NACE_A / EMP_NACE_A,
         `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
         Prodx_F = GVA_NACE_F / EMP_NACE_F,
         `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
         Prodx_J = GVA_NACE_J / EMP_NACE_J,
         Prodx_K = GVA_NACE_K / EMP_NACE_K,
         Prodx_L = GVA_NACE_L / EMP_NACE_L,
         `Prodx_M-N` = `GVA_NACE_M-N` / `EMP_NACE_M-N`,
         `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
         `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
         inv_rate = GFCF_EUR / GVA_EUR,
         GFCF_share = GFCF_EUR/GDP_EUR,
         # for Albania trick:
            Labor_force_edu_1_AL_abs = Labor_force*(A__LF_edu_1_share), 
            Labor_force_edu_2_AL_abs = Labor_force*(A__LF_edu_2_share),
            Labor_force_edu_3_AL_abs = Labor_force*(A__LF_edu_2_share),
            Unempl_edu_1_abs = Labor_force_edu_1_AL_abs*A__Unempl_edu_1_share,
            Unempl_edu_2_abs = Labor_force_edu_2_AL_abs*A__Unempl_edu_2_share,
            Unempl_edu_3_abs = Labor_force_edu_3_AL_abs*A__Unempl_edu_3_share,
            ULFS_1.AL = Unempl_edu_1_abs/ Labor_force,
            ULFS_2.AL = Unempl_edu_2_abs/ Labor_force,
            ULFS_3.AL = Unempl_edu_3_abs/ Labor_force,
            ULFS_1 = coalesce(ULFS_1, ULFS_1.AL),
            ULFS_2 = coalesce(ULFS_2, ULFS_3.AL),
            ULFS_3 = coalesce(ULFS_2, ULFS_3.AL)) %>% 
  select(1:3, Migration_abs, Labor_Productivity_abs, Wage_EUR, Wage_NCU,
         35:41, 48:75)

data_B <- extra %>%
  rename(GFCF_share = GFCF_share_EXTRA) %>% 
  mutate(Labor_productivity_abs = GDP_EUR/Employment_abs,
         emp_rate = Employment_abs / Population_abs,
         Labor_force = Employment_abs + Unemployment_abs,
         ULFS_1 = Unempl_edu_1 / Labor_force,
         ULFS_2 = Unempl_edu_2 / Labor_force,
         ULFS_3 = Unempl_edu_3 / Labor_force,
         LFS_1 = Empl_edu_1 / Labor_force,
         LFS_2 = Empl_edu_2 / Labor_force,
         LFS_3 = Empl_edu_3 / Labor_force,
         GDP_capita = GDP_EUR / Population_abs,
         Prodx_A = GVA_NACE_A / EMP_NACE_A,
         `Prodx_B-E` = `GVA_NACE_B-E` / `EMP_NACE_B-E`,
         Prodx_F = GVA_NACE_F / EMP_NACE_F,
         `Prodx_G-I` = `GVA_NACE_G-I` / `EMP_NACE_G-I`,
         Prodx_J = GVA_NACE_J / EMP_NACE_J,
         Prodx_K = GVA_NACE_K / EMP_NACE_K,
         Prodx_L = GVA_NACE_L / EMP_NACE_L,
         `Prodx_M-N` = `GVA_NACE_M-N` / `EMP_NACE_M-N`,
         `Prodx_O-Q` = `GVA_NACE_O-Q` / `EMP_NACE_O-Q`,
         `Prodx_R-U` = `GVA_NACE_R-U` / `EMP_NACE_R-U`,
         inv_rate = GFCF_NCU / GVA_NCU,
         # for BIH trick:
         Labor_force_edu_1_abs = Labor_force*(A__LF_edu_1_share), 
         Labor_force_edu_2_abs = Labor_force*(A__LF_edu_2_share),
         Labor_force_edu_3_abs = Labor_force*(A__LF_edu_2_share),
         Unempl_edu_1_abs = Labor_force_edu_1_abs*A__Unempl_edu_1_share,
         Unempl_edu_2_abs = Labor_force_edu_2_abs*A__Unempl_edu_2_share,
         Unempl_edu_3_abs = Labor_force_edu_3_abs*A__Unempl_edu_3_share,
         ULFS_1.BA = Unempl_edu_1_abs/ Labor_force,
         ULFS_2.BA = Unempl_edu_2_abs/ Labor_force,
         ULFS_3.BA = Unempl_edu_3_abs/ Labor_force,
         ULFS_1 = coalesce(ULFS_1, ULFS_1.BA),
         ULFS_2 = coalesce(ULFS_2, ULFS_3.BA),
         ULFS_3 = coalesce(ULFS_2, ULFS_3.BA))
