getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#READING DATA
Fertility_rate <- read_excel("01_data-input/Eurostat/demo_r_frate2.xlsx", sheet = "Data_clean", na = ":")
Life_expectancy <- read_excel("01_data-input/Eurostat/demo_r_mlifexp.xlsx", sheet = "Data_clean", na = ":")
NEET <- read_excel("01_data-input/Eurostat/edat_lfse_22.xlsx", sheet = "Data1_clean", na = ":")
Activity_rate <- read_excel("01_data-input/Eurostat/lfst_r_lfp2actrt.xlsx", sheet = "Data2_clean", na = ":")

primary_education <- read_excel("01_data-input/Eurostat/lfst_r_lfsd2pop.xlsx", sheet = "Data_clean", na = ":")
secondary_education <- read_excel("01_data-input/Eurostat/lfst_r_lfsd2pop.xlsx", sheet = "Data1_clean", na = ":")
tertiary_education <- read_excel("01_data-input/Eurostat/lfst_r_lfsd2pop.xlsx", sheet = "Data2_clean", na = ":")

employment_primary_edu <- read_excel("01_data-input/Eurostat/lfst_r_lfe2eedu.xlsx", sheet = "Data_clean", na = ":")
employment_secondary_edu <- read_excel("01_data-input/Eurostat/lfst_r_lfe2eedu.xlsx", sheet = "Data1_clean", na = ":")
employment_tertiary_edu <- read_excel("01_data-input/Eurostat/lfst_r_lfe2eedu.xlsx", sheet = "Data2_clean", na = ":")

unempl_primary_edu <- read_excel("01_data-input/Eurostat/lfst_r_lfu3pers.xlsx", sheet = "Data1_clean", na = ":")
unempl_secondary_edu <- read_excel("01_data-input/Eurostat/lfst_r_lfu3pers.xlsx", sheet = "Data2_clean", na = ":")
unempl_tertiary_edu <- read_excel("01_data-input/Eurostat/lfst_r_lfu3pers.xlsx", sheet = "Data3_clean", na = ":")

# Cleaning ----
neet <- Activity_rate %>%
  select(`GEO (Codes)`, `GEO (Labels)`) %>%
  right_join(NEET, by = c("GEO (Labels)" = "GEO (Labels)")) %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "NEET_share")

Fertility_ardeco <- Fertility_rate %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Fertility_rate")

Life_exp_ardeco <- Life_expectancy %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Life_exp")

Actrt_ardeco <- Activity_rate %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Activity_rate")

lfs_edu_1 <- employment_primary_edu %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Empl_edu_1") %>% 
  mutate(Empl_edu_1 = Empl_edu_1*1000)
lfs_edu_2 <- employment_secondary_edu %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Empl_edu_2") %>% 
  mutate(Empl_edu_2 = Empl_edu_2*1000)
lfs_edu_3 <- employment_tertiary_edu %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Empl_edu_3") %>% 
  mutate(Empl_edu_3 = Empl_edu_3*1000)

pop_edu_1 <- primary_education %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Pop_edu_1") %>% 
  mutate(Pop_edu_1 = Pop_edu_1*1000)
pop_edu_2 <- secondary_education %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Pop_edu_2")%>% 
  mutate(Pop_edu_2 = Pop_edu_2*1000)
pop_edu_3 <- tertiary_education %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Pop_edu_3") %>% 
  mutate(Pop_edu_3 = Pop_edu_3*1000)

ulfs_edu_1 <- unempl_primary_edu %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Unempl_edu_1") %>% 
  mutate(Unempl_edu_1 = Unempl_edu_1*1000)
ulfs_edu_2 <- unempl_secondary_edu %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Unempl_edu_2")  %>% 
  mutate(Unempl_edu_2 = Unempl_edu_2*1000)
ulfs_edu_3 <- unempl_tertiary_edu %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Unempl_edu_3")%>% 
  mutate(Unempl_edu_3 = Unempl_edu_3*1000)

# Additional data ----
Fertility_RS <- read_excel("01_data-input/National/RS/fertility.xlsx", sheet = "Data_clean") %>% 
  select(-2) %>% 
  rename(Fertility_rate = 3,
         `GEO (Codes)` = 1) %>% 
  mutate(Fertility_rate = as.numeric(Fertility_rate)) %>% 
  filter(Year %in% c(2000:2019))

Life_expectancy_RS <- read_excel("01_data-input/National/RS/life_expectancy.xlsx", sheet = "Data_clean") %>% 
  select(-2) %>% 
  rename(Life_exp = 3,
         `GEO (Codes)` = 1) %>% 
  mutate(Life_exp = as.numeric(Life_exp)) %>% 
  filter(Year %in% c(2000:2019))

Actrt_AL <- read_excel("01_data-input/wiiw/activity.xlsx", sheet = "Data_clean", na = ".") %>% 
  slice(1) %>% 
  select(-"Indicator", -"Unit") %>%
  rename("GEO (Codes)" = 1,
         "GEO (Labels)" = 2) %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Activity_rate")


WDI_AL <- read_excel("01_data-input/World Bank/WDI.xlsx", sheet = "Data_clean_AL", na = "..")

Fertility <- Fertility_ardeco %>%
  left_join(Fertility_RS %>% 
              filter(grepl("RS", `GEO (Codes)`)), 
            by = c("GEO (Codes)", "Year"), 
            suffix = c("_ardeco", "_RS")) %>%
  mutate(Fertility_rate = coalesce(Fertility_rate_RS, Fertility_rate_ardeco)) %>%
  select(-Fertility_rate_ardeco, -Fertility_rate_RS)

Life <- Life_exp_ardeco %>%
  left_join(Life_expectancy_RS %>% 
              filter(grepl("RS", `GEO (Codes)`)), 
            by = c("GEO (Codes)", "Year"), 
            suffix = c("_ardeco", "_RS")) %>%
  mutate(Life_exp = coalesce(Life_exp_RS, Life_exp_ardeco)) %>%
  select(-Life_exp_ardeco, -Life_exp_RS)

Actrt <- bind_rows(Actrt_AL, Actrt_ardeco) %>% 
  arrange("GEO (Codes)", "Year")

glimpse(WDI_AL)
round_1_AL <- WDI_AL %>%
  slice(c(1, 16, 17)) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year", 
               values_to = "Values") %>%
  pivot_wider(names_from = `Series Name`, values_from = `Values`) %>% 
  rename(NUTS = 1,
         Names = 2,
         NEET_share = 4,
         Life_exp = 5,
         Fertility_rate =6) %>% 
  filter(Year %in% 2009:2019)

# Merging ----
data <- list(Actrt, Life, Fertility, neet,
             lfs_edu_1, lfs_edu_2, lfs_edu_3,
             ulfs_edu_1, ulfs_edu_2, ulfs_edu_3,
             pop_edu_1, pop_edu_2, pop_edu_3)

eurostat <- Reduce(function(x, y) merge(x, y, by = c("GEO (Codes)", "GEO (Labels)", "Year"), all = TRUE), data) %>%
  rename(NUTS = 1,
         Name = 2) %>% 
  arrange(NUTS, Year) %>% 
  filter(!NUTS %in% c("AL01", "AL02", "AL03", "BA"),
         Year %in% 2009:2019) %>%
  select(-"Name") # 3465 observations!

eurostat_complete <- eurostat %>%
  full_join(round_1_AL, by = c("NUTS", "Year"), suffix = c("_euro", "_nat")) %>%
  mutate(
    NEET_share = coalesce(NEET_share_nat, NEET_share_euro),
    Life_exp = coalesce(Life_exp_nat, Life_exp_euro),
    Fertility_rate = coalesce(Fertility_rate_nat, Fertility_rate_euro)) %>% 
  select(NUTS, Year, Activity_rate, Life_exp, Fertility_rate, NEET_share,
         Empl_edu_1, Empl_edu_2, Empl_edu_3,
         Unempl_edu_1, Unempl_edu_2, Unempl_edu_3,
         Pop_edu_1, Pop_edu_2, Pop_edu_3)

#SAVING
write.csv(eurostat_complete, file = here("02_intermediary-input", "eurostat_dataset.csv"), row.names = FALSE)
