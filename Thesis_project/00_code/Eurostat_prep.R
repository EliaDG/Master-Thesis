getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#READING DATA
fertility_rate <- read_excel("01_data-input/Eurostat/demo_r_frate2.xlsx", sheet = "Data_clean", na = ":")
life_expectancy <- read_excel("01_data-input/Eurostat/demo_r_mlifexp.xlsx", sheet = "Data_clean", na = ":")
neet <- read_excel("01_data-input/Eurostat/edat_lfse_22.xlsx", sheet = "Data1_clean", na = ":")
activity_rate <- read_excel("01_data-input/Eurostat/lfst_r_lfp2actrt.xlsx", sheet = "Data2_clean", na = ":")

primary_education <- read_excel("01_data-input/Eurostat/edat_lfse_04.xlsx", sheet = "Data1_clean", na = ":")
secondary_education <- read_excel("01_data-input/Eurostat/edat_lfse_04.xlsx", sheet = "Data3_clean", na = ":")
tertiary_education <- read_excel("01_data-input/Eurostat/edat_lfse_04.xlsx", sheet = "Data4_clean", na = ":")

# Cleaning ----
NEET_ardeco <- activity_rate %>%
  select(`GEO (Codes)`, `GEO (Labels)`) %>%
  right_join(neet, by = c("GEO (Labels)" = "GEO (Labels)")) %>%
  pivot_longer(cols = -c("GEO (Codes)" , "GEO (Labels)"),
               names_to = "Year", 
               values_to = "NEET_share") %>% 
  mutate(NEET_share = NEET_share/100)

Fertility_ardeco <- fertility_rate %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Fertility_rate")

Life_exp_ardeco <- life_expectancy %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Life_exp")

Actrt_ardeco <- activity_rate %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Activity_rate") %>% 
  mutate(Activity_rate = Activity_rate/100)

Pop_edu_1_ardeco <- activity_rate %>%
  select(`GEO (Codes)`, `GEO (Labels)`) %>%
  right_join(primary_education, by = c("GEO (Labels)" = "GEO (Labels)")) %>%
  pivot_longer(cols = -c("GEO (Codes)" , "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Pop_edu_1") %>% 
  mutate(Pop_edu_1 = Pop_edu_1/100)
Pop_edu_2_ardeco <- activity_rate %>%
  select(`GEO (Codes)`, `GEO (Labels)`) %>%
  right_join(secondary_education, by = c("GEO (Labels)" = "GEO (Labels)")) %>%
  pivot_longer(cols = -c("GEO (Codes)" , "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Pop_edu_2") %>% 
  mutate(Pop_edu_2 = Pop_edu_2/100)
Pop_edu_3_ardeco <- activity_rate %>%
  select(`GEO (Codes)`, `GEO (Labels)`) %>%
  right_join(tertiary_education, by = c("GEO (Labels)" = "GEO (Labels)")) %>%
  pivot_longer(cols = -c("GEO (Codes)" , "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Pop_edu_3") %>% 
  mutate(Pop_edu_3 = Pop_edu_3/100)

# Additional data ----
Fertility_RS <- read_excel("01_data-input/National/RS/fertility.xlsx", sheet = "Data_clean")
Life_exp_RS <- read_excel("01_data-input/National/RS/life_expectancy.xlsx", sheet = "Data_clean")
Actrt_AL <- read_excel("01_data-input/wiiw/activity.xlsx", sheet = "Data_clean", na = ".")
WDI_AL <- read_excel("01_data-input/World Bank/WDI.xlsx", sheet = "Data_clean_AL", na = "..") 

Fertility_RS <- Fertility_RS %>% 
  select(-2) %>% 
  rename(Fertility_rate = 3,
         `GEO (Codes)` = 1) %>% 
  mutate(Fertility_rate = as.numeric(Fertility_rate)) %>% 
  filter(Year %in% c(2000:2019))

Life_exp_RS <- Life_exp_RS %>% 
  select(-2) %>% 
  rename(Life_exp = 3,
         `GEO (Codes)` = 1) %>% 
  mutate(Life_exp = as.numeric(Life_exp)) %>% 
  filter(Year %in% c(2000:2019))

Actrt_AL <- Actrt_AL %>% 
  slice(1) %>% 
  select(-"Indicator", -"Unit") %>%
  rename("GEO (Codes)" = 1,
         "GEO (Labels)" = 2) %>% 
  pivot_longer(cols = -c("GEO (Codes)", "GEO (Labels)"),
               names_to = "Year", 
               values_to = "Activity_rate") %>% 
  mutate(Activity_rate = Activity_rate/100)

WDI_extra_AL <- WDI_AL %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year", 
               values_to = "Values") %>%
  pivot_wider(names_from = `Series Name`, values_from = `Values`) %>% 
  rename(`GEO (Codes)` = 1,
         `GEO (Labels)` = 2,
         NEET_share = 4,
         Life_exp = 13,
         Fertility_rate = 14,
         ISCED_1 = `Educational attainment, at least completed primary, population 25+ years, total (%) (cumulative)`, #highest cumulative percentage
         ISCED_2 = `Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)`,
         ISCED_3 = `Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)`,
         ISCED_4 = `Educational attainment, at least completed post-secondary, population 25+, total (%) (cumulative)`,
         ISCED_5 = `Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)`,
         ISCED_6 = `Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)`,
         ISCED_7 = `Educational attainment, at least Master's or equivalent, population 25+, total (%) (cumulative)`,
         ISCED_8 = `Educational attainment, Doctoral or equivalent, population 25+, total (%) (cumulative)`) %>% 
  filter(Year %in% 2009:2019) %>% 
  mutate(Pop_edu_3 = ISCED_5/100,
         Pop_edu_2 = (ISCED_3 - ISCED_5)/100, #here ISCED_4 and ISCED_5 are the same
         Pop_edu_1 = (ISCED_2 - ISCED_3)/100,
         NEET_share = NEET_share/100) %>%
  select(1:4, 13:17)

# Merging ----
Fertility <- Fertility_ardeco %>%
  full_join(Fertility_RS %>% 
              filter(grepl("RS", `GEO (Codes)`, Fertility_rate)), 
            by = c("GEO (Codes)", "Year"),
            suffix = c("_ardeco", "_RS")) %>%
  mutate(Fertility_rate = coalesce(Fertility_rate_RS, Fertility_rate_ardeco)) %>%
  select(-Fertility_rate_ardeco, -Fertility_rate_RS)

Life_exp <- Life_exp_ardeco %>%
  left_join(Life_exp_RS %>% 
              filter(grepl("RS", `GEO (Codes)`)), 
            by = c("GEO (Codes)", "Year"), 
            suffix = c("_ardeco", "_RS")) %>%
  mutate(Life_exp = coalesce(Life_exp_RS, Life_exp_ardeco)) %>%
  select(-Life_exp_ardeco, -Life_exp_RS)

Actrt <- bind_rows(Actrt_AL, Actrt_ardeco) %>% 
  arrange("GEO (Codes)", "Year")

data <- list(Actrt, NEET_ardeco, Life_exp, Fertility,
             Pop_edu_1_ardeco, Pop_edu_2_ardeco, Pop_edu_3_ardeco,
             WDI_extra_AL)

eurostat <- Reduce(function(x, y) full_join(x, y, by = c("GEO (Codes)", "GEO (Labels)", "Year" )), data) %>%
  rename(NUTS = 1,
         Name = 2) %>%
  mutate(NEET_share = coalesce(NEET_share.x, NEET_share.y),
         Life_exp = coalesce(Life_exp.x, Life_exp.y),
         Fertility_rate = coalesce(Fertility_rate.x, Fertility_rate.y),
         Pop_edu_1 = coalesce(Pop_edu_1.x, Pop_edu_1.y),
         Pop_edu_2 = coalesce(Pop_edu_2.x, Pop_edu_2.y),
         Pop_edu_3 = coalesce(Pop_edu_3.x, Pop_edu_3.y)) %>%
  arrange(NUTS, Year) %>% 
  filter(!NUTS %in% c("AL01", "AL02", "AL03", "BA"),
         Year %in% 2009:2019) %>% 
  select(-ends_with(".x"), -ends_with(".y")) # 3465 observations!

#SAVING
write.csv(eurostat, file = here("02_intermediary-input", "eurostat_dataset.csv"), row.names = FALSE)
