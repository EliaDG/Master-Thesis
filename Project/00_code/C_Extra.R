getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#READING DATA
activity_rate <- read_excel("01_data-input/wiiw/activity.xlsx", sheet = "Data_clean", na = ".")
emp_nace <- read_excel("01_data-input/wiiw/emp.xlsx", sheet = "Data_clean_extra", na = ".")
unemp <- read_excel("01_data-input/wiiw/unemp.xlsx", sheet = "Data_clean", na = ".")
gva_nace <- read_excel("01_data-input/wiiw/gva_nace.xlsx", sheet = "Data_clean_extra", na = ".")
wages <- read_excel("01_data-input/wiiw/wages1.xlsx", sheet = "Data_clean", na = ".")
fexc <- read_excel("01_data-input/wiiw/fexc.xlsx", sheet = "Data_clean")

WDI <- read_excel("01_data-input/World Bank/WDI.xlsx", sheet = "Data_clean_extra", na = "..")

Pop_edu <- read_csv("01_data-input/ILOSTAT/POP_XWAP_SEX_AGE_EDU.csv", na = "..")

#CLEANING
Actrt <- activity_rate %>% 
  filter(!Name == "Albania") %>% 
  select(-c(3,4)) %>% 
  pivot_longer(cols = -c(NUTS, Name),
               names_to = "Year",
               values_to = "Activity_rate") %>% 
  mutate(Activity_rate = Activity_rate/100)

Emp_Nace <- emp_nace %>% 
  rename(NUTS = 1,
         Name = 2,
         NACE = 3) %>% 
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "NACE"), 
               names_to = "Year", 
               values_to = "Emp_Nace_abs") %>%
  mutate(Emp_Nace_abs = Emp_Nace_abs*1000,
         Emp_Nace_abs = if_else(NUTS == "XK00" & NACE == "L" & Year %in% as.character(2000:2011), NA_real_, Emp_Nace_abs)) %>% #This is necessary because of change in reporting between NACE 1 & 2
  pivot_wider(names_from = NACE,
              values_from = Emp_Nace_abs,
              names_prefix = "EMP_NACE_")

Unemployment <- unemp %>% 
  select(-Unit) %>% 
  pivot_longer(cols = -c(NUTS, Name),
               names_to = "Year",
               values_to = "Unemployment_abs") %>% 
  mutate(Unemployment_abs = Unemployment_abs*1000)

exc_rate <- fexc %>%
  pivot_longer(cols = -c("NUTS", "Name", "Unit"),
               names_to = "Year",
               values_to = "Exchange_rate") %>%
  pivot_wider(names_from = Unit,
              values_from = Exchange_rate) %>%
  select(-`NCU/USD`) %>% 
  arrange(NUTS, Name, Year)

GVA <- gva_nace %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "Nace"), 
               names_to = "Year", 
               values_to = "GVA_Nace_abs") %>%
  mutate(GVA_Nace_abs = GVA_Nace_abs*1000000) %>%  # In Millions
  arrange(NUTS, Name, Year)

GVA_Nace <- GVA %>% 
  left_join(exc_rate, by = c('NUTS', 'Name', 'Year')) %>%
  arrange(NUTS, Name, Year) %>% 
  mutate(GVA_Euro = GVA_Nace_abs / `NCU/EUR`) %>% 
  select(NUTS, Name, Year, everything()) %>% 
  select(-c(GVA_Nace_abs, `NCU/EUR`)) %>% 
  pivot_wider(names_from = Nace,
              values_from = GVA_Euro,
              names_prefix = "GVA_NACE_") %>%
  rename(GVA_EUR = GVA_NACE_Total) %>%
  mutate(`GVA_NACE_L-M-N` = `GVA_NACE_L` + `GVA_NACE_M-N`)

GVA_NCU <- GVA %>% 
  filter(Nace == "Total") %>%
  rename(GVA_NCU = GVA_Nace_abs) %>% 
  select(-Nace)

Wages <- wages %>%
  slice(3:8) %>% 
  select(-3) %>%
  pivot_longer(cols = -c("NUTS", "Name", "Unit"), 
               names_to = "Year", 
               values_to = "Wage_abs") %>%
  pivot_wider(names_from = Unit,
              values_from = Wage_abs,
              names_prefix = "Wage_") %>% 
  select(-Wage_NCU)

WDI_data <- WDI %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Values") %>%
  pivot_wider(names_from = `Series Name`,
              values_from = Values) %>%
  rename(Name = 2,
         Labor_force_abs = 4,
         NEET_share = 5,
         Unempl_rate = 6,
         Migration_abs = 7,
         LF_edu_2_share = 8,
         LF_edu_1_share = 9,
         LF_edu_3_share = 10,
         GFCF_share = 11,
         GFCF_NCU = 12,
         ISCED_1 = `Educational attainment, at least completed primary, population 25+ years, total (%) (cumulative)`, #highest cumulative percentage
         ISCED_2 = `Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)`,
         ISCED_3 = `Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)`,
         ISCED_4 = `Educational attainment, at least completed post-secondary, population 25+, total (%) (cumulative)`,
         ISCED_5 = `Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)`,
         ISCED_6 = `Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)`,
         ISCED_7 = `Educational attainment, at least Master's or equivalent, population 25+, total (%) (cumulative)`,
         ISCED_8 = `Educational attainment, Doctoral or equivalent, population 25+, total (%) (cumulative)`,
         Life_exp = 21,
         Fertility_rate = 22) %>% 
  filter(Year %in% 2009:2019) %>% 
  mutate(#Pop_edu_3 = ISCED_5/100,
         #Pop_edu_2 = (ISCED_3 - ISCED_5)/100, #here ISCED_4 and ISCED_5 are the same
         #Pop_edu_1 = (ISCED_1 - ISCED_3)/100,
         NEET_share = NEET_share/100,
         GFCF_share = GFCF_share/100) %>% 
  select(-Unempl_rate, -Labor_force_abs, -starts_with("ISCED_"), -starts_with("LF_edu_"))

#Merging ----
data_final <- list(Emp_Nace, GVA_Nace, GVA_NCU,
                   Actrt, Wages, Unemployment,
                   WDI_data)

candidates <- reduce(data_final, full_join, by = join_by(NUTS, Name, Year)) %>%
  arrange(NUTS, Year) %>%
  filter(Year %in% c(2009:2019)) %>%
  rename(Employment_abs = EMP_NACE_Total)

Pop_edu_levels <-  Pop_edu %>% 
  filter(!NUTS == "AL00") %>% 
  arrange(NUTS, Year) %>% 
  mutate(Pop_edu_1 = (Basic+`Less than basic`)/Total,
         Pop_edu_2 = Intermediate/Total,
         Pop_edu_3 = Advanced/Total) %>% 
  select(-c(Total, Basic, `Less than basic`, Intermediate, Advanced))

candidates_final <- candidates %>%
  mutate(Year = as.double(Year)) %>% 
  full_join(Pop_edu_levels, by = c("NUTS", "Name", "Year"))

#SAVING
write.csv(candidates_final, file = here("02_intermediary-input", "extra_dataset.csv"), row.names = FALSE)
