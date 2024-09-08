getwd()

#NOTICE
# Although here the Ardeco Datasets will be mainly cleaned and prepared, it also
# includes additional sources to ensure completeness from: 
# - Eurostat for Türkiye,
# - MONSTAT National Statistical office for Montenegro,
# - WIIW for Albania

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#READING DATA
RNUTN_Unemployment <- read_excel("01_data-input/Ardeco/RNUTN_Unemployment.xlsx", sheet = "Data_clean")
RUIGT_GFCF <- read_excel("01_data-input/Ardeco/RUIGT_GFCF(current).xlsx", sheet = "Data_clean")
RUWCDW_Wage <- read_excel("01_data-input/Ardeco/RUWCDW_Wage.xlsx", sheet = "Data_clean")
SNETD_Employment <- read_excel("01_data-input/Ardeco/SNETD_Employment.xlsx", sheet = "Data_clean")
SNMTN_Migration <- read_excel("01_data-input/Ardeco/SNMTN_Migration.xlsx", sheet = "Data_clean")
SUVGDE_Productivity <- read_excel("01_data-input/Ardeco/SUVGDE_Productivity.xlsx", sheet = "Data_clean")
SUVGE_GVA <- read_excel("01_data-input/Ardeco/SUVGE_GVA(basic).xlsx", sheet = "Data_clean")
SUVGZ <- read_csv("01_data-input/Ardeco/SUVGZ.csv")
SNETZ <- read_csv("01_data-input/Ardeco/SNETZ.csv")

# Cleaning ----
Unemp <- RNUTN_Unemployment %>% 
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Unemployment_abs") %>% 
  mutate(Unemployment_abs = Unemployment_abs*1000) #Thousand People

GFCF_EUR <- RUIGT_GFCF %>% 
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GFCF_EUR") %>% 
  mutate(GFCF_EUR = GFCF_EUR*1000000) #Million EUR

Wage_ardeco <- RUWCDW_Wage %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Annual_Wage_EUR") %>%  #Million EUR
  mutate(Wage_EUR = Annual_Wage_EUR/12) %>% 
  select(-Annual_Wage_EUR)

Emp <- SNETD_Employment %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Employment_abs") %>% 
  mutate(Employment_abs = Employment_abs*1000) #Thousand People

Migr <- SNMTN_Migration %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Migration_abs") # People

Prod <- SUVGDE_Productivity %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Labor_Productivity_abs") #In EUR

GVA <- SUVGE_GVA %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GVA_EUR") %>% 
  mutate(GVA_EUR = GVA_EUR*1000000) #Million EUR

GVA_Nace <- SUVGZ %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "NACE"), 
               names_to = "Year", 
               values_to = "GVA_Nace_abs") %>% 
  mutate(GVA_Nace_abs = GVA_Nace_abs*1000000,
         Name = if_else(NUTS == "HU10", "Közép-Magyarország (NUTS 2013)", Name)) %>% #Million EUR  
  pivot_wider(names_from = NACE,
              values_from = GVA_Nace_abs,
              names_prefix = "GVA_NACE_") %>% 
  mutate(`GVA_NACE_L-M-N` = GVA_NACE_L + `GVA_NACE_M-N`)

Emp_Nace_Ardeco <- SNETZ %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "NACE"), 
               names_to = "Year", 
               values_to = "Emp_Nace_abs") %>% 
  mutate(Emp_Nace_abs = Emp_Nace_abs*1000,
         Name = if_else(NUTS == "HU10", "Közép-Magyarország (NUTS 2013)", Name)) %>% #Thousand People  
  pivot_wider(names_from = NACE,
              values_from = Emp_Nace_abs,
              names_prefix = "EMP_NACE_") %>% 
  mutate(`EMP_NACE_L-M-N` = EMP_NACE_L + `EMP_NACE_M-N`)

# Additional sources ----
Emp_Nace_TR <- read_excel("01_data-input/Eurostat/lfst_r_lfe2en2_Turkey.xlsx", sheet = "Data_clean", na = ":")
Emp_Nace_ME <- read_excel("01_data-input/National/ME/NACE_Employment_ME.xlsx", na = ":")
Emp_Nace_AL <- read_excel("01_data-input/wiiw/emp.xlsx", sheet = "Data_clean_AL", na = ".")
Wage_AL <- read_excel("01_data-input/wiiw/wages1.xlsx", sheet = "Data_clean", na = ".")

Emp_Nace_TR <- Emp_Nace_TR %>%
  mutate(across(where(is.double), ~ . * 1000),
         `EMP_NACE_L-M-N` = EMP_NACE_L + `EMP_NACE_M-N`) %>% 
  rename( NUTS = 1,
          Name = 2,
          Year = 3)

Emp_Nace_ME <- Emp_Nace_ME %>% 
  mutate(Year = as.character(Year))

Emp_Nace_AL <- Emp_Nace_AL %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Value") %>%
  mutate(Value = Value*1000) %>% 
  select(NUTS, Name, Year, Nace, Value) %>% 
  pivot_wider(names_from = Nace, 
              values_from = Value,
              names_prefix = "EMP_NACE_") %>% 
  select(-4) #Remove total, we don't need it

Emp_Nace <- rbind(Emp_Nace_Ardeco, Emp_Nace_TR, Emp_Nace_ME, Emp_Nace_AL) %>%
  arrange(NUTS, Year) %>% 
  select(-Name)

Wage_AL <- Wage_AL %>% 
  slice(1,2) %>% 
  select(-3) %>% 
  pivot_longer(cols = -c("NUTS", "Name", "Unit"), 
               names_to = "Year", 
               values_to = "Wage_abs") %>%
  pivot_wider(names_from = "Unit",
              values_from = "Wage_abs",
              names_prefix = "Wage_") %>% 
  select(-Wage_NCU)

# Merging ----
Wage <- full_join(Wage_AL, Wage_ardeco, by = c("NUTS", "Name", "Year")) %>%
  arrange(NUTS, Year) %>%
  mutate(Wage_EUR = coalesce(Wage_EUR.x, Wage_EUR.y)) %>%
  select(NUTS, Name, Year, Wage_EUR)

ardeco_list <- list(Emp, GFCF_EUR, GVA, GVA_Nace, Migr, Prod, Unemp, Wage)
ardeco_data <- Reduce(function(x, y) full_join(x, y, by = c("NUTS", "Name", "Year")), ardeco_list)

ardeco <- ardeco_data %>%
  full_join(Emp_Nace, by = c("NUTS", "Year")) %>% # Second round of merging because of Name discrepancy
  arrange(NUTS, Year)
  # group_by(NUTS) %>% 
  # mutate(across(-c(Name, Year), lag)) %>%
  # ungroup() %>% 
  # filter(!NUTS %in% c("AL01", "AL02", "AL03"), Year %in% c(2009:2019))

#SAVING
write.csv(ardeco, file = here("02_intermediary-input", "ardeco_dataset.csv"), row.names = FALSE)
