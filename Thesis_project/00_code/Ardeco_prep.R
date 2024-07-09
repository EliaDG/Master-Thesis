getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#READING DATA
RNUTN_Unemployment <- read_excel("01_data-input/Ardeco/RNUTN_Unemployment.xlsx", sheet = "Data_clean")
RUIGT_GFCF <- read_excel("01_data-input/Ardeco/RUIGT_GFCF(current).xlsx", sheet = "Data_clean")
RUWCDW_Wage <- read_excel("01_data-input/Ardeco/RUWCDW_Wage.xlsx", sheet = "Data_clean")
SNETD_Employment <- read_excel("01_data-input/Ardeco/SNETD_Employment.xlsx", sheet = "Data_clean")
SNMTN_Migration <- read_excel("01_data-input/Ardeco/SNMTN_Migration.xlsx", sheet = "Data_clean")
SNPTD_Population <- read_excel("01_data-input/Ardeco/SNPTD_Population.xlsx", sheet = "Data_clean")
SUVGD_GDP <- read_excel("01_data-input/Ardeco/SUVGD_GDP(current).xlsx", sheet = "Data_clean")
SUVGDE_Productivity <- read_excel("01_data-input/Ardeco/SUVGDE_Productivity.xlsx", sheet = "Data_clean")
SUVGE_GVA <- read_excel("01_data-input/Ardeco/SUVGE_GVA(basic).xlsx", sheet = "Data_clean")
SUVGZ <- read_csv("01_data-input/Ardeco/SUVGZ.csv")
SNETZ <- read_csv("01_data-input/Ardeco/SNETZ.csv")

# Cleaning ---
Unemp <- RNUTN_Unemployment %>% 
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Unemployment_abs") %>% 
  mutate(Unemployment_abs = Unemployment_abs*1000)# People

GFCF <- RUIGT_GFCF %>% 
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GFCF_abs") %>% 
  mutate(GFCF_abs = GFCF_abs*1000000) #In EUR

Wage <- RUWCDW_Wage %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "A_Wage_abs") %>%  #In EUR
  mutate(M_Wage_abs = A_Wage_abs/12)

Emp <- SNETD_Employment %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Employment_abs") %>% 
  mutate(Employment_abs = Employment_abs*1000) # People

Migr <- SNMTN_Migration %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Migration_abs") # People

Pop <- SNPTD_Population %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Population_abs") # People

GDP <- SUVGD_GDP %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GDP_abs") %>% 
  mutate(GDP_abs = GDP_abs*1000000) #In EUR

Prod <- SUVGDE_Productivity %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Productivity_abs") #In EUR

GVA <- SUVGE_GVA %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GVA_abs") %>% 
  mutate(GVA_abs = GVA_abs*1000000) #In EUR

GVA_Nace <- SUVGZ %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "NACE"), 
               names_to = "Year", 
               values_to = "GVA_Nace_abs") %>% 
  mutate(GVA_Nace_abs = GVA_Nace_abs*1000000,   #In EUR
         Name = if_else(NUTS == "HU10", "Közép-Magyarország (NUTS 2013)", Name)) %>%  
  pivot_wider(names_from = NACE, values_from = GVA_Nace_abs, names_prefix = "GVA_NACE_")

Emp_Nace <- SNETZ %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "NACE"), 
               names_to = "Year", 
               values_to = "Emp_Nace_abs") %>% 
  mutate(Emp_Nace_abs = Emp_Nace_abs*1000,   # People
         Name = if_else(NUTS == "HU10", "Közép-Magyarország (NUTS 2013)", Name)) %>%  
  pivot_wider(names_from = NACE, values_from = Emp_Nace_abs, names_prefix = "EMP_NACE_")


# Merging ---
ardeco_list <- list(Emp, Emp_Nace, GDP, GFCF, GVA, GVA_Nace, Migr, Pop, Prod, Unemp, Wage)
ardeco_data <- Reduce(function(x, y) merge(x, y, by = c("NUTS", "Name", "Year"), all = TRUE), ardeco_list)

ardeco <- ardeco_data %>% 
  filter(!NUTS %in% c("AL01", "AL02", "AL03")) %>%
  arrange(NUTS, Year)

#SAVING
write.csv(ardeco, file = here("02_intermediary-input", "ardeco_dataset.csv"), row.names = FALSE)
