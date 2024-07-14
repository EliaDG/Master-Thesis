getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#READING DATA
activity_rate <- read_excel("01_data-input/wiiw/activity.xlsx", sheet = "Data_clean", na = ".")
emp_NACE <- read_excel("01_data-input/wiiw/emp.xlsx", sheet = "Data_clean_extra", na = ".")
gdp <- read_excel("01_data-input/wiiw/gdp.xlsx", sheet = "Data_clean", na = ".")
gva_NACE <- read_excel("01_data-input/wiiw/gva_nace.xlsx", sheet = "Data_clean_extra", na = ".")
pop <- read_excel("01_data-input/wiiw/pop_lifexp.xlsx", sheet = "Data_clean", na = ".")
wages <- read_excel("01_data-input/wiiw/wages1.xlsx", sheet = "Data_clean", na = ".")

pop_empl_unem_edu_MD <- read_excel("01_data-input/National/MD/pop_empl_unem_edu.xlsx", sheet = "Data_clean")
emp_edu_XK <- read_excel("01_data-input/National/KS/EMP_EDU.xlsx", sheet = "Data_clean")
unemp_edu_XK <- read_excel("01_data-input/National/KS/UNEMP_EDU.xlsx", sheet = "Data_clean")

WDI <- read_excel("01_data-input/World Bank/WDI.xlsx", sheet = "Data_clean_extra", na = "..")

#CLEANING
Actrt <- activity_rate %>% 
  filter(!Name == "Albania") %>% 
  select(-c(3,4)) %>% 
  pivot_longer(cols = -c(NUTS, Name),
               names_to = "Year",
               values_to = "Activity_rate")

Emp_nace <- emp_NACE %>% 
  rename(NUTS = 1,
         Name = 2,
         NACE = 3) %>% 
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "NACE"), 
               names_to = "Year", 
               values_to = "Emp_Nace_abs") %>%
  mutate(Emp_Nace_abs = Emp_Nace_abs*1000) %>% # People
  pivot_wider(names_from = NACE,
              values_from = Emp_Nace_abs,
              names_prefix = "EMP_NACE_") #Double check XK Real Estate!

GDP <- gdp %>% 
  select(-c(3,5)) %>% 
  rename(NUTS = 1,
         Name = 2) %>% 
    pivot_longer(cols = -c("NUTS", "Name", "Unit"), 
               names_to = "Year", 
               values_to = "GDP") %>%
  mutate(GDP = GDP*1000000) %>% 
  pivot_wider(names_from = Unit,
              values_from = GDP,
              names_prefix = "GDP_")

GVA_nace <- gva_NACE %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "Nace"), 
               names_to = "Year", 
               values_to = "GVA_Nace_abs") %>%
  mutate(GVA_Nace_abs = GVA_Nace_abs*1000000) %>% # In Millions
  pivot_wider(names_from = Nace,
              values_from = GVA_Nace_abs,
              names_prefix = "GVA_NACE_")

Population <- pop %>% 
  rename(NUTS = 1,
         Name = 2) %>% 
  select(-c(3,4)) %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Population_abs")%>%
  mutate(Population_abs = Population_abs*1000)

Wages <- wages %>%
  select(-3) %>%
  pivot_longer(cols = -c("NUTS", "Name", "Unit"), 
               names_to = "Year", 
               values_to = "M_Wage_abs") %>%
  pivot_wider(names_from = Unit,
              values_from = M_Wage_abs,
              names_prefix = "Wage_")

Edu_MD <- pop_empl_unem_edu_MD %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Population_by_status") %>%
  mutate(Population_by_status = Population_by_status * 1000) %>% 
  pivot_wider(names_from = c(Status, Level),
              values_from = Population_by_status,
              names_glue = "{Level}_{Status}") %>% 
  select(-c(7:12,16:18)) %>% 
  rename(Pop_edu_3 = 4,
         Empl_edu_3 = 5,
         Unempl_edu_3 = 6,
         Pop_edu_2 = 7,
         Empl_edu_2 = 8,
         Unempl_edu_2 = 9,
         Pop_edu_1 = 10,
         Empl_edu_1 = 11,
         Unempl_edu_1 = 12)

Emp_edu_XK <- emp_edu_XK %>%
  select(-4) %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Empl_edu") %>%
  mutate(Empl_edu = Empl_edu*1000) %>% 
  pivot_wider(names_from = Level,
              values_from = Empl_edu,
              names_prefix = "Empl_") %>% 
  mutate(Empl_edu_1 = Empl_Primary + `Empl_No formal education`,
         Empl_edu_2 = `Empl_Secondary education, gymnasium` + `Empl_Secondary education, vocational`,
         Empl_edu_3 = Empl_Tertiary) %>% 
  select(-c(4:9))

Unemp_edu_XK <- unemp_edu_XK %>%
  select(-4) %>%
  rename(Level = 3) %>% 
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Unempl_edu") %>%
  mutate(Unempl_edu = Unempl_edu*1000) %>% 
  pivot_wider(names_from = Level,
              values_from = Unempl_edu,
              names_prefix = "Unempl_") %>%
  arrange(Year) %>% 
  mutate(Unempl_edu_1 = Unempl_Primary + `Unempl_No formal education`,
         Unempl_edu_2 = `Unempl_Secondary education, gymnasium` + `Unempl_Secondary education, vocational`,
         Unempl_edu_3 = Unempl_Tertiary) %>% 
  select(-c(4:9))

years <- 2000:2011
all_years <- expand.grid(
  NUTS = "XK00",
  Name = "Kosovo",
  Year = as.character(years)
)
LFS_XK <- merge(Emp_edu_XK, Unemp_edu_XK, by = c("NUTS", "Name", "Year")) %>% 
  full_join(all_years, by = c("NUTS", "Name", "Year")) %>% 
  arrange(Year)
candidates <- full_join(Edu_MD, LFS_XK, by = c("NUTS", "Name", "Year",
                                               "Empl_edu_1", "Empl_edu_2", "Empl_edu_3",
                                               "Unempl_edu_1", "Unempl_edu_2", "Unempl_edu_3"))

WDI_data <- WDI %>% 
  pivot_longer(cols = -c(NUTS, `Country Name`, `Series Name`),
               names_to = "Year",
               values_to = "Values") %>%
  pivot_wider(names_from = `Series Name`,
              values_from = Values) %>%
  select(-10) %>% 
  rename(Name = 2,
         Labour_force = 4,
         NEET_share = 5,
         A__Unemp_edu_3 = 6,
         A__Unempl_edu_2 = 7,
         A__Unempl_edu_1 = 8,
         A__Unempl_rate = 9,
         Migration = 10,
         LF_edu_2_share = 11,
         LF_edu_1_share = 12,
         LF_edu_3_share = 13,
         GFCF_share = 14,
         GFCG_NCU_abs = 15,
         Life_exp = 24,
         Fertility_rate = 25) %>% 
  filter(Year %in% 2009:2019)

data_final <- list(candidates, Emp_nace, GVA_nace, GDP,
                   Actrt, Population, Wages)
candidates_final <- reduce(data_final, full_join, by = join_by(NUTS, Name, Year)) %>% 
  filter(Year %in% c(2009:2019)) %>% 
  arrange(NUTS, Year)

#SAVING
write.csv(candidates_final, file = here("02_intermediary-input", "extra_dataset.csv"), row.names = FALSE)
