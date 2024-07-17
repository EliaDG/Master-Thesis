getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#READING DATA
activity_rate <- read_excel("01_data-input/wiiw/activity.xlsx", sheet = "Data_clean", na = ".")
emp_nace <- read_excel("01_data-input/wiiw/emp.xlsx", sheet = "Data_clean_extra", na = ".")
unemp <- read_excel("01_data-input/wiiw/unemp.xlsx", sheet = "Data_clean", na = ".")
gdp <- read_excel("01_data-input/wiiw/gdp.xlsx", sheet = "Data_clean", na = ".")
gva_nace <- read_excel("01_data-input/wiiw/gva_nace.xlsx", sheet = "Data_clean_extra", na = ".")
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

GDP <- gdp %>%
  slice(3:8) %>% 
  select(-c(3,5)) %>% 
  rename(NUTS = 1,
         Name = 2) %>% 
    pivot_longer(cols = -c("NUTS", "Name", "Unit"), 
               names_to = "Year", 
               values_to = "GDP_abs") %>%
  mutate(GDP_abs = GDP_abs*1000000) %>% 
  pivot_wider(names_from = Unit,
              values_from = GDP_abs,
              names_prefix = "GDP_")

GVA_Nace <- gva_nace %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "Nace"), 
               names_to = "Year", 
               values_to = "GVA_Nace_abs") %>%
  mutate(GVA_Nace_abs = GVA_Nace_abs*1000000) %>% # In Millions
  pivot_wider(names_from = Nace,
              values_from = GVA_Nace_abs,
              names_prefix = "GVA_NACE_") %>% 
  rename(GVA_NCU = GVA_NACE_Total)

Population <- pop %>% 
  rename(NUTS = 1,
         Name = 2) %>% 
  select(-c(3,4)) %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Population_abs")%>%
  mutate(Population_abs = Population_abs*1000)

Wages <- wages %>%
  slice(3:8) %>% 
  select(-3) %>%
  pivot_longer(cols = -c("NUTS", "Name", "Unit"), 
               names_to = "Year", 
               values_to = "Wage_abs") %>%
  pivot_wider(names_from = Unit,
              values_from = Wage_abs,
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

WDI_data <- WDI %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Values") %>%
  pivot_wider(names_from = `Series Name`,
              values_from = Values) %>%
  rename(Name = 2,
         Labor_force_abs = 4,
         NEET_share = 5,
         A__Unempl_edu_3_share = 6,
         A__Unempl_edu_2_share = 8,
         A__Unempl_edu_1_share = 7,
         Unempl_rate = 9,
         Migration_abs = 11,
         A__LF_edu_2_share = 12,
         A__LF_edu_1_share = 13,
         A__LF_edu_3_share = 14,
         GFCF_share = 15,
         GFCF_NCU = 16,
         ISCED_1 = `Educational attainment, at least completed primary, population 25+ years, total (%) (cumulative)`, #highest cumulative percentage
         ISCED_2 = `Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)`,
         ISCED_3 = `Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)`,
         ISCED_4 = `Educational attainment, at least completed post-secondary, population 25+, total (%) (cumulative)`,
         ISCED_5 = `Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)`,
         ISCED_6 = `Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)`,
         ISCED_7 = `Educational attainment, at least Master's or equivalent, population 25+, total (%) (cumulative)`,
         ISCED_8 = `Educational attainment, Doctoral or equivalent, population 25+, total (%) (cumulative)`,
         Life_exp = 25,
         Fertility_rate = 26) %>% 
  filter(Year %in% 2009:2019) %>% 
  mutate(Pop_edu_3 = ISCED_5/100,
         Pop_edu_2 = (ISCED_3 - ISCED_5)/100, #here ISCED_4 and ISCED_5 are the same
         Pop_edu_1 = (ISCED_1 - ISCED_3)/100,
         NEET_share = NEET_share/100,
         GFCF_share = GFCF_share/100,
         A__Unempl_edu_1_share = A__Unempl_edu_1_share/100,
         A__Unempl_edu_2_share = A__Unempl_edu_2_share/100,
         A__Unempl_edu_3_share = A__Unempl_edu_3_share/100,
         A__LF_edu_1_share = A__LF_edu_1_share/100,
         A__LF_edu_2_share = A__LF_edu_2_share/100,
         A__LF_edu_3_share = A__LF_edu_3_share/100) %>% 
  select(-10, -Unempl_rate, -Labor_force_abs, -starts_with("ISCED_"))

#Merging ----
candidates <- full_join(Edu_MD, LFS_XK, by = c("NUTS", "Name", "Year",
                                               "Empl_edu_1", "Empl_edu_2", "Empl_edu_3",
                                               "Unempl_edu_1", "Unempl_edu_2", "Unempl_edu_3"))

data_final <- list(candidates, Emp_Nace, GVA_Nace, GDP,
                   Actrt, Population, Wages, Unemployment,
                   WDI_data)

candidates_final <- reduce(data_final, full_join, by = join_by(NUTS, Name, Year)) %>% 
  filter(Year %in% c(2009:2019)) %>% 
  arrange(NUTS, Year) %>% 
  mutate(Pop_edu_1 = coalesce(Pop_edu_1.x, Pop_edu_1.y),
         Pop_edu_2 = coalesce(Pop_edu_2.x, Pop_edu_2.y),
         Pop_edu_3 = coalesce(Pop_edu_3.x, Pop_edu_3.y)) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>% 
  rename(Employment_abs = EMP_NACE_Total)

#SAVING
write.csv(candidates_final, file = here("02_intermediary-input", "extra_dataset.csv"), row.names = FALSE)

#Appendix ----
# WDI_BA_Edu <- WDI %>% 
#   pivot_longer(cols = starts_with("20"),
#                names_to = "Year",
#                values_to = "Values") %>%
#   pivot_wider(names_from = `Series Name`,
#               values_from = Values) %>%
#   rename(Name = 2,
#          Labor_force.x = 4,
#          NEET_share.x = 5,
#          A__Unempl_edu_3 = 6,
#          A__Unempl_edu_2 = 8,
#          A__Unempl_edu_1 = 7,
#          A__Unempl_rate = 9,
#          Migration.x = 11,
#          A__LF_edu_2_share = 12,
#          A__LF_edu_1_share = 13,
#          A__LF_edu_3_share = 14,
#          GFCF_share.x = 15,
#          GFCG_NCU_abs.x = 16,
#          ISCED_1 = `Educational attainment, at least completed primary, population 25+ years, total (%) (cumulative)`, #highest cumulative percentage
#          ISCED_2 = `Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)`,
#          ISCED_3 = `Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)`,
#          ISCED_4 = `Educational attainment, at least completed post-secondary, population 25+, total (%) (cumulative)`,
#          ISCED_5 = `Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)`,
#          ISCED_6 = `Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)`,
#          ISCED_7 = `Educational attainment, at least Master's or equivalent, population 25+, total (%) (cumulative)`,
#          ISCED_8 = `Educational attainment, Doctoral or equivalent, population 25+, total (%) (cumulative)`,
#          Life_exp.x = 25,
#          Fertility_rate.x = 26) %>% 
#   filter(Year %in% 2009:2019) %>%
#   select(-ends_with(".x"), -10) %>% 
#   select(NUTS, Name, Year,
#          ISCED_8, ISCED_7, ISCED_6, ISCED_5,
#          ISCED_4, ISCED_3, ISCED_2, ISCED_1, everything()) %>% 
#   mutate(Pop_edu_3 = ISCED_5,
#          Pop_edu_2 = (ISCED_3 - ISCED_5), #here ISCED_4 and ISCED_5 are the same
#          Pop_edu_1 = (ISCED_1 - ISCED_3))
