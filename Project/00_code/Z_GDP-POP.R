getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

# GDP Ardeco ----
SOVGD_GDP <- read_excel("01_data-input/Ardeco/SOVGD_GDP(constant).xlsx", sheet = "Data_clean")
SUVGD_GDP <- read_excel("01_data-input/Ardeco/SUVGD_GDP(current).xlsx", sheet = "Data_clean")

GDP_constant <- SOVGD_GDP %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GDP_EUR_2015") %>% 
  mutate(GDP_EUR_2015 = GDP_EUR_2015*1000000)

GDP_current <- SUVGD_GDP %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GDP_EUR") %>% 
  mutate(GDP_EUR = GDP_EUR*1000000)

GDP_ardeco <- full_join(GDP_constant, GDP_current, by = c("NUTS", "Name" ,"Year"))
GDP_def_2015 <- GDP_ardeco %>% 
  mutate(Deflator_2015 = (GDP_EUR/GDP_EUR_2015)*100) %>% 
  select(-starts_with("GDP_"))

GDP_def_2009 <- GDP_def_2015 %>%
  pivot_wider(names_from = Year,
              values_from = Deflator_2015) %>% 
  mutate(across(starts_with("20"), ~ .x / `2009`*100)) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Deflator_2009")

list <- list(GDP_ardeco, GDP_def_2015, GDP_def_2009)
GDP_EU_final <- Reduce(function(x, y) full_join(x, y, by = c("NUTS", "Name", "Year")), list) %>% 
  mutate(GDP_EUR_2009 = GDP_EUR/`Deflator_2009`*100)

# GDP Extra ----
#GDP_wiiw <- read_excel("01_data-input/wiiw/gdp.xlsx", sheet = "Data_clean", na = ".")
GDP_wdi <- read_excel("01_data-input/World Bank/GDP_Extra.xlsx", sheet = "Data_clean", na = "..")
ECB <- read_excel("01_data-input/ECB/exchange_rate.xlsx", sheet = "Data_clean")

GDP_USD <- GDP_wdi %>% 
  slice(7:12) %>% 
  mutate(Series = case_when(Series == "GDP (constant 2015 US$)" ~ "constant",
                            Series == "GDP (current US$)" ~ "current")) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "GDP") %>% 
  pivot_wider(names_from = Series,
              values_from = GDP,
              names_prefix = "GDP_US_") %>% 
  arrange(NUTS, Name, Year)


GDP_EUR <- full_join(GDP_USD, ECB, by = "Year") %>% 
  mutate(GDP_EUR_2015 = GDP_US_constant/`USD/EUR`,
         GDP_EUR = GDP_US_current/`USD/EUR`,
         Deflator_2015 = (GDP_EUR/GDP_EUR_2015)*100) %>% 
  select(-contains("US"))

GDP_DEF <- GDP_EUR %>% 
  select(NUTS, Name, Year, Deflator_2015) %>% 
  pivot_wider(names_from = Year,
              values_from = Deflator_2015) %>% 
  mutate(across(starts_with("20"), ~ .x / `2009`*100)) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Deflator_2009")

GDP_extra_final <- full_join(GDP_EUR, GDP_DEF, by = c("NUTS","Name" ,"Year")) %>% 
  mutate(GDP_EUR_2009 = GDP_EUR/`Deflator_2009`*100)

# Population ---------
SNPTD_Population <- read_excel("01_data-input/Ardeco/SNPTD_Population.xlsx", sheet = "Data_clean")
Population_extra <- read_excel("01_data-input/wiiw/pop_lifexp.xlsx", sheet = "Data_clean", na = ".")
Population_bosnia <- read_excel("01_data-input/World Bank/Pop_BiH.xlsx", sheet = "Data_clean",  na = "..")

Pop_ardeco <- SNPTD_Population %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Population_abs") %>%  # People
  group_by(NUTS) %>%
  mutate(Pop_growth = (Population_abs - lag(Population_abs)) / lag(Population_abs)) %>% 
  ungroup()

Population_MD_XK <- Population_extra %>%
  filter(!Country == "Bosnia and Herzegovina") %>% 
  rename(NUTS = 1,
         Name = 2) %>% 
  select(-c(3,4)) %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "Population_abs")%>%
  mutate(Population_abs = Population_abs*1000) %>% 
  group_by(NUTS) %>%
  mutate(Pop_growth = (Population_abs - lag(Population_abs)) / lag(Population_abs)) %>% 
  ungroup()

Population_BA <- Population_bosnia %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Population_abs") %>% 
  mutate(Pop_growth = (Population_abs - lag(Population_abs)) / lag(Population_abs))

Population <- rbind(Population_BA, Population_MD_XK, Pop_ardeco) %>% 
  arrange(NUTS, Year) %>% 
  filter(!NUTS %in% c("AL01", "AL02", "AL03"))

# Merging -----
GDP_Pop_Europe_correct <- rbind(GDP_extra_final, GDP_EU_final) %>%
  select(-starts_with("Defl")) %>% 
  full_join(Population, by = c("NUTS", "Name", "Year")) %>% 
  arrange(NUTS, Name, Year) %>% 
  group_by(Name, NUTS) %>%
  mutate(GDP_capita_09 = GDP_EUR_2009/Population_abs,
         GDP_capita_15 = GDP_EUR_2015/Population_abs,
         GDP_capita = GDP_EUR/Population_abs,
         GDP_growth_09 = (GDP_capita_09 - lag(GDP_capita_09)) / lag(GDP_capita_09),
         GDP_growth_15 = (GDP_capita_15 - lag(GDP_capita_15)) / lag(GDP_capita_15),
         GDP_growth = (GDP_capita - lag(GDP_capita)) / lag(GDP_capita)) %>%
  filter(!NUTS %in% c("AL01", "AL02", "AL03"), Year %in% c(2009:2019)) %>%
  ungroup() %>% 
  select(NUTS, Name, Year, GDP_EUR_2009, GDP_capita_09, GDP_growth_09, Population_abs, Pop_growth) %>% 
  rename(GDP_EUR = GDP_EUR_2009,
         GDP_capita = GDP_capita_09, 
         GDP_growth = GDP_growth_09)

#SAVING
write.csv(GDP_Pop_Europe_correct, file = here("02_intermediary-input", "GDP-POP_dataset.csv"), row.names = FALSE)
