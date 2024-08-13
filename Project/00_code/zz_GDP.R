getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

# Ardeco ----
SOVGD_GDP <- read_excel("01_data-input/Ardeco/SOVGD_GDP(constant).xlsx", sheet = "Data_clean")
SUVGD_GDP <- read_excel("01_data-input/Ardeco/SUVGD_GDP(current).xlsx", sheet = "Data_clean")

GDP_constant <- SOVGD_GDP %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GDP_EUR_2015")

GDP_current <- SUVGD_GDP %>%
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GDP_EUR")

GDP <- full_join(GDP_constant, GDP_current, by = c("NUTS", "Name" ,"Year"))
glimpse(GDP)
GDP_correct <- GDP %>% 
  mutate(Deflator_2015 = (GDP_EUR/GDP_EUR_2015)*100)

GDP_new <- GDP_correct %>% 
  select(-starts_with("GDP_")) %>% 
  pivot_wider(names_from = Year,
              values_from = Deflator_2015) %>% 
  mutate(across(starts_with("20"), ~ .x / `2009`*100)) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Deflator_2009")

GDP_final <- full_join(GDP_correct, GDP_new, by = c("NUTS","Name" ,"Year")) %>% 
  mutate(GDP_EUR_2009 = GDP_EUR/`Deflator_2009`*100,
         GDP_EUR_2009 = GDP_EUR_2009*1000000) %>% 
  select(NUTS, Name, Year, GDP_EUR_2009)

# Extra ----
GDP_wiiw <- read_excel("01_data-input/wiiw/gdp.xlsx", sheet = "Data_clean", na = ".")
GDP_wdi <- read_excel("01_data-input/World Bank/GDP_Extra.xlsx", sheet = "Data_clean", na = "..")
fexc <- read_excel("01_data-input/wiiw/fexc.xlsx", sheet = "Data_clean")
ECB <- read_excel("01_data-input/ECB/exchange_rate.xlsx", sheet = "Data_clean")

GDP_world <- GDP_wdi %>% 
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


exc_rate <- fexc %>%
  pivot_longer(cols = -c("NUTS", "Name", "Unit"), 
               names_to = "Year", 
               values_to = "Exchange_rate") %>%
  pivot_wider(names_from = Unit,
              values_from = Exchange_rate) %>% 
  arrange(NUTS, Name, Year)

GDP_useur <- full_join(GDP_world, ECB, by = "Year") %>% 
  mutate(GDP_EUR_2015 = GDP_US_constant*`USD/EUR`,
         GDP_EUR = GDP_US_current*`USD/EUR`,
         Deflator_2015 = (GDP_EUR/GDP_EUR_2015)*100)

GDP_extra_new <- GDP_useur %>% 
  select(NUTS, Name, Year, Deflator_2015) %>% 
  pivot_wider(names_from = Year,
              values_from = Deflator_2015) %>% 
  mutate(across(starts_with("20"), ~ .x / `2009`*100)) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Deflator_2009")

GDP_extra_final <- full_join(GDP_useur, GDP_extra_new, by = c("NUTS","Name" ,"Year")) %>% 
  mutate(GDP_EUR_2009 = GDP_EUR/`Deflator_2009`*100) %>% 
  select(NUTS, Name, Year, GDP_EUR_2009)

GDP_Europe_correct <- rbind(GDP_extra_final, GDP_final) %>% 
  arrange(NUTS, Name, Year) %>% 
  group_by(Name, NUTS) %>%
  mutate(GDP_growth = (GDP_EUR_2009 - lag(GDP_EUR_2009)) / lag(GDP_EUR_2009)) %>%
  filter(!NUTS %in% c("AL01", "AL02", "AL03"), Year %in% c(2009:2019)) %>%
  ungroup()
