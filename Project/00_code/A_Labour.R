getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
emp_rate <- read_excel("01_data-input/Eurostat/lfst_r_lfe2emprt.xlsx", sheet = "Data_clean", na = ":")
unemp_rate <- read_excel("01_data-input/Eurostat/lfst_r_lfu3rt.xlsx", sheet = "Data_clean", na = ":")
extra <- read_excel("01_data-input/World Bank/emp-unemp_extra.xlsx", sheet = "Data_clean", na = "..")


employment <- emp_rate %>% 
  rename(NUTS = 1,
         Name = 2) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "emp_rate") %>% 
  mutate(emp_rate = emp_rate/100)

unemployment <- unemp_rate %>% 
  rename(NUTS = 1,
         Name = 2) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "unemp_rate") %>% 
  mutate(unemp_rate = unemp_rate/100)

extra_rate <- extra %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Values") %>% 
  mutate(Values = Values/100) %>% 
  pivot_wider(names_from = "Series Name",
              values_from = "Values") %>% 
  rename(emp_rate = 4,
         unemp_rate = 5)

data_labour <- full_join(employment, unemployment, by = c("NUTS", "Name", "Year")) %>% 
  rbind(extra_rate) %>% 
  arrange(NUTS, Year) %>%
  mutate(Year = as.numeric(Year)) %>%
  select(-Name)

#SAVING
write.csv(data_labour, file = here("02_intermediary-input", "labour.csv"), row.names = FALSE)
