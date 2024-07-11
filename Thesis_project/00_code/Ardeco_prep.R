getwd()

#NOTICE
#Although here the Ardeco Datasets will be mainly cleaned and prepared, it also
#includes sources from: 
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
SNPTD_Population <- read_excel("01_data-input/Ardeco/SNPTD_Population.xlsx", sheet = "Data_clean")
SUVGD_GDP <- read_excel("01_data-input/Ardeco/SUVGD_GDP(current).xlsx", sheet = "Data_clean")
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
  mutate(Unemployment_abs = Unemployment_abs*1000)# People

GFCF <- RUIGT_GFCF %>% 
  select(-"Unit") %>% 
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "GFCF_abs") %>% 
  mutate(GFCF_abs = GFCF_abs*1000000) #In EUR

Wage_ardeco <- RUWCDW_Wage %>%
  pivot_longer(cols = -c("NUTS", "Name"), 
               names_to = "Year", 
               values_to = "A_Wage_abs") %>%  #In EUR
  mutate(M_Wage_abs = A_Wage_abs/12) %>% 
  select(-4) %>% 
  filter(!NUTS %in% c("AL00", "AL01", "AL02", "AL03"))

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
               values_to = "Labor_Productivity_abs") #In EUR

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

Emp_NACE_Ardeco <- SNETZ %>%
  select(-"Unit") %>%
  pivot_longer(cols = -c("NUTS", "Name", "NACE"), 
               names_to = "Year", 
               values_to = "Emp_Nace_abs") %>% 
  mutate(Emp_Nace_abs = Emp_Nace_abs*1000,   # People
         Name = if_else(NUTS == "HU10", "Közép-Magyarország (NUTS 2013)", Name)) %>%  
  pivot_wider(names_from = NACE,
              values_from = Emp_Nace_abs,
              names_prefix = "EMP_NACE_")

# Additional sources ----
LFS_NACE_TR <- read_excel("01_data-input/Eurostat/lfst_r_lfe2en2_Turkey.xlsx", sheet = "Data_clean", na = ":")

#Necessary step because Ardeco adopts differnt encoding names
nuts_names_lookup <- tibble(
  NUTS = c("TR10", "TR21", "TR22", "TR31", "TR32", "TR33", "TR41", "TR42", 
           "TR51", "TR52", "TR61", "TR62", "TR63", "TR71", "TR72", "TR81", 
           "TR82", "TR83", "TR90", "TRA1", "TRA2", "TRB1", "TRB2", "TRC1", 
           "TRC2", "TRC3"),
  Name = c("Istanbul", "Tekirdag, Edirne, Kirklareli", "Balikesir, Çanakkale", 
           "Izmir", "Aydin, Denizli, Mugla", "Manisa, Afyonkarahisar, Kütahya, Usak", 
           "Bursa, Eskisehir, Bilecik", "Kocaeli, Sakarya, Düzce, Bolu, Yalova", 
           "Ankara", "Konya, Karaman", "Antalya, Isparta, Burdur", "Adana, Mersin", 
           "Hatay, Kahramanmaras, Osmaniye", "Kirikkale, Aksaray, Nigde, Nevsehir, Kirsehir", 
           "Kayseri, Sivas, Yozgat", "Zonguldak, Karabük, Bartin", "Kastamonu, Çankiri, Sinop", 
           "Samsun, Tokat, Çorum, Amasya", "Trabzon, Ordu, Giresun, Rize, Artvin, Gümüshane", 
           "Erzurum, Erzincan, Bayburt", "Agri, Kars, Igdir, Ardahan", "Malatya, Elazig, Bingöl, Tunceli", 
           "Van, Mus, Bitlis, Hakkari", "Gaziantep, Adiyaman, Kilis", "Sanliurfa, Diyarbakir", 
           "Mardin, Batman, Sirnak, Siirt")
)

Emp_NACE_TR <- LFS_NACE_TR %>%
  mutate(across(where(is.double), ~ . * 1000)) %>%
  rename(NUTS = 1, Name = 2, Year = 3) %>%
  left_join(nuts_names_lookup, by = "NUTS") %>%
  mutate(Name = coalesce(Name.y, Name.x)) %>%
  select(-Name.x, -Name.y)

Emp_NACE_ME <- read_excel("01_data-input/National/ME/NACE_Employment_ME.xlsx", na = ":") %>% 
  mutate(Year = as.character(Year))

Emp_NACE_AL <- read_excel("01_data-input/wiiw/emp.xlsx", sheet = "Data_clean_AL", na = ".") %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Value") %>%
  mutate(Value = Value*1000) %>% 
  select(NUTS, Name, Year, Nace, Value) %>% 
  pivot_wider(names_from = Nace, 
              values_from = Value,
              names_prefix = "EMP_NACE_") %>% 
  select(-4)

Emp_Nace <- bind_rows(Emp_NACE_Ardeco, Emp_NACE_TR, Emp_NACE_ME, Emp_NACE_AL) %>% 
  arrange(NUTS, Year)

Wage_AL <- read_excel("01_data-input/wiiw/wages1.xlsx", sheet = "Data_clean", na = ".") %>% 
  slice(2) %>% 
  select(-c(3,4)) %>% 
  pivot_longer(cols = -c("NUTS", "Name", ), 
               names_to = "Year", 
               values_to = "M_Wage_abs")

Wage <- bind_rows(Wage_AL, Wage_ardeco) %>% 
  arrange(NUTS, Year)

# Merging ----
ardeco_list <- list(Emp, Emp_Nace, GDP, GFCF, GVA, GVA_Nace, Migr, Pop, Prod, Unemp, Wage)
ardeco_data <- Reduce(function(x, y) merge(x, y, by = c("NUTS", "Year", "Name"), all = TRUE), ardeco_list)

ardeco <- ardeco_data %>% 
  filter(!NUTS %in% c("AL01", "AL02", "AL03"),
         Year  %in% c(2009:2019)) %>%
  arrange(NUTS, Year)

#SAVING
write.csv(ardeco, file = here("02_intermediary-input", "ardeco_dataset.csv"), row.names = FALSE)
