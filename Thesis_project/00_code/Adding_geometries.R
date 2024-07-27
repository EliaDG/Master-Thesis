getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- read_csv("03_final-input/dataset.csv")
BIH <- st_read(dsn = "01_data-input/Shapefiles/gadm41_BIH_0.shp") %>%
  rename(NUTS = 1,
         Name = 2) %>% 
  mutate(NUTS = ifelse(NUTS == "BIH", "BA00", NUTS))
MDA <- st_read(dsn ="01_data-input/Shapefiles/gadm41_MDA_0.shp") %>%
  rename(NUTS = 1,
         Name = 2) %>% 
  mutate(NUTS = ifelse(NUTS == "MDA", "MD00", NUTS))
XKO <- st_read(dsn ="01_data-input/Shapefiles/gadm41_XKO_0.shp") %>%
  rename(NUTS = 1,
         Name = 2) %>% 
  mutate(NUTS = ifelse(NUTS == "XKO", "XK00", NUTS))

NUTS2_2021 <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2021) %>%
  select(1, 5, 12) %>%
  rename(NUTS = 1,
         Name = 2)
# If it doesn't work you can add:
# cache = TRUE,
# update_cache = TRUE

NUTS2_2013 <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2013) %>%
  select(1, 4, 5, 12) %>%
  rename(NUTS = 1,
         Country = 2,
         Name = 3) %>% 
  filter(Country %in% c("IE", "UK")) %>% 
  select(-2) %>% 
  filter(NUTS %in% c("UKM3", "UKM2", "IE01", "IE02"))

NUTS2_extra_merged <- NUTS2_2021 %>%
  filter(NUTS %in% c("LT01", "LT02",
                     "UKI5", "UKI6", "UKI7",
                     "UKI3", "UKI4",
                     "HR02", "HR05", "HR06",
                     "HU11", "HU12",
                     "AL01", "AL02", "AL03")) %>%
  merge_geometries(c("LT01", "LT02"), "LT00", "Lietuva (NUTS 2013)") %>%
  merge_geometries(c("UKI5", "UKI6", "UKI7"), "UKI2", "Outer London (NUTS 2010)") %>%
  merge_geometries(c("UKI3", "UKI4"), "UKI1", "Inner London (NUTS 2010)") %>%
  merge_geometries(c("HU11", "HU12"), "HU10", "Közép-Magyarország (NUTS 2013)") %>%
  merge_geometries(c("HR02", "HR05", "HR06"), "HR04", "Kontinentalna Hrvatska (NUTS 2016)") %>%
  merge_geometries(c("AL01", "AL02", "AL03"), "AL00", "Albania")

NUTS2_complete <- rbind(NUTS2_2021, NUTS2_2013, NUTS2_extra_merged, XKO, MDA, BIH) %>% 
  filter(!NUTS %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5",
                      "PT20", "PT30",
                      "ES70", "ES63", "ES64",
                      "AL01", "AL02", "AL03")) %>%
  filter(!grepl("ZZ|CH|NO|LI|IS", NUTS)) %>% 
  arrange(NUTS) %>% 
  select(-2)

dataset_complete <- dataset %>%
  full_join(NUTS2_complete, by = c("NUTS")) %>%
  st_as_sf() %>%
  mutate(Area = st_area(geometry),
         Centroid = st_centroid(geometry),
         Subregion = case_when(
           Country %in% c("Bosnia and Herzegovina", "Serbia", "North Macedonia", "Montenegro", "Albania", "Moldova", "Kosovo", "Turkey") ~ "EU Candidates",
           Country %in% c("Poland", "Czeck Republic", "Slovakia", "Hungary", "Lithuania", "Lavtia", "Estonia", "Croatia", "Bulgaria","Greece", "Romania", "Cyprus") ~ "Central-Eastern Europe",
           TRUE ~ "Western Europe"),
         Subregion = as.factor(Subregion),
         Capital = case_when(
           NUTS %in% c("AT13", "BE10", "BG41", "CZ01", "DE30", "DK01", "EL30", "ES30", "FI1B", "FR10", "HR05", "HR04", "HU11", "IE02", "IE06", "ITI4", "LT01", "NL32", "PL91", "PT17", "RO32", "RS11", "SE11", "SI04", "SK01", "TR51") ~ "Capital Region",
           grepl("00", NUTS) | grepl("UKI", NUTS) | grepl("HU1", NUTS) ~ "Capital Region",
           TRUE ~ "Other"),
         Capital = as.factor(Capital),
         Coastal = case_when(
           NUTS %in% c("AL00", "BA00", "BE21", "BE25", "BG33", "BG34", "CY00", "DE50", "DE60", 
                       "DE80", "DE93", "DE94", "DEF0", "DK01", "DK02", "DK03", "DK04", "DK05", 
                       "EE00", "EL30", "EL41", "EL42", "EL43", "EL51", "EL52", "EL54", "EL61", 
                       "EL62", "EL63", "EL64", "EL65", "ES11", "ES12", "ES13", "ES21", "ES51", 
                       "ES52", "ES53", "ES61", "ES62", "FRE1", "FRE2", "FRD1", "FRD2", "FRG0", 
                       "FRH0", "FRI1", "FRI3", "FRJ1", "FRL0", "FRM0", "HR03", "IE01", "IE02", 
                       "IE04", "IE05", "IE06", "ITH3", "ITH4", "ITH5", "ITI1", "ITI3", "ITI4", 
                       "ITC3", "ITF1", "ITF2", "ITF3", "ITF4", "ITF5", "ITF6", "ITG1", "ITG2", 
                       "LT00", "LT02", "LV00", "ME00", "MT00", "NL11", "NL12", "NL32", "NL33", 
                       "NL34", "PL42", "PL62", "PL63", "PT11", "PT15", "PT16", "PT17", "PT18", 
                       "RO22", "SE11", "SE12", "SE21", "SE22", "SE23", "SE31", "SE32", "SE33", 
                       "SI04", "FI19", "FI1B", "FI1C", "FI1D", "FI20", "TR10", "TR21", "TR22", 
                       "TR31", "TR32", "TR41", "TR42", "TR61", "TR62", "TR63", "TR81", "TR82", 
                       "TR83", "TR90", "UKC1", "UKC2", "UKD1", "UKD4", "UKD6", "UKD7", "UKE1", "UKM3",
                       "UKE2", "UKF3", "UKH1", "UKH3", "UKJ2", "UKJ3", "UKJ4", "UKK1", "UKK2", "UKM2",
                       "UKK3", "UKK4", "UKL1", "UKL2", "UKM5", "UKM6", "UKM7", "UKM8", "UKM9", "UKN0") ~ "Coastal",
           TRUE ~ "Land-locked"),
         Coastal = as.factor(Coastal),
         Island = case_when(
           NUTS %in% c("CY00", "DK01", "DK02", "EL41", "EL42", "EL43", "EL62", "ES53", "FI20", "FRM0", "ITG1", "ITG2", "MT00") ~ "Yes",
           grepl("IE", NUTS) | grepl("UK", NUTS) ~ "Yes",
           TRUE ~ "No")) %>%
  select(Country, NUTS, Name, Subregion, Year, everything(), geometry)


# Output_density = ,
# Employment_density = ,
# Population_density =
# Objective_1
