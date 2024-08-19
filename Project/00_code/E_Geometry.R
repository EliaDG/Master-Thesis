getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- read_csv("02_intermediary-input/dataset-merged.csv")
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

shapefile_NUTS2 <- rbind(NUTS2_2021, NUTS2_2013, NUTS2_extra_merged, XKO, MDA, BIH) %>% 
  filter(!NUTS %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5",
                      "PT20", "PT30",
                      "ES70", "ES63", "ES64",
                      "AL01", "AL02", "AL03")) %>%
  filter(!grepl("ZZ|CH|NO|LI|IS", NUTS)) %>% 
  arrange(NUTS) %>%
  select(-Name) %>% 
  filter(!NUTS %in% c("HR02", "HR05", "HR06", "HU11", "HU12", "LT01", "LT02", "UKI3",
                      "UKI4", "UKI5" , "UKI6", "UKI7", "IE01", "IE02", "UKM2", "UKM3", "FI20"))

dataset_complete <- dataset %>%
  filter(!NUTS %in% c("HR02", "HR05", "HR06", "HU11", "HU12", "LT01", "LT02", "UKI3",
                      "UKI4", "UKI5" , "UKI6", "UKI7", "IE01", "IE02", "UKM2", "UKM3", "FI20")) %>% 
  full_join(shapefile_NUTS2, by = "NUTS") %>%
  st_as_sf(crs = "WGS84") %>% 
  mutate(Area = as.numeric(st_area(geometry))/1e6,
         Candidates = ifelse(Country %in% c("Bosnia and Herzegovina", "Serbia", "North Macedonia", "Montenegro", "Albania", "Moldova", "Kosovo", "Turkey"), "Yes", "No"),
         CEE = ifelse(Country %in% c("Poland", "Czech Republic", "Slovakia", "Hungary", "Lithuania", "Latvia", "Estonia", "Croatia", "Bulgaria", "Romania", "Cyprus"), "Yes", "No"),
         Capital = case_when(
           NUTS %in% c("AT13", "BE10", "BG41", "CZ01", "DE30", "DK01", "EL30", "ES30", "FI1B", "FR10", 
                       "HR05", "HR04", "HU11", "IE02", "IE06", "ITI4", "LT01", "NL32", "PL91", "PT17", 
                       "RO32", "RS11", "SE11", "SI04", "SK01", "TR51") ~ "Yes",
           grepl("00", NUTS) | grepl("UKI", NUTS) | grepl("HU1", NUTS) ~ "Yes",
           TRUE ~ "No"),
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
                       "UKK3", "UKK4", "UKL1", "UKL2", "UKM5", "UKM6", "UKM7", "UKM8", "UKM9", "UKN0") ~ "Yes",
           TRUE ~ "No"),
         Island = case_when(
           NUTS %in% c("CY00", "DK01", "DK02", "EL41", "EL42", "EL43", "EL62", "ES53", "FI20", "FRM0", "ITG1", "ITG2", "MT00") ~ "Yes",
           grepl("IE", NUTS) | grepl("UK", NUTS) ~ "Yes",
           TRUE ~ "No"),
         Objective_1 = case_when(
           NUTS %in% c("CZ02", "CZ03", "CZ04", "CZ05", "CZ06", "CZ07", "CZ08", "DE40",
                       "DE80", "DED2", "DED4", "DEE0", "DEG0", "EL41", "EL43", "EL51",
                       "EL54", "EL61", "EL62", "EL63", "EL65", "ES11", "ES42", "ES43",
                       "ES61", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33", "ITF3",
                       "ITF4", "ITF6", "ITG1", "PT11", "PT16", "PT18", "SK02", "SK03",
                       "SK04", "UKK3", "UKL1") & Year %in% 2009:2013 ~ "Yes",
           grepl("BG", NUTS) & Year %in% 2009:2013 
           | grepl("MT", NUTS) & Year %in% 2009:2013 
           | grepl("EE", NUTS) & Year %in% 2009:2013 
           | grepl("LV", NUTS) & Year %in% 2009:2013
           | grepl("LT", NUTS) & Year %in% 2009:2013
           | grepl("PL", NUTS) & Year %in% 2009:2013
           | grepl("RO", NUTS) & Year %in% 2009:2013
           | grepl("SI", NUTS) & Year %in% 2009:2013 ~ "Yes",
           grepl("HR", NUTS) & Year %in% 2013:2019 ~ "Yes",
           NUTS %in% c("BG31", "BG32", "BG33", "BG34", "BG41", "BG42",
                       "CZ02", "CZ03", "CZ04", "CZ05", "CZ06", "CZ07", "CZ08",
                       "EE00", "EL51", "EL52", "EL61", "EL54", "EL63",
                       "ES43", "HR03", "HR04", "ITF3", "ITF4", "ITF5", "ITF6", "ITG1",
                       "LV00", "LT00", "LT01", "LT02", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33",
                       "PL71", "PL21", "PL22", "PL43", "PL82", "PL72", "PL84", "PL81",
                       "PL41", "PL42", "PL43", "PL51", "PL52", "PL61", "PL62", "PL63",
                       "PT11", "PT16", "PT18", "RO11", "RO12", "RO21", "RO22",
                       "RO31", "RO41", "RO42", "SI01", "SK02", "SK03", "SK04", "UKK3", "UKL1") & Year %in% 2014:2019 ~ "Yes",
           TRUE ~ "No"),
         Euro = case_when(
           Country %in% c("Austria", "Belgium", "Cyprus", "Finland",
                          "France", "Germany", "Greece", "Ireland", "Italy", 
                          "Luxembourg", "Malta", "Netherlands", "Portugal", 
                          "Slovakia", "Slovenia", "Spain", "Montenegro", "North Macedonia") ~ "Yes",
           Country == "Estonia" & Year %in% 2011:2019 ~ "Yes",
           Country == "Latvia" & Year %in% 2014:2019 ~ "Yes",
           Country == "Lithuania" & Year %in% 2015:2019 ~ "Yes",
           TRUE ~ "No"),
         Output_density = (GDP_EUR/1000000)/Area,
         Employment_density = (Employment_abs/1000)/Area,
         Population_density = (Population_abs/1000)/Area) %>%
  select(-geometry, Country, NUTS, Name, Year, everything(), geometry) %>% 
  select(-c(Area, Population_abs, GDP_EUR, Employment_abs)) %>% 
  st_cast("MULTIPOLYGON")

bruxelles_centroid <- shapefile_NUTS2 %>%
  filter(NUTS == "BE10") %>%
  mutate(Centroid = st_centroid(geometry)) %>% 
  st_geometry()

dataset_centroids <- shapefile_NUTS2 %>%
  mutate(Centroid = st_centroid(geometry))

Dist_BRUX <- shapefile_NUTS2 %>% 
  mutate(Dist_BRUX = as.numeric(st_distance(st_geometry(dataset_centroids), bruxelles_centroid[1])),
         Dist_BRUX = Dist_BRUX/1000) %>%
  st_set_geometry(NULL)

dataset_final <- dataset_complete %>% 
  full_join(Dist_BRUX, by = "NUTS") %>%
  mutate(Country = as.factor(Country),
         across(c(Capital, Coastal, Island, Objective_1, Euro, CEE, Candidates),
                ~ as.numeric(ifelse(. == "Yes", 1, ifelse(. == "No", 0, .))))) %>%
  st_set_geometry(NULL)

encoded_dataset <- dataset_final
one_hot <- model.matrix(~ Country - 1, data = encoded_dataset)
colnames(one_hot) <- gsub("Country", "", colnames(one_hot))
encoded_dataset <- cbind(encoded_dataset, one_hot)
encoded_dataset <- encoded_dataset[, !(names(encoded_dataset) %in% c("Country"))]

colnames(encoded_dataset) <- make.names(colnames(encoded_dataset))

#SAVING
saveRDS(encoded_dataset, "03_final-input/dataset.rds")
saveRDS(shapefile_NUTS2, "03_final-input/shapefile.rds")