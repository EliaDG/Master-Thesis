
#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

dataset <- readRDS("03_final-input/dataset.rds")

## Overview dataset: ----
ex1 <- dataset %>%
  filter(Country %in% c("Albania", "Bosnia and Herzegovina","Montenegro", "North Macedonia", "Turkey", "Moldova", "Kosovo", "Serbia"))
length(unique(ex1$NUTS))
ex2 <- dataset %>%
  filter(!Country %in% c("Albania","Bosnia and Herzegovina", "Montenegro", "North Macedonia", "Turkey", "Moldova", "Kosovo", "Serbia"))
length(unique(ex2$NUTS))

data <- dataset %>% 
  select(-c(Country, Subregion, Centroid, Dist_BRUX, Lon, Lat, Capital, Coastal, Island, Beneficiary, EU_Member, GDP_growth)) %>% 
  st_set_geometry(NULL)

total_observations <- nrow(data) * ncol(data)
total_NAs <- sum(is.na(data))
dataset_NAs <- round((total_NAs / total_observations) * 100,2)

NAs_per_column <- colSums(is.na(data))
variable_NAs <- round((NAs_per_column / nrow(data)) * 100,2)

nuts_NAs <- data %>%
  group_by(NUTS, Name) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  rowwise() %>%
  mutate(total_NAs = sum(c_across(starts_with("MIgration_abs"):starts_with("Population_density"))),
         total_years = length(unique(data$Year)),
         total_variables = ncol(data) - 3,  # Subtracting 3 for NUTS, Name, and Year columns
         total_values = total_years * total_variables,
         total_values_1 = n() * (ncol(data) - 2),
         share_NAs = total_NAs / total_values *100) %>%
  ungroup() %>%
  select(NUTS, Name, total_NAs, total_values, share_NAs) %>%
  arrange(desc(total_NAs))

glimpse(dataset)
filtered_data <- dataset %>%
  filter(Subregion == "EU Candidates")

ggplot(filtered_data, aes(x = Year, y = Pop_edu_2, color = Country, group = NUTS)) +
  geom_line() +
  labs(title = "Pop_edu_1 over Time for NUTS Regions in EU Candidates Subregion",
       x = "Year",
       y = "Pop_edu_1",
       color = "NUTS Region") +
  scale_size_manual(values = c(1, 2), guide = "none") +  # Set sizes and remove legend for size
  theme_minimal()
