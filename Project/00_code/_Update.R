getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

dataset <- readRDS("03_final-input/dataset.rds")

## Overview ----
length(unique(dataset$Country))
ex1 <- dataset %>%
  filter(Subregion == "EU Candidates")
ex2 <- dataset %>%
  filter(Subregion != "EU Candidates")
length(unique(ex1$NUTS)) + length(unique(ex2$NUTS))
core <- dataset %>% 
  select(-c(Country, Dist_BRUX, Subregion, Capital, Coastal, Island, Beneficiary, EU_Member, GDP_growth)) %>%
  st_set_geometry(NULL)

total_observations <- nrow(core) * ncol(core)
total_NAs <- sum(is.na(core))
dataset_NAs <- round((total_NAs / total_observations) * 100,2)

NAs_per_column <- colSums(is.na(core))
variable_NAs <- round((NAs_per_column / nrow(core)) * 100,2)

nuts_NAs <- core %>%
  group_by(NUTS, Name) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  rowwise() %>%
  mutate(total_NAs = sum(c_across(starts_with("MIgration_abs"):starts_with("Population_density"))),
         total_years = length(unique(core$Year)),
         total_variables = ncol(core) - 3,  # Subtracting 3 for NUTS, Name, and Year columns
         total_values = total_years * total_variables,
         total_values_1 = n() * (ncol(core) - 2),
         share_NAs = total_NAs / total_values *100) %>%
  ungroup() %>%
  select(NUTS, Name, share_NAs) %>%
  arrange(desc(share_NAs))
head(nuts_NAs, 10)

# NUTS shape across time ----
data_ex <- dataset %>%
  filter(NUTS %in% c("LT00", "LT01", "LT02", "UKN0") & Year == 2014 | Country == "Ireland" & Year == 2014) %>% 
  select(NUTS, Name, geometry)
glimpse(data_ex)

plot1_data <- data_ex %>% filter(NUTS %in% c("LT00"))
plot2_data <- data_ex %>% filter(NUTS %in% c("LT01", "LT02"))
plot3_data <- data_ex %>% filter(NUTS %in% c("UKN0", "IE01", "IE02"))
plot4_data <- data_ex %>% filter(NUTS %in% c("UKN0","IE04", "IE05", "IE06"))

# Set up the plotting area with 2 rows and 2 columns, adjust margins
par(mfrow = c(2, 2), mar = c(1, 1, 1, 1), oma = c(4, 4, 2, 2))  # Adjust mar and oma values as needed

# Plot 1: LT00 (Upper Left)
plot(st_geometry(plot1_data), main = "Lithuania Before 2013")

# Plot 2: LT01 and LT02 (Upper Right)
plot(st_geometry(plot2_data), main = "Lithuania After")

# Plot 3: UKN0, IE01, and IE02 (Lower Left)
plot(st_geometry(plot3_data), main = "Ireland Before 2013")

# Plot 4: UKN0, IE04, IE05, and IE06 (Lower Right)
plot(st_geometry(plot4_data), main = "Ireland After")

# Add text along the y-axis
mtext("Group 1", side = 2, line = -6, outer = TRUE, at = 0.75, las = 0)
mtext("Group 2", side = 2, line = -6, outer = TRUE, at = 0.25, las = 0)

# Population by educational attainment ----
filtered_data <- dataset %>%
  filter(Subregion == "EU Candidates",
         !Country == "Turkey")

ggplot(filtered_data, aes(x = Year, y = Pop_edu_1, color = Country, group = NUTS)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Pop_edu_3",
       color = "Country:") +
  scale_size_manual(values = c(1, 2), guide = "none") + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(filtered_data$Year), max(filtered_data$Year), by = 1))

# Kosovo Outlier
data_2018 <- dataset %>%
  filter(Year == 2018)

World <- ne_countries(scale = "large", returnclass = "sf") %>% 
  select(10,169) %>% 
  st_transform(., crs = "EPSG:4326") %>% 
  st_as_sf(., wkt = "geometry")

Pop_1 <- ggplot(World) +
  geom_sf(color = "black") +
  geom_sf(data = data_2018, aes(fill = Pop_edu_1), color = NA) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50", name = "Pop_edu_1") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70))

Pop_2 <- ggplot(World) +
  geom_sf(color = "black") +
  geom_sf(data = data_2018, aes(fill = Pop_edu_2), color = NA) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50", name = "Pop_edu_2") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70))

Pop_3 <- ggplot(World) +
  geom_sf(color = "black") +
  geom_sf(data = data_2018, aes(fill = Pop_edu_3), color = NA) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50", name = "Pop_edu_3") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70))
grid.arrange(Pop_1, Pop_2, Pop_3, nrow = 1)
