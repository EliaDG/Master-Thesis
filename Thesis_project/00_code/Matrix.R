getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- readRDS("03_final-input/dataset.rds")

data_2021 <- dataset %>% 
  filter(!NUTS %in% c("HR04", "HU10", "IE01", "IE02", "LT00", "UKI1" , "UKI2", "UKM2", "UKM3"),
         Year == 2015) %>% 
  select(NUTS, Name, geometry)
data_alt <- dataset %>% 
  filter(!NUTS %in% c("HR02", "HR05", "HR06", "HU11", "HU12", "IE04", "IE05", "IE06", "LT01", "LT02", "UKI1" , "UKI4", "UKI5" , "UKI6", "UKI7" , "UKI3", "UKM7", "UKM8", "UKM9"),
         Year == 2015) %>% 
  select(NUTS, Name, geometry)

# Spatial Matrix for 2021 version----
queen_nb <- poly2nb(data_2021, row.names=data_2021$NUTS, queen=TRUE)
queen_listw <- nb2listw(queen_nb, style = "B", zero.policy=TRUE)
queen_matrix <- listw2mat(queen_listw)
colnames(queen_matrix) <- data_2021$NUTS

region_pairs <- list(
  c("BA00", "ME00"), # Bosnia and Herzegovina, Crna Gora
  c("BA00", "RS21"), # Bosnia and Herzegovina, Region Šumadije i Zapadne Srbije
  c("BA00", "RS12"), # Bosnia and Herzegovina, Autonomous Province of Vojvodina
  c("BA00", "HR03"), # Bosnia and Herzegovina, Jadranska Hrvatska
  c("BA00", "HR02"), # Bosnia and Herzegovina, Panonska Hrvatska
  c("XK00", "MK00"), # Kosovo, Severna Makedonija
  c("XK00", "ME00"), # Kosovo, Crna Gora
  c("XK00", "RS21"), # Kosovo, Region Šumadije i Zapadne Srbije
  c("XK00", "RS22"), # Kosovo, Region Južne i Istočne Srbije
  c("XK00", "AL00"), # Kosovo, Albania
  c("MD00", "RO22"), # Moldova, Sud-Est
  c("MD00", "RO21"), # Moldova, Nord-Est
  c("ITG1", "ITF6"), # Sicilia, Calabria
  c("ITG2", "ITI4"), # Sardegna, Lazio
  c("FRM0", "FRL0"), # Corse, Provence-Alpes-Côte d’Azur
  c("ES53", "ES51"), # Illes Balears, Cataluña
  c("CY00", "TR62"), # Kýpros, Adana, Mersin
  c("EL65", "EL43"), # Peloponnisos, Kriti
  c("EL42", "EL30"), # Notio Aigaio, Attiki
  c("EL64", "EL41"), # Sterea Elláda, Voreio Aigaio
  c("DK02", "DK03"), # Sjælland, Syddanmark
  c("DK01", "SE22"), # Hovedstaden, Sydsverige
  #c("FI20", "FI1C"), # Åland, Etelä-Suomi
  c("MT00", "ITG1"), # Malta, Sicilia
  c("UKJ4", "FRE1"), # Kent, Nord-Pas de Calais
  c("FI1B", "EE00"), # Helsinki-Uusimaa, Eesti
  c("UKN0", "UKM7"), # Northern Ireland, Southern Scotland
  c("IE06", "UKD7"), # Eastern and Midland, Merseyside
  c("EL62", "EL63")
)

value_pairs <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1)

new_W_matrix <- update_w_queen(queen_matrix, region_pairs, value_pairs)
queen_listw <- mat2listw(new_W_matrix, style = "B", row.names = data_2021$NUTS, zero.policy = TRUE)
queen_nb <- queen_listw$neighbours
# queen_lines <- listw2lines(queen_listw, coords = st_centroid(data_2021$geometry))
# 
# ggplot(data = data_2021) +
#   geom_sf(color = "black") +
#   theme_light() +
#   geom_sf(data = queen_lines, color = "red", size = 0.8)

coords <- st_coordinates(st_centroid(data_2021$geometry))
dist <- nbdists(queen_nb, coords, longlat = TRUE)
idw <- lapply(dist, function(x) 1/(x))
# idw_squared <- lapply(dist, function(x) 1/(x^2))
region_names <- attr(queen_nb, "region.id")
idw_named <- setNames(idw, region_names)

# Number of regions
num_regions <- length(region_names)

# Initialize an empty matrix
idw_matrix_A <- matrix(0, nrow = num_regions, ncol = num_regions)
rownames(idw_matrix_A) <- region_names
colnames(idw_matrix_A) <- region_names

# Fill the matrix with idw values
for (i in seq_along(idw_named)) {
  region <- names(idw_named)[i]
  neighbors <- queen_nb[[i]]
  idw_values <- idw_named[[i]]
  
  # Assign the idw values to the corresponding positions in the matrix
  idw_matrix_A[region, region_names[neighbors]] <- idw_values
}


# Spatial Matrix for alt version -----
queen_nb <- poly2nb(data_alt, row.names=data_alt$NUTS, queen=TRUE)
queen_listw <- nb2listw(queen_nb, style = "B", zero.policy=TRUE)
queen_matrix <- listw2mat(queen_listw)
colnames(queen_matrix) <- data_alt$NUTS

region_pairs <- list(
  c("BA00", "ME00"), # Bosnia and Herzegovina, Crna Gora
  c("BA00", "RS21"), # Bosnia and Herzegovina, Region Šumadije i Zapadne Srbije
  c("BA00", "RS12"), # Bosnia and Herzegovina, Autonomous Province of Vojvodina
  c("BA00", "HR03"), # Bosnia and Herzegovina, Jadranska Hrvatska
  c("BA00", "HR04"), # Bosnia and Herzegovina, Kontinetalna Hrvatska
  c("XK00", "MK00"), # Kosovo, Severna Makedonija
  c("XK00", "ME00"), # Kosovo, Crna Gora
  c("XK00", "RS21"), # Kosovo, Region Šumadije i Zapadne Srbije
  c("XK00", "RS22"), # Kosovo, Region Južne i Istočne Srbije
  c("XK00", "AL00"), # Kosovo, Albania
  c("MD00", "RO22"), # Moldova, Sud-Est
  c("MD00", "RO21"), # Moldova, Nord-Est
  c("ITG1", "ITF6"), # Sicilia, Calabria
  c("ITG2", "ITI4"), # Sardegna, Lazio
  c("FRM0", "FRL0"), # Corse, Provence-Alpes-Côte d’Azur
  c("ES53", "ES51"), # Illes Balears, Cataluña
  c("CY00", "TR62"), # Kýpros, Adana, Mersin
  c("EL65", "EL43"), # Peloponnisos, Kriti
  c("EL42", "EL30"), # Notio Aigaio, Attiki
  c("EL64", "EL41"), # Sterea Elláda, Voreio Aigaio
  c("DK02", "DK03"), # Sjælland, Syddanmark
  c("DK01", "SE22"), # Hovedstaden, Sydsverige
  #c("FI20", "FI1C"), # Åland, Etelä-Suomi
  c("MT00", "ITG1"), # Malta, Sicilia
  c("UKJ4", "FRE1"), # Kent, Nord-Pas de Calais
  c("FI1B", "EE00"), # Helsinki-Uusimaa, Eesti
  c("UKN0", "UKM3"), # Northern Ireland, Southern Scotland
  c("IE02", "UKD7"), # Southern-Eastern and Midland, Merseyside
  c("EL62", "EL63")
)

value_pairs <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1)

new_W_matrix <- update_w_queen(queen_matrix, region_pairs, value_pairs)
queen_listw <- mat2listw(new_W_matrix, style = "B", row.names = data_alt$NUTS, zero.policy = TRUE)
queen_nb <- queen_listw$neighbours
# queen_lines <- listw2lines(queen_listw, coords = st_centroid(data_alt$geometry))
# 
# ggplot(data = data_alt) +
#   geom_sf(color = "black") +
#   theme_light() +
#   geom_sf(data = queen_lines, color = "red", size = 0.8)

coords <- st_coordinates(st_centroid(data_alt$geometry))
dist <- nbdists(queen_nb, coords, longlat = TRUE)
idw <- lapply(dist, function(x) 1/(x))
# idw_squared <- lapply(dist, function(x) 1/(x^2))
region_names <- attr(queen_nb, "region.id")
idw_named <- setNames(idw, region_names)

# Number of regions
num_regions <- length(region_names)

# Initialize an empty matrix
idw_matrix_B <- matrix(0, nrow = num_regions, ncol = num_regions)
rownames(idw_matrix_B) <- region_names
colnames(idw_matrix_B) <- region_names

# Fill the matrix with idw values
for (i in seq_along(idw_named)) {
  region <- names(idw_named)[i]
  neighbors <- queen_nb[[i]]
  idw_values <- idw_named[[i]]
  
  # Assign the idw values to the corresponding positions in the matrix
  idw_matrix_B[region, region_names[neighbors]] <- idw_values
}

#SAVING
saveRDS(idw_matrix_A, file = "03_final-input/idw_matrix_A.rds")
saveRDS(idw_matrix_B, file = "03_final-input/idw_matrix_B.rds")