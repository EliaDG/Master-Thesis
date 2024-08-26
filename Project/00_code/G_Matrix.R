getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
geom <- readRDS("03_final-input/geometries.rds")

# Spatial Matrix----
queen_nb <- poly2nb(geom, row.names=geom$NUTS, queen=TRUE)
queen_listw <- nb2listw(queen_nb, style = "B", zero.policy=TRUE)
queen_matrix <- listw2mat(queen_listw)
colnames(queen_matrix) <- geom$NUTS

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
  c("MT00", "ITG1"), # Malta, Sicilia
  c("UKJ4", "FRE1"), # Kent, Nord-Pas de Calais
  c("FI1B", "EE00"), # Helsinki-Uusimaa, Eesti
  c("UKN0", "UKM7"), # Northern Ireland, Southern Scotland
  c("IE06", "UKD7"), # Eastern and Midland, Merseyside
  c("EL62", "EL63")) # Forgotten islands of Greece

value_pairs <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1)

new_W_matrix <- update_w_queen(queen_matrix, region_pairs, value_pairs)
queen_listw <- mat2listw(new_W_matrix, style = "B", row.names = geom$NUTS, zero.policy = TRUE)
queen_nb <- queen_listw$neighbours
# attr(queen_nb, "region.id") <- row.names(geom)

coords <- st_coordinates(st_centroid(geom$geometry))
dist <- nbdists(queen_nb, coords, longlat = TRUE)
idw <- lapply(dist, function(x) 1/(x))
idw_2 <- lapply(dist, function(x) 1/(x^2))
# region_names <- attr(queen_nb, "region.id")
# 
# idw_named <- setNames(idw, region_names)
# idw_2_named <- setNames(idw_2, region_names)
#
# idw_listw <- nb2listw(queen_nb, glist = idw_named, style = "B", zero.policy = TRUE)
# idw_2_listw <- nb2listw(queen_nb, glist = idw_2_named, style = "B", zero.policy = TRUE)

# # Turn list into matrix -----
# # Number of regions
# num_regions <- length(region_names)
# 
# # Initialize an empty matrix
# idw_matrix <- matrix(0, nrow = num_regions, ncol = num_regions)
# rownames(idw_matrix) <- region_names
# colnames(idw_matrix) <- region_names
# 
# # Fill the matrix with idw values
# for (i in seq_along(idw_named)) {
#   region <- names(idw_named)[i]
#   neighbors <- queen_nb[[i]]
#   idw_values <- idw_named[[i]]
# 
#   # Assign the idw values to the corresponding positions in the matrix
#   idw_matrix[region, region_names[neighbors]] <- idw_values
# }
# 
# # Initialize an empty matrix
# idw_2_matrix <- matrix(0, nrow = num_regions, ncol = num_regions)
# rownames(idw_2_matrix) <- region_names
# colnames(idw_2_matrix) <- region_names
# 
# # Fill the matrix with idw values
# for (i in seq_along(idw_named)) {
#   region <- names(idw_2_named)[i]
#   neighbors <- queen_nb[[i]]
#   idw_2_values <- idw_2_named[[i]]
# 
#   # Assign the idw values to the corresponding positions in the matrix
#   idw_2_matrix[region, region_names[neighbors]] <- idw_2_values
# }
# 
# idw_listw <- mat2listw(idw_matrix, row.names = geom$NUTS, zero.policy = TRUE)
# idw_2_listw <- mat2listw(idw_2_matrix, row.names = geom$NUTS, zero.policy = TRUE)

#SAVING
# saveRDS(idw_matrix, file = "03_final-input/idw_matrix.rds")
# saveRDS(idw_2_matrix, file = "03_final-input/idw_2_matrix.rds")
saveRDS(idw, file = "03_final-input/idw_list.rds")
saveRDS(idw_2, file = "03_final-input/idw_2_list.rds")

# Appendix
queen_lines <- listw2lines(queen_listw, coords = st_centroid(geom$geometry))

ggplot(data = geom) +
  geom_sf(color = "black") +
  theme_light() +
  geom_sf(data = queen_lines, color = "red", size = 0.8)
  #coord_sf(xlim = c(18, 23), ylim = c(43, 48))
