getwd()
set.seed(1105)

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

# Given that original shapefiles come from different sources (Eurostat and GADM) not always neighboring regions
# appear as such. I manually set them so, besides connecting regions
# where infrastructure has brought a tangible connection, and connecting islands to at
# least one region.

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
  c("EL62", "EL63")) # Ionia Nisia, Dytiki Ellada

value_pairs <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1)

new_W_matrix <- update_w_queen(queen_matrix, region_pairs, value_pairs)
queen_listw <- mat2listw(new_W_matrix, style = "B", zero.policy = TRUE)
queen_nb <- queen_listw$neighbours

coords <- st_coordinates(st_centroid(geom$geometry))
dist <- nbdists(queen_nb, coords, longlat = TRUE)
idw <- lapply(dist, function(x) 1/(x))
idw_2 <- lapply(dist, function(x) 1/(x^2))

W1 <- nb2listw(queen_nb, glist = idw, zero.policy = TRUE)
W2 <- nb2listw(queen_nb, glist = idw_2, zero.policy = TRUE)
W3 <- nb2listw(queen_nb, style = "B", zero.policy = TRUE)
W4 <- nb2listw(nblag_cumul(nblag(queen_nb, 2)), style = "B", zero.policy = TRUE)

coords <- st_coordinates(st_centroid(geom))
k.near <- knearneigh(coords, k=5)
k5 <- knn2nb(k.near)
W5 <- nb2listw(k5, style = "W", zero.policy = TRUE)


#Spatial Filtering -----
GDP_capita_raw <- read_csv("03_final-input/dataset.csv") %>%
  select(NUTS, Year, GDP_capita)

datas_decade <- GDP_capita_raw %>%
  pivot_wider(names_from = Year,
              values_from = GDP_capita) %>%
  mutate(`2009` = (`2019`-`2009`)/`2009`) %>%
  select(NUTS, `2009`) %>% 
  pivot_longer(cols = -"NUTS",
               names_to = "Year",
               values_to = "GDP_growth") %>% 
  select(GDP_growth) %>% 
  as.data.frame()

y <- as.data.frame(datas_decade[, 1, drop = F])
yFilt1 <- SpatialFiltering(datas_decade[, 1] ~ 1, ~-1, data = y,
                           nb = W1$neighbours, glist = W1$weights, ExactEV = TRUE)
yFilt2 <- SpatialFiltering(datas_decade[, 1] ~ 1, ~-1, data = y,
                           nb = W2$neighbours, glist = W2$weights, ExactEV = TRUE)
yFilt3 <- SpatialFiltering(datas_decade[, 1] ~ 1, ~-1, data = y,
                           nb = W3$neighbours, glist = W3$weights, style = "B", ExactEV = TRUE)
yFilt4 <- SpatialFiltering(datas_decade[, 1] ~ 1, ~-1, data = y,
                           nb = W4$neighbours, glist = W4$weights, style = "B", ExactEV = TRUE)
yFilt5 <- SpatialFiltering(datas_decade[, 1] ~ 1, ~-1, data = y,
                           nb = W5$neighbours, glist = W5$weights, style = "W", ExactEV = TRUE)

WL_decade <- list(Col_A = fitted(yFilt1), Col_B = fitted(yFilt2), Col_C = fitted(yFilt3), Col_D = fitted(yFilt4), Col_E = fitted(yFilt5))
WL_alt <- list(Col_C = fitted(yFilt3), Col_D = fitted(yFilt4))
WL_alt_new <- list(Col_E = fitted(yFilt5), Col_F = fitted(yFilt5))

# Visualization queen continuity network -------
queen_lines <- listw2lines(W1, coords = st_centroid(geom$geometry))
ggplot(data = geom) +
  geom_sf(color = "black") +
  theme_light() +
  geom_sf(data = queen_lines, color = "red", size = 0.8)
#coord_sf(xlim = c(18, 23), ylim = c(43, 48)) to zoom in
queen_lines_k <- listw2lines(W5, coords = st_centroid(geom$geometry))
ggplot(data = geom) +
  geom_sf(color = "black") +
  theme_light() +
  geom_sf(data = queen_lines_k, color = "green", size = 0.8)

#SAVING
saveRDS(WL_decade, file = "03_final-input/WL_10.rds")
saveRDS(WL_alt, file = "03_final-input/WL_10_alt1.rds")
saveRDS(WL_alt_new, file = "03_final-input/WL_10_alt2.rds")
saveRDS(W1, file = "03_final-input/idw.rds")
saveRDS(W4, file = "03_final-input/idw2.rds")
saveRDS(W5, file = "03_final-input/idw5.rds")