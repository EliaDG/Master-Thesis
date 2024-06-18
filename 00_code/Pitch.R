setwd("C:/Users/eliad/Desktop/Edu/WU Wien/Thesis/Master-Thesis/plots")

library(pacman)
p_load(
  ggplot2,
  maps,
  rnaturalearth,
  rnaturalearthdata,
  sf,
  sp,
  dplyr,
  tidyr,
  grid,
  cowplot,
  gridExtra,
  eurostat,
  spdep)

# Get world map data
world <- ne_countries(scale = "large", returnclass = "sf") %>% 
  select(10,11,95,169) %>% 
  st_transform(., crs = "EPSG:4326") %>% 
  st_as_sf(., wkt = "geometry") %>% 
  mutate(subregion = as.factor(subregion))

# Filter candidate countries and create a numbering column
candidates <- world %>% 
  filter(admin %in% c("Republic of Serbia", 
                      "Bosnia and Herzegovina",
                      "Montenegro", "Kosovo",
                      "North Macedonia", 
                      "Albania", "Turkey", "Georgia",
                      "Ukraine", "Moldova")) %>% 
  mutate(subregion = as.factor(subregion),
         number = as.factor(as.integer(factor(admin, levels = c("Republic of Serbia", "Bosnia and Herzegovina", "Montenegro", "Kosovo", "North Macedonia", "Albania", 
                                                                "Turkey", "Georgia",
                                                                "Ukraine", "Moldova")))),
         centroid = st_centroid(.$geometry),
         X = st_coordinates(centroid)[,1],
         Y = st_coordinates(centroid)[,2])

glimpse(candidates)

# Memory
# zipfile <- "C:/Users/eliad/Desktop/Edu/WU Wien/Thesis/Master-Thesis/plots/plot/gadm41_MDA_shp.zip"
# outdir <- "C:/Users/eliad/Desktop/Edu/WU Wien/Thesis/Master-Thesis/plots/plot"
# unzip(zipfile, exdir = outdir)

BIH <- st_read(dsn = "./plot/gadm41_BIH_0.shp") %>%
  rename(NAME_1 = COUNTRY)
GEO <- st_read(dsn ="./plot/gadm41_GEO_1.shp") %>% 
  select(2,4,12)

# Read the shapefile and preprocess it
UKR <- st_read(dsn ="./plot/gadm41_UKR_1.shp") %>%
  select(2, 4, 12) %>% 
  filter(!NAME_1 %in% c("Crimea", "Sevastopol'"))

# Filter the polygons to be merged
kiev_city <- UKR %>% filter(NAME_1 == "Kiev City")
unknown_region <- UKR %>% filter(NAME_1 == "?")

# Merge the selected polygons using st_union
merged_polygon <- st_union(kiev_city$geometry, unknown_region$geometry, by_feature = TRUE, is_coverage = TRUE)
UKR <- UKR %>% filter(!NAME_1 %in% c("Kiev City", "?"))
merged_sf <- st_sf(GID_0 = "UKR", NAME_1 = "Kiev City", geometry = st_sfc(merged_polygon), crs = st_crs(UKR))
UKR <- bind_rows(UKR, merged_sf)

MDA <- st_read(dsn ="./plot/gadm41_MDA_0.shp") %>%
  rename(NAME_1 = COUNTRY)
XKO <- st_read(dsn ="./plot/gadm41_XKO_0.shp") %>% 
  rename(NAME_1 = COUNTRY)

NUTS2 <- get_eurostat_geospatial(
  resolution = "01",
  nuts_level = 2,
  year = 2021) %>%
  select(1, 4:5, 12) %>%
  rename(GID_0 = CNTR_CODE,
         NAME_1 = NAME_LATN)

extra <- NUTS2 %>% 
  filter(GID_0 %in% c("RS", "ME", "MK", "TR", "AL", "HR")) %>%
  select(2:4) %>% 
  rbind(., UKR, GEO, MDA, BIH, XKO)

jesus <- NUTS2 %>%
  filter(!GID_0 %in% c("RS", "ME", "MK", "TR", "AL", "NO", "CH", "IS", "HR", "LI"),
         !id %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRZZ", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")) %>%
  filter(!NAME_1 %in% c("Ciudad Autónoma de Melilla", "Ciudad Autónoma de Ceuta")) %>% 
  select(2:4)

glimpse(extra)
glimpse(jesus)

# Plot the map
plot <- ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(data = jesus, aes(fill = "Crespo Cuaresma et al. (2014)"), color = "black") +
  geom_sf(data = extra, aes(fill = "Extension"), color = "black") +
  geom_text(data = candidates, aes(x = X, y = Y, label = number), color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(name = "Group:", values = c("Crespo Cuaresma et al. (2014)" = "chartreuse", "Extension" = "salmon")) +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        title = element_text(face = "bold", size = 12),
        legend.position = c(0.83, 0.85),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key = element_rect(fill = "white", color = "black")) +
  labs(x = NULL, y = NULL,
       title = "Area of Research",
       subtitle = "Regional Level",
       caption = "Source: Eurostat, GADM") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70)) +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic")

legend_data <- candidates %>%
  select(admin, number) %>%
  distinct()

plot_1 <- plot + 
  geom_label(data = legend_data, aes(x = 47, y = 63 - as.numeric(as.factor(number)) * 1.15, 
                                     label = paste(admin, "-", number)), 
             size = 4, fill = "white", color = "black", hjust = 1) +
  annotate("text", x = 47, y = 63, label = "EU Candidates:", size = 5, fontface = "bold", hjust = 1)

# regions <- rbind(extra, jesus) %>%
#   mutate(subregion = case_when(
#     GID_0 %in% c("IE", "UK", "SE", "FI", "DK", "LV", "LT", "EE") ~ "Northern Europe",
#     GID_0 %in% c("DE", "FR", "ES", "PT", "IT", "BE", "NL", "LU", "MT", "AT") ~ "Western Europe",
#     GID_0 %in% c("BIH", "RS", "ME", "MK", "AL", "SI", "HR", "BG", "RO", "XKO") ~ "Balkans",
#     GID_0 %in% c("PL", "CZ", "SK", "HU", "UKR", "MDA") ~ "Central-Eastern Europe",
#     GID_0 %in% c("GEO", "CY", "TR", "EL") ~ "Southern Europe",
#     TRUE ~ "Other"),
#     subregion = as.factor(subregion),
#     centroid = st_centroid(.$geometry)) %>% 
#   st_make_valid() %>%
#   st_transform(crs = st_crs(4326)) %>% 
#   arrange(GID_0)

regions <- rbind(extra, jesus) %>%
  mutate(subregion = case_when(
    GID_0 %in% c("IE", "UK", "SE", "FI","EL", "DK", "DE", "FR", "ES", "PT", "IT", "BE", "NL", "LU", "MT", "AT") ~ "Western Europe",
    GID_0 %in% c("BIH", "RS", "ME", "MK", "AL", "UKR", "MDA", "XKO", "GEO", "TR") ~ "EU Candidates",
    GID_0 %in% c("PL", "CZ", "SK", "HU", "LV", "LT", "EE", "HR", "BG","SI", "RO", "CY") ~ "Central-Eastern Europe",
    TRUE ~ "Other"),
    subregion = as.factor(subregion),
    centroid = st_centroid(.$geometry)) %>% 
  st_make_valid() %>%
  st_transform(crs = st_crs(4326)) %>% 
  arrange(GID_0)


glimpse(regions)

plot_2 <- ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(data = regions, aes(fill = subregion), color = "black") +
  scale_fill_manual(values = c("Central-Eastern Europe" = "#fc8d62", 
                               "Western Europe" = "#66c2a5", 
                               "EU Candidates" = "#998ec3"), 
                    name = "Macroregions:") +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        title = element_text(face = "bold", size = 14),
        legend.position = c(0.85, 0.65),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  labs(x = NULL, y = NULL,
       title = "Macro-regions of Europe",
       caption = "Source: Eurostat, GADM") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70)) +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic")

## Queen
# mynb <- function(sp.sample){
# 
#   queen_nb <- poly2nb(sp.sample, row.names=sp.sample$NAME_1, queen=TRUE)
# 
#   count = card(queen_nb)
#   if(!any(count==0)){
#     return(queen_nb)
#   }
# 
#   nnbs = knearneigh(sp.sample$centroid)$nn
# 
#   no_edges_from = which(count==0)
#   for(i in no_edges_from){
#     queen_nb[[i]] = nnbs[i]
#   }
#   names(queen_nb) <- sp.sample$NAME_1
#   return(queen_nb)
# }
# 
# queen_nb <- mynb(regions)
# queen_lines <- nb2lines(mynb(regions), coords = regions$centroid)
queen_nb <- poly2nb(regions, row.names=regions$NAME_1, queen=TRUE)
W.list.queen <- nb2listw(queen_nb, style = "B", zero.policy=TRUE)
W.queen <- listw2mat(W.list.queen)
colnames(W.queen) <- regions$NAME_1

update_w_queen <- function(W.queen, region_pairs, value_pairs) {
  # Ensure the lengths of region_pairs and value_pairs match
  if (length(region_pairs) != length(value_pairs)) {
    stop("The lengths of region_pairs and value_pairs must match.")
  }
  
  for (i in seq_along(region_pairs)) {
    region1 <- region_pairs[[i]][1]
    region2 <- region_pairs[[i]][2]
    value <- value_pairs[i]
    
    row_index1 <- which(rownames(W.queen) == region1)
    col_index1 <- which(colnames(W.queen) == region2)
    row_index2 <- which(rownames(W.queen) == region2)
    col_index2 <- which(colnames(W.queen) == region1)
    
    if (length(row_index1) > 0 && length(col_index1) > 0 && length(row_index2) > 0 && length(col_index2) > 0) {
      W.queen[row_index1, col_index1] <- value
      W.queen[row_index2, col_index2] <- value
    } else {
      warning(paste("One or both regions not found in the matrix:", region1, region2))
    }
  }
  
  return(W.queen)
}
region_pairs <- list(c("Sicilia", "Calabria"),
                     c("Sardegna", "Lazio"),
                     c("Corse", "Provence-Alpes-Côte d’Azur"),
                     c("Illes Balears", "Cataluña"),
                     c("Kýpros", "Adana, Mersin"),
                     c("Bosnia and Herzegovina", "Crna Gora"),
                     c("Bosnia and Herzegovina", "Region Šumadije i Zapadne Srbije"),
                     c("Bosnia and Herzegovina", "Autonomous Province of Vojvodina"),
                     c("Bosnia and Herzegovina", "Jadranska Hrvatska"),
                     c("Bosnia and Herzegovina", "Panonska Hrvatska"),
                     c("Peloponnisos", "Kriti"),
                     c("Notio Aigaio", "Attiki"),
                     c("Sterea Elláda", "Voreio Aigaio"),
                     c("Samtskhe-Javakheti", "Ağrı, Kars, Iğdır, Ardahan"),
                     c("Ajaria", "Trabzon, Ordu, Giresun, Rize, Artvin, Gümüşhane"),
                     c("Ajaria", "Ağrı, Kars, Iğdır, Ardahan"),
                     c("Kosovo", "Severna Makedonija"),
                     c("Kosovo", "Crna Gora"),
                     c("Kosovo", "Region Šumadije i Zapadne Srbije"),
                     c("Kosovo", "Region Južne i Istočne Srbije"),
                     c("Moldova", "Odessa"),
                     c("Moldova", "Sud-Est"),
                     c("Moldova", "Nord-Est"),
                     c("Nord-Est", "Chernivtsi"),
                     c("Odessa", "Sud-Est"),
                     c("Nord-Vest", "Ivano-Frankivs'k"),
                     c("Nord-Vest", "Zakarpattia"),
                     c("Východné Slovensko", "Zakarpattia"),
                     c("Észak-Alföld", "Zakarpattia"),
                     c("Lubelskie", "L'viv"),
                     c("Podkarpackie", "L'viv"),
                     c("Podkarpackie", "Zakarpattia"),
                     c("Lubelskie", "Volyn"),
                     c("Sjælland", "Syddanmark"),
                     c("Hovedstaden", "Sydsverige"),
                     c("Åland", "Etelä-Suomi"),
                     c("Kosovo", "Veri"),
                     c("Malta", "Sicilia"))
value_pairs <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
new <- update_w_queen(W.queen, region_pairs, value_pairs)
W_list_2 <- mat2listw(new, zero.policy = NULL)
queen_lines <- listw2lines(W_list_2, coords = regions$centroid)

plot_3 <- ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(data = regions, fill = "blue", color = "black") +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        title = element_text(face = "bold", size = 14),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  labs(x = NULL, y = NULL,
       title = "Queen Contiguity Spatial Connections",
       caption = "Source: Eurostat, GADM") +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  geom_sf(data = queen_lines, color = "yellow", size = 0.8) +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70))

# #zoom:
ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(data = regions, aes(fill = GID_0), color = "black") +
  geom_sf(data = queen_lines, color = "red", size = 0.8) +
  coord_sf(xlim = c(-0, 25), ylim = c(50, 65))

# # SAVING
# ggsave("study_area.png", plot = plot_1, device = "png", width = 12.8, height = 9.06)
# ggsave("macroregions.png", plot = plot_2, device = "png", width = 12.8, height = 9.06)
# ggsave("queen.png", plot = plot_3, device = "png", width = 12.8, height = 9.06)
