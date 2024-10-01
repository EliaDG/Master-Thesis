getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

geom <- readRDS("03_final-input/geometries.rds")
N1 <- readRDS("03_final-input/idw.rds")
N2 <- readRDS("03_final-input/idw2.rds")
K5 <- readRDS("03_final-input/idw5.rds")

#Loading Data
geom <- readRDS("03_final-input/geometries.rds")
data <- read_csv("03_final-input/dataset_amelia.csv") %>% 
  select(NUTS, Name, Year, Pop_edu_3, GDP_capita) %>% 
  filter(Year %in% c(2009:2019))
load("04_final-output/Models-annual.RData")

World <- ne_countries(scale = "large", returnclass = "sf") %>% 
  select(10,11,95,169) %>% 
  st_transform(., crs = "EPSG:4326") %>% 
  st_as_sf(., wkt = "geometry") %>% 
  mutate(subregion = as.factor(subregion))

Extra <- World %>% 
  filter(admin %in% c("Republic of Serbia", 
                      "Bosnia and Herzegovina",
                      "Montenegro", "Kosovo",
                      "North Macedonia", 
                      "Albania", "Turkey",
                      "Moldova", "Ukraine", "Georgia")) %>% 
  mutate(number = as.factor(as.integer(factor(admin, levels = c("Republic of Serbia", "Bosnia and Herzegovina", "Montenegro", "Kosovo",
                                                                "North Macedonia", "Albania", "Turkey", "Moldova", "Ukraine", "Georgia")))),
         centroid = st_centroid(.$geometry),
         X = st_coordinates(centroid)[,1],
         Y = st_coordinates(centroid)[,2])

EU <- World %>% 
  filter(admin %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                      "Czech Republic", "Denmark", "Estonia", "Finland", 
                      "France", "Germany", "Greece", "Hungary", "Ireland", 
                      "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
                      "Netherlands", "Poland", "Portugal", "Romania", 
                      "Slovakia", "Slovenia", "Spain", "Sweden", "Aland", "United Kingdom"))

plot <- ggplot(data = World) +
  geom_sf(color = "black") +
  geom_sf(data = EU, aes(fill = "European Union"), color = "black") +
  geom_sf(data = Extra, aes(fill = "Candidates"), color = "black") +
  geom_text(data = Extra, aes(x = X, y = Y, label = number), color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(name = "Group:", values = c("European Union" = "#2ca25f", "Candidates" = "salmon")) +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key = element_rect(fill = "white", color = "black")) +
  labs(x = NULL, y = NULL,
       title = "Area of Research",
       caption = "Source: Eurostat, GADM") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70)) +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic")

legend_data <- Extra %>%
  select(admin, number) %>%
  distinct()

plot_1 <- plot + 
  geom_label(data = legend_data, aes(x = 47, y = 63 - as.numeric(as.factor(number)) * 1.15, 
                                     label = paste(admin, "-", number)), 
             size = 4, fill = "white", color = "black", hjust = 1) +
  annotate("text", x = 47, y = 63, label = "Candidates:", size = 5, fontface = "bold", hjust = 1)


# zipfile <- "01_data-input/Shapefiles/gadm41_MDA_shp.zip"
# outdir <- "01_data-input/Shapefiles"
# unzip(zipfile, exdir = outdir)
# 
# BIH <- st_read(dsn = "01_data-input/Shapefiles/gadm41_BIH_0.shp") %>%
#   rename(NAME_1 = COUNTRY)
# MDA <- st_read(dsn ="01_data-input/Shapefiles/gadm41_MDA_0.shp") %>%
#   rename(NAME_1 = COUNTRY)
# XKO <- st_read(dsn ="01_data-input/Shapefiles/gadm41_XKO_0.shp") %>% 
#   rename(NAME_1 = COUNTRY)


regions <- geom %>%
  mutate(Country = as.character(sapply(NUTS, mapping_nuts)),
         subregion = case_when(
           Country %in% c("Austria", "Belgium","Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Luxembourg", "Malta", "Netherlands", "Portugal","Spain", "Sweden", "United Kingdom") ~ "Western Europe",
           Country %in% c("Bosnia and Herzegovina", "Serbia", "North Macedonia", "Montenegro", "Albania", "Moldova", "Kosovo", "Turkey") ~ "EU Candidates",
           Country %in% c("Poland", "Czech Republic", "Slovakia","Slovenia","Hungary", "Lithuania", "Latvia", "Estonia", "Croatia", "Bulgaria", "Romania", "Cyprus") ~ "Central-Eastern Europe",
           TRUE ~ "Other"),
         subregion = as.factor(subregion),
         centroid = st_centroid(.$geometry)) %>% 
  st_make_valid() %>%
  st_transform(crs = st_crs(4326)) %>% 
  arrange(NUTS)
levels(regions$subregion)
table(regions$subregion)

plot_2 <- ggplot(data = World) +
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
       title = "NUTS-2 of Europe",
       subtitle = "Area of Reseach",
       caption = "Source: Eurostat, GADM") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70)) +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic")

## Queen
queen_lines_N1 <- listw2lines(N1, coords = st_centroid(geom$geometry))
queen_lines_N2 <- listw2lines(N2, coords = st_centroid(geom$geometry))
queen_lines_K5 <- listw2lines(K5, coords = st_centroid(geom$geometry))

firstq <- ggplot(data = World) +
  geom_sf(color = "black") +
  geom_sf(data = regions, fill = "#b8e186", color = "black") +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  labs(x = NULL, y = NULL,
       title = "Fist Order Queen Contiguity") +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  geom_sf(data = queen_lines_N1, color = "#d01c8b", size = 0.8) +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70))

secondq <- ggplot(data = World) +
  geom_sf(color = "black") +
  geom_sf(data = regions, fill = "#b8e186", color = "black") +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  labs(x = NULL, y = NULL,
       title = "Second Order Queen Contiguity") +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  geom_sf(data = queen_lines_N2, color = "#7b3294", size = 0.8) +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70))

knn <- ggplot(data = World) +
  geom_sf(color = "black") +
  geom_sf(data = regions, fill = "#b8e186", color = "black") +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  labs(x = NULL, y = NULL,
       title = "K Neighbourghs") +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  geom_sf(data = queen_lines_K5, color = "#d7191c", size = 0.8) +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70))

plot_3 <- (firstq | secondq | knn) +
  plot_annotation(
    title = "Comparison of Spatial Connections",
    caption = "Source: Eurostat, GADM",
    theme = theme(plot.title = element_text(face = "bold")))

# DISTRIBUTION COEFF -------
posterior_means <- coef(mfls_base3, exact = TRUE)
var_mean <- posterior_means["Pop_edu_3",2]
var_values <- data$Pop_edu_3
variable <- var_mean * var_values %>% 
  as.data.frame()

data.edu <- cbind(data, variable) %>% 
  full_join(geom, by = "NUTS") %>% 
  rename(pred = ".") %>%
  group_by(NUTS) %>%
  mutate(pred_average = mean(pred)) %>%
  ungroup() %>%
  filter(Year == 2019) %>%
  st_as_sf() %>% 
  mutate(pred_bin = cut(pred_average, breaks = 5, labels = FALSE))

breaks_seq <- seq(min(data.edu$pred_average, na.rm = TRUE), max(data.edu$pred_average, na.rm = TRUE), length.out = 6)
labels_seq <- sprintf("%.3f to %.3f", breaks_seq[-length(breaks_seq)], breaks_seq[-1])

education <- ggplot(data = World) +
  geom_sf(color = "black") +
  geom_sf(data = data.edu, aes(geometry = geometry, fill = factor(pred_bin)), color = "black") +
  scale_fill_viridis_d(option = "plasma", direction = -1, 
                       name = "Average estimated effect:",
                       labels = labels_seq,
                       na.value = "grey50",
                       guide = guide_legend(direction = "vertical")) +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "bottom",
        legend.text = element_text(size = 11)) +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70)) +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Human Capital")

posterior_means <- coef(mfls_base3, exact = TRUE)
var_mean <- posterior_means["GDP_capita",2]
var_values <- data$GDP_capita
variable <- var_mean * var_values %>% 
  as.data.frame()

data.gdp <- cbind(data, variable) %>% 
  full_join(geom, by = "NUTS") %>% 
  rename(pred = ".") %>%
  group_by(NUTS) %>%
  mutate(pred_average = mean(pred)) %>%
  ungroup() %>%
  filter(Year == 2019) %>%
  st_as_sf() %>% 
  mutate(pred_bin = cut(pred_average, breaks = 5, labels = FALSE))

breaks_seq <- seq(min(data.gdp$pred_average, na.rm = TRUE), max(data.gdp$pred_average, na.rm = TRUE), length.out = 6)
labels_seq <- sprintf("%.3f to %.3f", breaks_seq[-length(breaks_seq)], breaks_seq[-1])

convergence  <- ggplot(data = World) +
  geom_sf(color = "black") +
  geom_sf(data = data.gdp, aes(geometry = geometry, fill = factor(pred_bin)), color = "black") +
  scale_fill_viridis_d(option = "plasma", direction = -1, 
                       name = "Average estimated effect:",
                       labels = labels_seq,
                       na.value = "grey50",
                       guide = guide_legend(direction = "vertical")) +
  theme_light() +
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "bottom",
        legend.text = element_text(size = 11)) +
  annotate("text", x = 18, y = 35, label = "Mediterranean Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 34, y = 43.5, label = "Black Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = -8, y = 45.5, label = "Atlantic Ocean", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 0, y = 65, label = "North Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  annotate("text", x = 18, y = 55.75, label = "Baltic Sea", color = "blue", size = 3, angle = 0, fontface = "italic") +
  coord_sf(xlim = c(-10, 48), ylim = c(35, 70)) +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Initial Income",)

plot_4 <- (convergence |education) +
  plot_annotation(
    title = "Spatial Distribution PMs",
    theme = theme(plot.title = element_text(face = "bold")))


# SAVING
ggsave("05_pictures/country.png", plot = plot_1, device = "png", width = 12.8, height = 9.06)
ggsave("05_pictures/nuts2.png", plot = plot_2, device = "png", width = 12.8, height = 9.06)
ggsave("05_pictures/network.png", plot = plot_3, device = "png", width = 14, height = 10)
ggsave("05_pictures/coeff.png", plot = plot_4, device = "png", width = 14, height = 10)
