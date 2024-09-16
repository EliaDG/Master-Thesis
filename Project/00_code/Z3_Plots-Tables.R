#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")
geom <- readRDS("03_final-input/geometries.rds")
data <- read_csv("03_final-input/dataset_amelia.csv") %>% 
  select(NUTS, Name, Year, Pop_edu_3, GDP_capita) %>% 
  filter(Year %in% c(2009:2019))

posterior_means <- coef(mfls_base3, exact = TRUE)
tertiary_mean <- posterior_means["GDP_capita",2]
tertiary_values <- data$GDP_capita
tertiary_contribution <- tertiary_mean * tertiary_values %>% 
  as.data.frame()

dataset <- cbind(data, tertiary_contribution) %>% 
  full_join(geom, by = "NUTS") %>% 
  rename(pred = ".") %>%
  group_by(NUTS) %>%
  mutate(pred_average = mean(pred)) %>%
  ungroup() %>%
  filter(Year == 2019) %>% 
  st_as_sf() %>%
  mutate(pred_bin = cut(pred_average, 
                        breaks = 4, 
                        labels = c("Low", "Medium-Low", "Medium-High", "High")))


ggplot(data = dataset) +
  geom_sf(aes(geometry = geometry, fill = pred_bin), color = "black") +
  theme_light() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  scale_fill_viridis_d(na.value = "grey50", name = "Pop_edu_3_bma") 
