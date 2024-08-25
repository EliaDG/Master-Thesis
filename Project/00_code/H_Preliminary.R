getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")
dataset_mice <- read_csv("03_final-input/dataset_mice.csv")
geom <- readRDS("03_final-input/geometries.rds")
W <- readRDS("03_final-input/idw_listw.rds")

# Convergence Analysis - Amelia ------
data <- dataset_amelia %>% 
  select(NUTS, Name, Year, GDP_capita) %>%
  pivot_longer(cols = -c(NUTS, Name, Year),
               names_to = "Series",
               values_to = "Values") %>% 
  pivot_wider(names_from = "Year",
              values_from = "Values") %>%
  mutate(
    GDP_gg = `2019` - `2009`,
    ColorMap = case_when(
      str_detect(NUTS, "^BA|^RS|^ME|^MK|^AL|^MD|^XK|^TR") ~ "EU Candidates",
      str_detect(NUTS, "^PL|^CZ|^SK|^HU|^LT|^LV|^EE|^HR|^BG|^RO|^CY") ~ "Central-Eastern Europe",
      str_detect(NUTS, "^EL") ~ "Greece",
      TRUE ~ "Western Europe"
    )
  ) %>%
  select(-c(starts_with("201"))) %>%
  select(-Series)

beta_con <- lm(GDP_gg ~ `2009`, data = data)
summary(beta_con)

top_gdp_gg <- data %>%
  group_by(ColorMap) %>%
  top_n(3, GDP_gg) %>%
  filter(!ColorMap == "Greece") %>% 
  ungroup()

top_log_2009 <- data %>%
  filter(ColorMap == "Central-Eastern Europe") %>% 
  top_n(3, `2009`) %>%
  ungroup() %>% 
  slice(1:3)

highlighted_gdp_gg <- bind_rows(top_gdp_gg, top_log_2009)

beta <- ggplot(data, aes(x = `2009`, y = GDP_gg)) +
  geom_point(aes(color = ColorMap), size = 4) +  # Increase point size
  scale_color_manual(values = c("Greece" = "red", 
                                "Central-Eastern Europe" = "#fc8d62", 
                                "Western Europe" = "#66c2a5", 
                                "EU Candidates" = "#998ec3"),
                     name = "Legend:") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text(data = highlighted_gdp_gg, aes(label = NUTS, fontface = "bold"), vjust = 0.2, hjust = -0.2) +
  labs(x = "Log of GDP per Capita in 2009",
       y = "Growth Rate of GDP per Capita") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        title = element_text(face = "bold", size = 14),
        legend.position = c(0.15, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14))

# sigma_convergence <- dataset_amelia %>%
#   group_by(Year) %>%
#   summarize(sd_gdp = sd(GDP_capita),
#             var_gdp = var(GDP_capita))
# ggplot(sigma_convergence, aes(x = Year, y = sd_gdp)) +
#   geom_line(color = "blue") +
#   geom_point(color = "red") +
#   labs(title = "Sigma Convergence",
#        x = "Year",
#        y = "Standard Deviation of GDP per Capita") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(dataset$Year), max(dataset$Year), by = 1))


# Spatial Autocorrelation --------
### GLOBAL Moran's I ###
dataset_moran <- dataset_amelia %>% 
  filter(Year == 2009)
moran.test(dataset_moran$GDP_growth, listw = W, alternative = "greater", 
           randomisation = FALSE)
moran.mc(dataset_moran$GDP_growth, listw = W, alternative = "greater", nsim = 100)


# SAVING
# ggsave("plot_beta.png", plot = beta, device = "png")