getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- readRDS("03_final-input/dataset_amelia.rds")
glimpse(data)
data <- dataset %>% 
  select(NUTS, Name, Country, Year, Subregion, GDP_capita) %>%
  pivot_longer(cols = -c(NUTS, Name, Country, Year, Subregion),
               names_to = "Series",
               values_to = "Values") %>% 
  pivot_wider(names_from = "Year",
              values_from = "Values") %>%
  mutate(GDP_gg = `2019` - `2009`,
         Color_Group = ifelse(Country == "Greece", "Greece", Subregion)) %>%
  select(-c(starts_with("201"))) %>%
  select(-Series)
glimpse(data)

# Perform the OLS regression
beta_con <- lm(GDP_gg ~ `2009`, data = data)
summary(beta_con)

top_gdp_gg <- data %>%
  group_by(Subregion) %>%
  top_n(3, GDP_gg) %>%
  ungroup()

# Find the bottom 3 GDP_gg values specifically for the "EU Candidates" subregion
top_log_2009 <- data %>%
  filter(Subregion == "Central-Eastern Europe") %>% 
  top_n(3, `2009`) %>%
  ungroup() %>% 
  slice(1:3)

# Combine the top 3 from all subregions and bottom 3 from "EU Candidates"
highlighted_gdp_gg <- bind_rows(top_gdp_gg, top_log_2009)

beta <- ggplot(data, aes(x = `2009`, y = GDP_gg)) +
  geom_point(aes(color = Color_Group)) +
  scale_color_manual(values = c("Greece" = "red", 
                                "Central-Eastern Europe" = "#fc8d62", 
                                "Western Europe" = "#66c2a5", 
                                "EU Candidates" = "#998ec3"),
                     name = "Legend:") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text(data = highlighted_gdp_gg, aes(label = NUTS, fontface = "bold"), vjust = 0.2, hjust = -0.2) +
  labs(title = "Beta Convergence (2009-2019)",
       x = "Log of GDP per Capita in 2009",
       y = "Growth Rate of GDP per Capita") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        title = element_text(face = "bold", size = 14),
        legend.position = c(0.20, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12))

sigma_convergence <- dataset %>%
  group_by(Year) %>%
  summarize(sd_gdp = sd(GDP_capita),
            var_gdp = var(GDP_capita))
ggplot(sigma_convergence, aes(x = Year, y = sd_gdp)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Sigma Convergence",
       x = "Year",
       y = "Standard Deviation of GDP per Capita") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(dataset$Year), max(dataset$Year), by = 1))


ggsave("plot_beta.png", plot = beta, device = "png")
