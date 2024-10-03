getwd()
set.seed(1105)

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#Loading Data
data <- read_csv("03_final-input/dataset_amelia.csv") %>% 
  select(NUTS, Name, Year, Pop_edu_3, GDP_capita) %>% 
  #filter(Year %in% c(2009:2019)) %>% 
  filter(Year == 2009)
load("04_final-output/Models-decade.RData")
W1 <- readRDS("03_final-input/idw.rds")
idw1 <- W1
idw1$neighbours <- rep(W1$neighbours, each = 11);
idw1$weights <- rep(W1$weights, each = 11)

attr(idw1$neighbours, "class") <- "nb"
attr(idw1$neighbours, "region.id") <- rep(attr(W1$neighbours, "region.id"), each = 11)
attr(idw1$neighbours, "sym") <- TRUE
attr(idw1$neighbours, "call") <- attr(W1$neighbours, "call")

# PERFORMANCE STATISTICS AND SPAT AUTOCORRELATION --------
model <- dec_spat3
model
pmp.bma(model)[1,]
colSums(pmp.bma(model)[1:25,])
colSums(pmp.bma(model)[1:50,])
fullmodel.ssq(model)
lm_base1 <- lm(model.frame(as.zlm(model), model = 1)); summary(lm_base1)
lm_res_base <- residuals(lm_base1)
moran.test(lm_res_base, idw1)
moran.test(lm_res_base, W1)

# density(model[1:500], reg = "GVA_industry", addons = "mle")

model_spat <- mfls_spat3
model_spat$Wcount
pmpW.bma(model_spat)
mTest = moranTest.bma(object = model_spat, variants = "double",
                       W = idw1, nmodel = 1)
moran_results <- cbind(
  Moran_I = sapply(mTest$moran, function(x) x$estimate[1]),
  SD = sqrt(sapply(mTest$moran, function(x) x$estimate[3])),
  P_value = sapply(mTest$moranEV, function(x) x$p.value)
); moran_results


# BETA CONVERGENCE -------
dataset <- read_csv("03_final-input/dataset.csv") %>%
  select(NUTS, Name, Year, GDP_capita) %>% 
  mutate(GDP_capita = log(GDP_capita))
data <- dataset %>% 
  select(NUTS, Name, Year, GDP_capita) %>%
  pivot_longer(cols = -c(NUTS, Name, Year),
               names_to = "Series",
               values_to = "Values") %>% 
  pivot_wider(names_from = "Year",
              values_from = "Values") %>%
  mutate(
    GDP_gg = `2019`-`2009`,
    ColorMap = case_when(
      str_detect(NUTS, "^BA|^RS|^ME|^MK|^AL|^MD|^XK|^TR") ~ "Candidate",
      str_detect(NUTS, "^PL|^CZ|^SK|^HU|^LT|^LV|^EE|^HR|^BG|^RO|^CY|^SI") ~ "CEE",
      str_detect(NUTS, "^EL") ~ "Greece",
      TRUE ~ "EU"
    )
  ) %>%
  select(-c(starts_with("201"), `2008`, Series))

beta_con <- lm(GDP_gg ~ `2009`, data = data)
summary(beta_con)

top_gdp_gg <- data %>%
  group_by(ColorMap) %>%
  top_n(3, GDP_gg) %>%
  filter(!ColorMap == "Greece") %>% 
  ungroup()

plot_5 <- ggplot(data, aes(x = `2009`, y = GDP_gg)) +
  geom_point(aes(color = ColorMap), size = 3) + 
  scale_color_manual(values = c("Greece" = "red",
                                "EU" = "#66c2a5",
                                "CEE" = "#fc8d62",
                                "Candidate" = "#998ec3"),
                     name = "Legend:") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text(data = top_gdp_gg, aes(label = NUTS, fontface = "bold"), vjust = 0.2, hjust = -0.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 1) +
  labs(x = "Log GDP per Capita in 2009",
       y = "Growth Rate of GDP per Capita",
       caption = "Data source: Eurostat/World Bank") +  # Add caption
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        title = element_text(face = "bold", size = 14),
        legend.position = c(0.15, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14),
        plot.caption = element_text(hjust = 1, face = "italic", size = 12))

# SAVING
ggsave("05_pictures/betaconv.png", plot = plot_5, device = "png", width = 14, height = 10)
