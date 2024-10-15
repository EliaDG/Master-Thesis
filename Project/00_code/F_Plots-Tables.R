getwd()
set.seed(1105)

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#Loading Data
load("04_final-output/Models-annual.RData")
load("04_final-output/Models-decade.RData")

dataset <- read_csv("03_final-input/dataset.csv") %>%
  select(NUTS, Name, Year, GDP_capita) %>% 
  mutate(GDP_capita = log(GDP_capita))

W1 <- readRDS("03_final-input/idw.rds")
idw1 <- W1
idw1$neighbours <- rep(W1$neighbours, each = 11);
idw1$weights <- rep(W1$weights, each = 11)

attr(idw1$neighbours, "class") <- "nb"
attr(idw1$neighbours, "region.id") <- rep(attr(W1$neighbours, "region.id"), each = 11)
attr(idw1$neighbours, "sym") <- TRUE
attr(idw1$neighbours, "call") <- attr(W1$neighbours, "call")

# PERFORMANCE STATISTICS AND SPAT AUTOCORRELATION --------
model <- mfls_base1; model
pmp.bma(model)[1,]
colSums(pmp.bma(model)[1:25,])
colSums(pmp.bma(model)[1:50,])
fullmodel.ssq(model)

#Top Model
model <- mfls_spat3
lm_base <- lm(model.frame(as.zlm(model), model = 1)); summary(lm_base)
lm_res_base <- residuals(lm_base)
moran.test(lm_res_base, idw1)
#moran.test(lm_res_base, W1)

model_spat <- mfls_spat1
model_spat$Wcount
pmpW.bma(model_spat)
mTest = moranTest.bma(object = model_spat, variants = "double",
                       W = idw1, nmodel = 1); mTest

#DISTRIBUTION DENSITIES ----
density(mfls_base1[1:500], reg = "GVA_industry", addons = "mle")
density(mfls_base2[1:500], reg = "GVA_industry", addons = "mle")
density(mfls_base3[1:500], reg = "GVA_industry", addons = "mle")

density(mfls_base1[1:500], reg = "CEE#Pop_edu_3", addons = "mle")
density(mfls_base2[1:500], reg = "Pop_edu_3", addons = "mle")
density(mfls_base3[1:500], reg = "CEE#Pop_edu_3", addons = "mle")

# BETA CONVERGENCE -------
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

# COMPARISON SPAT COEFFICIENTS -----
names <- c("Pop_edu_3", "Candidates", "Candidates#Pop_edu_3", "GVA_construction", "Wage_EUR", 
           "GVA_industry", "Candidates#GVA_industry", "Labor_Prodx", "NEET_share", "Capital", 
           "Unemployment_rate", "Output_density", "Pop_edu_2", "Pop_edu_1", 
           "Candidates#GVA_construction", "Population_density", "Employment_rate", "GDP_capita", 
           "Employment_density", "CEE", "GVA_services", "inv_rate", "Activity_rate", 
           "GVA_agriculture", "GVA_public", "CEE#Pop_edu_3", "Migration_rate", "Border", 
           "Life_exp", "Dist_BRUX", "Fertility_rate", "Objective_1", "Island", 
           "CEE#Capital", "Candidates#GVA_services", "Candidates#Capital", 
           "Candidates#GVA_public", "Candidates#GDP_capita", "CEE#GVA_agriculture", 
           "Candidates#GVA_agriculture", "CEE#GVA_construction", "CEE#GVA_industry", 
           "CEE#GVA_services", "CEE#GDP_capita", "CEE#GVA_public")

plot_6 <- plotComp(Base=mfls_spat3, "Only QC" = mfls_spat3_alt1, "Only kNN"=mfls_spat3_alt2, include.legend = F, add.grid= T, varNr=names)
legend("topright", 
       legend = c("Base", "Only QC", "Only kNN"),  # Labels for the legend
       col = c("green", "orange", "purple"),       # Colors matching the series
       pch = c(1, 2, 3),                           # Point characters (1 = circle, 2 = triangle, 3 = plus sign)
       pt.cex = 1.2,                               # Scaling the points if necessary
       bty = "Y")                                  # No box around the legend

plot_7 <- plotComp(Base = mfls_spat3, "Only QC" = mfls_spat3_alt1, "Only kNN" = mfls_spat3_alt2, include.legend = F, comp = "Post Mean", add.grid = TRUE, varNr = names)
legend("bottomright", 
       legend = c("Base", "Only QC", "Only kNN"),
       col = c("green", "orange", "purple"),
       pch = c(1, 2, 3),
       pt.cex = 1.2,
       bty = "Y")

# SAVING
ggsave("05_pictures/betaconv.png", plot = plot_5, device = "png", width = 14, height = 10)
