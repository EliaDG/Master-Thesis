getwd()
set.seed(1105)

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#Loading Data
geom <- readRDS("03_final-input/geometries.rds")
data <- read_csv("03_final-input/dataset_amelia.csv") %>% 
  select(NUTS, Name, Year, Pop_edu_3, GDP_capita) %>% 
  filter(Year %in% c(2009:2019))

# DISTRIBUTION COEFF -------
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

# PERFORMANCE STATISTICS AND SPAT AUTOCORRELATION --------
pmp.bma(mfls_base1)[1,]
colSums(pmp.bma(mfls_base1)[1:25,])
colSums(pmp.bma(mfls_base1)[1:50,])
fullmodel.ssq(mfls_base1)
lm_base1 <- lm(model.frame(as.zlm(mfls_base1), model = 1))
lm_res_base1 <- residuals(lm_base1)
moran.test(lm_res_base1, idw1)

# density(mfls_base1[1:500], reg = "GVA_industry", addons = "mle")
# density(mfls_base2[1:500], reg = "GVA_industry", addons = "mle")
# density(mfls_base3[1:500], reg = "GVA_industry", addons = "mle")

pmp.bma(mfls_base2)[1,]
colSums(pmp.bma(mfls_base2)[1:25,])
colSums(pmp.bma(mfls_base2)[1:50,])
fullmodel.ssq(mfls_base2)
lm_base2 <- lm(model.frame(as.zlm(mfls_base2), model = 1))
lm_res_base2 <- residuals(lm_base2)
moran.test(lm_res_base2, idw1)

pmp.bma(mfls_base3)[1,]
colSums(pmp.bma(mfls_base3)[1:25,])
colSums(pmp.bma(mfls_base3)[1:50,])
fullmodel.ssq(mfls_base3)
lm_base3 <- lm(model.frame(as.zlm(mfls_base3), model = 1))
lm_res_base3 <- residuals(lm_base3)
moran.test(lm_res_base3, idw1)

pmp.bma(mfls_fix1)[1,]
colSums(pmp.bma(mfls_fix1)[1:25,])
colSums(pmp.bma(mfls_fix1)[1:50,])
fullmodel.ssq(mfls_fix1)
lm_fix1 <- lm(model.frame(as.zlm(mfls_fix1), model = 1))
lm_res_fix1 <- residuals(lm_fix1)
moran.test(lm_res_fix1, idw1)

pmp.bma(mfls_fix2)[1,]
colSums(pmp.bma(mfls_fix2)[1:25,])
colSums(pmp.bma(mfls_fix2)[1:50,])
fullmodel.ssq(mfls_fix2)
lm_fix2 <- lm(model.frame(as.zlm(mfls_fix2), model = 1))
lm_res_fix2 <- residuals(lm_fix2)
moran.test(lm_res_fix2, idw1)

# density(mfls_fix1[1:500], reg = "GDP_capita", addons = "mle")
# density(mfls_fix2[1:500], reg = "GDP_capita", addons = "mle")

pmp.bma(mfls_spat1)[1,]
colSums(pmp.bma(mfls_spat1)[1:25,])
colSums(pmp.bma(mfls_spat1)[1:50,])
fullmodel.ssq(mfls_spat1)
lm_spat1 <- lm(model.frame(as.zlm(mfls_spat1), model = 1))
lm_res_spat1 <- residuals(lm_spat1)
moran.test(lm_res_spat1, idw1)

mfls_spat1$Wcount
pmpW.bma(mfls_spat1)
mTest1 = moranTest.bma(object = mfls_spat1, variants = "double",
                       W = idw1, nmodel = 1)
moran_results1 <- cbind(
  Moran_I = sapply(mTest1$moran, function(x) x$estimate[1]),
  SD = sqrt(sapply(mTest1$moran, function(x) x$estimate[3])),
  P_value = sapply(mTest1$moranEV, function(x) x$p.value)
)


pmp.bma(mfls_spat2)[1,]
colSums(pmp.bma(mfls_spat2)[1:25,])
colSums(pmp.bma(mfls_spat2)[1:50,])
fullmodel.ssq(mfls_spat2)
lm_spat2 <- lm(model.frame(as.zlm(mfls_spat2), model = 1))
lm_res_spat2 <- residuals(lm_spat2)
moran.test(lm_res_spat2, idw1)

mfls_spat2$Wcount
pmpW.bma(mfls_spat2)
mTest2 = moranTest.bma(object = mfls_spat2, variants = "double", 
                       W = idw1, nmodel = 1)
moran_results2 <- cbind(
  Moran_I = sapply(mTest2$moran, function(x) x$estimate[1]),
  SD = sqrt(sapply(mTest2$moran, function(x) x$estimate[3])),
  P_value = sapply(mTest2$moranEV, function(x) x$p.value)
)

pmp.bma(mfls_spat3)[1,]
colSums(pmp.bma(mfls_spat3)[1:25,])
colSums(pmp.bma(mfls_spat3)[1:50,])
fullmodel.ssq(mfls_spat3)
lm_spat3 <- lm(model.frame(as.zlm(mfls_spat3), model = 1))
lm_res_spat3 <- residuals(lm_spat3)
moran.test(lm_res_spat3, idw1)

mfls_spat3$Wcount
pmpW.bma(mfls_spat3)
mTest3 = moranTest.bma(object = mfls_spat3, variants = "double", 
                       W = idw1, nmodel = 1)
moran_results3 <- cbind(
  Moran_I = sapply(mTest3$moran, function(x) x$estimate[1]),
  SD = sqrt(sapply(mTest3$moran, function(x) x$estimate[3])),
  P_value = sapply(mTest3$moranEV, function(x) x$p.value)
)

# BETA CONVERGENCE -------
dataset <- read_csv("03_final-input/dataset.csv") %>%
  select(NUTS, Year, GDP_capita)
data <- dataset %>% 
  select(NUTS, Name, Year, GDP_capita) %>%
  pivot_longer(cols = -c(NUTS, Name, Year),
               names_to = "Series",
               values_to = "Values") %>% 
  pivot_wider(names_from = "Year",
              values_from = "Values") %>%
  mutate(
    GDP_gg = (`2019`-`2009`)/`2009`,
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

ggplot(data, aes(x = `2009`, y = GDP_gg)) +
  geom_point(aes(color = ColorMap), size = 4) +  # Increase point size
  scale_color_manual(values = c("Greece" = "red",
                                "EU" = "#66c2a5",
                                "CEE" = "#fc8d62",
                                "Candidate" = "#998ec3"),
                     name = "Legend:") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text(data = top_gdp_gg, aes(label = NUTS, fontface = "bold"), vjust = 0.2, hjust = -0.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 1) +
  labs(x = "GDP per Capita in 2009",
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
#ggsave("plot_beta.png", plot = beta, device = "png")