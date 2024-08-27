getwd()

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

# LOADING DATA
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")
geom <- readRDS("03_final-input/geometries.rds")
W <- readRDS("03_final-input/idw.rds")

# HOT ONE ENCODING
data <- dataset_amelia %>% 
  mutate(#NUTS_Year = paste0(sapply(NUTS, modify_NUTS) , "_", as.character(Year)),
         Year = as.factor(Year),
         Country = as.factor(Country))
  #column_to_rownames(var = "NUTS_Year")

country_encoded <- model.matrix(~ Country- 1, data = data)[, -which(levels(data$Country) == "France")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

year_encoded <- model.matrix(~ Year- 1, data = data)[, -which(levels(data$Year) == "2009")]
colnames(year_encoded) <- gsub("^Year", "Y_", colnames(year_encoded))

data_encoded <- cbind(data, country_encoded, year_encoded)
data_encoded$Country <- NULL
data_encoded$Year <- NULL

TF <- c("C_Albania", "C_Austria", "C_Belgium", "C_Bosnia and Herzegovina", "C_Bulgaria", 
        "C_Croatia", "C_Cyprus", "C_Czech Republic", "C_Denmark", "C_Estonia", "C_Finland", 
        "C_Germany", "C_Greece", "C_Hungary", "C_Ireland", "C_Italy", "C_Kosovo", 
        "C_Latvia", "C_Lithuania", "C_Luxembourg", "C_Malta", "C_Moldova", "C_Montenegro", 
        "C_Netherlands", "C_North Macedonia", "C_Poland", "C_Portugal", "C_Romania", 
        "C_Serbia", "C_Slovakia", "C_Slovenia", "C_Spain", "C_Sweden", "C_Turkey", "C_United Kingdom", 
        "Y_2010", "Y_2011", "Y_2012", "Y_2013", "Y_2014", 
        "Y_2015", "Y_2016", "Y_2017", "Y_2018", "Y_2019")

YF <- c("Y_2010", "Y_2011","Y_2012", "Y_2013", "Y_2014", 
        "Y_2015", "Y_2016", "Y_2017", "Y_2018", "Y_2019")

# TEST: BASE MODEL
datas_base <- data_encoded %>%
  select(-c(Name, NUTS, starts_with("C_"),
            #Because still to double check real/nominal nature
            Wage_EUR, Labor_Prodx))

## Correlation Check
# cor_matrix <- cor(datas_base)
# corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", diag = FALSE)
# high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.7)
# datas_corr <- select(datas_base, high_corr_columns)

## Tutorial
# datas_base_mat <- as.matrix(datas_base)
# dat.array_base = panel_unstack(datas_base_mat, tstep=11)

doParallel::registerDoParallel()
mfls_base = bms(datas_base, burn=2000000, iter=3000000,
                g="BRIC", mprior="random", mcmc="bd", 
                force.full.ols = TRUE, user.int=TRUE,
                fixed.reg = YF)
# coef(mfls_base, exact=TRUE)
# estimates.bma(mfls_base, condi.coef = TRUE)

# TEST: FIXED EFFECTS
datas_fix <- data_encoded %>%
  select(-c(Name, NUTS, Candidates,
            #Because still to double check real/nominal nature
            Wage_EUR, Labor_Prodx))

doParallel::registerDoParallel()
mfls_fix = bms(datas_fix, burn=2000000, iter=3000000, 
               g="BRIC", mprior="random", mcmc="bd", 
               force.full.ols = TRUE, user.int= TRUE,
               fixed.reg = TF)

# TEST: SAR+BMA
#library(spatBMS)
datas_spat <- data_encoded %>%
  select(-c(Name, NUTS, starts_with("C_"),
            #Because still to double check real/nominal nature
            Wage_EUR, Labor_Prodx))

y <- as.data.frame(datas_spat[1:301, 1, drop = F])
yFilt <- SpatialFiltering(datas_spat[1:301, 1] ~ 1, ~-1, data = y, nb = W$neighbours, glist = W$weights, style = "B", ExactEV = TRUE)
WL <- list(Col0 = fitted(yFilt))

mfls_spat = spatFilt.bms(X.data = datas_spat[1:301,], WList = WL, 
                         burn = 2000000,iter = 3000000,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)

# Alternative approach Jesus -----
subdataset_amelia <- read_csv("03_final-input/dataset_amelia.csv") %>% 
  select(-GDP_growth)
GDP_capita_raw <- read_csv("03_final-input/dataset.csv") %>% 
  select(NUTS, Year, GDP_capita)

GDPG_intervals <- GDP_capita_raw %>%
  pivot_wider(names_from = Year,
              values_from = GDP_capita) %>% 
  mutate(`2014` = (`2014`-`2009`)/`2009`,
         `2019` = (`2019`-`2015`)/`2015`) %>%
  select(NUTS, `2014`, `2019` ) %>% 
  pivot_longer(cols = -"NUTS",
               names_to = "Year",
               values_to = "GDP_growth") %>% 
  mutate(Year =as.numeric(Year))

subdata <- inner_join(subdataset_amelia, GDPG_intervals, by = c("NUTS", "Year")) %>% 
  select(NUTS, Name, Country, Year, GDP_growth, everything()) %>% 
  arrange(NUTS, Year) %>% 
  mutate(Year = as.factor(Year),
         Country = as.factor(Country))

country_encoded <- model.matrix(~ Country- 1, data = subdata)[, -which(levels(subdata$Country) == "France")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

Y_2019 <- model.matrix(~ Year- 1, data = subdata)[, -which(levels(subdata$Year) == "2014")]

subdata_encoded <- cbind(subdata, country_encoded, Y_2019)
subdata_encoded$Country <- NULL
subdata_encoded$Year <- NULL

TF <- c("C_Albania", "C_Austria", "C_Belgium", "C_Bosnia and Herzegovina", "C_Bulgaria", 
        "C_Croatia", "C_Cyprus", "C_Czech Republic", "C_Denmark", "C_Estonia", "C_Finland", 
        "C_Germany", "C_Greece", "C_Hungary", "C_Ireland", "C_Italy", "C_Kosovo", 
        "C_Latvia", "C_Lithuania", "C_Luxembourg", "C_Malta", "C_Moldova", "C_Montenegro", 
        "C_Netherlands", "C_North Macedonia", "C_Poland", "C_Portugal", "C_Romania", 
        "C_Serbia", "C_Slovakia", "C_Slovenia", "C_Spain", "C_Sweden", "C_Turkey", "C_United Kingdom",
        "Y_2019")

YF <- c("Y_2019")

# TEST: BASE MODEL
subdatas_base <- subdata_encoded %>%
  select(-c(Name, NUTS, starts_with("C_"),
            #Because still to double check real/nominal nature
            Wage_EUR, Labor_Prodx))

doParallel::registerDoParallel()
mfls_base = bms(subdatas_base, burn=2000000, iter=3000000,
                g="BRIC", mprior="random", mcmc="bd", 
                force.full.ols = TRUE, user.int=TRUE,
                fixed.reg = YF)

subdatas_fix <- subdata_encoded %>%
  select(-c(Name, NUTS, Candidates,
            #Because still to double check real/nominal nature
            Wage_EUR, Labor_Prodx))

doParallel::registerDoParallel()
mfls_fix = bms(subdatas_fix, burn=2000000, iter=3000000, 
               g="BRIC", mprior="random", mcmc="bd", 
               force.full.ols = TRUE, user.int= TRUE,
               fixed.reg = TF)