getwd()

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

# LOADING DATA
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")

# HOT ONE ENCODING -----
data <- dataset_amelia %>%
  filter(Candidates == 1 | CEE == 1) %>%
  select(-c(Candidates, Island)) %>% #I delete Islands because is constant.
  rename(EU = CEE) %>% 
  mutate(#NUTS_Year = paste0(sapply(NUTS, modify_NUTS) , "_", as.character(Year)),
    Year = as.factor(Year),
    Country = as.factor(Country))
#column_to_rownames(var = "NUTS_Year")

country_encoded <- model.matrix(~ Country- 1, data = data)[, -which(levels(data$Country) == "Albania")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

year_encoded <- model.matrix(~ Year- 1, data = data)[, -which(levels(data$Year) == "2009")]
colnames(year_encoded) <- gsub("^Year", "Y_", colnames(year_encoded))

data_encoded <- cbind(data, country_encoded, year_encoded)
data_encoded$Country <- NULL
data_encoded$Year <- NULL
colnames(data_encoded) <- make.names(colnames(data_encoded))

CF <- c("C_Croatia", "C_Bosnia.and.Herzegovina", "C_Bulgaria", 
        "C_Cyprus", "C_Czech.Republic", "C_Estonia", 
        "C_Hungary", "C_Kosovo", "C_Latvia", "C_Lithuania",
        "C_Moldova", "C_Montenegro", "C_North.Macedonia", "C_Poland", "C_Romania", 
        "C_Serbia", "C_Slovakia", "C_Slovenia", "C_Turkey")

YF <- c("Y_2010", "Y_2011","Y_2012", "Y_2013", "Y_2014", 
        "Y_2015", "Y_2016", "Y_2017", "Y_2018", "Y_2019")

TF <- c(CF,YF)

# TEST: BASE MODEL -----
datas_base <- data_encoded %>%
  mutate(`EU#Capital` = EU*Capital) %>% 
  select(-c(Name, NUTS, starts_with("C_")))

cesee_base = bms(datas_base[,!names(datas_base) %in% c("EU", "Euro", "Objective_1", "Coastal", "EU#Capital")], 
                  burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                  force.full.ols = TRUE, user.int=TRUE, fixed.reg = YF)

cesee_base1 = bms(datas_base[,!names(datas_base) %in% c("EU", "EU#Capital")], 
                  burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd",
                  force.full.ols = TRUE, user.int=TRUE, fixed.reg = YF)

cesee_base2 = bms(datas_base, burn=2e+06, iter=3e+06,
                  g="BRIC", mprior="random", mcmc="bd.int",
                  force.full.ols = TRUE, user.int=TRUE)

cesee_base3 = bms(datas_base, burn=2e+06, iter=3e+06,
                  g="BRIC", mprior="random", mcmc="bd",
                  force.full.ols = TRUE, user.int=TRUE,
                  fixed.reg = YF)

# TEST: FIXED EFFECTS -----
datas_fix <- data_encoded %>%
  select(-c(Name, NUTS, EU))

cesee_fix = bms(datas_fix[,!names(datas_fix) %in% c("Island", "Euro", "Objective_1", "Coastal")], 
                 burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                force.full.ols = TRUE, user.int= TRUE, fixed.reg = TF)

cesee_fix1 = bms(datas_fix, burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                force.full.ols = TRUE, user.int= TRUE, fixed.reg = TF)

cesee_fix2 = bms(datas_fix, burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                 force.full.ols = TRUE, user.int= TRUE)

# TEST: SAR+BMA ------
W <- readRDS("03_final-input/idw.rds")
WL <- readRDS("03_final-input/Filter.rds")
idw <- W
idw$neighbours <- rep(W$neighbours, each = 11);
idw$weights <- rep(W$weights, each = 11)

class(idw$neighbours) <- "nb"
attr(idw$neighbours, "region.id") <- rep(attr(W$neighbours, "region.id"), each = 11)
attr(idw$neighbours, "sym") <- attr(W$neighbours, "sym")
attr(idw$neighbours, "call") <- attr(W$neighbours, "call")
attr(idw$neighbours, "class") <- c("nb")

datas_spat <- data_encoded %>%
  #slice(seq(11, 3311, by = 11)) %>% 
  select(-c(Name, NUTS, starts_with("C_"),
            #Because still to double check real/nominal nature
            Wage_EUR, Labor_Prodx))

y <- as.data.frame(datas_spat[, 1, drop = F])
yFilt <- SpatialFiltering(datas_spat[, 1] ~ 1, ~-1, data = y,
                          nb = idw$neighbours, glist = idw$weights, style = "B", ExactEV = FALSE)
WL_new <- list(Col0 = fitted(yFilt))
datas_spat_mat <- as.matrix(apply(datas_spat, 2, as.numeric))
#dimnames(datas_spat_mat) <- list(NULL, colnames(datas_spat_mat))
cesee_spat = spatFilt.bms(X.data = datas_spat_mat, WList = WL_new, 
                         burn = 100000,iter = 1000000,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)
# Comparison to tutorial:
data(dataBoston)
data("WL.boston")
data("boston.soi")
dataM = as.matrix(apply(dataBoston, 2, as.numeric))
model1 = spatFilt.bms(X.data = dataM, WList = WL.boston, burn = 1e+05, iter = 1e+06, nmodel=100, mcmc="bd", g="BRIC", 
                      mprior="random", user.int = TRUE)
class(dataM)
class(WL.boston); dim(WL.boston)
class(datas_spat_mat)
class(WL); dim(WL)

# #Appendix -----
# subdataset_amelia <- read_csv("03_final-input/dataset_amelia.csv") %>% 
#   select(-GDP_growth)
# GDP_capita_raw <- read_csv("03_final-input/dataset.csv") %>% 
#   select(NUTS, Year, GDP_capita)
# 
# GDPG_intervals <- GDP_capita_raw %>%
#   pivot_wider(names_from = Year,
#               values_from = GDP_capita) %>% 
#   mutate(`2014` = (`2014`-`2009`)/`2009`,
#          `2019` = (`2019`-`2015`)/`2015`) %>%
#   select(NUTS, `2014`, `2019` ) %>% 
#   pivot_longer(cols = -"NUTS",
#                names_to = "Year",
#                values_to = "GDP_growth") %>% 
#   mutate(Year =as.numeric(Year))
# 
# subdata <- inner_join(subdataset_amelia, GDPG_intervals, by = c("NUTS", "Year")) %>% 
#   select(NUTS, Name, Country, Year, GDP_growth, everything()) %>% 
#   arrange(NUTS, Year) %>% 
#   mutate(Year = as.factor(Year),
#          Country = as.factor(Country))
# 
# country_encoded <- model.matrix(~ Country- 1, data = subdata)[, -which(levels(subdata$Country) == "France")]
# colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))
# 
# Y_2019 <- model.matrix(~ Year- 1, data = subdata)[, -which(levels(subdata$Year) == "2014")]
# 
# subdata_encoded <- cbind(subdata, country_encoded, Y_2019)
# subdata_encoded$Country <- NULL
# subdata_encoded$Year <- NULL
# 
# TF <- c("C_Albania", "C_Austria", "C_Belgium", "C_Bosnia and Herzegovina", "C_Bulgaria", 
#         "C_Croatia", "C_Cyprus", "C_Czech Republic", "C_Denmark", "C_Estonia", "C_Finland", 
#         "C_Germany", "C_Greece", "C_Hungary", "C_Ireland", "C_Italy", "C_Kosovo", 
#         "C_Latvia", "C_Lithuania", "C_Luxembourg", "C_Malta", "C_Moldova", "C_Montenegro", 
#         "C_Netherlands", "C_North Macedonia", "C_Poland", "C_Portugal", "C_Romania", 
#         "C_Serbia", "C_Slovakia", "C_Slovenia", "C_Spain", "C_Sweden", "C_Turkey", "C_United Kingdom",
#         "Y_2019")
# 
# YF <- c("Y_2019")
# 
# # TEST: BASE MODEL
# subdatas_base <- subdata_encoded %>%
#   select(-c(Name, NUTS, starts_with("C_"),
#             #Because still to double check real/nominal nature
#             Wage_EUR, Labor_Prodx))
# 
# doParallel::registerDoParallel()
# cesee_base = bms(subdatas_base, burn=2e+06, iter=3e+06,
#                 g="BRIC", mprior="random", mcmc="bd", 
#                 force.full.ols = TRUE, user.int=TRUE,
#                 fixed.reg = YF)
# 
# subdatas_fix <- subdata_encoded %>%
#   select(-c(Name, NUTS, Candidates,
#             #Because still to double check real/nominal nature
#             Wage_EUR, Labor_Prodx))
# 
# doParallel::registerDoParallel()
# cesee_fix = bms(subdatas_fix, burn=2e+06, iter=3e+06, 
#                g="BRIC", mprior="random", mcmc="bd", 
#                force.full.ols = TRUE, user.int= TRUE,
#                fixed.reg = TF)
