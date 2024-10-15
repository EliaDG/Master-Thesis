getwd()
set.seed(1105)

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#Loading Data
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")
idw <- readRDS("03_final-input/idw.rds")

# HOT ONE ENCODING -----
data <- dataset_amelia %>%
  group_by(NUTS) %>% 
  mutate(across(-c(Name, Country, Year, GDP_growth,
                   Candidates, CEE, Capital, Coastal, Island, Border, Eurozone, Objective_1), ~ lag(.))) %>%
  ungroup() %>% 
  filter(Year %in% 2009:2019) %>% 
  mutate(Year = as.factor(Year),
         Country = as.factor(Country)) %>% 
  arrange(Year)

country_encoded <- model.matrix(~ Country- 1, data = data)[, -which(levels(data$Country) == "France")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

year_encoded <- model.matrix(~ Year- 1, data = data)[, -which(levels(data$Year) == "2009")]
colnames(year_encoded) <- gsub("^Year", "Y_", colnames(year_encoded))
data_encoded <- cbind(data, country_encoded, year_encoded)
data_encoded$Country <- NULL
data_encoded$Year <- NULL
colnames(data_encoded) <- make.names(colnames(data_encoded))

YF <- c("Y_2010", "Y_2011","Y_2012", "Y_2013", "Y_2014", 
        "Y_2015", "Y_2016", "Y_2017", "Y_2018", "Y_2019")

# TEST: DUBLIN MODEL -----
datas_durbin <- data_encoded %>%
  mutate(`CEE#Capital` = CEE*Capital,
         `Candidates#Capital` = Candidates*Capital,
         `CEE#GVA_services` = CEE*GVA_services,
         `CEE#GVA_public` = CEE*GVA_public,
         `CEE#GVA_agriculture` = CEE*GVA_agriculture,
         `CEE#GVA_industry` = CEE*GVA_industry,
         `CEE#GVA_construction` = CEE*GVA_construction,
         `Candidates#GVA_public` = Candidates*GVA_public,
         `Candidates#GVA_services` = Candidates*GVA_services,
         `Candidates#GVA_agriculture` = Candidates*GVA_agriculture,
         `Candidates#GVA_industry` = Candidates*GVA_industry,
         `Candidates#GVA_construction` = Candidates*GVA_construction,
         `CEE#GDP_capita` = CEE*GDP_capita,
         `Candidates#GDP_capita` = Candidates*GDP_capita,
         `CEE#Pop_edu_3` = CEE*Pop_edu_3,
         `Candidates#Pop_edu_3` = Candidates*Pop_edu_3) %>% 
  select(-c(Name, NUTS, starts_with("C_"), Wage_growth, GFCF_share, Pop_growth, Coastal, Eurozone, NEET_share))
vars <- datas_durbin %>% 
  select(-c(GDP_growth, YF, Candidates, CEE, Capital, Island, Border, Objective_1)) %>% 
  as.matrix()

W <- listw2mat(idw)
W_block_diag <- bdiag(replicate(11, W, simplify = FALSE))
W_vars <- as.matrix(W_block_diag) %*% vars
W_x <- as.data.frame(W_vars)
colnames(W_x) <- paste0("W_", colnames(vars))

datas_durbin <- cbind(datas_durbin, W_x)
interaction <- grep("#", names(datas_durbin), value = TRUE)

durb_base1 = bms(datas_durbin[,!names(datas_durbin) %in% c("CEE", "Candidates", interaction)], 
                 burn=3e+06, iter=10e+06, g="BRIC", mprior="random", mcmc="bd", 
                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

durb_base2 = bms(datas_durbin[, !names(datas_durbin) %in% interaction], burn=3e+06, iter=10e+06,
                 g="BRIC", mprior="random", mcmc="bd",
                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

durb_base3 = bms(datas_durbin, burn=3e+06, iter=10e+06,
                 g="BRIC", mprior="random", mcmc="bd",
                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

### SAVING
rm(list = setdiff(ls(), c("durb_base1", "durb_base2", "durb_base3")))
save.image(file = "04_final-output/Models-durbin.RData")
rm(list = ls())
