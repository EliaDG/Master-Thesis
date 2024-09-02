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
  mutate(`EU#Capital` = EU*Capital,
         `EU#GVA_services` = EU*GVA_services,
         `EU#GVA_public` = EU*GVA_public,
         `EU#GVA_primary` = EU*GVA_primary,
         `EU#Pop_edu_3` = EU*Pop_edu_3) %>% 
  select(-c(Name, NUTS, starts_with("C_")))

interaction <- grep("#", names(datas_base), value = TRUE)
cesee_base = bms(datas_base[,!names(datas_base) %in% c("EU", "Euro", "Objective_1", "Coastal", interaction)], 
                  burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                  force.full.ols = TRUE, user.int=TRUE, fixed.reg = YF)

cesee_base1 = bms(datas_base[,!names(datas_base) %in% c("EU", interaction)], 
                  burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd",
                  force.full.ols = TRUE, user.int=TRUE, fixed.reg = YF)

cesee_base2 = bms(datas_base, burn=2e+06, iter=3e+06,
                  g="BRIC", mprior="random", mcmc="bd",
                  force.full.ols = TRUE, user.int=TRUE,
                  fixed.reg = YF)

cesee_base3 = bms(datas_base, burn=2e+06, iter=3e+06,
                  g="BRIC", mprior="random", mcmc="bd.int",
                  force.full.ols = TRUE, user.int=TRUE)

# TEST: FIXED EFFECTS -----
datas_fix <- data_encoded %>%
  select(-c(Name, NUTS, EU))

cesee_fix = bms(datas_fix[,!names(datas_fix) %in% c("Island", "Capital", "Euro", "Objective_1", "Coastal")], 
                 burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                force.full.ols = TRUE, user.int= TRUE, fixed.reg = TF)

cesee_fix1 = bms(datas_fix, burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                force.full.ols = TRUE, user.int= TRUE, fixed.reg = TF)

# TEST: SAR+BMA ------
W1 <- readRDS("03_final-input/idw.rds")

idw1 <- W1
idw1$neighbours <- rep(W1$neighbours, each = 11);
idw1$weights <- rep(W1$weights, each = 11)

attr(idw1$neighbours, "class") <- "nb"
attr(idw1$neighbours, "region.id") <- rep(attr(W1$neighbours, "region.id"), each = 11)
attr(idw1$neighbours, "sym") <- TRUE
attr(idw1$neighbours, "call") <- attr(W1$neighbours, "call")

nb1 <- idw1$neighbours

datas_spat <- data_encoded %>%
  mutate(`EU#Capital` = EU*Capital,
         `EU#GVA_services` = EU*GVA_services,
         `EU#GVA_public` = EU*GVA_public,
         `EU#GVA_primary` = EU*GVA_primary,
         `EU#GVA_services` = EU*GVA_services,
         `Candidates#GVA_public` = Candidates*GVA_public,
         `Candidates#GVA_primary` = Candidates*GVA_primary,
         `Candidates#GVA_services` = Candidates*GVA_services,
         `EU#Pop_edu_3` = EU*Pop_edu_3,
         `Candidates#Pop_edu_3` = Candidates*Pop_edu_3) %>% 
  select(-c(Name, NUTS, starts_with("C_")))

y <- as.data.frame(datas_spat[, 1, drop = F])
yFilt1 <- SpatialFiltering(datas_spat[, 1] ~ 1, ~-1, data = y,
                           nb = nb1, glist = idw1$weights, style = "B", ExactEV = FALSE)

WL <- list(Col0 = fitted(yFilt1))

cesee_spat = spatFilt.bms(X.data = datas_spat[,!names(datas_base) %in% c("EU", "Island", "Capital", "Euro", "Objective_1", "Coastal", interaction)], WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)
cesee_spat1 = spatFilt.bms(X.data = datas_spat[,!names(datas_base) %in% interaction], WList = WL, 
                          burn = 2e+06,iter = 3e+06,
                          nmodel=100, mcmc="bd", g="BRIC", 
                          mprior="random", user.int = TRUE)
cesee_spat3 = spatFilt.bms(X.data = datas_spat, WList = WL, 
                          burn = 2e+06,iter = 3e+06,
                          nmodel=100, mcmc="bd.int", g="BRIC", 
                          mprior="random", user.int = TRUE)
