getwd()

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

# LOADING DATA
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")

# HOT ONE ENCODING -----
data <- dataset_amelia %>%
  group_by(NUTS) %>% 
  mutate(across(-c(Name, Country, Year, GDP_growth,
                   Pop_growth, Pop_edu_1, Pop_edu_2, Pop_edu_3,
                   Fertility_rate, Life_exp, Migration_rate,
                   Candidates, CEE, Capital, Coastal, Island, Objective_1, Eurozone), ~ lag(.))) %>%
  ungroup() %>% 
  filter(Year %in% 2009:2019) %>% 
  mutate(Year = as.factor(Year),
         Country = as.factor(Country))

country_encoded <- model.matrix(~ Country- 1, data = data)[, -which(levels(data$Country) == "France")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

year_encoded <- model.matrix(~ Year- 1, data = data)[, -which(levels(data$Year) == "2009")]
colnames(year_encoded) <- gsub("^Year", "Y_", colnames(year_encoded))
data_encoded <- cbind(data, country_encoded, year_encoded)
data_encoded$Country <- NULL
data_encoded$Year <- NULL
colnames(data_encoded) <- make.names(colnames(data_encoded))

write.csv(data_encoded, file = here("03_final-input", "encoded_dataset.csv"), row.names = FALSE)

CF <- c("C_Albania", "C_Austria", "C_Belgium", "C_Bosnia.and.Herzegovina", "C_Bulgaria", 
        "C_Croatia", "C_Cyprus", "C_Czech.Republic", "C_Denmark", "C_Estonia", "C_Finland", 
        "C_Germany", "C_Greece", "C_Hungary", "C_Ireland", "C_Italy", "C_Kosovo", 
        "C_Latvia", "C_Lithuania", "C_Luxembourg", "C_Malta", "C_Moldova", "C_Montenegro", 
        "C_Netherlands", "C_North.Macedonia", "C_Poland", "C_Portugal", "C_Romania", 
        "C_Serbia", "C_Slovakia", "C_Slovenia", "C_Spain", "C_Sweden", "C_Turkey", "C_United.Kingdom")

YF <- c("Y_2010", "Y_2011","Y_2012", "Y_2013", "Y_2014", 
        "Y_2015", "Y_2016", "Y_2017", "Y_2018", "Y_2019")

TF <- c(CF,YF)

# TEST: BASE MODEL -----
datas_base <- data_encoded %>%
  mutate(`CEE#Capital` = CEE*Capital,
         #No Candidates x Capital because most of these countries are single regions
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
         `Candidates#GDP_capita` = Candidates*GDP_capita) %>% 
  select(-c(Name, NUTS, starts_with("C_")))

interaction <- grep("#", names(datas_base), value = TRUE)
# mfls_base = bms(datas_base[,!names(datas_base) %in% c("CEE", "Island", "Capital", "Eurozone", "Objective_1", "Coastal", "Candidates", interaction)],
#                 burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd",
#                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

mfls_base1 = bms(datas_base[,!names(datas_base) %in% c("CEE", "Candidates", interaction)], 
                 burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

mfls_base2 = bms(datas_base[, !names(datas_base) %in% interaction], burn=2e+06, iter=3e+06,
                 g="BRIC", mprior="random", mcmc="bd",
                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

mfls_base3 = bms(datas_base, burn=2e+06, iter=3e+06,
                 g="BRIC", mprior="random", mcmc="bd.int",
                 user.int=TRUE, force.full.ols = TRUE)

# TEST: FIXED EFFECTS -----
datas_fix <- data_encoded %>%
  mutate(`CEE#Capital` = CEE*Capital,
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
         `Candidates#GDP_capita` = Candidates*GDP_capita) %>% 
  select(-c(Name, NUTS, Candidates, CEE))

interaction <- grep("#", names(datas_fix), value = TRUE)
# mfls_fix = bms(datas_fix[,!names(datas_fix) %in% c("Capital", "Island", "Eurozone", "Objective_1", "Coastal", interaction)],
#                burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd",
#                user.int= TRUE, force.full.ols = TRUE, fixed.reg = TF)

mfls_fix1 = bms(datas_fix[,!names(datas_fix) %in% interaction], burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                user.int= TRUE, force.full.ols = TRUE, fixed.reg = TF)

mfls_fix2 = bms(datas_fix, burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                user.int= TRUE, force.full.ols = TRUE, fixed.reg = TF)

# TEST: SAR+BMA -----
WL <- readRDS("03_final-input/WL.rds")
datas_spat <- data_encoded %>%
  mutate(`CEE#Capital` = CEE*Capital,
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
         `Candidates#GDP_capita` = Candidates*GDP_capita) %>% 
  select(-c(Name, NUTS, starts_with("C_")))
interaction <- grep("#", names(datas_spat), value = TRUE)

# mfls_spat = spatFilt.bms(X.data = datas_spat[,!names(datas_base) %in% c("CEE", "Island", "Capital", "Eurozone", "Objective_1", "Coastal", "Candidates", interaction)], WList = WL, 
#                          burn = 2e+06,iter = 3e+06,
#                          nmodel=100, mcmc="bd", g="BRIC", 
#                          mprior="random", user.int = TRUE)
mfls_spat1 = spatFilt.bms(X.data = datas_spat[,!names(datas_spat) %in% c("CEE", "Candidates", interaction)], WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)
mfls_spat2 = spatFilt.bms(X.data = datas_spat[,!names(datas_spat) %in% interaction], WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)
mfls_spat3 = spatFilt.bms(X.data = datas_spat, WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd.int", g="BRIC", 
                         mprior="random", user.int = TRUE)

# ### SAVING
rm(list = setdiff(ls(), c("mfls_base1", "mfls_base2", "mfls_base3",
                          "mfls_fix1", "mfls_fix2",
                          "mfls_spat1", "mfls_spat2", "mfls_spat3",
                          "sub_base1", "sub_base2", "sub_base3",
                          "sub_fix1", "sub_fix2")))
save.image(file = "Models_1.RData")
