getwd()
set.seed(1105)

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#Loading Data
dataset_amelia <- read_csv("03_final-input/dataset_mice.csv")

# HOT ONE ENCODING -----
data <- dataset_amelia %>%
  group_by(NUTS) %>% 
  mutate(across(-c(Name, Country, Year, GDP_growth,
                   Candidates, CEE, Capital, Coastal, Island, Border, Eurozone, Objective_1), ~ lag(.))) %>%
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
  select(-c(Name, NUTS, starts_with("C_"), Wage_growth, GFCF_share, Pop_growth, Coastal, Eurozone))
interaction <- grep("#", names(datas_base), value = TRUE)

mice_base1 = bms(datas_base[,!names(datas_base) %in% c("CEE", "Candidates", interaction)], 
                 burn=3e+06, iter=10e+06, g="BRIC", mprior="dilut", mcmc="bd", 
                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

mice_base2 = bms(datas_base[, !names(datas_base) %in% interaction], burn=3e+06, iter=10e+06,
                 g="BRIC", mprior="dilut", mcmc="bd",
                 user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

mice_base3 = bms(datas_base, burn=3e+06, iter=10e+06,
                 g="BRIC", mprior="dilut", mcmc="bd.int",
                 user.int=TRUE, force.full.ols = TRUE)


# TEST: FIXED EFFECTS -----
datas_fix <- data_encoded %>%
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
  select(-c(Name, NUTS, Candidates, CEE, Wage_growth, GFCF_share, Pop_growth, Coastal, Eurozone))
interaction <- grep("#", names(datas_fix), value = TRUE)

mice_fix1 = bms(datas_fix[,!names(datas_fix) %in% interaction], burn=3e+06, iter=10e+06, g="BRIC", mprior="dilut", mcmc="bd", 
                user.int= TRUE, force.full.ols = TRUE, fixed.reg = TF)

mice_fix2 = bms(datas_fix, burn=3e+06, iter=10e+06, g="BRIC", mprior="dilut", mcmc="bd", 
                user.int= TRUE, force.full.ols = TRUE, fixed.reg = TF)


# TEST: SAR+BMA -----
WL_decade <- readRDS("03_final-input/WL_10.rds")
WL_decade_ext <- lapply(WL_decade, repeat_rows)

datas_spat <- data_encoded %>%
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
  select(-c(Name, NUTS, starts_with("C_"), Wage_growth, GFCF_share, Pop_growth, Coastal, Eurozone))
interaction <- grep("#", names(datas_spat), value = TRUE)

mice_spat1 = spatFilt.bms(X.data = datas_spat[,!names(datas_spat) %in% c("CEE", "Candidates", interaction)], WList = WL_decade_ext, 
                          burn = 3e+06,iter = 10e+06,
                          nmodel=100, mcmc="bd", g="BRIC", 
                          mprior="dilut", user.int = TRUE)

mice_spat2 = spatFilt.bms(X.data = datas_spat[,!names(datas_spat) %in% interaction], WList = WL_decade_ext, 
                          burn = 3e+06,iter = 10e+06,
                          nmodel=100, mcmc="bd", g="BRIC", 
                          mprior="dilut", user.int = TRUE)

mice_spat3 = spatFilt.bms(X.data = datas_spat, WList = WL_decade_ext, 
                          burn = 3e+06,iter = 10e+06,
                          nmodel=100, mcmc="bd.int", g="BRIC", 
                          mprior="dilut", user.int = TRUE)

### SAVING
rm(list = setdiff(ls(), c("mice_base1", "mice_base2", "mice_base3",
                          "mice_fix1", "mice_fix2",
                          "mice_spat1", "mice_spat2", "mice_spat3")))
save.image(file = "04_final-output/Models-annual.RData")
rm(list = ls())
