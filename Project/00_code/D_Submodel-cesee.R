getwd()
set.seed(1105)

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#Loading Data
subdataset_amelia <- read_csv("03_final-input/dataset_amelia.csv") %>%
  select(-c(GDP_growth, Pop_growth, Wage_growth))
Growth_variables <- read_csv("03_final-input/dataset.csv") %>%
  select(NUTS, Year, GDP_capita, Population_abs, Wage_EUR)

# HOT ONE ENCODING -----
Growth_decade <- Growth_variables %>%
  pivot_longer(cols = -c(NUTS, Year),
               names_to = "Series",
               values_to = "Values") %>% 
  pivot_wider(names_from = Year,
              values_from = Values) %>%
  mutate(`2009` = (`2019`-`2009`)/`2009`) %>%
  select(NUTS, `2009`, Series) %>% 
  pivot_longer(cols = -c(NUTS, Series),
               names_to = "Year",
               values_to = "Values") %>%
  pivot_wider(names_from = "Series",
              values_from = "Values") %>% 
  mutate(Year = as.numeric(Year)) %>%
  rename(GDP_growth = GDP_capita,
         Pop_growth = Population_abs,
         Wage_growth = Wage_EUR) %>% 
  as.data.frame()

subdata <- inner_join(subdataset_amelia, Growth_decade, by = c("NUTS", "Year")) %>%
  select(NUTS, Name, Country, GDP_growth, everything()) %>%
  filter(CEE == 1 | Candidates == 1) %>%
  select(-c(Year, CEE, Island)) %>% 
  mutate(Country = as.factor(Country)) %>% 
  as.data.frame()


country_encoded <- model.matrix(~ Country- 1, data = subdata)[, -which(levels(subdata$Country) == "Slovenia")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

subdata_encoded <- cbind(subdata, country_encoded)
subdata_encoded$Country <- NULL
colnames(subdata_encoded) <- make.names(colnames(subdata_encoded))

CF <- c("C_Albania", "C_Bosnia.and.Herzegovina", "C_Bulgaria", "C_Croatia", "C_Cyprus", 
        "C_Czech.Republic", "C_Estonia", "C_Hungary", "C_Kosovo", "C_Latvia", 
        "C_Lithuania", "C_Moldova", "C_Montenegro", "C_North.Macedonia", 
        "C_Poland", "C_Romania", "C_Serbia", "C_Slovakia", "C_Turkey")

# TEST: BASE MODEL --------
subdatas_base <- subdata_encoded %>%
  mutate(`Candidates#GDP_capita` = Candidates*GDP_capita,
         `Candidates#Pop_edu_3` = Candidates*Pop_edu_3,
         `Candidates#GVA_services` = Candidates*GVA_services,
         `Candidates#GVA_public` = Candidates*GVA_public,
         `Candidates#GVA_agriculture` = Candidates*GVA_agriculture,
         `Candidates#GVA_industry` = Candidates*GVA_industry,
         `Candidates#GVA_construction` = Candidates*GVA_construction) %>%  
  select(-c(Name, NUTS, starts_with("C_"), Wage_growth, Coastal, GFCF_share, Pop_growth, Eurozone))
interaction <- grep("#", names(subdatas_base), value = TRUE)

cesee_base1 = bms(subdatas_base[,!names(subdatas_base) %in% c("Candidates", interaction)], burn=3e+06, iter=10e+06,
                g="BRIC", mprior="random", mcmc="bd",
                force.full.ols = TRUE, user.int=TRUE)

cesee_base2 = bms(subdatas_base[,!names(subdatas_base) %in% interaction], burn=3e+06, iter=10e+06,
                g="BRIC", mprior="random", mcmc="bd",
                force.full.ols = TRUE, user.int=TRUE)

cesee_base3 = bms(subdatas_base, burn=3e+06, iter=10e+06,
                g="BRIC", mprior="random", mcmc="bd.int",
                force.full.ols = TRUE, user.int=TRUE)

# TEST: COUNTRY EFFECT --------
subdatas_fix <- subdata_encoded %>%
  mutate(`Candidates#GDP_capita` = Candidates*GDP_capita,
         `Candidates#Pop_edu_3` = Candidates*Pop_edu_3,
         `Candidates#GVA_services` = Candidates*GVA_services,
         `Candidates#GVA_public` = Candidates*GVA_public,
         `Candidates#GVA_agriculture` = Candidates*GVA_agriculture,
         `Candidates#GVA_industry` = Candidates*GVA_industry,
         `Candidates#GVA_construction` = Candidates*GVA_construction) %>%  
  select(-c(Name, NUTS, Candidates, Wage_growth, Coastal, GFCF_share, Pop_growth, Eurozone))
interaction <- grep("#", names(subdatas_fix), value = TRUE)

cesee_fix1 = bms(subdatas_fix[,!names(subdatas_fix) %in% interaction], burn=3e+06, iter=10e+06,
               g="BRIC", mprior="random", mcmc="bd",
               force.full.ols = TRUE, user.int= TRUE, fixed.reg = CF)

cesee_fix2 = bms(subdatas_fix, burn=3e+06, iter=10e+06,
               g="BRIC", mprior="random", mcmc="bd",
               force.full.ols = TRUE, user.int= TRUE, fixed.reg = CF)

# TEST: SAR+BMA -----
WL_cesee <- readRDS("03_final-input/WL_10_cesee.rds")

subdatas_spat <- subdata_encoded %>%
  mutate(`Candidates#GDP_capita` = Candidates*GDP_capita,
         `Candidates#Pop_edu_3` = Candidates*Pop_edu_3,
         `Candidates#GVA_services` = Candidates*GVA_services,
         `Candidates#GVA_public` = Candidates*GVA_public,
         `Candidates#GVA_agriculture` = Candidates*GVA_agriculture,
         `Candidates#GVA_industry` = Candidates*GVA_industry,
         `Candidates#GVA_construction` = Candidates*GVA_construction) %>%  
  select(-c(Name, NUTS, starts_with("C_"), Wage_growth, Coastal, GFCF_share, Pop_growth, Eurozone))
interaction <- grep("#", names(subdatas_spat), value = TRUE)


cesee_spat1 = spatFilt.bms(X.data = subdatas_spat[,!names(subdatas_spat) %in% c("Candidates", interaction)], WList = WL_cesee, 
                         burn = 3e+06,iter = 10e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)

cesee_spat2 = spatFilt.bms(X.data = subdatas_spat[,!names(subdatas_spat) %in% interaction], WList = WL_cesee, 
                         burn = 3e+06,iter = 10e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)

cesee_spat3 = spatFilt.bms(X.data = subdatas_spat, WList = WL_cesee, 
                         burn = 3e+06,iter = 10e+06,
                         nmodel=100, mcmc="bd.int", g="BRIC", 
                         mprior="random", user.int = TRUE)

### SAVING
rm(list = setdiff(ls(), c("cesee_base1", "cesee_base2", "cesee_base3",
                          "cesee_fix1", "cesee_fix2",
                          "cesee_spat1", "cesee_spat2", "cesee_spat3")))
save.image(file = "04_final-output/Models-cesee.RData")
rm(list = ls())