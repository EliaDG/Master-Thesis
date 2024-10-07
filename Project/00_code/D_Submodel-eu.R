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
  filter(!Candidates == 1) %>%
  select(-c(Year, Candidates)) %>% 
  mutate(Country = as.factor(Country)) %>% 
  as.data.frame()

country_encoded <- model.matrix(~ Country- 1, data = subdata)[, -which(levels(subdata$Country) == "France")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

subdata_encoded <- cbind(subdata, country_encoded)
subdata_encoded$Country <- NULL
colnames(subdata_encoded) <- make.names(colnames(subdata_encoded))

CF <- c("C_Austria", "C_Belgium", "C_Bulgaria", "C_Croatia", "C_Cyprus", 
        "C_Czech.Republic", "C_Denmark", "C_Estonia", "C_Finland", 
        "C_Germany", "C_Greece", "C_Hungary", "C_Ireland", 
        "C_Italy", "C_Latvia", "C_Lithuania", "C_Luxembourg", "C_Malta", 
        "C_Netherlands", "C_Poland", "C_Portugal", "C_Romania", 
        "C_Slovakia", "C_Slovenia", "C_Spain", "C_Sweden", "C_United.Kingdom")


# TEST: BASE MODEL --------
subdatas_base <- subdata_encoded %>%
  mutate(`CEE#GDP_capita` = CEE*GDP_capita,
         `CEE#Capital` = CEE*Capital,
         `CEE#Pop_edu_3` = CEE*Pop_edu_3,
         `CEE#GVA_services` = CEE*GVA_services,
         `CEE#GVA_public` = CEE*GVA_public,
         `CEE#GVA_agriculture` = CEE*GVA_agriculture,
         `CEE#GVA_industry` = CEE*GVA_industry,
         `CEE#GVA_construction` = CEE*GVA_construction) %>%  
  select(-c(Name, NUTS, starts_with("C_"), Wage_growth, Coastal, Pop_growth, GFCF_share, Eurozone))
interaction <- grep("#", names(subdatas_base), value = TRUE)

eu_base1 = bms(subdatas_base[,!names(subdatas_base) %in% c("CEE", interaction)], burn=3e+06, iter=10e+06,
                  g="BRIC", mprior="random", mcmc="bd",
                  force.full.ols = TRUE, user.int=TRUE)

eu_base2 = bms(subdatas_base[,!names(subdatas_base) %in% interaction], burn=3e+06, iter=10e+06,
                  g="BRIC", mprior="random", mcmc="bd",
                  force.full.ols = TRUE, user.int=TRUE)

eu_base3 = bms(subdatas_base, burn=3e+06, iter=10e+06,
                  g="BRIC", mprior="random", mcmc="bd.int",
                  force.full.ols = TRUE, user.int=TRUE)

# TEST: COUNTRY EFFECT --------
subdatas_fix <- subdata_encoded %>%
  mutate(`CEE#GDP_capita` = CEE*GDP_capita,
         `CEE#Capital` = CEE*Capital,
         `CEE#Pop_edu_3` = CEE*Pop_edu_3,
         `CEE#GVA_services` = CEE*GVA_services,
         `CEE#GVA_public` = CEE*GVA_public,
         `CEE#GVA_agriculture` = CEE*GVA_agriculture,
         `CEE#GVA_industry` = CEE*GVA_industry,
         `CEE#GVA_construction` = CEE*GVA_construction) %>%  
  select(-c(Name, NUTS, CEE, Wage_growth, Coastal, GFCF_share, Pop_growth, Eurozone))
interaction <- grep("#", names(subdatas_fix), value = TRUE)

eu_fix1 = bms(subdatas_fix[,!names(subdatas_fix) %in% interaction], burn=3e+06, iter=10e+06,
                 g="BRIC", mprior="random", mcmc="bd",
                 force.full.ols = TRUE, user.int= TRUE, fixed.reg = CF)

eu_fix2 = bms(subdatas_fix, burn=3e+06, iter=10e+06,
                 g="BRIC", mprior="random", mcmc="bd",
                 force.full.ols = TRUE, user.int= TRUE, fixed.reg = CF)

# TEST: SAR+BMA -----
WL_eu <- readRDS("03_final-input/WL_10_eu.rds")

subdatas_spat <- subdata_encoded %>%
  mutate(`CEE#GDP_capita` = CEE*GDP_capita,
         `CEE#Capital` = CEE*Capital,
         `CEE#Pop_edu_3` = CEE*Pop_edu_3,
         `CEE#GVA_services` = CEE*GVA_services,
         `CEE#GVA_public` = CEE*GVA_public,
         `CEE#GVA_agriculture` = CEE*GVA_agriculture,
         `CEE#GVA_industry` = CEE*GVA_industry,
         `CEE#GVA_construction` = CEE*GVA_construction) %>%  
  select(-c(Name, NUTS, starts_with("C_"), Wage_growth, Coastal, Pop_growth, GFCF_share, Eurozone))
interaction <- grep("#", names(subdatas_spat), value = TRUE)


eu_spat1 = spatFilt.bms(X.data = subdatas_spat[,!names(subdatas_spat) %in% c("CEE", interaction)], WList = WL_eu, 
                           burn = 3e+06,iter = 10e+06,
                           nmodel=100, mcmc="bd", g="BRIC", 
                           mprior="random", user.int = TRUE)

eu_spat2 = spatFilt.bms(X.data = subdatas_spat[,!names(subdatas_spat) %in% interaction], WList = WL_eu, 
                           burn = 3e+06,iter = 10e+06,
                           nmodel=100, mcmc="bd", g="BRIC", 
                           mprior="random", user.int = TRUE)

eu_spat3 = spatFilt.bms(X.data = subdatas_spat, WList = WL_eu, 
                           burn = 3e+06,iter = 10e+06,
                           nmodel=100, mcmc="bd.int", g="BRIC", 
                           mprior="random", user.int = TRUE)

### SAVING
rm(list = setdiff(ls(), c("eu_base1", "eu_base2", "eu_base3",
                          "eu_fix1", "eu_fix2",
                          "eu_spat1", "eu_spat2", "eu_spat3")))
save.image(file = "04_final-output/Models-eu.RData")
rm(list = ls())
