getwd()
set.seed(1105)

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#Loading Data
subdataset_amelia <- read_csv("03_final-input/dataset_amelia.csv") %>%
  select(-GDP_growth)
GDP_capita_raw <- read_csv("03_final-input/dataset.csv") %>%
  select(NUTS, Year, GDP_capita)
W <- readRDS("03_final-input/idw.rds")
idW <- W
idW$neighbours <- rep(W$neighbours, each = 2);
idW$weights <- rep(W$weights, each = 2)

attr(idW$neighbours, "class") <- "nb"
attr(idW$neighbours, "region.id") <- rep(attr(W$neighbours, "region.id"), each = 2)
attr(idW$neighbours, "sym") <- TRUE
attr(idW$neighbours, "call") <- attr(W$neighbours, "call")

# HOT ONE ENCODING -----
GDP_decade <- GDP_capita_raw %>%
  pivot_wider(names_from = Year,
              values_from = GDP_capita) %>%
  mutate(`2009` = (`2014`-`2009`)/`2009`,
         `2015` = (`2019`-`2015`)/`2015`) %>%
  select(NUTS, `2009`, `2015`) %>%
  pivot_longer(cols = -"NUTS",
               names_to = "Year",
               values_to = "GDP_growth") %>% 
  mutate(Year = as.numeric(Year))

subdata <- inner_join(subdataset_amelia, GDP_decade, by = c("NUTS", "Year")) %>%
  select(NUTS, Name, Country, GDP_growth, everything()) %>%
  mutate(Country = as.factor(Country),
         Year = as.factor(Year))

country_encoded <- model.matrix(~ Country- 1, data = subdata)[, -which(levels(subdata$Country) == "France")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))
Y_period <- model.matrix(~ Year- 1, data = subdata)[, -which(levels(subdata$Year) == "2009")]

subdata_encoded <- cbind(subdata, country_encoded, Y_period)
subdata_encoded$Country <- NULL
subdata_encoded$Year <- NULL
colnames(subdata_encoded) <- make.names(colnames(subdata_encoded))

CF <- c("C_Albania", "C_Austria", "C_Belgium", "C_Bosnia.and.Herzegovina", "C_Bulgaria", 
        "C_Croatia", "C_Cyprus", "C_Czech.Republic", "C_Denmark", "C_Estonia", "C_Finland", 
        "C_Germany", "C_Greece", "C_Hungary", "C_Ireland", "C_Italy", "C_Kosovo", 
        "C_Latvia", "C_Lithuania", "C_Luxembourg", "C_Malta", "C_Moldova", "C_Montenegro", 
        "C_Netherlands", "C_North.Macedonia", "C_Poland", "C_Portugal", "C_Romania", 
        "C_Serbia", "C_Slovakia", "C_Slovenia", "C_Spain", "C_Sweden", "C_Turkey", "C_United.Kingdom")
YF <- "Y_period"

TF <- c(CF, YF)

# TEST: BASE MODEL -------
subdatas_base <- subdata_encoded %>%
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
  select(-c(Name, NUTS, starts_with("C_"), Wage_EUR, Coastal, Eurozone))
interaction <- grep("#", names(subdatas_base), value = TRUE)

sub_base1 = bms(subdatas_base[,!names(subdatas_base) %in% c("CEE", "Candidates", interaction)], burn=3e+06, iter=10e+06,
               g="BRIC", mprior="random", mcmc="bd",
               force.full.ols = TRUE, user.int=TRUE,
               fixed.reg = YF)

sub_base2 = bms(subdatas_base[,!names(subdatas_base) %in% interaction], burn=3e+06, iter=10e+06,
                g="BRIC", mprior="random", mcmc="bd",
                force.full.ols = TRUE, user.int=TRUE,
                fixed.reg = YF)

sub_base3 = bms(subdatas_base, burn=3e+06, iter=10e+06,
               g="BRIC", mprior="random", mcmc="bd.int",
               force.full.ols = TRUE, user.int=TRUE)

#TEST: COUNTRY FIXED EFFECTS -------
subdatas_fix <- subdata_encoded %>%
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
  select(-c(Name, NUTS, Candidates, CEE, Wage_EUR, Coastal, Eurozone))
interaction <- grep("#", names(subdatas_fix), value = TRUE)

sub_fix1 = bms(subdatas_fix[,!names(subdatas_fix) %in% interaction], burn=3e+06, iter=10e+06,
               g="BRIC", mprior="random", mcmc="bd",
               force.full.ols = TRUE, user.int= TRUE,
               fixed.reg = TF)

sub_fix2 = bms(subdatas_fix, burn=3e+06, iter=10e+06,
              g="BRIC", mprior="random", mcmc="bd",
              force.full.ols = TRUE, user.int= TRUE,
              fixed.reg = TF)

# TEST: SAR+BMA -----
WL_panel <- readRDS("03_final-input/WL_5.rds")

subdatas_spat <- subdata_encoded %>%
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
  select(-c(Name, NUTS, starts_with("C_"), Wage_EUR, Coastal, Eurozone))
interaction <- grep("#", names(subdatas_spat), value = TRUE)

sub_spat1 = spatFilt.bms(X.data = subdatas_spat[,!names(subdatas_spat) %in% c("CEE", "Candidates", interaction)], WList = WL_panel, 
                          burn = 3e+06,iter = 10e+06,
                          nmodel=100, mcmc="bd", g="BRIC", 
                          mprior="random", user.int = TRUE)

sub_spat2 = spatFilt.bms(X.data = subdatas_spat[,!names(subdatas_spat) %in% interaction], WList = WL_panel, 
                          burn = 3e+06,iter = 10e+06,
                          nmodel=100, mcmc="bd", g="BRIC", 
                          mprior="random", user.int = TRUE)

sub_spat3 = spatFilt.bms(X.data = subdatas_spat, WList = WL_panel, 
                          burn = 3e+06,iter = 10e+06,
                          nmodel=100, mcmc="bd.int", g="BRIC", 
                          mprior="random", user.int = TRUE)

### SAVING
rm(list = setdiff(ls(), c("sub_base1", "sub_base2", "sub_base3",
                          "sub_fix1", "sub_fix2",
                          "sub_spat1", "sub_spat2", "sub_spat3")))
save.image(file = "04_final-output/Models-panel.RData")
