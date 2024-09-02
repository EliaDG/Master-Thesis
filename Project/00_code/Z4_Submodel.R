getwd()

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

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

Y_period <- model.matrix(~ Year- 1, data = subdata)[, -which(levels(subdata$Year) == "2014")]

subdata_encoded <- cbind(subdata, country_encoded, Y_period)
subdata_encoded$Country <- NULL
subdata_encoded$Year <- NULL

CF <- c("C_Albania", "C_Austria", "C_Belgium", "C_Bosnia and Herzegovina", "C_Bulgaria",
        "C_Croatia", "C_Cyprus", "C_Czech Republic", "C_Denmark", "C_Estonia", "C_Finland",
        "C_Germany", "C_Greece", "C_Hungary", "C_Ireland", "C_Italy", "C_Kosovo",
        "C_Latvia", "C_Lithuania", "C_Luxembourg", "C_Malta", "C_Moldova", "C_Montenegro",
        "C_Netherlands", "C_North Macedonia", "C_Poland", "C_Portugal", "C_Romania",
        "C_Serbia", "C_Slovakia", "C_Slovenia", "C_Spain", "C_Sweden", "C_Turkey", "C_United Kingdom")

YF <- "Y_period"

TF <- c(CF, YF)

# TEST: BASE MODEL
subdatas_base <- subdata_encoded %>%
  mutate(`CEE#Capital` = CEE*Capital,
         `CEE#GVA_services` = CEE*GVA_services,
         `CEE#GVA_public` = CEE*GVA_public,
         `CEE#GVA_primary` = CEE*GVA_primary,
         `Candidates#GVA_public` = Candidates*GVA_public,
         `Candidates#GVA_primary` = Candidates*GVA_primary,
         `Candidates#GVA_services` = Candidates*GVA_services,
         `CEE#Pop_edu_3` = CEE*Pop_edu_3,
         `Candidates#Pop_edu_3` = Candidates*Pop_edu_3) %>% 
  select(-c(Name, NUTS, starts_with("C_")))

sub_base = bms(subdatas_base, burn=2e+06, iter=3e+06,
                g="BRIC", mprior="random", mcmc="bd",
                force.full.ols = TRUE, user.int=TRUE,
                fixed.reg = YF)

sub_base1 = bms(subdatas_base, burn=2e+06, iter=3e+06,
               g="BRIC", mprior="random", mcmc="bd.int",
               force.full.ols = TRUE, user.int=TRUE)

subdatas_fix <- subdata_encoded %>%
  select(-c(Name, NUTS, Candidates, CEE))

sub_fix = bms(subdatas_fix, burn=2e+06, iter=3e+06,
               g="BRIC", mprior="random", mcmc="bd",
               force.full.ols = TRUE, user.int= TRUE,
               fixed.reg = TF)
