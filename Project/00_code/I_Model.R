getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset1 <- readRDS("03_final-input/dataset_amelia.rds")
dataset2 <- readRDS("03_final-input/dataset_mice.rds")
geom <- readRDS("03_final-input/geometries.rds")

# Running test on subset basic
datas_base <- dataset1 %>%
  mutate(NUTS_Year = paste0(NUTS , "_", as.character(Year))) %>% 
  column_to_rownames(var = "NUTS_Year") %>%
  select(-c(Name, Year, NUTS, 50:85, Unemployment_rate,
            #Because still to double check real/nominal nature
            Wage_EUR, contains("Prodx"), GFCF_share,
            #risk overfitting
            Island, Coastal, Objective_1))

#datas_mat <- as.matrix(datas_complete)

mfls_base = bms(datas_base, burn=2000000, iter=3000000, g="BRIC", mprior="random", mcmc="bd", user.int=FALSE)
coef(mfls_base, exact=TRUE)

# Running test fixed effects
datas_fix <- dataset1 %>%
  mutate(NUTS_Year = paste0(NUTS , "_", as.character(Year))) %>% 
  column_to_rownames(var = "NUTS_Year") %>%
  select(-c(Name, Year, NUTS, Unemployment_rate, France,
            #Because still to double check real/nominal nature
            Wage_EUR, contains("Prodx"), GFCF_share,
            #risk overfitting
            Island, Coastal, Objective_1))

#datas_mat <- as.matrix(datas_complete)

mfls_fix = bms(datas_run, burn=2000000, iter=3000000, g="BRIC", mprior="random", mcmc="bd", user.int=FALSE)
coef(mfls_fix, exact=TRUE)
