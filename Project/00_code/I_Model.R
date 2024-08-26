getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")
dataset_mice <- read_csv("03_final-input/dataset_mice.csv")
geom <- readRDS("03_final-input/geometries.rds")
W <- readRDS("03_final-input/idw_list.rds")

# Running test on subset basic
datas_base <- dataset_amelia %>%
  mutate(NUTS_Year = paste0(NUTS , "_", as.character(Year))) %>% 
  column_to_rownames(var = "NUTS_Year") %>%
  select(-c(Name, Year, NUTS, 35:70,
            #Because still to double check real/nominal nature
            Wage_EUR, GFCF_share, Labor_Prodx, 
            #High correlation:
            Objective_1, Activity_rate))

cor_matrix <- cor(datas_base)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", diag = FALSE)
high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.7)
datas_corr <- select(datas_base, high_corr_columns)

doParallel::registerDoParallel()
mfls_base = bms(datas_base, burn=2000000, iter=3000000,
                g="BRIC", mprior="random", mcmc="bd", force.full.ols = TRUE, 
                user.int=TRUE)
coef(mfls_base, exact=TRUE)
estimates.bma(mfls_base, condi.coef = FALSE)

# Running test fixed effects
datas_fix <- dataset_amelia %>%
  mutate(NUTS_Year = paste0(NUTS , "_", as.character(Year))) %>% 
  column_to_rownames(var = "NUTS_Year") %>%
  select(-c(Name, Year, NUTS, Albania,
            #Because still to double check real/nominal nature
            Wage_EUR, GFCF_share, Labor_Prodx))

doParallel::registerDoParallel()
mfls_fix = bms(datas_fix, burn=2000000, iter=3000000, g="BRIC", mprior="random",
               mcmc="bd", force.full.ols = TRUE, user.int= FALSE)
coef(mfls_fix, exact=TRUE)

# Running Spatial autoregressive
datas_base_mat <- as.matrix(datas_base)
mfls_spat = spatFilt.bms(X.data = datas_base_mat, WList = W, burn = 2000000,iter = 3000000,
                         nmodel=100, mcmc="bd", g="BRIC", mprior="random", user.int = TRUE)
coef(mfls_base, exact=TRUE)