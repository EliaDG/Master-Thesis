getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")
dataset_mice <- read_csv("03_final-input/dataset_mice.csv")
geom <- readRDS("03_final-input/geometries.rds")

# Running test on subset basic
datas_base <- dataset_amelia %>%
  mutate(NUTS_Year = paste0(NUTS , "_", as.character(Year))) %>% 
  column_to_rownames(var = "NUTS_Year") %>%
  select(-c(Name, Year, NUTS, 35:70,
            #Because still to double check real/nominal nature
            Wage_EUR, GFCF_share, Labor_Prodx))

# cor_matrix <- cor(datas_base)
# corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", diag = FALSE)
# high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.8)
# datas_uncorr <- select(datas_base, -high_corr_columns)

doParallel::registerDoParallel()
mfls_base = bms(datas_base, burn=2000000, iter=3000000,
                g="BRIC", mprior="random", mcmc="bd", force.full.ols = TRUE, 
                user.int=TRUE)
coef(mfls_base, exact=TRUE)

# Running test fixed effects
datas_fix <- dataset_amelia %>%
  mutate(NUTS_Year = paste0(NUTS , "_", as.character(Year))) %>% 
  column_to_rownames(var = "NUTS_Year") %>%
  select(-c(Name, Year, NUTS, Albania,
            #Because still to double check real/nominal nature
            Wage_EUR, GFCF_share, Labor_Prodx))

doParallel::registerDoParallel()
mfls_fix = bms(datas_fix, burn=2000000, iter=3000000, g="BRIC", mprior="random",
               mcmc="bd", force.full.ols = TRUE, user.int= TRUE)
coef(mfls_fix, exact=TRUE)
