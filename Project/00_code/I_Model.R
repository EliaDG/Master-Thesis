getwd()

# DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

# LOADING DATA
dataset_amelia <- read_csv("03_final-input/dataset_amelia.csv")

# HOT ONE ENCODING -----
data <- dataset_amelia %>% 
  mutate( #NUTS_Year = paste0(sapply(NUTS, modify_NUTS) , "_", as.character(Year)),
         Year = as.factor(Year),
         Country = as.factor(Country))
  #column_to_rownames(var = "NUTS_Year")

country_encoded <- model.matrix(~ Country- 1, data = data)[, -which(levels(data$Country) == "France")]
colnames(country_encoded) <- gsub("^Country", "C_", colnames(country_encoded))

year_encoded <- model.matrix(~ Year- 1, data = data)[, -which(levels(data$Year) == "2009")]
colnames(year_encoded) <- gsub("^Year", "Y_", colnames(year_encoded))
data_encoded <- cbind(data, country_encoded, year_encoded)
data_encoded$Country <- NULL
data_encoded$Year <- NULL
colnames(data_encoded) <- make.names(colnames(data_encoded))

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
         `CEE#GVA_services` = CEE*GVA_services,
         `CEE#GVA_public` = CEE*GVA_public,
         `CEE#GVA_primary` = CEE*GVA_primary,
         `Candidates#GVA_public` = Candidates*GVA_public,
         `Candidates#GVA_primary` = Candidates*GVA_primary,
         `Candidates#GVA_services` = Candidates*GVA_services,
         `CEE#Pop_edu_3` = CEE*Pop_edu_3,
         `Candidates#Pop_edu_3` = Candidates*Pop_edu_3) %>% 
  select(-c(Name, NUTS, starts_with("C_")))

## Correlation Check
# cor_matrix <- cor(datas_base)
# corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", diag = FALSE)
# high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.7)
# datas_corr <- select(datas_base, high_corr_columns)

interaction <- grep("#", names(datas_base), value = TRUE)
mfls_base = bms(datas_base[,!names(datas_base) %in% c("CEE", "Island", "Capital", "Euro", "Objective_1", "Coastal", "Candidates", interaction)],
                burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd",
                user.int=TRUE, force.full.ols = TRUE, fixed.reg = YF)

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
  select(-c(Name, NUTS, Candidates, CEE))

mfls_fix = bms(datas_fix[,!names(datas_fix) %in% c("Capital", "Island", "Euro", "Objective_1", "Coastal")],
               burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd",
               user.int= TRUE, force.full.ols = TRUE, fixed.reg = TF)

mfls_fix1 = bms(datas_fix, burn=2e+06, iter=3e+06, g="BRIC", mprior="random", mcmc="bd", 
                user.int= TRUE, force.full.ols = TRUE, fixed.reg = TF)

# TEST: SAR+BMA -----
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

y <- as.data.frame(datas_spat[, 1, drop = F])
yFilt1 <- SpatialFiltering(datas_spat[, 1] ~ 1, ~-1, data = y,
                          nb = nb1, glist = idw1$weights, style = "B", ExactEV = FALSE)

WL <- list(Col0 = fitted(yFilt1))

mfls_spat = spatFilt.bms(X.data = datas_spat[,!names(datas_base) %in% c("CEE", "Island", "Capital", "Euro", "Objective_1", "Coastal", "Candidates", interaction)], WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)
mfls_spat1 = spatFilt.bms(X.data = datas_spat[,!names(datas_base) %in% c("CEE", "Candidates", interaction)], WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)
mfls_spat2 = spatFilt.bms(X.data = datas_spat[,!names(datas_base) %in% interaction], WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd", g="BRIC", 
                         mprior="random", user.int = TRUE)
mfls_spat3 = spatFilt.bms(X.data = datas_spat, WList = WL, 
                         burn = 2e+06,iter = 3e+06,
                         nmodel=100, mcmc="bd.int", g="BRIC", 
                         mprior="random", user.int = TRUE)

#SAVING
# rm(list = setdiff(ls(), c("cesee_base", "cesee_base1", "cesee_base2", "cesee_base3", 
#                           "cesee_fix", "cesee_fix1", 
#                           "mfls_base", "mfls_base1", "mfls_base2", "mfls_base3", 
#                           "mfls_fix", "mfls_fix1", 
#                           "sub_base", "sub_base1", "sub_fix")))
# save.image(file = "Models.RData")

                          