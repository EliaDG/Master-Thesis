getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- readRDS("03_final-input/dataset.rds")
glimpse(dataset)
colnames(dataset) <- make.names(colnames(dataset))

#Transformation
summary(dataset$Migration_abs)
hist(dataset$Migration_abs,
     main = "Histogram of Your Variable", 
     xlab = "Your Variable", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")

data <- dataset %>% 
  st_set_geometry(NULL) %>%
  mutate(across(starts_with("Prodx") | Labor_Productivity_abs | Wage_EUR | GDP_capita, log1p),
         Migration = sign(Migration_abs) * abs(Migration_abs)^(1/3),
         Migration = as.numeric(scale(Migration))) %>%
  select(-Migration_abs) %>%
  rename(Labor_Prodx = Labor_Productivity_abs) %>%
  as.data.frame()

#Imputation
data_amelia <- amelia(data, m = 5, ts = "Year", cs = "NUTS", polytime = 1,
                      noms = c("Subregion","Capital","Coastal","Island","Beneficiary","EU_Member"),
                      idvars = c("Name", "Country"))
plot(data_amelia)

medians_data_amelia <- list()
for (variable in names(data_amelia$imputations[[1]])) {
  imputed_values <- sapply(data_amelia$imputations, function(x) x[[variable]])
  median_value <- apply(imputed_values, 1, median, na.rm = TRUE)
  medians_data_amelia[[variable]] <- median_value
}
medians_data_amelia <- as.data.frame(medians_data_amelia)

data <- data %>% 
  select(-c("NUTS", "Name", "Country", "Subregion","Year",
            "Capital", "Coastal", "Island", "Beneficiary", "EU_Member"),)
predictormatrix<-quickpred(data,
                           include = c("Pop_growth", "Labor_Prodx", "Wage_EUR", "GDP_capita", 
                                       "Activity_rate", "NEET_share", "Life_exp", "Fertility_rate", 
                                       "Pop_edu_1", "Pop_edu_2", "Pop_edu_3", "Employment_rate", 
                                       "Unemployment_rate", "Prodx_A", "Prodx_B.E", "Prodx_F", 
                                       "Prodx_G.I", "Prodx_J", "Prodx_K", "Prodx_L.M.N", 
                                       "Prodx_O.Q", "Prodx_R.U", "inv_rate", "GFCF_share", 
                                       "GVA_A", "GVA_B.E", "GVA_F", "GVA_G.I", 
                                       "GVA_J", "GVA_K", "GVA_L.M.N", "GVA_O.Q", 
                                       "GVA_R.U", "Lon", "Lat", "Output_density", 
                                       "Employment_density", "Population_density", "Dist_BRUX","Migration"),
                           mincor = 0.6)

data_mice <- mice(data, m = 5,
                  predictorMatrix = predictormatrix,
                  method = 'pmm', maxit=15, seed = 1105)
# densityplot(data_mice)
# plot(data_mice) #, which = "Prodx_J"

data_median <- data
for(i in 1:nrow(data_median)){
  for(j in 1:ncol(data_median)) {
    data_median[i,j] <- median(c(complete(data_mice,1)[i,j],
                                 complete(data_mice,2)[i,j],
                                 complete(data_mice,3)[i,j],
                                 complete(data_mice,4)[i,j],
                                 complete(data_mice,5)[i,j]))
  }
}

# Appendix
# mean_data_amelia <- list()
# for (variable in names(data_amelia$imputations[[1]])) {
#   if (variable %in% c("Country", "Subregion", "Capital", "Coastal", "Island", "Beneficiary", "EU_Member", "Year", "NUTS")) {
#     next
#   }
#   imputed_values <- sapply(data_amelia$imputations, function(x) x[[variable]])
#   mean_value <- apply(imputed_values, 1, mean, na.rm = TRUE)
#   mean_data_amelia[[variable]] <- mean_value
# }
# mean_data_amelia <- as.data.frame(mean_data_amelia)