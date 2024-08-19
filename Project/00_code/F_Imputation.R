getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- readRDS("03_final-input/dataset.rds")
glimpse(dataset)
summary(dataset)

#Transformation --------------
summary(dataset$Migration_rate)
# hist(dataset$Migration_rate,
#      main = "Histogram of Your Variable",
#      xlab = "Your Variable",
#      ylab = "Frequency",
#      col = "lightblue",
#      border = "black")

data <- dataset %>%
  mutate(across(starts_with("Prodx") | Labor_Productivity_abs | Wage_EUR | GDP_capita, log1p)) %>%
  rename(Labor_Prodx = Labor_Productivity_abs) %>%
  as.data.frame() %>% 
  select(-c(50:85))

data_country <- dataset %>% 
  select(c(NUTS, Year, 50:85))

#Imputation ----------
doParallel::registerDoParallel()
data_amelia <- amelia(data, m = 5, ts = "Year", cs = "NUTS", polytime = 1,
                      noms = c("Candidates", "CEE", "Capital", "Coastal", "Island", "Objective_1", "Euro"),
                      idvars = "Name")

#plot(data_amelia)

medians_data_amelia <- list()
for (variable in names(data_amelia$imputations[[1]])) {
  imputed_values <- sapply(data_amelia$imputations, function(x) x[[variable]])
  median_value <- apply(imputed_values, 1, median)
  medians_data_amelia[[variable]] <- median_value
}
medians_data_amelia <- as.data.frame(medians_data_amelia)

predictormatrix<-quickpred(data,
                           include = c("Pop_growth", "Labor_Prodx", "Wage_EUR", "GDP_capita", 
                                       "Activity_rate", "NEET_share", "Life_exp", "Fertility_rate", 
                                       "Pop_edu_1", "Pop_edu_2", "Pop_edu_3", "Employment_rate", 
                                       "Unemployment_rate", "Migration_rate" ,"Prodx_A", "Prodx_B.E", "Prodx_F", 
                                       "Prodx_G.I", "Prodx_J", "Prodx_K", "Prodx_L.M.N", 
                                       "Prodx_O.Q", "Prodx_R.U", "inv_rate", "GFCF_share", 
                                       "GVA_A", "GVA_B.E", "GVA_F", "GVA_G.I", 
                                       "GVA_J", "GVA_K", "GVA_L.M.N", "GVA_O.Q", 
                                       "GVA_R.U", "Output_density", 
                                       "Employment_density", "Population_density", "Dist_BRUX",
                                       "Candidates", "CEE", "Capital", "Coastal", "Island", "Objective_1", "Euro"),
                           exclude = c("NUTS", "Name", "Year"),
                           mincor = 0.6)

doParallel::registerDoParallel()
data_mice <- mice(data, m = 5,
                  predictorMatrix = predictormatrix,
                  method = 'pmm', maxit = 30, seed = 1105)
# densityplot(data_mice)
# plot(data_mice) #, which = "Prodx_J"

imputed_datasets <- lapply(1:5, function(i) complete(data_mice, i))
imputed_array <- array(unlist(imputed_datasets), 
                       dim = c(nrow(imputed_datasets[[1]]), 
                               ncol(imputed_datasets[[1]]), 
                               5))
medians_imputed <- apply(imputed_array, c(1, 2), median)
medians_imputed <- as.data.frame(medians_imputed)
names(medians_imputed) <- names(data)

medians_data_mice <- data
missing_indices <- is.na(data)
medians_data_mice[missing_indices] <- medians_imputed[missing_indices]
columns_to_convert <- c("Labor_Prodx", "Activity_rate", "NEET_share", "Life_exp", 
                        "Fertility_rate", "Pop_edu_1", "Pop_edu_2", "Pop_edu_3", 
                        "Employment_rate", "Unemployment_rate", "Prodx_A", 
                        "Prodx_B.E", "Prodx_F", "Prodx_G.I", "Prodx_J", "Prodx_K", 
                        "Prodx_L.M.N", "Prodx_O.Q", "Prodx_R.U", "GVA_K", 
                        "Employment_density")

# Convert the specified columns to numeric
medians_data_mice <- medians_data_mice %>%
  mutate_at(vars(one_of(columns_to_convert)), as.numeric)

dataset_amelia <- medians_data_amelia %>% 
  full_join(data_country)
dataset_mice <- medians_data_mice %>% 
  full_join(data_country)

#SAVING -----------
saveRDS(dataset_amelia, "03_final-input/dataset_amelia.rds")
saveRDS(dataset_mice, "03_final-input/dataset_mice.rds")

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
#
# data_mean <- data_new
# for(i in 1:nrow(data_mean)){
#   for(j in 1:ncol(data_mean)) {
#     data_mean[i,j] <- mean(c(complete(data_mice,1)[i,j],
#                              complete(data_mice,2)[i,j],
#                              complete(data_mice,3)[i,j],
#                              complete(data_mice,4)[i,j],
#                              complete(data_mice,5)[i,j]))
#   }
# }