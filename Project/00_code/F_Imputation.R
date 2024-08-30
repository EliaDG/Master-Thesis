getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- read_csv("03_final-input/dataset.csv")
glimpse(dataset)
summary(dataset)

#Transformation --------------
# summary(dataset$Pop_growth)
# hist(dataset$Pop_growth,
#      main = "Histogram of Your Variable",
#      xlab = "Your Variable",
#      ylab = "Frequency",
#      col = "lightblue",
#      border = "black")

data <- dataset %>%
  mutate(across(starts_with("Prodx") | Labor_Productivity_abs | Wage_EUR | GDP_capita, log)) %>%
  rename(Labor_Prodx = Labor_Productivity_abs) %>% 
  as_data_frame()

#Imputation ----------
doParallel::registerDoParallel()
data_amelia <- amelia(data, m = 5, ts = "Year", cs = "NUTS", polytime = 1,
                      noms = c("Candidates", "CEE", "Capital", "Coastal", "Island", "Objective_1", "Euro"),
                      idvars = c("Name", "Country"))

#plot(data_amelia)

medians_data_amelia <- list()
for (variable in names(data_amelia$imputations[[1]])) {
  imputed_values <- sapply(data_amelia$imputations, function(x) x[[variable]])
  median_value <- apply(imputed_values, 1, median)
  medians_data_amelia[[variable]] <- median_value
}
medians_data_amelia <- as.data.frame(medians_data_amelia)

#Mice process
predictormatrix<-quickpred(data,
                           include = c("GDP_growth", "Labor_Prodx", "Wage_EUR", 
                                       "Activity_rate", "NEET_share", "Life_exp", 
                                       "Fertility_rate", "Pop_edu_1", "Pop_edu_2", 
                                       "Pop_edu_3", "inv_rate", "GVA_primary", 
                                       "GVA_services", "GVA_public", "GFCF_share", 
                                       "GDP_capita", "Pop_growth", "emp_rate", 
                                       "unemp_rate", "Migration_rate", 
                                       "Candidates", "CEE", "Capital", "Coastal", 
                                       "Island", "Objective_1", "Euro", "Output_density",
                                       "Employment_density", "Population_density", "Dist_BRUX"),
                           exclude = c("NUTS", "Name", "Country", "Year"),
                           mincor = 0.7)

doParallel::registerDoParallel()
data_mice <- mice(data, m = 5,
                  predictorMatrix = predictormatrix,
                  method = 'pmm', maxit = 30, seed = 1105)

imputed_datasets <- lapply(1:5, function(i) complete(data_mice, i))
imputed_array <- array(unlist(imputed_datasets), 
                       dim = c(nrow(imputed_datasets[[1]]), 
                               ncol(imputed_datasets[[1]]), 
                               5))
medians_imputed <- apply(imputed_array, c(1, 2), median)
medians_imputed <- as.data.frame(medians_imputed)
names(medians_imputed) <- names(data)

medians_data_mice <- data %>% 
  as.data.frame()
missing_indices <- is.na(data)
medians_data_mice[missing_indices] <- medians_imputed[missing_indices]

columns_to_convert <- c("Labor_Prodx", "Activity_rate", "NEET_share", "Life_exp", 
                        "Fertility_rate", "Pop_edu_1", "Pop_edu_2", "Pop_edu_3", 
                        "emp_rate", "unemp_rate", "GVA_services", "Employment_density")
medians_data_mice <- medians_data_mice %>%
  mutate_at(vars(one_of(columns_to_convert)), as.numeric)

#SAVING -----------
write.csv(medians_data_amelia, file = here("03_final-input", "dataset_amelia.csv"), row.names = FALSE)
write.csv(medians_data_mice, file = here("03_final-input", "dataset_mice.csv"), row.names = FALSE)

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