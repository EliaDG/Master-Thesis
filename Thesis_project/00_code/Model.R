getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset <- readRDS("03_final-input/dataset.rds")
matrix <- readRDS("03_final-input/idw_matrix.rds")
colnames(dataset) <- make.names(colnames(dataset))
glimpse(dataset)
glimpse(matrix)

#Analysis
data <- dataset %>% 
  st_set_geometry(NULL) %>%
  select(-NUTS, -Name) %>%
  select_if(~!is.factor(.))

data_mice <- mice(data, m = 5, method = 'pmm', maxit = 30, seed = 1105)
completed_data <- complete(data_imputed)
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

data_knn <- kNN(data, k = 5)
data_rf <- missForest(data)
