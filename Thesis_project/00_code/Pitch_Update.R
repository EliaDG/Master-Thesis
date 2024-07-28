
dataset_complete_rds <- readRDS("03_final-input/dataset_complete.rds")


## Overview dataset: ----
ex1 <- dataset %>%
  filter(Country %in% c("Albania", "Bosnia and Herzegovina","Montenegro", "North Macedonia", "Turkey", "Moldova", "Kosovo", "Serbia"))
length(unique(ex1$NUTS))
ex2 <- dataset %>%
  filter(!Country %in% c("Albania","Bosnia and Herzegovina", "Montenegro", "North Macedonia", "Turkey", "Moldova", "Kosovo", "Serbia"))
length(unique(ex2$NUTS))

summary(dataset)
length(unique(dataset$NUTS))
total_observations <- nrow(dataset) * ncol(dataset)
total_NAs <- sum(is.na(dataset))
(total_NAs / total_observations) * 100

NAs_per_column <- colSums(is.na(dataset))
round((NAs_per_column / nrow(dataset)) * 100,2)