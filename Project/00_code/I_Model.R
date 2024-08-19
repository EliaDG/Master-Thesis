getwd()

#DEPENDENCIES
source("00_code/__packages.R")
source("00_code/__functions.R")

#LOADING DATA
dataset1 <- readRDS("03_final-input/dataset_amelia.rds")
dataset2 <- readRDS("03_final-input/dataset_mice.rds")

datas_complete <- dataset1 %>%
  mutate(NUTS_Year = paste0(NUTS, substr(as.character(Year), nchar(as.character(Year)) - 1, nchar(as.character(Year)))),
         "CEE#Capital" = CEE*Capital,
         "Candidates#Capital" = Candidates*Capital) %>% 
  column_to_rownames(var = "NUTS_Year") %>%
  select(-c(Name, Year, NUTS, 50:85)) %>% 
  select(GDP_growth, GDP_capita, everything())

datas_intermediate <- datas_complete %>% 
  select(-c("CEE#Capital", "Candidates#Capital"))

datas_base <- datas_complete %>% 
  select(-c(CEE, Candidates, "CEE#Capital", "Candidates#Capital"))

mfls1 = bms(datas_base, burn=2000000, iter=3000000, g="BRIC", mprior="random", mcmc="bd", user.int=FALSE)
mfls2 = bms(datas_intermediate, burn=2000000, iter=3000000, g="BRIC", mprior="random", mcmc="bd", user.int=FALSE)
mfls3 = bms(datas_complete, burn=2000000, iter=3000000, g="BRIC", mprior="random", mcmc="bd.int", user.int=FALSE)

coef(mfls1,exact=TRUE)
coef(mfls2,exact=TRUE)
coef(mfls3,exact=TRUE)
