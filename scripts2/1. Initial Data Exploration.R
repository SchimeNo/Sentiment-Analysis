p_load(rstudioapi, dplyr, ggplot2, plotly, caret, corrplot, GGally,
       doParallel, tidyverse, e1071, randomForest, caTools)

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("datasets/")

### ---- Import Small Matrices ----
iphone.small.matrix <- read.csv("./datasets/iphone_smallmatrix_labeled_9d.csv")
galaxy.small.matrix <-  read.csv("./datasets/galaxy_smallmatrix_labeled_9d.csv")
matrix <- read.csv("./datasets/LargeMatrix.csv")

#Setting up Parallel processing
detectCores() 
cl <- makeCluster(2)
registerDoParallel(cl)
getDoParWorkers() 

### ---- Data Exploration ----
## Distribution among the sentiment ratings
iphone.small.matrix %>% 
  filter(iphone != 0) %>% 
  group_by(iphonesentiment) 

galaxy.small.matrix %>% 
  filter(samsunggalaxy != 0) %>% 
  group_by(galaxysentiment) 

## Detect NA's
any(is.na(iphone.small.matrix)) # Result = 0
any(is.na(galaxy.small.matrix)) # Result = 0

## Inspect the data
summary(iphone.small.matrix)
str(iphone.small.matrix)

summary(galaxy.small.matrix)
str(galaxy.small.matrix)

### ---- Feature Selection ----
## Removing rows with no iphone and galaxy observations
# Column 1-5 represents the number of instances that type of phone mentioned in a webpage
iphone.small.matrix %>%
  filter(iphone != 0) %>% 
  select(starts_with("ios"), starts_with("iphone")) -> iphone.df

galaxy.small.matrix %>%
  filter(samsunggalaxy != 0) %>% 
  select(starts_with("google"), starts_with("samsung"), starts_with("galaxy")) -> galaxy.df

## Dependent variable visualizations
plot_ly(iphone.df, x= ~iphone.df$iphonesentiment, type='histogram')
plot_ly(galaxy.df, x= ~galaxy.df$galaxysentiment, type='histogram')

## Increase max print 
options(max.print = 1000000)

## Check correlations
cor(iphone.df)
ggcorr(iphone.df)
cor(galaxy.df)
ggcorr(galaxy.df)

### ---- Preprocessing ----
## Inspect the data
summary(iphone.df)
str(iphone.df)

summary(galaxy.df)
str(galaxy.df)

iphone.df$iphonesentiment <- as.factor(iphone.df$iphonesentiment)
galaxy.df$galaxysentiment <- as.factor(galaxy.df$galaxysentiment)

### ---- Recursive Feature Elimination ----
set.seed(123)
iphone.sample <- iphone.df[sample(1:nrow(iphone.df),
                                  1000, 
                                  replace = FALSE), ]

## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 4,
                   verbose = FALSE)

## Use rfe and omit the response variable (attribute 15 iphonesentiment & galaxysentiment) 
system.time(rfeResults1 <- rfe(iphone.sample[,1:14], 
                   iphone.sample$iphonesentiment, 
                   sizes = (1:14), 
                   rfeControl = ctrl))

rfeResults2 <- rfe(galaxy.df[,1:14], 
                   galaxy.df$galaxysentiment, 
                   sizes = (1:14), 
                   rfeControl = ctrl)

## Get results
rfeResults1
predictors(rfeResults1)
rfeResults2
predictors(rfeResults2)

## Plot results
plot(rfeResults1, type=c("g", "o"))
plot(rfeResults2, type=c("g", "o"))

## Create new data set with rfe recommended features
iphoneRFE <- iphone.df[,predictors(rfeResults1)]
galaxyRFE <- galaxy.df[,predictors(rfeResults2)]

## Add the dependent variable to iphoneRFE & galaxyRFE
iphoneRFE$iphonesentiment <- iphone.df$iphonesentiment
galaxyRFE$galaxysentiment <- galaxy.df$galaxysentiment

## Review outcome
str(iphoneRFE)
str(galaxyRFE)

### ---- Rename Levels of Factor ----
## 6 level factor
iphoneRFE %>%
  mutate(
    iphone.sentiment = 
      case_when(iphonesentiment %in% "0" ~ "VN",
                iphonesentiment %in% "1" ~ "N",
                iphonesentiment %in% "2" ~ "SN",
                iphonesentiment %in% "3" ~ "SP",
                iphonesentiment %in% "4" ~ "P",
                iphonesentiment %in% "5" ~ "VP")) -> iphoneRFE2

iphoneRFE2$iphonesentiment <- NULL
names(iphoneRFE2)[names(iphoneRFE2) == "iphone.sentiment"] <- "iphonesentiment"

galaxyRFE %>%
  mutate(
    galaxy.sentiment = 
      case_when(galaxysentiment %in% "0" ~ "VN",
                galaxysentiment %in% "1" ~ "N",
                galaxysentiment %in% "2" ~ "SN",
                galaxysentiment %in% "3" ~ "SP",
                galaxysentiment %in% "4" ~ "P",
                galaxysentiment %in% "5" ~ "VP")) -> galaxyRFE2

galaxyRFE2$galaxysentiment <- NULL
names(galaxyRFE2)[names(galaxyRFE2) == "galaxy.sentiment"] <- "galaxysentiment"

### ---- Save Datasets for Modelization ----
saveRDS(iphoneRFE2, file = "./datasets/iphone_dataframe.rds")
saveRDS(galaxyRFE2, file = "./datasets/galaxy_dataframe.rds")

## Stop Cluster
stopCluster(cl)
