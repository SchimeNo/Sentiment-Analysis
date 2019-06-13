####0. Libraries and directories####
pacman::p_load(rstudioapi, dplyr, caret, doParallel, 
               plotly, corrplot, corrr)

#setting up directory
current_path=getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
list.files("datasets/")

#Loading the data
samsung<- read.csv("./datasets/galaxy_smallmatrix_labeled_9d.csv")
iphone<- read.csv("./datasets/iphone_smallmatrix_labeled_9d.csv")
matrix <- read.csv("./datasets/LargeMatrix.csv")

#Setting up Parallel processing
detectCores() 
cl <- makeCluster(2)
registerDoParallel(cl)
getDoParWorkers() 

####1.Data Exploration####
colnames(iphone)
plot_ly(iphone, x= iphone$iphonesentiment, type='histogram')
#missing data
which(is.na(iphone)==FALSE)


####2.Feature Selection####
#PRERDICTED VARIABLE iphonesentiment

#correlation Matrix
corrIPHONE<-cor(iphone)

#high correlated variables
corr_names<-findCorrelation(corrIPHONE, cutoff = .8, exact = TRUE, names = TRUE)
corr_high<- cor(iphone[corr_names])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#FE9929"))
corrplot(corr_high, method = "color", col = col(200), order = "hclust",diag = FALSE)

#correlation for only iphonesentiment
iphonecorr<-as.data.frame(corrIPHONE[,59])

stopCluster(cl)