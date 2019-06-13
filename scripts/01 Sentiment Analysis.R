####0. Libraries and directories####
pacman::p_load(rstudioapi, dplyr, caret, ggplot2)

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
