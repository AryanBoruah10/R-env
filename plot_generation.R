library(pheatmap)
library(ggrepel)
library(ggtext)
library(compositions)
library(igraph)
library(metap)
library(robumeta)
library(metafor)
library(dplyr)
library(effsize)
library(MASS)
library(gplots)
library(RColorBrewer)
library(sfsmisc)
library(pcaPP)
library(psych)
library(dendextend)
library(gplots)
library(RColorBrewer)
library(pROC)
library(randomForest)
library(gtools)
library(vegan)
library(ade4)
library(effsize)
library(ggplot2)
library(haven)
library(ppcor)
install.packages("class")
library(class)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("multtest")

#Library to unlock kNN and CrossTable model evaluation  
library(gmodels)
library(class)

#load iris dataset
iris_dataset <- iris

#Noarmalize all the numeric columns from 1:4
iris_n <- scale(iris_dataset[,1:4])

#Summary of the idividiual column in iris dataset
summary(iris_n[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])

# Create train and test dataset. In this case model, I took 80% as train dataset and 20% as test dataset
iris_train <- iris_n[1:120, ]
iris_test <- iris_n[121:150, ]

#Create the labels data frames which contains the species of flowers both for train and test dataset  
iris_train_labels <- iris_dataset[1:120, 5]
iris_test_labels <- iris_dataset[121:150, 5]

#Assess the percentage of each unique labels in the data set
round(prop.table(table(iris_dataset$Species)) * 100, digits = 1)

#run the k-nearest neighbor model to predict test labels of test dataset.   
p <- knn(train = iris_train, test = iris_test, cl = iris_train_labels, k=11)
p

#The test dataset that will be used to estimate the predictive accuracy of the model. 
#Evaluate how well the predicted classes in the iris_test_labels vector match up 
#with the known values in the kNN predicted labels
CrossTable(x = iris_test_labels, y = p, prop.chisq=FALSE)





#####################################
