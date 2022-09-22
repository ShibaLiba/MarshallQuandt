title: "Assigment - kNN"
author:
  - Bram Jansen - Author
- Marshall Quandt - Reviewer
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_notebook:
  toc: true
toc_depth: 2
---
  
  ```{r}
library(tidyverse)
library(caret)
library(class)
library(readr)
```

## Business Understanding

Automatically identifying if the blood of a person is suitable for blood donation can improve the efficiency of blood donating and the detection of other diseases.

## Read data

```{r}
read_csv("datasets/KNN-hcvdat0.csv")
rawDF <- read_csv("datasets/KNN-hcvdat0.csv")
```

## Data understanding

```{r}
str(rawDF) #Basic information about the data frame
```

## Data preparation

```{r}
cleanDF <- rawDF [sample(1:nrow(rawDF)),] #randomize the data
cleanDF <- cleanDF[-1] #deleting the first row
head(cleanDF) #show the changes
```

```{r}
cntDiag <- table(cleanDF$Category) 
propDiag <- round(prop.table(cntDiag)* 100 , digits = 1)
cntDiag #show the total count for each category
propDiag #show the proportions for each category
```

```{r}
cleanDF$Sex <- factor(cleanDF$Sex, levels = c("m", "f"), labels = c("Male", "Female")) %>% relevel("Male","Female") #changing the type of the variable to a factor
```

```{r}
cleanDF$Category <- factor(cleanDF$Category, levels = c("0=Blood Donor", "0s=suspect Blood Donor", "1=Hepatitis", "2=Fibrosis", "3=Cirrhosis"), labels = c("Blood Donor", "suspect Blood Donor", "Hepatitis", "Fibrosis", "Cirrhosis")) %>% relevel("Blood Donor", "suspect Blood Donor", "Hepatitis", "Fibrosis", "Cirrhosis") #changing the type of the variable to a factor
```

```{r}
cleanDF[is.na(cleanDF)] <- 0 #remove the NA from the data to 0
```

```{r}
head(cleanDF, 10) #show data
```

```{r}
#measures the given characteristics on three measurements
summary(cleanDF[c("ALB", "ALP", "ALT", "AST", "BIL", "CHE", "CHOL", "CREA", "GGT", "PROT")]) 
```

```{r}
#re-scales all features to a standard range of values
normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x))) 
} 
testSet1 <- c(1:5)
testSet2 <- c(1:5) * 10
#testing the normalization
cat("testSet1:", testSet1, "\n")
cat("testSet2:", testSet2, "\n")
cat("Normalized testSet1:", normalize(testSet1), "\n")
cat("Normalized testSet2:", normalize(testSet2)) 
```

```{r}
#apply the function of normalization to the data
nCols <- dim(cleanDF)[2]
cleanDF_n <- sapply(4:nCols,
                    function(x) {
                      normalize(cleanDF[,x])
                    }) %>% as.data.frame() 
summary(cleanDF_n[c("ALB", "ALP", "ALT", "AST", "BIL", "CHE", "CHOL", "CREA", "GGT", "PROT")])
```

```{r}
#creating test data frame
trainDF_feat <- cleanDF_n[1:515, ]
testDF_feat <- cleanDF_n[516:615, ]
trainDF_labels <- cleanDF[1:515,  1]
testDF_labels <- cleanDF[516:615,  1]
```

## Data modelling

```{r}
cleanDF_test_pred <- knn(train = as.matrix(trainDF_feat), test = as.matrix(testDF_feat), cl = as.matrix(trainDF_labels), k = 21)
head(cleanDF_test_pred)
```

```{r}
confusionMatrix(cleanDF_test_pred, testDF_labels[[1]], positive = NULL, dnn = c("Prediction", "True"))
```

## Evaluation and Deployment

text and code here

reviewer adds suggestions for improving the model


##line 27 read_cvs in order to read the file

##line 40 must only remove one row, so must be -1 instead -2

##line 45 <- missing symbol 

## line 46 <- missing symbol

## line 60 <- is.na forgot to put a dot after is. 


#suggestions
##change headers to be more readable
##being able to display the relationship between gender vs most common disease
