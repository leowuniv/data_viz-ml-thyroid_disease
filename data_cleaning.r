#---
title: "Data_Cleaning_Stats_20_Project"
output: html_document
---

#importing datasets
```{r}
allhypo <- read.csv("allhypo.data", header = TRUE) #import of allhypo.data file
allbp <- read.csv("allhypo.data", header = TRUE)
allrep <- read.csv("allrep.data", header = TRUE)
dis <- read.csv("dis.data", header = TRUE)
hypothyroid <- read.csv("hypothyroid.data", header = TRUE)
new_thyroid <- read.csv("new-thyroid.data", header = TRUE) 
sick_euthyroid <- read.csv("sick-euthyroid.data", header = TRUE)
sick <- read.csv("sick.data", header = TRUE)
thyroid0387 <- read.csv("thyroid0387.data", header = TRUE)
```
