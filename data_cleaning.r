---
title: "Data_Cleaning_Stats_20_Project"
output: html_document
---

#importing datasets
```{r}
allhypo <- read.csv("allhypo.data", header = F) #import of allhypo.data file
allbp <- read.csv("allhypo.data", header = F)
allrep <- read.csv("allrep.data", header = F)
dis <- read.csv("dis.data", header = F)
hypothyroid <- read.csv("hypothyroid.data", header = F)
new_thyroid <- read.csv("new-thyroid.data", header = F) 
sick_euthyroid <- read.csv("sick-euthyroid.data", header = F)
sick <- read.csv("sick.data", header = F)
thyroid0387 <- read.csv("thyroid0387.data", header = F)
```

```{r}
head(allbp)
#drop column V28 because it contains almost all "?"
```


#rename columns for allbp
```{r}
names(allbp)[names(allbp) == "V1"] <- "Age"
names(allbp)[names(allbp) == "V2"] <- "Sex"
names(allbp)[names(allbp) == "V3"] <- "presc_thyroxine"
names(allbp)[names(allbp) == "V4"] <- "queried_why_on_thyroxine"
names(allbp)[names(allbp) == "V5"] <- "presc_anthyroid_meds"
names(allbp)[names(allbp) == "V6"] <- "sick"
names(allbp)[names(allbp) == "V7"] <- "pregnant"
names(allbp)[names(allbp) == "V8"] <- "thyroid_surgery"
names(allbp)[names(allbp) == "V9"] <- "radioactive_iodine_therapyI131"
names(allbp)[names(allbp) == "V10"] <- "query_hypothyroid"
names(allbp)[names(allbp) == "V11"] <- "query_hyperthyroid"
names(allbp)[names(allbp) == "V12"] <- "lithium"
names(allbp)[names(allbp) == "V13"] <- "goitre"
names(allbp)[names(allbp) == "V14"] <- "tumor"
names(allbp)[names(allbp) == "V15"] <- "hypopituitarism"
names(allbp)[names(allbp) == "V16"] <- "psych_condition"
names(allbp)[names(allbp) == "V17"] <- "TSH_measured"
names(allbp)[names(allbp) == "V18"] <- "TSH_reading"
names(allbp)[names(allbp) == "V19"] <- "T3_measured"
names(allbp)[names(allbp) == "V20"] <- "T3_measured"
names(allbp)[names(allbp) == "V21"] <- "T4_measured"
names(allbp)[names(allbp) == "V22"] <- "T4_reading"
names(allbp)[names(allbp) == "V23"] <- "thyrox_util_rate_T4U_measured"
names(allbp)[names(allbp) == "V24"] <- "thyrox_util_rate_T4U_reading"
names(allbp)[names(allbp) == "V25"] <- "FTI_measured"
names(allbp)[names(allbp) == "V26"] <- "FTI_reading"
names(allbp)[names(allbp) == "V27"] <- "ref_src"
## names(allbp)[names(allbp) == "V28"] <- "psych_condition" drop column 
allbp <- subset(allbp, select = -V28)
names(allbp)[names(allbp) == "V29"] <- "ThyroidClass"
names(allbp)[names(allbp) == "V30"] <- "record_id"
```

#now allhypo

```{r}
View(allhypo)
```

```{r}
names(allhypo)[names(allhypo) == "V1"] <- "Age"
names(allhypo)[names(allhypo) == "V2"] <- "Sex"
names(allhypo)[names(allhypo) == "V3"] <- "presc_thyroxine"
names(allhypo)[names(allhypo) == "V4"] <- "queried_why_on_thyroxine"
names(allhypo)[names(allhypo) == "V5"] <- "presc_anthyroid_meds"
names(allhypo)[names(allhypo) == "V6"] <- "sick"
names(allhypo)[names(allhypo) == "V7"] <- "pregnant"
names(allhypo)[names(allhypo) == "V8"] <- "thyroid_surgery"
names(allhypo)[names(allhypo) == "V9"] <- "radioactive_iodine_therapyI131"
names(allhypo)[names(allhypo) == "V10"] <- "query_hypothyroid"
names(allhypo)[names(allhypo) == "V11"] <- "query_hyperthyroid"
names(allhypo)[names(allhypo) == "V12"] <- "lithium"
names(allhypo)[names(allhypo) == "V13"] <- "goitre"
names(allhypo)[names(allhypo) == "V14"] <- "tumor"
names(allhypo)[names(allhypo) == "V15"] <- "hypopituitarism"
names(allhypo)[names(allhypo) == "V16"] <- "psych_condition"
names(allhypo)[names(allhypo) == "V17"] <- "TSH_measured"
names(allhypo)[names(allhypo) == "V18"] <- "TSH_reading"
names(allhypo)[names(allhypo) == "V19"] <- "T3_measured"
names(allhypo)[names(allhypo) == "V20"] <- "T3_measured"
names(allhypo)[names(allhypo) == "V21"] <- "T4_measured"
names(allhypo)[names(allhypo) == "V22"] <- "T4_reading"
names(allhypo)[names(allhypo) == "V23"] <- "thyrox_util_rate_T4U_measured"
names(allhypo)[names(allhypo) == "V24"] <- "thyrox_util_rate_T4U_reading"
names(allhypo)[names(allhypo) == "V25"] <- "FTI_measured"
names(allhypo)[names(allhypo) == "V26"] <- "FTI_reading"
names(allhypo)[names(allhypo) == "V27"] <- "ref_src"
## names(allhypo)[names(allhypo) == "V28"] drop column 
allhypo <- subset(allhypo, select = -V28)
names(allhypo)[names(allhypo) == "V29"] <- "ThyroidClass"
names(allhypo)[names(allhypo) == "V30"] <- "record_id"
```








