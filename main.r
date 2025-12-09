# Merge all branches into the main file of the project

# --> Compiled all into .Rmd file (Submitted)

---
title: "Stats 20 - Group 67 DataViz Project : ml-thyroid_disease"
author: "Names/UIDs on BruinLearn"
date: "2025-12-08"
output: pdf_document
---

# FOR MORE INFORMATION ON DOCUMENTATION/CODE:

GITHUB: [https://github.com/leowuniv/data_viz-ml-thyroid_disease]

Report: Submitted via PDF on BruinLearn
Slides (DataFest): Submitted via PDF on BruinLearn
Dataset Files: Submitted as "group_67_datasets" on BruinLearn

```{r}
# ============================================

# Set your Working Directory:

#getwd() 
setwd("C:/Users/...") # Set your own WD

# ============================================
```

```{r}

# Menu Printout (Condensed Version)

print("▀█▀ █░█ █▄█ █▀█ █▀█ █ █▀▄   █░█ █ ▀█")
print("░█░ █▀█ ░█░ █▀▄ █▄█ █ █▄▀   ▀▄▀ █ █▄")
```

# Disclaimer: From what our team has evaluated, each dataset contains different variables within the Thyroid class mainly based on their file purpose for testing (i.e. "sick_euthyroid" tests for euthyroid sick syndrome (ESS)) but many attributes (around 29 are mostly the same set over all the databases) where they are mostly numeric or Boolean valued attributes.

```{r}
# [DATA CLEANING | STEP]

# ----------------------------------------

#importing datasets
allhypo <- read.csv("allhypo.data", header = F) #import of allhypo.data file
allhypo_test <- read.csv("allhypo.test", header = F)
allhyper <- read.csv("allhyper.data", header = F)
allhyper_test <- read.csv("allhyper.test", header = F)
allbp <- read.csv("allbp.data", header = F)
allrep <- read.csv("allrep.data", header = F)
dis <- read.csv("dis.data", header = F)
hypothyroid <- read.csv("hypothyroid.data", header = F)
new_thyroid <- read.csv("new-thyroid.data", header = F) 
sick_euthyroid <- read.csv("sick-euthyroid.data", header = F)
sick <- read.csv("sick.data", header = F)
thyroid0387 <- read.csv("thyroid0387.data", header = F)
ann_test <- read.csv("ann-test.data", header = F)
ann_train <- read.csv("ann-train.data", header = F)

# function to rename 30-column datasets as they all have the same column headers
rename_30col <- function(dataset){
  names(dataset)[names(dataset) == "V1"] <- "age"
  names(dataset)[names(dataset) == "V2"] <- "sex"
  names(dataset)[names(dataset) == "V3"] <- "presc_thyroxine"
  names(dataset)[names(dataset) == "V4"] <- "queried_why_on_thyroxine"
  names(dataset)[names(dataset) == "V5"] <- "presc_anthyroid_meds"
  names(dataset)[names(dataset) == "V6"] <- "sick"
  names(dataset)[names(dataset) == "V7"] <- "pregnant"
  names(dataset)[names(dataset) == "V8"] <- "thyroid_surgery"
  names(dataset)[names(dataset) == "V9"] <- "radioactive_iodine_therapyI131"
  names(dataset)[names(dataset) == "V10"] <- "query_hypothyroid"
  names(dataset)[names(dataset) == "V11"] <- "query_hyperthyroid"
  names(dataset)[names(dataset) == "V12"] <- "lithium"
  names(dataset)[names(dataset) == "V13"] <- "goitre"
  names(dataset)[names(dataset) == "V14"] <- "tumor"
  names(dataset)[names(dataset) == "V15"] <- "hypopituitarism"
  names(dataset)[names(dataset) == "V16"] <- "psych_condition"
  names(dataset)[names(dataset) == "V17"] <- "TSH_measured"
  names(dataset)[names(dataset) == "V18"] <- "TSH_reading"
  names(dataset)[names(dataset) == "V19"] <- "T3_measured"
  names(dataset)[names(dataset) == "V20"] <- "T3_reading"
  names(dataset)[names(dataset) == "V21"] <- "T4_measured"
  names(dataset)[names(dataset) == "V22"] <- "T4_reading"
  names(dataset)[names(dataset) == "V23"] <- "thyrox_util_rate_T4U_measured"
  names(dataset)[names(dataset) == "V24"] <- "thyrox_util_rate_T4U_reading"
  names(dataset)[names(dataset) == "V25"] <- "FTI_measured"
  names(dataset)[names(dataset) == "V26"] <- "FTI_reading"
  
  # drop columns 27 as all f values and 28 all ?
  dataset <- subset(dataset, select = -V27)
  dataset <- subset(dataset, select = -V28)
  
  names(dataset)[names(dataset) == "V29"] <- "ref_src"
  
  # split the strings in col30 using splitter ".|"
  # set fixed= TRUE since every element in the column has ".|" as the splitter
  # Link to source for strsplit() function: https://builtin.com/articles/strsplit
  # slightly different from the str_split() function from stringr package taught in lecture
  # as the R built in method does not have the pattern argument; instead uses argument split 
  split_col_30 <- strsplit(dataset$V30, split = ".|", fixed = TRUE)
  
  # apply simple lambda functions that get the proper split for the new columns
  # ThyroidClass -> first element of the split; character type
  # record_id -> second element of the split; numeric type (has to be converted from char first)
  ThyroidClass <- vapply(split_col_30, FUN = function(x) x[1], FUN.VALUE = character(1))
  record_id <- vapply(split_col_30, FUN = function(x) as.numeric(x[2]), FUN.VALUE = numeric(1))
  
  # drop column 30 
  dataset <- subset(dataset, select = -V30)
  
  # add new columns
  dataset$ThyroidClass <- ThyroidClass
  dataset$record_id <- record_id
  
  # return the updated names for the 30 column datasets
  return(dataset)
}

# function to rename 26-column datasets
rename_26col <- function(dataset){
  names(dataset)[names(dataset) == "V1"] <- "ThyroidClass"
  names(dataset)[names(dataset) == "V2"] <- "age"
  names(dataset)[names(dataset) == "V3"] <- "sex"
  names(dataset)[names(dataset) == "V4"] <- "presc_thyroxine"
  names(dataset)[names(dataset) == "V5"] <- "query_on_thyroxine"
  names(dataset)[names(dataset) == "V6"] <- "presc_anthyroid_meds"
  names(dataset)[names(dataset) == "V7"] <- "thyroid_surgery"
  names(dataset)[names(dataset) == "V8"] <- "query_hypothyroid"
  names(dataset)[names(dataset) == "V9"] <- "query_hyperthyroid"
  names(dataset)[names(dataset) == "V10"] <- "pregnant"
  names(dataset)[names(dataset) == "V11"] <- "sick"
  names(dataset)[names(dataset) == "V12"] <- "tumor"
  names(dataset)[names(dataset) == "V13"] <- "lithium"
  names(dataset)[names(dataset) == "V14"] <- "goitre"
  names(dataset)[names(dataset) == "V15"] <- "TSH_measured"
  names(dataset)[names(dataset) == "V16"] <- "TSH_reading"
  names(dataset)[names(dataset) == "V17"] <- "T3_measured"
  names(dataset)[names(dataset) == "V18"] <- "T3_reading"
  names(dataset)[names(dataset) == "V19"] <- "T4_measured"
  names(dataset)[names(dataset) == "V20"] <- "T4_reading"
  names(dataset)[names(dataset) == "V21"] <- "T4U_measured"
  names(dataset)[names(dataset) == "V22"] <- "T4U_reading"
  names(dataset)[names(dataset) == "V23"] <- "FTI_measured"
  names(dataset)[names(dataset) == "V24"] <- "FTI_reading"
  names(dataset)[names(dataset) == "V25"] <- "TBG_measured"
  names(dataset)[names(dataset) == "V26"] <- "TBG_reading"
  
  # return the updated names for the 26 column datasets
  return(dataset)
}

# rename 30-col datasets
allhypo <- rename_30col(allhypo)
allhypo_test <- rename_30col(allhypo_test)
allhyper <- rename_30col(allhyper)
allhyper_test <- rename_30col(allhyper_test)
allbp <- rename_30col(allbp)
allrep <- rename_30col(allrep)
dis <- rename_30col(dis)
sick <- rename_30col(sick)
thyroid0387 <- rename_30col(thyroid0387)

# rename 26-col datasets
sick_euthyroid <- rename_26col(sick_euthyroid)
hypothyroid <- rename_26col(hypothyroid)

# rename new_thyroid
names(new_thyroid)[names(new_thyroid) == "V1"] <- "ThyroidClass"
names(new_thyroid)[names(new_thyroid) == "V2"] <- "T3_resin_uptake"
names(new_thyroid)[names(new_thyroid) == "V3"] <- "total_serum_thyroxin"
names(new_thyroid)[names(new_thyroid) == "V4"] <- "total_serum_triiodothyronine"
names(new_thyroid)[names(new_thyroid) == "V5"] <- "basal_TSH"
names(new_thyroid)[names(new_thyroid) == "V6"] <- "TSH_diff"

#replace ? with NA
allhypo[allhypo == "?"] <- NA 
allhypo_test[allhypo_test == "?"] <- NA
allhyper[allhyper == "?"] <- NA 
allhyper_test[allhyper_test == "?"] <- NA
allbp[allbp == "?"] <- NA
allrep[allrep == "?"] <- NA
dis[dis == "?"] <- NA
hypothyroid[hypothyroid == "?"] <- NA
new_thyroid[new_thyroid == "?"] <- NA
sick_euthyroid[sick_euthyroid == "?"] <- NA
sick[sick == "?"] <- NA
thyroid0387[thyroid0387 == "?"] <- NA
ann_test[ann_test == "?"] <- NA
ann_train[ann_train == "?"] <- NA

#remove nas from the datasets. 
remove_nas_allhypo <- na.omit(allhypo)
remove_nas_allhypo_test <- na.omit(allhypo_test)
remove_nas_allhyper <- na.omit(allhyper)
remove_nas_allhyper_test <- na.omit(allhyper_test)
remove_nas_allbp <- na.omit(allbp)
remove_nas_allrep <- na.omit(allrep)
remove_nas_dis <- na.omit(dis)
remove_nas_hypothyroid <- na.omit(hypothyroid)
remove_nas_new_thyroid <- na.omit(new_thyroid)
remove_nas_sick_euthyroid <- na.omit(sick_euthyroid)
remove_nas_sick <- na.omit(sick)
remove_nas_thyroid0387 <- na.omit(thyroid0387)
remove_nas_ann_test <- na.omit(ann_test)
remove_nas_ann_train <- na.omit(ann_train)
```

```{r}
#ann_test
#ann_train

head(allhypo)
head(allhyper)

head(remove_nas_allhypo) # cleaned for NAs
head(remove_nas_allhyper) # cleaned for NAs
```

# Hypothyroidism vs Hyperthyroidism

According to the UCLA Geffen School of Medicine, they state in the article "Hypothyroidism vs Hyperthyroidism - What's The Difference?", that "hypothyroidism is an autoimmune condition called Hashimoto's Thyroiditis, where the immune system attacks the thyroid gland" and that "[hyperthyroidism]...is having an overactive thyroid gland or having too much thyroid hormone in the body” (Sievert, 3 - 4). Essentially, hypothyroidism (underactive - producing too little thyroid hormone) and hyperthyroidism (overactive - producing too much thyroid hormone) are common thyroid diseases that affect how a human's body functions which includes a variety of factors. These factors for hyperthyroidism include cons like weight loss, rapid heart rate recordings, anxiety, tremors, diarrhea and much more for instance while hypothyroidism has the opposite effects of hyperthyroidism which include but are not limited to weight gain, fatigue, and slow heart rate recordings. 

Link to source: https://medschool.ucla.edu/news-article/hypothyroidism-vs-hyperthyroidism-whats-the-difference

# Our main focus

Research Question: How do/which patients' attributes (i.e. age, sex, etc...) and hormone levels contribute to the association with thyroid disease diagnosis and sub-type (hypothyroid vs hyperthyroid) the most?

```{r}

# ============================================================

# [DATA VISUALIZATIONS/MENU | STEP]

# For Color Documentation: https://search.r-project.org/CRAN/refmans/insight/html/print_color.html [Add to Sources if plan to use]

# =====================================

# Installation of Packages

#install.packages("ggplot2")
#install.packages("tidyverse")

# =====================================

# Load in package

library(ggplot2)

# CSV files:
# [Refer to/pulled from data_cleaning R file for more clearance on data sets]
# read.csv("") # void out later when merging into main file

# =====================================
```

```{r}
# Examining variables for Thyroid Class (hypo vs hyper)
# --> Update: Use files accounting for "?"/NAs

unique(remove_nas_allhypo$ThyroidClass) # "negative","compensated hypothyroid", "primary hypothyroid", "secondary hypothyroid" | where negative = no Thyroid Disease
unique(remove_nas_allhyper$ThyroidClass) # "negative", "hyperthyroid", "T3 toxic" --> (T3 toxic hyperthyroidism, or T3 toxicosis), "goitre" (too big/enlarged of gland --> hyperthyroid too much hormone) | where negative = no Thyroid Disease

print("==================================================================") # spacer

names(remove_nas_allhyper)
names(remove_nas_allhypo)
# -->  [1] "age" "sex" [3] "presc_thyroxine" "queried_why_on_thyroxine" [5] "presc_anthyroid_meds" "sick" [7] "pregnant" "thyroid_surgery" [9] "radioactive_iodine_therapyI131" "query_hypothyroid" [11] "query_hyperthyroid" "lithium" [13] "goitre" "tumor" [15] "hypopituitarism" "psych_condition" [17] "TSH_measured" "TSH_reading" [19] "T3_measured" "T3_reading" [21] "T4_measured" "T4_reading" [23] "thyrox_util_rate_T4U_measured" "thyrox_util_rate_T4U_reading" [25] "FTI_measured" "FTI_reading" [27] "ref_src" "ThyroidClass" [29] "record_id"

str(allhypo)
str(allhyper)
str(remove_nas_allhyper)
str(remove_nas_allhypo)
```

```{r}
# Testing each variable in Hypothyroid against ThyroidClass

# NOTE: Do NOT need to test "record_id" --> tracking ID, not measurement form
numeric_vars_hypo <- c("age", "TSH_reading", "T3_reading", "T4_reading", "thyrox_util_rate_T4U_reading", "FTI_reading") # 6 total

categorical_vars_hypo <- c("sex", "presc_thyroxine", "queried_why_on_thyroxine", "presc_anthyroid_meds", "sick", "pregnant", "thyroid_surgery", "radioactive_iodine_therapyI131", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitarism", "psych_condition", "TSH_measured", "T3_measured", "T4_measured", "thyrox_util_rate_T4U_measured", "FTI_measured", "ref_src", "ThyroidClass") 
# 22 total ; exclude "record_ID"

# Check if numeric/cat data from allhypo is displayed in a table
hypo_numeric_data <- remove_nas_allhypo[ , numeric_vars_hypo]
#hypo_numeric_data

hypo_categorical_data <- remove_nas_allhypo[ , categorical_vars_hypo]
hypo_categorical_data

# -----------------------------------------------------------------------

# Testing each variable in Hyperthyroid against ThyroidClass

# NOTE: Do NOT need to test "record_id" --> tracking ID, not measurement form
numeric_vars_hyper <- c("age", "TSH_reading", "T3_reading", "T4_reading", "thyrox_util_rate_T4U_reading", "FTI_reading") # 6 total

categorical_vars_hyper <- c("sex", "presc_thyroxine", "queried_why_on_thyroxine", "presc_anthyroid_meds", "sick", "pregnant", "thyroid_surgery", "radioactive_iodine_therapyI131", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitarism", "psych_condition", "TSH_measured", "T3_measured", "T4_measured", "thyrox_util_rate_T4U_measured", "FTI_measured", "ref_src", "ThyroidClass") 
# 22 total ; exclude "record_ID"

# Check if numeric/cat data from allhypo is displayed in a table
hyper_numeric_data <- remove_nas_allhyper[ , numeric_vars_hyper]
#hyper_numeric_data

hyper_categorical_data <- remove_nas_allhyper[ , categorical_vars_hyper]
hyper_categorical_data

```

```{r}
# (1) General Visualizations (Simple) | Observing numeric Variables in Hypothyroid & Hyperthyroid

# Testing each num_variable in Hypothyroid & Hyperthyroid against ThyroidClass --> Can be looped (optional) less precision/choice

# ===========================================================

# [INDEX For Graphs]:

# Green --> Numerical Data Visualizations
# "hypothyroid" --> = "red",
# "hyperthyroid"--> = "blue"

# ===========================================================

# [Convert to numeric values from char]

# 1. Testing Age Variable vs. Thyroid Disease Result

# Hypo

# Outlier at age --> 400+? (possible 40) Error in Data set recording provided
ggplot(remove_nas_allhypo, aes(x = ThyroidClass, y = as.numeric(age))) +
  geom_boxplot(fill = "green", color = "darkred") +
  labs(title="Age vs Hypothyroidism - Thyroid Disease",
       x="Hypothyroid sub-type disease",
       y="Age")

# Hyper

ggplot(remove_nas_allhyper, aes(x = ThyroidClass, y = as.numeric(age))) +
  geom_boxplot(fill = "green", color = "darkblue") +
  labs(title="Age vs Hyperthyroidism - Thyroid Disease",
       x="Hyperthyroid sub-type disease",
       y="Age")

# ===========================================================

# 2. Testing TSH_reading vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = ThyroidClass, y = as.numeric(TSH_reading))) +
  geom_boxplot(fill = "green", color = "darkred") +
  labs(title="Thyroid-Stimulating Hormone (TSH) vs Hypothyroidism - Thyroid Disease)", 
       x="Hypothyroid sub-type disease",
       y="TSH")

# Hyper

ggplot(remove_nas_allhyper, aes(x = ThyroidClass, y = as.numeric(TSH_reading))) +
  geom_boxplot(fill = "green", color = "darkblue") +
  labs(title="Thyroid-Stimulating Hormone (TSH) vs Hyperthyroidism - Thyroid Disease)", 
       x="Hyperthyroid sub-type disease",
       y="TSH")

# ===========================================================

# 3. Testing T3_reading vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = ThyroidClass, y = as.numeric(T3_reading))) +
  geom_boxplot(fill = "green", color = "darkred") +
  labs(title="Triiodothyronine (T3_reading) vs Hypothyroidism - Thyroid Disease)", 
       x="Hypothyroid sub-type disease",
       y="T3-Reading")

# Hyper

ggplot(remove_nas_allhyper, aes(x = ThyroidClass, y = as.numeric(T3_reading))) +
  geom_boxplot(fill = "green", color = "darkblue") +
  labs(title="Triiodothyronine (T3_reading) vs Hyperthyroidism - Thyroid Disease)", 
       x="Hyperthyroid sub-type disease",
       y="T3-Reading")

# ===========================================================

# 4. Testing T4_reading vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = ThyroidClass, y = as.numeric(T4_reading))) +
  geom_boxplot(fill = "green", color = "darkred") +
  labs(title="Thyroxine (T4_reading) vs Hypothyroidism - Thyroid Disease)", 
       x="Hypothyroid sub-type disease",
       y="T4-Reading")

# Hyper

ggplot(remove_nas_allhyper, aes(x = ThyroidClass, y = as.numeric(T4_reading))) +
  geom_boxplot(fill = "green", color = "darkblue") +
  labs(title="Thyroxine (T4_reading) vs Hyperthyroidism - Thyroid Disease)", 
       x="Hyperthyroid sub-type disease",
       y="T4-Reading")

# ===========================================================

# 5. Testing thyrox_util_rate_T4U_reading vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = ThyroidClass, y = as.numeric(thyrox_util_rate_T4U_reading))) +
  geom_boxplot(fill = "green", color = "darkred") +
  labs(title="Thyroxine Utilization Rate (thyrox_util_rate_T4U_reading) vs Hypothyroidism - Thyroid Disease)", 
       x="Hypothyroid sub-type disease",
       y="Thyroxine utilization rate")

# Hyper

ggplot(remove_nas_allhyper, aes(x = ThyroidClass, y = as.numeric(thyrox_util_rate_T4U_reading))) +
  geom_boxplot(fill = "green", color = "darkblue") +
  labs(title="Thyroxine Utilization Rate (thyrox_util_rate_T4U_reading) vs Hyperthyroidism - Thyroid Disease)", 
       x="Hyperthyroid sub-type disease",
       y="Thyroxine utilization rate")

# ===========================================================

# 6. Testing FTI_reading vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = ThyroidClass, y = as.numeric(FTI_reading))) +
  geom_boxplot(fill = "green", color = "darkred") +
  labs(title="Free Thyroxine Index (FTI_reading) vs Hypothyroidism - Thyroid Disease)", 
       x="Hypothyroid sub-type disease",
       y="FTI-Reading")

# Hyper

ggplot(remove_nas_allhyper, aes(x = ThyroidClass, y = as.numeric(FTI_reading))) +
  geom_boxplot(fill = "green", color = "darkblue") +
  labs(title="Free Thyroxine Index (FTI_reading) vs Hyperthyroidism - Thyroid Disease)", 
       x="Hyperthyroid sub-type disease",
       y="FTI-Reading")

# ===========================================================
```

```{r}
# Brief Analysis (Numerical Variables):

# Hypo

# table(remove_nas_allhypo$ThyroidClass)

# Note: For the patients with hypothyroid, there is only one measured with secondary hypothyroid. Since there's inadequate data on patients with secondary hypothyroid, this subtype of 
#       hypothyroidism will be excluded in the following analyses.

# (1) - Age: This is the age of the patient in the context of the hypo dataset. As previously noted, there's an outlier of an age over 400, which is likely a simple data entry error.  
# The age distributions across the hypothyroid subtypes are largely the same, where they impact virtually every age group, but most common in middle-aged individuals around 40-60 years of age.
# These findings indicate that any age group can be impacted, but most commonly found among those in their 40s to 60s.

# (2) - TSH_reading: TSH or thyroid stimulating hormone. This reading determines the level of the TSH in your body, in the context of the hypo dataset. 
# Those with primary hypothyroid had a significant spike in TSh levels, with the median patient experiencing a level of 50. Compensated hypothyroid patients saw a similar spike, though
# not nearly as proununced with the median level of roughly 10, as opposed to the typical patient without hypothyroid with a typical TSH of around 0. Having high amounts of TSH
# is a clear indicator that the patient has some form of hypothyroid disease.

# (3) - T3_reading: Triiodothyronine or T3_reading regulates and controls factors like metabolism, growth, and heartrate. 
# Patients with hypothyroid show lower T3 levels, as opposed to patients without. The normal, healthy range hovering just around a T3 level of 2, while those with compensated hypothroid
# had a median level slightly lower. Patients with primary hypothyroid experience a significantly lower T3 level of less than 1, signifying that a lack of T3 can serve as another 
# indicator of a patient having hypothyroid disease. 

# (4) - T4_reading: T4 or thyroxine is another test used to find out the levels of thyroxine in your body and a lot of the time work with T3 a lot. This is in the context of the hypo dataset.
# The same conclusions of the T3 reading can be applied to the T4 reading as patients with compensated hypothyroid and primary hypothyroid patients experiencing lower levels of T4 than patients
# without, with the impact on primary hypothyroid being significantly larger.

# (5) - thyrox_util_rate_T4U_reading: This is the Thyroxine Utilization Rate, it measures how much of the T4U can be bounded to various proteins compared to those that are not bounded to various 
# proteins, this is in the context of the hypo dataset. 
# T4U readings across all hypothyroid groups remain relatively consistent. The similarity indicates that T4U readings may not necessarily be the most helpful in diagnosing 
# hypothyroidism, especially compared to the previous hormone measurements. 

# (6) - FTI_reading: This is known as the free thyroxine index, as the name suggests it is related to the T4. This reading measures the amount of T4. 
# As expected, the FTI reading follows a similar conclusion to the T4 readings, with very similar value ranges as well. Patients with hypothyroid experienced lower levels of T4 than patients
# without, with the impact on primary hypothyroid being significantly larger 

# =============================================

# Hyper 

# table(remove_nas_allhypo$ThyroidClass)

# (1) - Age: This is the age of the patient in the context of the hypo dataset. As previously noted, there's an outlier of an age over 400, which is likely a simple data entry error.
# Similarly to hypothyroidism, hyperthyroidism affects all age groups, with the majority of patients falling within the middle-aged group.

# (2) - TSH_reading: TSH or thyroid stimulating hormone. This reading determines the level of the TSH in your body, in the context of the hyper dataset. 
# Unlike the hypothyroid TSH readings, all subtypes of hyperthyroid diseases followed the same near zero TSH levels of patients with no thyroid disease. While TSH levels can help 
# determine whether a patient likely has hypothyroid disease, it does not help with hyperthyroid patients.

# (3) - T3_reading: Triiodothyronine or T3_reading regulates and controls factors like metabolism, growth, and heartrate in the context of the hyper dataset. 
# Patients with hyperthyroid saw an opposite effect on T3 levels than hypothyroid patients: those with a type of hyperthyroid experienced significantly higher levels of T3 levels than
# the negative control group. Higher than typical T3 levels can thus be used as an indiciator of hyperthyroid disease.

# (4) - T4_reading: T4 or thyroxine is another test used to find out the levels of thyroxine in your body and a lot of the time work with T3 a lot. This is in the context of the hyper dataset. 
# The same conclusions of the T3 reading can be applied to the T4 reading as patients with hyperthyroid experienced higher levels of T4.

# (5) - thyrox_util_rate_T4U_reading: This is the Thyroxine Utilization Rate, it measures how much of the t4 can be bounded to various proteins compared to those that are not bounded to various 
# proteins, this is in the context of the hyper dataset. 
# The goitre subtype shows the highest level of T4U levels, with the median hovering around 1.2, while the median of the other groups were all below a level of T4U level of 1. 
# T4U can thus be helpful in determining whether patients with hyperthyroid are specifically  of the goitre subtype.

# (6) - FTI_reading: This is known as the free thyroxine index, as the name suggests it is related to the T4. This reading measures the amount of T4.
# The FTI reading for hyperthyroid doesn't perfectly mirror the T4 readings like those for the hypothyroid readings. However, they are still similar with hyperthyroid patients having 
# significantly higher levels of FTI than the other groups, while the other subtypes of hyperthyroid falling close to the levels of patients without hyperthyroid. This indiactes 
# that the FTI reading can be helpful in classifying patients with hyperthyroid of simply having hyperthyroid. 
```

```{r}
# (2) General Visualizations (Simple) | Observing categorical Variables in Hypothyroid & Hyperthyroid

# Testing each cat_variable in Hypothyroid & Hyperthyroid against ThyroidClass --> Can be looped (optional) less precision/choice

# ===========================================================

# [INDEX For Graphs]:

# Blue/Gray (slategrey) --> Categorical Data Visualizations
# "hypothyroid" --> = "red",
# "hyperthyroid"--> = "blue"

# [Refer to: colorPaletteCheatsheet.pdf on BruinLearn - "code to produce R color chart from: http://www.biecek.pl/R/R.pdf and http://bc.bojanorama.pl/2013/04/r-color-reference-sheet"]

# ===========================================================

# [Convert to numeric values from char]

# 1. Sex (Female & Male) vs. Thyroid Disease Result

# Hypo

# Auto-fill colors based on each ThyroidClass instead of setting all different reds and blues [Easier to visualize for Categorical Bar Plots]
ggplot(remove_nas_allhypo, aes(x = sex, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Sex [Males & Females] vs Hypothyroidism - Thyroid Disease",
       x="Sex",
       y="Count")

# Hyper

ggplot(remove_nas_allhyper, aes(x = sex, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Sex [Males & Females] vs Hyperthyroidism - Thyroid Disease",
       x="Sex",
       y="Count")

# ===========================================================

# 2. presc_thyroxine vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = presc_thyroxine, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Medication: Patients Prescribed Thyroxine vs Hypothyroidism - Thyroid Disease",
       x = "Prescribed Thyroxine Medication") # true of false

# Hyper

ggplot(remove_nas_allhyper, aes(x = presc_thyroxine, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Medication: Patients Prescribed Thyroxine vs Hyperthyroidism - Thyroid Disease",
       x = "Prescribed Thyroxine Medication") # true of false

# ===========================================================

# 3. queried_why_on_thyroxine vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = queried_why_on_thyroxine, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient Queried on Thyroxine vs Hypothyroidism - Thyroid Disease",
       x = "Questioned on Thyroxine") # by a doctor possibly; intriguing variable

# Hyper

ggplot(remove_nas_allhyper, aes(x = queried_why_on_thyroxine, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient Queried on Thyroxine vs Hyperthyroidism - Thyroid Disease",
       x = "Questioned on Thyroxine")

# ===========================================================

# 4. presc_anthyroid_meds vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = presc_anthyroid_meds, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Medication: Patients Prescribed Anthyroid Meds vs Hypothyroidism - Thyroid Disease",
       x = "Prescribed Anthyroid Medication") # true of false

# Hyper

ggplot(remove_nas_allhyper, aes(x = presc_anthyroid_meds, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Medication: Patients Prescribed Anthyroid Meds vs Hyperthyroidism - Thyroid Disease",
       x = "Prescribed Anthyroid Medication") # true of false

# ===========================================================

# 5. sick vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = sick, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Recording of Sick Symptoms vs Hypothyroidism - Thyroid Disease",
       x = "Patient is Sick")

# Hyper

ggplot(remove_nas_allhyper, aes(x = sick, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Recording of Sick Symptoms vs Hyperthyroidism - Thyroid Disease",
       x = "Patient is Sick")

# ===========================================================

# 6. pregnant vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = pregnant, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient of Pregnant vs Hypothyroidism - Thyroid Disease",
       x = "Patient of Pregnant") # true of false

# Hyper

ggplot(remove_nas_allhyper, aes(x = pregnant, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient of Pregnant vs Hyperthyroidism - Thyroid Disease",
       x = "Patient of Pregnant") # true of false

# ===========================================================

# 7. thyroid_surgery vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = thyroid_surgery, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Thyroid Surgery vs Hypothyroidism - Thyroid Disease",
       x = "Patient had Thyroid Surgery")

# Hyper

ggplot(remove_nas_allhyper, aes(x = thyroid_surgery, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Thyroid Surgery vs Hyperthyroidism - Thyroid Disease",
       x = "Patient had Thyroid Surgery")

# ===========================================================

# 8. radioactive_iodine_therapyI131 vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = radioactive_iodine_therapyI131, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Radioactive Iodine Therapy vs Hypothyroidism - Thyroid Disease",
       x = "Patient Recieved Radioactive Iodine Therapy I131")

# Hyper

ggplot(remove_nas_allhyper, aes(x = radioactive_iodine_therapyI131, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Radioactive Iodine Therapy vs Hyperthyroidism - Thyroid Disease",
       x = "Patient Recieved Radioactive Iodine Therapy I131")

# ===========================================================

# 9. query_hypothyroid vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = query_hypothyroid, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient Queried on Hypothyroid by Clinic vs Hypothyroidism - Thyroid Disease",
       x = "Patient Questioned on Hypothyroid")

# Hyper

ggplot(remove_nas_allhyper, aes(x = query_hypothyroid, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient Queried on Hypothyroid by Clinic vs Hyperthyroidism - Thyroid Disease",
       x = "Patient Questioned on Hypothyroid")

# ===========================================================

# 10. query_hyperthyroid vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = query_hyperthyroid, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient Queried on Hyperthyroid by Clinic vs Hypothyroidism - Thyroid Disease",
       x = "Patient Questioned on Hyperthyroid")

# Hyper

ggplot(remove_nas_allhyper, aes(x = query_hyperthyroid, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient Queried on Hyperthyroid by Clinic vs Hyperthyroidism - Thyroid Disease",
       x = "Patient Questioned on Hyperthyroid")

# ===========================================================

# 11. lithium vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = lithium, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Consumption of Lithium (Mood Stabilizer) vs Hypothyroidism - Thyroid Disease",
       x = "Consuming Lithium")

# Hyper

ggplot(remove_nas_allhyper, aes(x = lithium, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Consumption of Lithium (Mood Stabilizer) vs Hyperthyroidism - Thyroid Disease",
       x = "Consuming Lithium")

# ===========================================================

# 12. goitre vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = goitre, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Goitre Condition vs Hypothyroidism - Thyroid Disease",
       x = "Doctor Observed Goitre in Patient") # Doctor usually confirms it

# Hyper

ggplot(remove_nas_allhyper, aes(x = goitre, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Goitre Condition vs Hyperthyroidism - Thyroid Disease",
       x = "Doctor Observed Goitre in Patient")

# ===========================================================

# 13. tumor vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = tumor, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Tumor Observed (Cancer) vs Hypothyroidism - Thyroid Disease",
       x = "Patient has Tumor") # --> Patient has thyroid cancer...

# Hyper

ggplot(remove_nas_allhyper, aes(x = tumor, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Tumor Observed (Cancer) vs Hyperthyroidism - Thyroid Disease",
       x = "Patient has Tumor")

# ===========================================================

# 14. hypopituitarism vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = hypopituitarism, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Hypopituitarism Condition vs Hypothyroidism - Thyroid Disease",
       x = "Patient experiences Hypopituitarism")

# Hyper

ggplot(remove_nas_allhyper, aes(x = hypopituitarism, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Hypopituitarism Condition vs Hyperthyroidism - Thyroid Disease",
       x = "Patient experiences Hypopituitarism")

# ===========================================================

# 15. psych_condition vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = psych_condition, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient's Psychiatric Condition vs Hypothyroidism - Thyroid Disease",
       x = "Experiences from Psychiatric Condition (Disorders - Mood)")

# Hyper

ggplot(remove_nas_allhyper, aes(x = psych_condition, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient's Psychiatric Condition vs Hyperthyroidism - Thyroid Disease",
       x = "Experiences from Psychiatric Condition (Disorders - Mood)")

# ===========================================================

# 16. TSH_measured vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = TSH_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="TSH (Thyroid-Stimulating Hormone) [Blood Test] vs Hypothyroidism - Thyroid Disease",
       x = "Took TSH Test")

# Hyper

ggplot(remove_nas_allhyper, aes(x = TSH_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="TSH (Thyroid-Stimulating Hormone) [Blood Test] vs Hyperthyroidism - Thyroid Disease",
       x = "Took TSH Test")

# ===========================================================

# 17. T3_measured vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = T3_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Triiodothyronine Test (T3) vs Hypothyroidism - Thyroid Disease",
       x = "Patient Performed T3_test")

# Hyper

ggplot(remove_nas_allhyper, aes(x = T3_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Triiodothyronine Test (T3) vs Hyperthyroidism - Thyroid Disease",
       x = "Patient Performed T3_test")

# ===========================================================

# 18. T4_measured vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = T4_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Thyroxine Test (T4) vs Hypothyroidism - Thyroid Disease",
       x = "Patient Performed T4_test")

# Hyper

ggplot(remove_nas_allhyper, aes(x = T4_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Thyroxine Test (T4) vs Hyperthyroidism - Thyroid Disease",
       x = "Patient Performed T4_test")

# ===========================================================

# 19. thyrox_util_rate_T4U_measured vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = thyrox_util_rate_T4U_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Thyroxine Test 2 (T4U - Utilization) vs Hypothyroidism - Thyroid Disease",
       x = "Patient had T4U Measurement")

# Hyper

ggplot(remove_nas_allhyper, aes(x = thyrox_util_rate_T4U_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Thyroxine Test 2 (T4U - Utilization) vs Hyperthyroidism - Thyroid Disease",
       x = "Patient had T4U Measurement")

# ===========================================================

# 20. FTI_measured vs. Thyroid Disease Result

# Hypo

ggplot(remove_nas_allhypo, aes(x = FTI_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Free Thyroxine Index (FTI) vs Hypothyroidism - Thyroid Disease",
       x = "Patient had FTI_measurement")

# Hyper

ggplot(remove_nas_allhyper, aes(x = FTI_measured, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Free Thyroxine Index (FTI) vs Hyperthyroidism - Thyroid Disease",
       x = "Patient had FTI_measurement")

# ===========================================================

# 21. ref_src vs. Thyroid Disease Result

# Note: The codes seem to be categorical codes made up for UCI machine learning or machine learning medical data sets in general --> Referring to specific health departments or areas for instance 

# Hypo

ggplot(remove_nas_allhypo, aes(x = ref_src, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient's Thyroid Disease Reference Source (STMW, SVHC, SVI, SVHD, other) vs Hypothyroidism - Thyroid Disease",
       x = "Reference Source") 

# Hyper

ggplot(remove_nas_allhyper, aes(x = ref_src, fill = ThyroidClass)) +
  geom_bar(color = "slategrey") +
  labs(title="Patient's Thyroid Disease Reference Source (STMW, SVHC, SVI, SVHD, other) vs Hyperthyroidism - Thyroid Disease",
       x = "Reference Source")

# ===========================================================

# 22. ThyroidClass --> Exclude (N/A) Comparing same variable

# ===========================================================

```

```{r}
# Brief Analysis (Categorical Variables):

# Hypo

# (1) - Sex: This is the sex of the patient in the context of the hypo dataset. There almost twice as many females compared to males who has the thyroid disease. 
#Both of them has the compensated, negative, and primary thyroid classes. The majority for both being negative class. 

# (2) - presc_thyroxine: Thyroxine is a medication that treats Thyroid Disease (primarily hypothyroidism). This notes whether patients were prescribed thyroxine (t) or not (f).
# It can be observed that patients who are prescribed thyroxine medication generally tend to have negative results of thyroid disease compared to those who are not. 
# Those who tested positive for thyroid disease mainly had "compensated hypothyroid" and "primary hypothyroid". 

# (3) - queried_why_on_thyroxine: This notes whether the patient was queried about why they were taking thyroxine (t) or not (f). There is a small amount the patient has thyroxine, they have the negative thyroid class. 
#The majority of them don't have thyroxine, their thyroid class is mainly negative with a small amount of compensated and primary.

# (4) - presc_anthyroid_meds: This notes whether the patient was prescribed antyhroid medication (t) or not (f). There is a small amount the patient has meds prescribed, they have the negative thyroid class. 
#The majority of them don't have meds prescribed, their thyroid class is mainly negative with a small amount of compensated and primary.

# (5) - sick: This notes whether the patient was sick (t) or not (f). There is a small amount the patient sick, they have the negative thyroid class. 
#The majority of them aren't sick, their thyroid class is mainly negative with a small amount of compensated and primary.

# (6) - pregnant: This notes whether the patient was pregnant (t) or not (f). 
# It can be observed that of the patients that are pregnant, none of them had hypothyroid disease. They have the negative thyroid class. 
#The majority of them aren't pregnant, their thyroid class is mainly negative with a small amount of compensated and primary.

# (7) - thyroid_surgery: This notes whether the patient had performed thyroid surgery (t) or not (t). There is a small amount the patient has surgery, they have the negative thyroid class. 
#The majority of them don't have surgery, their thyroid class is mainly negative with a small amount of compensated and primary.

# (8) - radioactive_iodine_therapyI131: This denotes whether or not the patient had Radioactive Iodine (I-131) treatment Either true or false. There is a small amount the patient has the therapy, they have the negative thyroid class. 
#The majority of them don't have the therapy, their thyroid class is mainly negative with a small amount of compensated and primary.

# (9) - query_hypothyroid: Notes whether the person has hypothyroid, T or F. There is a small amount the patient has hypothyroid, they have the negative thyroid class. 
#The majority of them don't have hypothyroid, their thyroid class is mainly negative with a small amount of compensated and primary.

# (10) - query_hyperthyroid: Notes whether the person has hyperthyroid, T or F. There is a small amount the patient has hyperthyroid, they have the negative thyroid class. 
#The majority of them don't have hyperthyroid, their thyroid class is mainly negative with a small amount of compensated and primary.

# (11) - lithium: Indicates whether lithium was used to help decrease the symptoms. T or F. All of the patients didn't consume lithium. 
#For those who didn't, a majority of them has negative thyroid class, with a small amount of compensated and primary.

# Source: https://my.clevelandclinic.org/health/diseases/12625-goiter
# (12) - goitre: Indicates whether someone has goiter causes by thyroid. T or F. There is a small amount the patient has goiter, they have the negative thyroid class. 
#The majority of them don't have goiter, their thyroid class is mainly negative with a small amount of compensated and primary.

# Source: https://my.clevelandclinic.org/health/diseases/12210-thyroid-cancer
# (13) - tumor: Indicates whether someone has tumor as a result of thyroid cancer. T or F. There is a small amount the patient has the tumor, they have the negative thyroid class. 
#The majority of them don't have tumor, their thyroid class is mainly negative with a small amount of compensated and primary.

# Source: # https://my.clevelandclinic.org/health/diseases/22102-hypopituitarism
# (14) - hypopituitarism: This is where due to the thyroid condition, you're producing less hormones. T or F. All of the patients don't experience it.  
# For those that don't, their thyroid class is mainly negative with a small amount of compensated and primary. 

# (15) - psych_condition: If a person has a psychological condition that came with the thyroid disease. T or F. More false of psychiatric condition than true. 
#For the false the majority is negative while there is a small amount comparably of compensated and primary. For the true, there is only negative.  

# (16) - TSH_measured: TSH or thyroid stimulating hormone. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their TSH levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their TSH levels.

# (17) - T3_measured: Triiodothyronine or T3_reading. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their T3 levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their T3 levels.

# (18) - T4_measured: T4 or thyroxine. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their T4 levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their T4 levels.

# (19) - thyrox_util_rate_T4U_measured: Thyroxine Utilization Rate. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their T4U levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their T4U levels.

# (20) - FTI_measured: Free Thyroxine Index. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their FTI levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their FTI levels.

# (21) - ref_src: This is a categorical code likely used to referring to a specific health department or area. The only thyroid class 
#for the STMW and SVHD is the negative thyroid class. SVHC has negaitve plus secondary. The last two has all three except seconday. 

# (22) - ThyroidClass: N/A [Invalid Test, void like record_id]

# =============================================

# Hyper 

# (1) - Sex: This is the sex of the patient in the context of the hyper dataset, where f = female and m = male. It can be observed that females are more susceptible to both hyperthyroid
# and goitre than males as they have an absolute value of patients with these conditions greater than their male counterparts. It also appears proportionally higher, however, actual
# proportions must be calculated to verify this observation.

# (2) - presc_thyroxine: Similarly to the hypothyroid bar plot, it can be observed that patients who are prescribed thyroxine medication generally tend to have negative results of thyroid 
# disease compared to those who are not. Those who tested positive for thyroid disease mainly had "hyperthyroid". However, it appears that the medication works much better on hyperthyroid 
# prevention although further research or statistical tests would have to be conducted further. 

# (3) - queried_why_on_thyroxine: This notes whether the patient was queried about why they were taking thyroxine (t) or not (f). The vast majority of patients were not queried about
# thyroxine use. It indicates that thyroxine use poses an issue of testing for other values, such as the various hormone levels. Of the patients that were queried, all of the patients
# did not have any hyperthyroid disease, furthering this idea.

# (4) - presc_anthyroid_meds: This notes whether the patient was prescribed antyhroid medication (t) or not (f). Of the patients prescribed antyhroid, all were of the negative control group.
# All patients with any subset of hyperthyroid disesase were not prescriebd the medication, except for a handful of T3 toxic cases.

# (5) - sick: This notes whether the patient was recording sick symptoms (t) or not (f). of the patients reporting the symptoms, almost all of them were of the negative control group. 
# This may indicate that the symptoms and signs of hyperthyroid disease are subtle or are slow developing.

# (6) - pregnant: This notes whether the patient was pregnant (t) or not (f). 
# It can be observed that of the patients that are pregnant, very few had any hyperthyroid disease, with that subtype only being goiter. This may indicate that pregnant women
# are no more susceptible to hyperthroidism than any other group.

# (7) - thyroid_surgery: This notes whether the patient had performed thyroid surgery (t) or not (t). Of the patients who had received thyroid surgery, none of them report having or still
# having hyperthyroidism. This may indicate that the surgery is very effective or that early preventive measures can stop the disease early on.

# (8) - radioactive_iodine_therapyI131: This notes whether the patient had Radioactive Iodine (I-131) treatment (t) or not (f). Of the patients who received the treatment, 
# all of them were of the negative control group. This may indicate that the therapy may not be the most effective or a popular treatment as patients with hyperthroidism aren't being treated
# with it.

# (9) - query_hypothyroid: This notes whether the person has hyperthyroid (t) or not (f). It's interesting to note how the groups of patients with hypothyroid are split between
# having and not having a subsets of hypothyroid disease, with the majority in the false group. This may indicate that some did not know they had their condition beforehand.

# (10) - query_hyperthyroid: This whether the person has hyperthyroid (t) or not (f). It's interesting to note how the groups of patients with hyperthyroid are split between
# having and not having a subsets of hyperthyroid disease. This may indicate that some did not know they had their condition beforehand.

# (11) - lithium: This notes whether lithium was used (t) or not (f). All hyperthyroid patients reported not consuming any lithium, with those reporting having consumed lithium as a mood
# stablizier were all of the negative control group.

# Source: https://my.clevelandclinic.org/health/diseases/12625-goiter
# (12) - goitre: This notes whether the patient has goiter condition (t) or not (f). Goiter is the overgrowth of the thyroid gland. It's interesting to note that despite patients 
# being diagnosed with goiter, they do not actually have goiter condition, with those who do  all being negative for any hyperthyroid disease.

# (13) - tumor: This notes whether the patient has a record tumor (t) or not (f). While patients with hyperthyroid and T3 toxic subtypes almost all reported having no tumors, a significant
# amount of patients with goiter reported having a tumor. While they may not be completely correlated with another, it is important to note and would be interesting to explore the 
# relationship between patients with goiter and a tumor.

# Source: https://my.clevelandclinic.org/health/diseases/22102-hypopituitarism
# (14) - hypopituitarism: This notes whether the patient has hypopituitarism (t) or not (f). Hypopituitarism is a condition where there's a lack of hormone production from the pituitary glands.
# All of the hyperthyroid patients did not have hypopituarism, with there only being a handful of negative patients having the condition at all. This indicates that hypopituitarism likely
# has little to no impact on hyperthyroid diseases, since of the patients who did have the condition, they were not linked to any hyperthyroid disease.

# (15) - psych_condition: This notes whether the patient has a documented psychiatric condition (t) or not (f). The vast majority of patients did not have any documented psychiatric conditions
# and those who did were all negative for any hyperthyroid disease. This indicates that psychiatric conditions don't lead to hyperthyroid disease as none of the patients with documented 
# conditions were diagnosed with any form of hyperthyroid disease. 

# (16) - TSH_measured: TSH or thyroid stimulating hormone. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their TSH levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their TSH levels.

# (17) - T3_measured: Triiodothyronine or T3_reading. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their T3 levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their T3 levels.

# (18) - T4_measured: T4 or thyroxine. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their T4 levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their T4 levels.

# (19) - thyrox_util_rate_T4U_measured: Thyroxine Utilization Rate. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their T4U levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their T4U levels.

# (20) - FTI_measured: Free Thyroxine Index. This notes whether the reading was measured (t) or not (f). As this visualization simply shows whether the reading was 
# taken and shows that all readings were in fact measured, there is virtually no analytical value when all patients recorded their FTI levels, after removing any potential NA readings.
# Thus, it indicates that there should be substantial data when looking at the difference of the thyroid groups among their FTI levels.

# (21) - ref_src: This is a categorical code likely used to referring to a specific health department or area. As this categorical code likely does not have significant correlation to whether
# a patient has a subtype of hyperthyroid, there's not many conclusions that can be drawn. Of the different codes, the "other" group had the most cases of hyperthyroid and T3 toxic, 
# and the highest diseased patients per capita. However, no clear information can be gathered as this "other" category remains ambiguous.

# (22) - ThyroidClass: N/A [Invalid Test, void like record_id]
```

```{r}
# Comparison of Hypothyroid vs Hyperthyroid [Higher Quality Statistical Visualizations]

# =========================================

#install.packages("ggrepel") # INSTALL IF NEEDED  
library(ggrepel) # used to prevent text-overlap [https://ggrepel.slowkow.com/]

# =========================================

# 0. ML Analysis For Highest Importance Classifying Thyroid Disease

# [Note: Coef. values were gathered from ML model; "." = not used in model -->                                          Ex: T3_measured . and T4_measured . ]

# Construct data.frame inputting values given

ml_coef_df <- data.frame(
  variables = c("age","sexM","presc_thyroxine","queried_why_on_thyroxine",
          "presc_anthyroid_meds","sick","pregnant","thyroid_surgery",
          "radioactive_iodine_therapyI131","query_hypothyroid",
          "query_hyperthyroid", "lithium","goitre","tumor","hypopituitarism",
          "psych_condition", "TSH_reading","T3_reading","T4_reading",
          "thyrox_util_rate_T4U_reading", "FTI_reading"),
  hyper_ml_coef = c(-0.006508914, 0.592310004, 1.359800950, 1.572507679, 
                    -1.279981556, -0.212045527, 1.197738090, 0.528055152,
                    -1.311378550, 0.855032378, -0.387778238, 0.334824702,
                    0.339558648, 0.023676527, 0.003350999, 0.759104138,
                    0.004636063, -0.646673359, -0.015782152, 1.654273090,
                    -0.028433950),
  hypo_ml_coef = c(0.001644279, 0.360173554, 1.247184967, -0.805817772,
                   0.446475096, -0.381574140, 1.155120158, 4.308487786,
                   0.866294000, -0.437844531, -0.146093108, -0.250629636,
                   1.517338999, -0.316632434, 2.574029211, 0.026682689,
                   -0.068778607, 0.346691317, 0.010000982, -0.878012799,
                   0.016010469)
)

#ml_coef_df #check table

ml_scatter <- ggplot(ml_coef_df, aes(x = hyper_ml_coef, y = hypo_ml_coef)) +
  geom_point(color = "black", size = 1) +
  geom_text_repel(aes(label = variables), size = 3) +
  labs(
    title = "Variable Importance Effectiveness (Based on ML): Hypothyroid vs Hyperthyroid",
    x = "Hyperthyroid coefficient",
    y = "Hypothyroid coefficient"
  )

ml_scatter + 
  theme_bw() # ggplot2 themes: [https://ggplot2.tidyverse.org/reference/ggtheme.html]

# Analysis: Observed that there seemed to be more variables leading to greater importance and affect in hyperthyroid compared to hypothyroid although the hypothyroid coefficient contains 
# more outliers such as (4.308487786) for thyroid_surgery which means that thyroid surgery is a leading factor in result of those who have the hypothyroid disease. 
# Meanwhile, it is intriguing that thyroid_surgery only has a coefficient importance of 0.528055152. This leads us to further research statistical questions and answers that certain variables 
# affect certain types of Thyroid disease (i.e. Hypo and Hyper-thyroid) particularly. 

#  ───────────────────────────────────────────────────────────────

# 1. Medication Factor - Testing for Prescribed Medication (Thyroxine)

combine_presc_thyroxine <- rbind(
  remove_nas_allhypo[, c("presc_thyroxine", "ThyroidClass")],
  remove_nas_allhyper[, c("presc_thyroxine", "ThyroidClass")]
)

#combine_presc_thyroxine # make sure combined properly

# Side bar graph instead of stacked ~ ref 2.
ggplot(combine_presc_thyroxine, aes(x = presc_thyroxine, fill = ThyroidClass)) +
  geom_bar(position = "dodge", color = "slategrey") +
  labs(
    title = "Prescribed Thyroxine vs Thyroid Subtype (Hypo vs Hyper)",
    x = "Patient Prescribed Thyroxine",
    y = "Count",
    fill = "Thyroid (hypo vs hyper) Subtype"
  ) +
  theme_bw()

# Analysis: In the plot we can observe in the side to side bar graph (after observing results from both categorical plots - refer to 2. in the categorical graphs) that the thyroxine prescribed medication is indeed effective in preventing thyroid disease types hypothyroid and hyperthyroid. Those who were not prescribed thyroxine mostly recorded negative for either disease but there were cases of development of compensated hypothyroid, hyperthyroid, and primary hypothyroid.

#  ───────────────────────────────────────────────────────────────

# 1.2: Medication Factor - Testing for Prescribed Medication (Anthyroid)

combine_presc_anthyroid <- rbind(
  remove_nas_allhypo[, c("presc_anthyroid_meds", "ThyroidClass")],
  remove_nas_allhyper[, c("presc_anthyroid_meds", "ThyroidClass")]
)

#combine_presc_anthyroid # make sure combined properly

# Side bar graph instead of stacked ~ ref 2.
ggplot(combine_presc_anthyroid, aes(x = presc_anthyroid_meds, fill = ThyroidClass)) +
  geom_bar(position = "dodge", color = "slategrey") +
  labs(
    title = "Prescribed Anthyroid vs Thyroid Subtype (Hypo vs Hyper)",
    x = "Patient Prescribed Anthyroid",
    y = "Count",
    fill = "Thyroid (hypo vs hyper) Subtype"
  ) +
  theme_bw()

# Analysis: In the plot we can observe in the side to side bar graph (after observing results from both categorical plots - refer to 4. in the categorical graphs) that the anthyroid prescribed medication is indeed effective in preventing thyroid disease types hypothyroid and hyperthyroid. It can even be said that it is used less by doctors to prescribe by patients as there are higher proportions of negative thyroid disease recordings for non-prescribed patients. Again, those who were not prescribed anthyroid mostly recorded negative for either disease but there were cases of development of compensated hypothyroid, hyperthyroid, and primary hypothyroid.

#  ───────────────────────────────────────────────────────────────

# 2. Surgery Factor - Testing for Effectiveness of Thyroid Surgery

combine_surgery_hormones <- rbind(
  remove_nas_allhypo[, c("thyroid_surgery", "ThyroidClass", "TSH_reading", "T3_reading", "T4_reading")],
  remove_nas_allhyper[, c("thyroid_surgery", "ThyroidClass", "TSH_reading", "T3_reading", "T4_reading")]
)

# combine_surgery_hormones # make sure combined properly

# convert readings to numeric as they're currently stored as chr
combine_surgery_hormones$TSH_reading <- as.numeric(combine_surgery_hormones$TSH_reading)
combine_surgery_hormones$T3_reading <- as.numeric(combine_surgery_hormones$T3_reading)
combine_surgery_hormones$T4_reading <- as.numeric(combine_surgery_hormones$T4_reading)

# 2.1 Surgery Factor of TSH Reading
ggplot(combine_surgery_hormones, aes(x = thyroid_surgery, y = TSH_reading, fill = ThyroidClass)) +
  geom_boxplot(position = "dodge", color = "slategrey") +
  labs(
    title = "Received Surgery vs Thyroid Subtype (Hypo vs Hyper)",
    x = "Patient Surgery Status",
    y = "TSH Reading",
    fill = "Thyroid (hypo vs hyper) Subtype"
  ) +
  theme_bw()

# Analysis: TSH levels indicate the greatest discrepancy with primary hypothyroid patients who have not had surgery, as these patients experienced a typical median level of about 50. This was
# drastically higher than the negative control groups, who both had near zero TSH levels. It is interesting to note that despite receiving surgery, there are still 25% of patients with slightly
# elevated TSH levels.This ultimately shows that thyroid surgery is effective for patients with hypothyroid in reducing TSH levels, but may slightly increase levels in hyperthyroid patients. 

#  ───────────────────────────────────────────────────────────────

# 2.2 Surgery Factor of T3 Reading
ggplot(combine_surgery_hormones, aes(x = thyroid_surgery, y = T3_reading, fill = ThyroidClass)) +
  geom_boxplot(position = "dodge", color = "slategrey") +
  labs(
    title = "Received Surgery vs Thyroid Subtype (Hypo vs Hyper)",
    x = "Patient Surgery Status",
    y = "T3 Reading",
    fill = "Thyroid (hypo vs hyper) Subtype"
  ) +
  theme_classic()

# Analysis: T3 levels indicate a clear difference among the different subtypes of hypo and hyperthyroid. The normal level maintained by the negative groups ranged from about ~1.5 to 2.5, 
# whereas patients with hyperthyroid and goitre experienced the highest T3 levels of about 3.5-4.0. Patients with compensated and primary hypothyroid fell on the opposite end of the spectrum, 
#receiving lower levels of roughly 2.0 and 1.0 respectively. With the groups of negative patients who had received thyroid surgery, it's once again clear that receiving thyroid surgery 
# returns T3 levels back to normal levels.

#  ───────────────────────────────────────────────────────────────

# 2.3 Surgery Factor of T4 Reading
ggplot(combine_surgery_hormones, aes(x = thyroid_surgery, y = T4_reading, fill = ThyroidClass)) +
  geom_boxplot(position = "dodge", color = "slategrey") +
  labs(
    title = "Received Surgery vs Thyroid Subtype (Hypo vs Hyper)",
    x = "Patient Surgery Status",
    y = "T4 Reading",
    fill = "Thyroid (hypo vs hyper) Subtype"
  ) +
  theme_test()

# Analysis: As expected, the T4 readings indicate similar level distributions as the T3 readings. The median of the normal range maintained by the negative groups hovered around a level of 100.
# However, there is a key difference in hyperthyroid patients and goitre, as those with goitre did not have nearly as high T4 levels, while hyperthyroid patients maintained a high T4 level
# of nearly double the control groups. Once again, the compensated and primary hypothyroid groups without surgery experienced lower levels of T4 of ~80 and ~50 respectively.These findings
# similarly indicate how surgery helps restore typical T4 levels by reducing and increasing the levels of hyperthyroid and hypothyroid patients respectively.

#  ───────────────────────────────────────────────────────────────
```

```{r}
# [MACHINE LEARNING | STEP]

# ----------------------------------------

# ml_hyper.r

# Following logistic regression implementation according to these resources 
# https://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/
# https://glmnet.stanford.edu/articles/glmnet.html#logistic-regression-family-binomial

# I will first be building a logistic regression ML model on the allhyper.data dataset.
# I will then analyze the model's performance and extract what features are the most important in determining
# what patients were classified with the hyperthyroid disease.

# install.packages("tidyverse")
# install.packages("caret")
# install.packages("glmnet")

# import packages
library(tidyverse)
library(caret)
library(glmnet)

# analyze dataset and convert types
str(remove_nas_allhyper)
str(remove_nas_allhyper_test)

train_data <- remove_nas_allhyper
test_data <- remove_nas_allhyper_test

# convert numeric variables from character to numeric
numeric_vars <- c("age", "TSH_reading", "T3_reading", "T4_reading", 
                  "thyrox_util_rate_T4U_reading", "FTI_reading")

for(var in numeric_vars) {
  train_data[[var]] <- as.numeric(train_data[[var]])
  test_data[[var]] <- as.numeric(test_data[[var]])
}

# convert boolean variables to numeric
binary_vars <- c("presc_thyroxine", "queried_why_on_thyroxine", 
                 "presc_anthyroid_meds", "sick", "pregnant", 
                 "thyroid_surgery", "radioactive_iodine_therapyI131",
                 "query_hypothyroid", "query_hyperthyroid", "lithium",
                 "goitre", "tumor", "hypopituitarism", "psych_condition",
                 "TSH_measured", "T3_measured", "T4_measured",
                 "thyrox_util_rate_T4U_measured", "FTI_measured", "ref_src")

for(var in binary_vars) {
  train_data[[var]] <- ifelse(train_data[[var]] == "t", 1, 0)
  test_data[[var]] <- ifelse(test_data[[var]] == "t", 1, 0)
}

# convert sex to factor
train_data$sex <- as.factor(train_data$sex)
test_data$sex <- as.factor(test_data$sex)

# convert target variable to factor
train_data$ThyroidClass <- as.factor(train_data$ThyroidClass)
test_data$ThyroidClass <- as.factor(test_data$ThyroidClass)

# remove record_id because useless for model
train_data$record_id <- NULL
test_data$record_id <- NULL

# observe different classes in thyroid class
print(table(unique(train_data$ThyroidClass)))
print(table(unique(test_data$ThyroidClass)))

# since test dataset contains secondary toxic class (which is not seen in training dataset), remove it
test_data <- test_data[test_data$ThyroidClass %in% unique(train_data$ThyroidClass),]

# we want to have the model be binary classification (hyperthyroid or not) but we have multiple class
# variables so we need to split them up based on their classification
# hyperthyroid -> hyperthyroid
# T3 toxic -> hyperthyroid
# negative -> non-hyperthyroid
# goitre -> non-hyperthyroid
train_data$ThyroidClass <- ifelse(train_data$ThyroidClass %in% c("hyperthyroid", "T3 toxic"),
                          "hyperthyroid",
                          "non-hyperthyroid")
train_data$ThyroidClass <- as.factor(train_data$ThyroidClass)
test_data$ThyroidClass <- ifelse(test_data$ThyroidClass %in% c("hyperthyroid", "T3 toxic"),
                          "hyperthyroid",
                          "non-hyperthyroid")
test_data$ThyroidClass <- as.factor(test_data$ThyroidClass)

# confirm data looks correct
str(train_data)
str(test_data)

# convert data into matrix for glmnet model
# [,-1] removes intercept column from matrix
x <- model.matrix(ThyroidClass ~ ., data=train_data)[,-1]
# set y to target variable
y <- train_data$ThyroidClass

# glmnet is a ridge regularized logistic regression model which penalizes the model during training process
# this model is better than base R glm because of the instability of the data (many cases of nonhyperthyroid
# vs hyperthyroid) Additionally, glmnet uses k-fold cross-validation which splits the dataset into k folds
# (default k=10) in which each fold is used as the validation set once, while the rest k-1 folds are used
# for training. This allows for a more stable model and reduces overfitting.
# source: https://machinelearningmastery.com/k-fold-cross-validation/

# parameters:
# x -> data in numeric matrix format
# y -> target variable
# family = "binomial" -> logistic regression
# alpha = 0 -> ridge regularization
# type.measure = "class" -> gives misclassification error
model <- cv.glmnet(
    x, y, 
    family = "binomial",
    alpha = 0,     # ridge
    type.measure = "class"
)

# convert data into matrix for glmnet model
# [,-1] removes intercept column from matrix
x_test <- model.matrix(ThyroidClass ~ ., test_data)[, -1]

# predict model's probabilities on test set
# newx -> test set
# s = "lambda.min" -> chooses the lambda with the lowest cross-validation error
# type = "response" -> returns probabilities (between 0 and 1)
probabilities <- predict(
    model,
    newx = x_test,
    s = "lambda.min",
    type = "response"
)

# creates binary predictions
# if the probability is over 0.8 (chosen intuitively), then the model will predict that the patient
# with its features is non-hyperthyroid and if it is less than the model will predict that the patient 
# is hyperthyroid.
predicted_classes_hyper <- ifelse(probabilities > 0.8,
                            "non-hyperthyroid",
                            "hyperthyroid")
# Necessary for confusion matrix so that the dimensions of the predictions are correct
predicted_classes_hyper <- factor(predicted_classes_hyper,
                            levels = levels(test_data$ThyroidClass))

# Counts how many predictions from the model matched the actual diagnoses
accuracy <- mean(predicted_classes_hyper == test_data$ThyroidClass)
print(accuracy)

# Confusion matrix offers some useful statistics on our model
print(confusionMatrix(predicted_classes_hyper, test_data$ThyroidClass))

# observe the coefficients of each variable at the best regularization value
# shows us what features contribute most to classification
print(coef(model, s = "lambda.min"))


# BASE R MODEL (fails to perform due to instability of data)

# create logistic regression model (binomial = lr)
#model <- glm(ThyroidClass ~., data = train_data, family = binomial, 
            #weights = ifelse(train_data$ThyroidClass == "non-hyperthyroid", 36, 1))
# summarize the model
#summary(model)
# make predictions
#probabilities <- model |> predict(test_data, type = "response")
#predicted_classes <- ifelse(probabilities > 0.5, "hyperthyroid", "non-hyperthyroid")
# model accuracy
#mean(predicted_classes == test_data$ThyroidClass)
```

```{r}
# ml_hypo.r

# Following logistic regression implementation according to these resources 
# https://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/
# https://glmnet.stanford.edu/articles/glmnet.html#logistic-regression-family-binomial

# I will first be building a logistic regression ML model on the allhypo.data dataset.
# I will then analyze the model's performance and extract what features are the most important in determining
# what patients were classified with the hypothyroid disease.

# import packages
library(tidyverse)
library(caret)
library(glmnet)

# analyze dataset and convert types
str(remove_nas_allhypo)
str(remove_nas_allhypo_test)

train_data_hypo <- remove_nas_allhypo
test_data_hypo <- remove_nas_allhypo_test

# convert numeric variables from character to numeric
numeric_vars <- c("age", "TSH_reading", "T3_reading", "T4_reading", 
                  "thyrox_util_rate_T4U_reading", "FTI_reading")

for(var in numeric_vars) {
  train_data_hypo[[var]] <- as.numeric(train_data_hypo[[var]])
  test_data_hypo[[var]] <- as.numeric(test_data_hypo[[var]])
}

# convert boolean variables to numeric
binary_vars <- c("presc_thyroxine", "queried_why_on_thyroxine", 
                 "presc_anthyroid_meds", "sick", "pregnant", 
                 "thyroid_surgery", "radioactive_iodine_therapyI131",
                 "query_hypothyroid", "query_hyperthyroid", "lithium",
                 "goitre", "tumor", "hypopituitarism", "psych_condition",
                 "TSH_measured", "T3_measured", "T4_measured",
                 "thyrox_util_rate_T4U_measured", "FTI_measured", "ref_src")

for(var in binary_vars) {
  train_data_hypo[[var]] <- ifelse(train_data_hypo[[var]] == "t", 1, 0)
  test_data_hypo[[var]] <- ifelse(test_data_hypo[[var]] == "t", 1, 0)
}

# convert sex to factor
train_data_hypo$sex <- as.factor(train_data_hypo$sex)
test_data_hypo$sex <- as.factor(test_data_hypo$sex)

# convert target variable to factor
train_data_hypo$ThyroidClass <- as.factor(train_data_hypo$ThyroidClass)
test_data_hypo$ThyroidClass <- as.factor(test_data_hypo$ThyroidClass)

# remove record_id because useless for model
train_data_hypo$record_id <- NULL
test_data_hypo$record_id <- NULL

# observe different classes in thyroid class
print(table(unique(train_data_hypo$ThyroidClass)))
print(table(unique(test_data_hypo$ThyroidClass)))

# since train dataset contains secondary hypothyroid class (which is not seen in test dataset), remove it
test_data_hypo <- test_data_hypo[test_data_hypo$ThyroidClass %in% unique(train_data_hypo$ThyroidClass),]

# we want to have the model be binary classification (hypothyroid or not) but we have multiple class
# variables so we need to split them up based on their classification
# compensated hypothyroid -> hypothyroid
# primary hypothyroid -> hypothyroid
# negative -> non-hypothyroid
train_data_hypo$ThyroidClass <- ifelse(train_data_hypo$ThyroidClass %in% c("compensated hypothyroid", "primary hypothyroid"),
                          "hypothyroid",
                          "non-hypothyroid")
train_data_hypo$ThyroidClass <- as.factor(train_data_hypo$ThyroidClass)
test_data_hypo$ThyroidClass <- ifelse(test_data_hypo$ThyroidClass %in% c("compensated hypothyroid", "primary hypothyroid"),
                          "hypothyroid",
                          "non-hypothyroid")
test_data_hypo$ThyroidClass <- as.factor(test_data_hypo$ThyroidClass)

# confirm data looks correct
str(train_data_hypo)
str(test_data_hypo)

# convert data into matrix for glmnet model
# [,-1] removes intercept column from matrix
x <- model.matrix(ThyroidClass ~ ., data=train_data_hypo)[,-1]
# set y to target variable
y <- train_data_hypo$ThyroidClass

# glmnet is a ridge regularized logistic regression model which penalizes the model during training process
# this model is better than base R glm because of the instability of the data (many cases of nonhypothyroid
# vs hypothyroid) Additionally, glmnet uses k-fold cross-validation which splits the dataset into k folds
# (default k=10) in which each fold is used as the validation set once, while the rest k-1 folds are used
# for training. This allows for a more stable model and reduces overfitting.
# source: https://machinelearningmastery.com/k-fold-cross-validation/

# parameters:
# x -> data in numeric matrix format
# y -> target variable
# family = "binomial" -> logistic regression
# alpha = 0 -> ridge regularization
# type.measure = "class" -> gives misclassification error
model <- cv.glmnet(
    x, y, 
    family = "binomial",
    alpha = 0,     # ridge
    type.measure = "class"
)

# convert data into matrix for glmnet model
# [,-1] removes intercept column from matrix
x_test <- model.matrix(ThyroidClass ~ ., test_data_hypo)[, -1]

# predict model's probabilities on test set
# newx -> test set
# s = "lambda.min" -> chooses the lambda with the lowest cross-validation error
# type = "response" -> returns probabilities (between 0 and 1)
probabilities <- predict(
    model,
    newx = x_test,
    s = "lambda.min",
    type = "response"
)

# creates binary predictions
# if the probability is over 0.85 (chosen intuitively), then the model will predict that the patient
# with its features is non-hyperthyroid and if it is less than the model will predict that the patient 
# is hypothyroid.
predicted_classes_hypo <- ifelse(probabilities > 0.85,
                            "non-hypothyroid",
                            "hypothyroid")
# confusion matrix expects factored type
predicted_classes_hypo <- factor(predicted_classes_hypo)
# Confusion matrix offers some useful statistics on our model
# source: https://www.digitalocean.com/community/tutorials/confusion-matrix-in-r
print(confusionMatrix(predicted_classes_hypo, test_data_hypo$ThyroidClass))

# Counts how many predictions from the model matched the actual diagnoses
accuracy <- mean(predicted_classes_hypo == test_data_hypo$ThyroidClass)
print(paste("Accuracy: ", accuracy))

# observe the coefficients of each variable at the best regularization value
# shows us what features contribute most to classification
print(coef(model, s = "lambda.min"))
```

```{r}
# Statistical Plots | Hypothesis Testing

# Reference to Hypothesis Testing in R: https://www.geeksforgeeks.org/r-language/hypothesis-testing-in-r-programming/

# ============================================================

# Based on our previous plots of the variables and our ML implementation; we analyzed some of the greatest variables that may increase or reduce the main contributing possibilities of Thyroid Disease.
# For instance in our ML model for Hyper:
# presc_anthyroid_meds : -1.279981556 | and | presc_thyroxine : 1.359800950
# While for Hypo:
# presc_anthyroid_meds : 0.446475096 | and | presc_thyroxine  : 1.247184967
# which leaves us to question whether giving certain prescriptions actually help reduce or hinder and worsen Thyroid Disease even further.

# [Note: Higher ML value means that it has greater importance when classifying whether someone has thyroid disease or not; Lower ML values (negative) are the opposite or vice-versa.]

# ============================================================

# 0. ML attributes test (CI)

# install.packages("caret") if needed
library(caret)

# print confusion matrix for hyperthyroid
print(confusionMatrix(predicted_classes_hyper, test_data$ThyroidClass))

# print confusion matrix for hypothyroid
print(confusionMatrix(predicted_classes_hypo, test_data_hypo$ThyroidClass))

print("───────────────────────────────────────────────────────────────") # spacer

# Source: https://www.digitalocean.com/community/tutorials/confusion-matrix-in-r

# Analysis: confusionMatrix() is a function in the caret library that takes in a factor vector of predicted classes and a factor vector of ground truth classes.
# The function will return a number of useful statistics from the result of the predicted classes vs true classes. 
# It features a table that displays the predicted classes from the model compared to the true classes, the accuracy, 95% confidence interval, and many other statistics from our model. 
# The table is the most important output from this function as it shows when our model correctly predicted hyperthyroid/hypothyroid vs when it incorrectly predicted it or failed to predict it.
# These metrics are useful in analyzing our model, specifically in the medical field, because we don't want our model to be mis-evaluating a patient for a disease. 
# Another important metric from the confusionMatrix() are the accuracy which is displayed as ~98% for our hyperthyroid model and ~96% for our hypothyroid model. These are solid numbers, but then again,
# the dataset is very imbalanced where there are many more negative cases than positive cases. Lastly, another important metric is the 95% confidence interval which explains that if we were to train
# our model on different random samples of our training dataset, the accuracy would fall within that range. For our hyperthyroid model, our 95% CI was given as (0.9737, 0.9931) and for our
# hypothyroid model, it was given as (0.9357, 0.9683).

#  ───────────────────────────────────────────────────────────────

# Chi-Square test (categorical variables)
# Reference --> https://www.geeksforgeeks.org/r-language/chi-square-test-in-r/

# 1. Question: Medication - Thyroxine
# Does the use of medication, prescribed Thyroxine, actually affect the result of Thyroid Disease?

# Null Hypothesis (H_0): The Thyroxine medication works
# Alt. Hypothesis (H_a): The Thyroxine medication doesn't work
# significance-level: auto tested at 5%

# From previous: # Skipped "Installing the libraries" --> Install if needed
combine_presc_thyroxine <- rbind(
  remove_nas_allhypo[, c("presc_thyroxine", "ThyroidClass")],
  remove_nas_allhyper[, c("presc_thyroxine", "ThyroidClass")]
)

# "Creating a Contingency Table from Survey Data"

combined_thyroxine <- data.frame(combine_presc_thyroxine$ThyroidClass, combine_presc_thyroxine$presc_thyroxine)

combined_thyroxine <- table(combine_presc_thyroxine$ThyroidClass, combine_presc_thyroxine$presc_thyroxine)

print(combined_thyroxine)

# "Applying Chi-Square Test"
chi_sq_test_thyroxine <-chisq.test(combined_thyroxine)
chi_sq_test_thyroxine

print("───────────────────────────────────────────────────────────────") # spacer

# Analysis: Since we have a p-value of 0.004355 which is very low, (0.004355 < 0.05), we have statistically significant evidence that we can reject the null hypothesis and suggest that the Thyroxine medication really works in limited and preventing Thyroid Disease.

#  ───────────────────────────────────────────────────────────────

# 2. Question: Medication - Anthyroid meds
# Does the use of medication, prescribed Anthyroid Meds, actually affect the result of Thyroid Disease?

# Null Hypothesis (H_0): The Anthyroid medication works
# Alt. Hypothesis (H_a): The Anthyroid medication doesn't work
# significance-level: auto tested at 5%

combine_presc_anthyroid <- rbind(
  remove_nas_allhypo[, c("presc_anthyroid_meds", "ThyroidClass")],
  remove_nas_allhyper[, c("presc_anthyroid_meds", "ThyroidClass")]
)

# "Creating a Contingency Table from Survey Data"
combined_anthyroid <- data.frame(combine_presc_anthyroid$ThyroidClass, combine_presc_anthyroid$presc_anthyroid_meds)

combined_anthyroid <- table(combine_presc_anthyroid$ThyroidClass, combine_presc_anthyroid$presc_anthyroid_meds)

print(combined_anthyroid)

# "Applying Chi-Square Test"
chi_sq_test_anthyroid <-chisq.test(combined_anthyroid)
chi_sq_test_anthyroid

print("───────────────────────────────────────────────────────────────") # spacer

# Analysis: Since we have a p-value of 0.02203 which is very low, (0.02203 < 0.05), we have statistically significant evidence that we can reject the null hypothesis and suggest that the Anthyroid medication really works in limited and preventing Thyroid Disease. However, it appears that prescribed Anthyroid may not be as effective as those who are prescribed Thyroxine.

# --------------------------------------------

#t-test analysis :
remove_nas_allhypo_age <- as.numeric(remove_nas_allhypo$age) #use as.numeric to convert from character to numeric 
#t-test with automatic 
t.test(remove_nas_allhypo_age, mu = 50)
#Ha: mean is not 50 years old 
#Ho: mean is 50 years old. 
#We automatically have 0.05, but our p-value is 1.425e-11 which is basically 0. No matter what we set the confidence level to be either 95% or some other one. 
#We have statistically significant evidence that we can reject the null hypothesis and suggest that the mean value of age is not 50 years old. 
#https://www.geeksforgeeks.org/r-language/t-test-approach-in-r-programming/
```

```{r}
# --------------------------------------------
# {R11}

# Future Implementation (Goal): Menu Visualization Integration/Website

# --------------------------------------------
```
