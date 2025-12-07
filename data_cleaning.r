# Disclaimer: From what our team has evaluated, each dataset contains different variables within the Thyroid class mainly based on their 
# file purpose for testing (i.e. "sick_euthyroid" tests for euthyroid sick syndrome (ESS)) but many attributes (around 29 are mostly the 
# same set over all the databases) where they are mostly numeric or Boolean valued attributes.

#importing datasets
allhypo <- read.csv("datasets/allhypo.data", header = F) #import of allhypo.data file
allhypo_test <- read.csv("datasets/allhypo.test", header = F)
allhyper <- read.csv("datasets/allhyper.data", header = F)
allhyper_test <- read.csv("datasets/allhyper.test", header = F)
allbp <- read.csv("datasets/allbp.data", header = F)
allrep <- read.csv("datasets/allrep.data", header = F)
dis <- read.csv("datasets/dis.data", header = F)
hypothyroid <- read.csv("datasets/hypothyroid.data", header = F)
new_thyroid <- read.csv("datasets/new-thyroid.data", header = F) 
sick_euthyroid <- read.csv("datasets/sick-euthyroid.data", header = F)
sick <- read.csv("datasets/sick.data", header = F)
thyroid0387 <- read.csv("datasets/thyroid0387.data", header = F)
ann_test <- read.csv("datasets/ann-test.data", header = F)
ann_train <- read.csv("datasets/ann-train.data", header = F)

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