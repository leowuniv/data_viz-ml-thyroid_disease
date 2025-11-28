#importing datasets
allhypo <- read.csv("allhypo.data", header = F) #import of allhypo.data file
allhyper <- read.csv("allhyper.data", header = F)
allbp <- read.csv("allbp.data", header = F)
allrep <- read.csv("allrep.data", header = F)
dis <- read.csv("dis.data", header = F)
hypothyroid <- read.csv("hypothyroid.data", header = F)
new_thyroid <- read.csv("new-thyroid.data", header = F) 
sick_euthyroid <- read.csv("sick-euthyroid.data", header = F)
sick <- read.csv("sick.data", header = F)
thyroid0387 <- read.csv("thyroid0387.data", header = F)

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
  names(dataset)[names(dataset) == "V27"] <- "ref_src"
  # names(dataset)[names(dataset) == "V28"] <- "psych_condition" drop column
  dataset <- subset(dataset, select = -V28) 
  names(dataset)[names(dataset) == "V29"] <- "ThyroidClass"
  names(dataset)[names(dataset) == "V30"] <- "record_id"
  
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
  
  return(dataset)
}

# rename 30-col datasets
allhypo <- rename_30col(allhypo)
allhyper <- rename_30col(allhyper)
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