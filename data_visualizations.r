# --------------------------------------------
# {R1}
#ann_test
#ann_train

head(allhypo)
head(allhyper)

head(remove_nas_allhypo) # cleaned for NAs
head(remove_nas_allhyper) # cleaned for NAs
# --------------------------------------------

# --------------------------------------------
# {Markdown File}

# Hypothyroidism vs Hyperthyroidism

#According to the UCLA Geffen School of Medicine, they state in the article "Hypothyroidism vs Hyperthyroidism - What's The Difference?", that "hypothyroidism is an autoimmune condition called Hashimoto's Thyroiditis, where the immune system attacks the thyroid gland" and that "[hyperthyroidism]...is having an overactive thyroid gland or having too much thyroid hormone in the body” (Sievert, 3 - 4). Essentially, hypothyroidism (underactive - producing too little thyroid hormone) and hyperthyroidism (overactive - producing too much thyroid hormone) are common thyroid diseases that affect how a human's body functions which includes a variety of factors. These factors for hyperthyroidism include cons like weight loss, rapid heart rate recordings, anxiety, tremors, diarrhea and much more for instance while hypothyroidism has the opposite effects of hyperthyroidism which include but are not limited to weight gain, fatigue, and slow heart rate recordings. 

#Link to source: https://medschool.ucla.edu/news-article/hypothyroidism-vs-hyperthyroidism-whats-the-difference

# Our main focus

#Research Question: How do/which patients' attributes (i.e. age, sex, etc...) and hormone levels contribute to the association with thyroid disease diagnosis and sub-type (hypothyroid vs hyperthyroid) the most?
# --------------------------------------------

# --------------------------------------------
# {R2}
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
# --------------------------------------------

# --------------------------------------------
# {R3}
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
# --------------------------------------------

# --------------------------------------------
# {R4}
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

# --------------------------------------------

# --------------------------------------------
# {R5}
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
# --------------------------------------------

# --------------------------------------------
# {R6}
# Brief Analysis (Numerical Variables):

# Hypo

# (1) - Age: This is the age of the patient in the context of the hypo dataset. 

# (2) - TSH_reading: TSH or thyorid stimulating hormone. This reading determines the level of the TSH in your body, in the context of the hypo dataset. 

# (3) - T3_reading: Triiodothyronine or T3_reading regulates and controls factors like metabolism, growth, and heartrate. 

# (4) - T4_reading: T4 or thyroxine is another test used to find out the levels of thyroxine in your body and a lot of the time work with T3 a lot.  
          #This is in the context of the hypo dataset.

# (5) - thyrox_util_rate_T4U_reading: This is the Thyroxine Utilization Rate, it measures how much of the t4 can be bounded to various proteins compared to those that are not bounded to various proteins, this is in the context of the hypo dataset. 

# (6) - FTI_reading: This is known as the free thyroxine indenx, as the name suggests it is related to the T4. This reading measures the amount of T4. 

# =============================================

# Hyper 

# (1) - Age: This is the age of the patient in the context of the hyper dataset.

# (2) - TSH_reading: TSH or thyorid stimulating hormone. This reading determines the level of the TSH in your body, in the context of the hyper dataset. 

# (3) - T3_reading: Triiodothyronine or T3_reading regulates and controls factors like metabolism, growth, and heartrate in the context of the hyper dataset. 

# (4) - T4_reading: T4 or thyroxine is another test used to find out the levels of thyroxine in your body and a lot of the time work with T3 a lot.  
          #This is in the context of the hyper dataset. 

# (5) - thyrox_util_rate_T4U_reading: This is the Thyroxine Utilization Rate, it measures how much of the t4 can be bounded to various proteins compared to those that are not bounded to various proteins, this is in the context of the hyper dataset. 

# (6) - FTI_reading: This is known as the free thyroxine indenx, as the name suggests it is related to the T4. This reading measures the amount of T4.

# --------------------------------------------

# --------------------------------------------
# {R7}

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

# --------------------------------------------

# --------------------------------------------
# {R8}
# Brief Analysis (Categorical Variables):

# Hypo

# (1) - Sex:

# (2) - presc_thyroxine: Thyroxine is a medication that treats Thyroid Disease (primarily hypothyroidism); It can be observed that patients who are prescribed thyroxine medication generally tend to have negative results of thyroid disease compared to those who are not. Those who tested positive for thyroid disease mainly had "compensated hypothyroid" and "primary hypothyroid". 

# (3) - queried_why_on_thyroxine:

# (4) - presc_anthyroid_meds:

# (5) - sick:

# (6) - pregnant:

# (7) - thyroid_surgery:

# (8) - radioactive_iodine_therapyI131:

# (9) - query_hypothyroid:

# (10) - query_hyperthyroid:

# (11) - lithium:

# (12) - goitre:

# (13) - tumor:

# (14) - hypopituitarism:

# (15) - psych_condition:

# (16) - TSH_measured:

# (17) - T3_measured:

# (18) - T4_measured:

# (19) - thyrox_util_rate_T4U_measured: 

# (20) - FTI_measured:

# (21) - ref_src:

# (22) - ThyroidClass: N/A [Invalid Test, void like record_id]

# =============================================

# Hyper 

# (1) - Sex:

# (2) - presc_thyroxine: Similarly to the hypothyroid bar plot, it can be observed that patients who are prescribed thyroxine medication generally tend to have negative results of thyroid disease compared to those who are not. Those who tested positive for thyroid disease mainly had "hyperthyroid". However, it appears that the medication works much better on hyperthyroid prevention although further research or statistical tests would have to be conducted further. 

# (3) - queried_why_on_thyroxine:

# (4) - presc_anthyroid_meds:

# (5) - sick:

# (6) - pregnant:

# (7) - thyroid_surgery:

# (8) - radioactive_iodine_therapyI131:

# (9) - query_hypothyroid:

# (10) - query_hyperthyroid:

# (11) - lithium:

# (12) - goitre:

# (13) - tumor:

# (14) - hypopituitarism:

# (15) - psych_condition:

# (16) - TSH_measured:

# (17) - T3_measured:

# (18) - T4_measured:

# (19) - thyrox_util_rate_T4U_measured: 

# (20) - FTI_measured:

# (21) - ref_src:

# (22) - ThyroidClass: N/A [Invalid Test, void like record_id]

# --------------------------------------------

# --------------------------------------------
# {R9}

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

# Analysis: Observed that there seemed to be more variables leading to greater importance and affect in hyperthyroid compared to hypothyroid although the hypothyroid coefficient contains more outliers such as (4.308487786) for thyroid_surgery which means that thyroid surgery is a leading factor in result of those who have the hypothyroid disease. Meanwhile, it is intriguing that thyroid_surgery only has a coefficient importance of 0.528055152. This leads us to further research statistical questions and answers that certain variables affect certain types of Thyroid disease (i.e. Hypo and Hyper-thyroid) particularly. 

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

# 2. Surgery?

#  ───────────────────────────────────────────────────────────────

# --------------------------------------------

# --------------------------------------------
# {R10}

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

# Confidence Interval Test

# 0. ML attributes test

print(confusionMatrix(predicted_classes, test_data$ThyroidClass))

#  ───────────────────────────────────────────────────────────────

# Chi-Square test (categorical variables)

# 1. Question: Medication 

# Null Hypothesis (H_0):
# Alt. Hypothesis (H_a): 

# --------------------------------------------

# --------------------------------------------
# {R11}



# --------------------------------------------
