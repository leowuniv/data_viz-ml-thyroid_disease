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
# Blue --> Categorical Data Visualizations
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

# (1) - Age:

# (2) - TSH_reading:

# (3) - T3_reading: Triiodothyronine or T3_reading regulates and controls factors like metabolism, growth, and heartrate. 

# (4) - T4_reading:

# (5) - thyrox_util_rate_T4U_reading:

# (6) - FTI_reading:

# =============================================

# Hyper 

# (1) - Age:

# (2) - TSH_reading:

# (3) - T3_reading:

# (4) - T4_reading:

# (5) - thyrox_util_rate_T4U_reading:

# (6) - FTI_reading:

# --------------------------------------------

# --------------------------------------------
# {R7}


# --------------------------------------------

# --------------------------------------------
# {R8}


# --------------------------------------------

# --------------------------------------------
# {R9}


# --------------------------------------------
