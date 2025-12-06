# --------------------------------------------
# {R1}
#ann_test
#ann_train

head(allhypo)
head(allhyper)
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

unique(allhypo$ThyroidClass) # "negative","compensated hypothyroid", "primary hypothyroid", "secondary hypothyroid" | where negative = no Thyroid Disease
unique(allhyper$ThyroidClass) # "negative", "hyperthyroid", "T3 toxic" --> (T3 toxic hyperthyroidism, or T3 toxicosis), "goitre" (too big/enlarged of gland --> hyperthyroid too much hormone) | where negative = no Thyroid Disease

print("==================================================================") # spacer

names(allhyper)
names(allhypo)
# -->  [1] "age" "sex" [3] "presc_thyroxine" "queried_why_on_thyroxine" [5] "presc_anthyroid_meds" "sick" [7] "pregnant" "thyroid_surgery" [9] "radioactive_iodine_therapyI131" "query_hypothyroid" [11] "query_hyperthyroid" "lithium" [13] "goitre" "tumor" [15] "hypopituitarism" "psych_condition" [17] "TSH_measured" "TSH_reading" [19] "T3_measured" "T3_reading" [21] "T4_measured" "T4_reading" [23] "thyrox_util_rate_T4U_measured" "thyrox_util_rate_T4U_reading" [25] "FTI_measured" "FTI_reading" [27] "ref_src" "ThyroidClass" [29] "record_id"
# --------------------------------------------

# --------------------------------------------
# {R4}
# Testing each variable in Hypothyroid against ThyroidClass

# --------------------------------------------

# --------------------------------------------
# {R5}
# Testing each variable in Hypothyroid against ThyroidClass

# --------------------------------------------
