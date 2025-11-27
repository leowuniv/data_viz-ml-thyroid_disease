# Research questions we plan to answer and analyze further:

# =====================================

#install.packages("ggplot2")
#install.packages("tidyverse")

# =====================================

library(ggplot2)

# CSV files:
# read.csv("") # void out later when merging into main file

# =====================================

# Histogram 1 [Comparison of Age Distribution between Patients]

ggplot(allbp, aes(x = Age)) + 
  geom_histogram()

# Plot 2

