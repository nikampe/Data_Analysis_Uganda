#####################################################################
# Niklas Leander Kampe | 16-611-618
# University of St. Gallen
# Development Economics 
# Prof. Dr. Charles Gottlieb
# Do File 1: Household identification
#####################################################################

install.packages("tidyverse")
library(tidyverse)

# Working directory
getwd()
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

# Structure
  # id_hh --> HHID --> DONE
  # wgt --> wgt --> DONE
  # ea --> es --> DONE
  # rural --> urban --> DONE

# Load data
data_raw <- read_csv("01 data_raw/gsec1.csv")

# Select necessary variables
data_selected <- 
  select(data_raw, HHID, wgt, ea, urban)

# Check the values of urban
count(data_selected, urban) 
# Result shows that just 1 and o are values

# Code dummy variable urban to rural with values 1 and 2
data_ruralAdjusted <- 
  mutate(data_selected, rural = ifelse(urban == 0, 2, 1))

# Cross-check if the variable urban got adjusted correctly (n has to be true)
table(data_ruralAdjusted$rural,data_ruralAdjusted$urban)

# Unselect the variable urban
data_urbanUnselected <- 
  select(data_ruralAdjusted, -urban)

# Rename the columns
data_renamed <- 
  rename(data_urbanUnselected, id_hh = HHID)

# Final data
data_final <- 
  arrange(data_renamed, id_hh)

# Set new working directory where to save the cleaned data
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/03 data_clean")

# Save cleaned data as CSV
write.csv(data_final, file = "Household_Identification.csv", row.names = FALSE)
