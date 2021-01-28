#####################################################################
# Niklas Leander Kampe | 16-611-618
# University of St. Gallen
# Development Economics 
# Prof. Dr. Charles Gottlieb
# Do File 2: Household Roster
#####################################################################

install.packages("tidyverse")
library(tidyverse)

# Working directory
getwd()
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

# Structure
  # id_hh --> 1: HHID --> DONE
  # id_ind --> 1: PID --> DONE
  # ind_sex --> 1: h2q3 --> DONE
  # ind_position --> 1: h2q4 --> DONE
  # ind_age --> 1: h2q8 --> DONE
  # ind_religion --> Code as dismiss
  # ind_marital_status --> 1: h2q10 --> DONE
  # ind_years_in_location --> 2: h3q15 --> DONE
  # ind_permanent --> Compute --> DONE
  # n_ind_household --> Compute --> DONE
  # n_child_15 --> Compute --> DONE
  # n_child_5 --> Compute --> DONE

data_raw_1 <- read_csv("01 data_raw/gsec2.csv")
data_raw_2 <- read_csv("01 data_raw/gsec3.csv")

# Selecting needed columns from first data set
data_selected_1 <- 
  select(data_raw_1, HHID, PID, h2q3, h2q4, h2q5, h2q8, h2q10)
  
# Selecting needed columns from second data set
data_selected_2 <- 
  select(data_raw_2, PID, h3q15)

# Merging the selected data from first and second data set
data_selected <- 
  left_join(data_selected_1, data_selected_2)

# Adding the number of individuals per household
data_mutated_n_ind_household <- 
  group_by(data_selected, HHID)  %>% 
  mutate(n_ind_household = n()) %>% 
  ungroup()

# Adding the number dummy variable of residence status and delete the absolute time in the household
data_mutated_ind_permanent <- 
  mutate(data_mutated_n_ind_household, ind_permanent = ifelse(h2q5 > 4, 1, 2)) %>% 
  select(-h2q5)
  
# Adding the number of individuals per household under the age of 15
data_mutated_n_child_15 <-
  group_by(data_mutated_ind_permanent, HHID)  %>% 
  mutate(n_child_15 = sum(h2q8 < 15, na.rm = T)) %>% 
  ungroup()

# Adding the number of individuals per household under the age of 5
data_mutated_n_child_5 <-
  group_by(data_mutated_n_child_15, HHID)  %>% 
  mutate(n_child_5 = sum(h2q8 < 5, na.rm = T)) %>% 
  ungroup()

# Adding missing values for religion
data_mutated_religion <-
  mutate(data_mutated_n_child_5, ind_religion = NA)

# Rename the columns
data_final <- 
  rename(data_mutated_n_child_5, 
         id_hh = HHID,
         id_ind = PID,
         ind_sex = h2q3,
         ind_position = h2q4,
         ind_age = h2q8,
         ind_marital_status = h2q10,
         ind_years_in_location = h3q15)

# Set new working directory where to save the cleaned data
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/03 data_clean")

# Save cleaned data as CSV
write.csv(data_final, file = "Household_Roster.csv", row.names = FALSE)
