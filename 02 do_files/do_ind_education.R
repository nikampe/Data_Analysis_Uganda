#####################################################################
# Niklas Leander Kampe | 16-611-618
# University of St. Gallen
# Development Economics 
# Prof. Dr. Charles Gottlieb
# Do File 3: Education Section
#####################################################################

install.packages("tidyverse")
library(tidyverse)

# Working directory
getwd()
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

# Structure
  # id_hh --> HHID --> DONE
  # id_ind --> PID --> DONE
  # ind_read --> h4q4 --> DONE
  # ind_write --> h4q4 --> DONE
  # ind_literacy --> h4q4 --> DONE
  # ind_any_schooling --> h4q5 --> DONE
  # ind_school --> h4q5 --> DONE
  # ind_education --> h4q7 --> DONE
  # ind_education_years --> h4q5 & h4q7 --> DONE

data_raw <- read_csv("01 data_raw/gsec4.csv")

# Selecting needed columns from the data set
data_selected <- 
  select(data_raw, HHID, PID, h4q4, h4q5, h4q5, h4q7)

# Adding a column which indicates the ability to read per individual
data_mutated_ind_read <- 
  mutate(data_selected, ind_read = ifelse(h4q4 == 2 | h4q4 == 4, 1, 2))

# Adding a column which indicates the ability to write per individual
data_mutated_ind_write <- 
  mutate(data_mutated_ind_read, ind_write = ifelse(h4q4 == 4, 1, 2))

# Adding a column which indicates the ability to read and write per individual
data_mutated_ind_read_write <- 
  mutate(data_mutated_ind_write, ind_literacy = ifelse(h4q4 == 4, 1, 2)) %>%
  select(-h4q4)

# Adding a column which indicates if individual has attended / is attending any school
data_mutated_ind_any_schooling <-
  mutate(data_mutated_ind_read_write, ind_any_schooling = ifelse(h4q5 == 2 | h4q5 == 3, 1, 2))

# Adding a column which indicates if individual is currently attending any school
data_mutated_ind_school <-
  mutate(data_mutated_ind_any_schooling, ind_school = ifelse(h4q5 == 3, 1, 2)) %>%
  select(-h4q5)

# Adding a column which indicated the highest completed degree per individual
data_mutated_ind_education <-
  mutate(data_mutated_ind_school, ind_education = ifelse(ind_school == 1, NA, 
                                                         ifelse(is.na(h4q7), NA, 
                                                                case_when(h4q7 == 10 ~ 0,
                                                                          h4q7 >= 11 & h4q7 <= 17 ~ 1,
                                                                          h4q7 >= 21 & h4q7 <= 33 ~ 2,
                                                                          h4q7 >= 34 & h4q7 <= 36 ~ 3,
                                                                          h4q7 == 41 | h4q7 == 51 ~ 4,
                                                                          h4q7 == 61 ~ 5))))

# Adding a column which indicated the highest completed degree per individual
data_mutated_ind_education_years <-
  mutate(data_mutated_ind_education, ind_education_years = ifelse(ind_school == 1, NA, 
                                                                  ifelse(is.na(h4q7), NA,
                                                                         case_when(h4q7 == 10 ~ 0,
                                                                                   h4q7 == 11 ~ 1,
                                                                                   h4q7 == 12 ~ 2,
                                                                                   h4q7 == 13 ~ 3,
                                                                                   h4q7 == 14 ~ 4,
                                                                                   h4q7 == 15 ~ 5,
                                                                                   h4q7 == 16 ~ 6,
                                                                                   h4q7 == 17 ~ 7,
                                                                                   h4q7 == 21 ~ 8,
                                                                                   h4q7 == 22 ~ 9,
                                                                                   h4q7 == 23 ~ 10,
                                                                                   h4q7 == 31 ~ 11,
                                                                                   h4q7 == 32 ~ 12,
                                                                                   h4q7 == 33 ~ 13,
                                                                                   h4q7 == 41 ~ 12,
                                                                                   h4q7 == 51 ~ 12,
                                                                                   h4q7 == 61 ~ 15)))) %>%
  select(-h4q7)

# Rename the columns
data_final <- 
  rename(data_mutated_ind_education_years, 
         id_hh = HHID,
         id_ind = PID)

# Set new working directory where to save the cleaned data
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/03 data_clean")

# Save cleaned data as CSV
write.csv(data_final, file = "Education_Section.csv", row.names = FALSE)


  