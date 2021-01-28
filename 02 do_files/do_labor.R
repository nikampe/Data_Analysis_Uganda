#####################################################################
# Niklas Leander Kampe | 16-611-618
# University of St. Gallen
# Development Economics 
# Prof. Dr. Charles Gottlieb
# Do File 4: Labor Section
#####################################################################

install.packages("tidyverse")
library(tidyverse)

# Working directory
getwd()
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

# Structure
  # PART 1
    # id_hh --> 1: HHID --> DONE
    # id_ind --> 1: PID --> DONE
    # ind_age --> 2: h2q8 --> DONE
    # ind_work --> 1: h8q4 
    # ind_emp_search --> 1: h8q16
    # ind_emp_search_mean --> 1: h8q17a & h8q17b
    # ind_start_se --> 1: h8q17
    # ind_nlf_reason --> 1: h8q18
  # PART 2
    # ind_main_job_type --> 1: h8q4(wage worker); h8q6 (self-employed); h8q10 (apprentice - wage worker); h8q8 (unpaid worker); h8q13 (household's farm - unpaid worker); h8q15 (unpaid worker)
    # ind_main_job_se_type --> 1: h8q22
    # ind_main_job_employer --> 1: h8q35
    # ind_main_job_hours --> 1: h8q36a, h8q36a, h8q36b, h8q36c, h8q36d, h8q36e, h8q36f, h8q36g
    # ind_main_job_wage --> 1: h8q31a, h8q31b, h8q31c
    # ind_main_job_wage_freq --> 1: h8q31c
    # ind_main_job_wage_hourly --> 1: h8q31a, h8q31b, h8q36a, h8q36b, h8q36c, h8q36d, h8q36e, h8q36f, h8q36g (ind_main_job_hours)

data_raw_1 <- read_csv("01 data_raw/gsec8_1.csv")
data_raw_2 <- read_csv("01 data_raw/gsec2.csv")

# Selecting needed columns from the first the data set (labor stats)
data_selected_1 <- 
  select(data_raw_1, HHID, PID, h8q4, h8q6, h8q8, h8q10, h8q12, h8q15, h8q16, h8q17a, h8q17, h8q18, h8q22, h8q31a, h8q31b, h8q31c, h8q35, h8q36a, h8q36b, h8q36c, h8q36d, h8q36e, h8q36f, h8q36g)

# Selecting needed columns from the second data set (age)
data_selected_2 <- 
  select(data_raw_2, PID, h2q8)

# Merging the selected data from first and second data set
data_selected <- 
  left_join(data_selected_1, data_selected_2)

# PART 1: Labor Market Status

# Adding a column that states if individual worked in the last 7 days
data_mutated_ind_work <-
  mutate(data_selected, ind_work = ifelse(h2q8 < 15, NA, 
                                                  ifelse (h8q4 == 1 | h8q6 == 1 | h8q8 == 1 | h8q10 == 1 | h8q12 == 1, 1, 2))) %>%
  select(-h2q8)

# Adding a column that states if individual was looking for a job
data_mutated_ind_emp_search <-
  mutate(data_mutated_ind_work, 
         ind_emp_search = ifelse(ind_work != 2, NA, 
                                 ifelse(h8q16 == 1, 1, 2))) %>%
  select(-h8q16)

# Adding a column that states if how job seekers were looking for a job
data_mutated_ind_emp_search_mean <-
  mutate(data_mutated_ind_emp_search,
         ind_emp_search_mean = ifelse(ind_emp_search != 1, NA,
                                      case_when(h8q17a == 1 ~ 1,
                                                h8q17a == 2 ~ 2,
                                                h8q17a == 3 ~ 4,
                                                h8q17a == 4 ~ 5))) %>%
  select(-h8q17a)

# Adding a column that states if individual tried to start a business in the last 4 weeks
data_mutated_ind_start_se <-
  mutate(data_mutated_ind_emp_search_mean, 
         ind_start_se = ifelse(ind_work != 2, NA,
                               ifelse(h8q17 == 1, 1, 2))) %>%
  select(-h8q17)

# Adding a column that states if individual tried to start a business in the last 4 weeks
data_mutated_ind_nlf_reason <-
  mutate(data_mutated_ind_start_se, 
         ind_nlf_reason = ifelse(ind_work != 2, NA,
                                 case_when(h8q18 == 1 | h8q18 == 2 ~ 3,
                                           h8q18 == 3 ~ 1,
                                           h8q18 == 4 ~ 2,
                                           h8q18 == 5 ~ 4,
                                           h8q18 >= 6 & h8q18 <= 8 ~ 5))) %>%
  select(-h8q18)

# PART 2: Main Job

# Adding a column that states the employment type of each individual
data_mutated_ind_main_job_type <-
  mutate(data_mutated_ind_nlf_reason, 
         ind_main_job_type = ifelse(ind_work != 1, NA, 
                                    case_when(h8q4 == 1 | h8q10 == 1 | h8q15 == 1 ~ 1,
                                              h8q6 == 1 ~ 2,
                                              h8q8 == 1 | h8q12 == 1 ~ 3))) %>%
  select(-h8q4, -h8q6, -h8q8, -h8q10, -h8q12, -h8q15)

# Adding a column that states if self-employed are employers or own-account workers
data_mutated_ind_main_job_se_type <-
  mutate(data_mutated_ind_main_job_type, 
         ind_main_job_se_type = ifelse(ind_main_job_type != 2, NA,
                                       case_when(h8q22 == 2 ~ 1, 
                                                 h8q22 == 3 ~ 2))) %>%
  select(-h8q22)

# Adding a column that states the sector of the individual's employer
data_mutated_in_main_job_employer <-
  mutate(data_mutated_ind_main_job_se_type, 
         ind_main_job_employer = ifelse(ind_work != 1, NA,
                                        case_when(h8q35 == 1 | h8q35 == 2 | h8q35 == 3 ~ 1, h8q35 == 5 ~ 2,
                                                  h8q35 == 7 ~ 4))) %>%
  select(-h8q35)

# Adding a column that states the amount of hours the individual worked the last 7 days
data_mutated_ind_main_job_hours <-
  mutate(data_mutated_in_main_job_employer, ind_main_job_hours = ifelse(ind_work != 1, NA, (h8q36a + h8q36b + h8q36c + h8q36d + h8q36e + h8q36f + h8q36g))) %>%
  select(-h8q36a, -h8q36b, -h8q36c, -h8q36d, -h8q36e, -h8q36f, -h8q36g)

# Adding a column that states the wage/labor income
data_mutated_ind_main_job_wage <-
  mutate(data_mutated_ind_main_job_hours, 
         ind_main_job_wage = ifelse(ind_work != 1 | ind_main_job_type != 1 | (h8q31a + h8q31b) < 0, NA, (h8q31a + h8q31b))) %>%
  select(-h8q31a, -h8q31b)

# Adding a column that states the payment period of the wage/labor income
data_mutated_ind_main_job_wage_freq <-
  mutate(data_mutated_ind_main_job_wage, ind_main_job_wage_freq = ifelse(ind_work != 1, NA,
                                                                         case_when(h8q31c == 1 ~ 0,
                                                                                   h8q31c == 2 ~ 1,
                                                                                   h8q31c == 3 ~ 2,
                                                                                   h8q31c == 4 ~ 3)))

# Adding a column that states the hourly wage/labor income
average_weeks_per_month <- 52/12
data_mutated_ind_main_job_wage_hourly <-
  mutate(data_mutated_ind_main_job_wage_freq, 
         ind_main_job_wage_hourly = ifelse(ind_work != 1, NA, 
                                           case_when(ind_main_job_wage_freq == 0 ~ ind_main_job_wage,
                                                     ind_main_job_wage_freq == 1 ~ ind_main_job_wage / (ind_main_job_hours/7),
                                                     ind_main_job_wage_freq == 2 ~ ind_main_job_wage / ind_main_job_hours,
                                                     ind_main_job_wage_freq == 3 ~ (ind_main_job_wage / average_weeks_per_month) / ind_main_job_hours))) %>%
  select(-h8q31c)

# Rename the columns
data_final <- 
  rename(data_mutated_ind_main_job_wage_hourly, 
         id_hh = HHID,
         id_ind = PID)

# Set new working directory where to save the cleaned data
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/03 data_clean")

# Save cleaned data as CSV
write.csv(data_final, file = "Labor_Section.csv", row.names = FALSE)


