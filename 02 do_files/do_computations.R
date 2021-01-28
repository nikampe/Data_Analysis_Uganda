#####################################################################
# Niklas Leander Kampe | 16-611-618
# University of St. Gallen
# Development Economics 
# Prof. Dr. Charles Gottlieb
# Do File 5: Computations
#####################################################################

install.packages("tidyverse")
library(tidyverse)
install.packages("ggeffects")
library(ggeffects)
install.packages("ggplot2")
library(ggplot2)
install.packages("texreg")
library(texreg)
install.packages("Hmisc")
library(Hmisc)
install.packages("xtable")
library(xtable)

# Working directory
getwd()
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

data_raw_hh <- read_csv("03 data_clean/Household_Identification.csv")
data_raw_ind_characteristics <- 
  read_csv("03 data_clean/Household_Roster.csv")
data_raw_ind_education <- 
  read_csv("03 data_clean/Education_Section.csv") %>%
  select(-id_hh)
data_raw_labor <- 
  read_csv("03 data_clean/Labor_Section.csv") %>%
  select(-id_hh)

# Merging data based on the personal identifier
data_raw_ind_hh <- 
  left_join(data_raw_ind_characteristics, data_raw_hh, by = "id_hh")

data_raw <-
  left_join(data_raw_ind_hh,
            left_join(data_raw_ind_education, data_raw_labor, by = "id_ind"),
            by = "id_ind")
  
# 1. Descriptive Statistics

## 1.1 Computation of average age and plot of age distribution
data_raw %>%
  select(ind_age, wgt) %>%
  drop_na(ind_age, wgt) %>%
  filter(ind_age >= 0 && wgt >= 0) %>%
  summarise(mean_population_age = weighted.mean(ind_age, wgt, na.rm = TRUE))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Age_Distribution.png")
ggplot(data_raw, aes(x = ind_age)) +
  geom_histogram() +
  geom_vline(xintercept = 22.5, color = "red") +
  annotate("text", x = 25, y = 2125, label = "Mean", size = 3.5, color = "red", angle = 90) +
  scale_x_continuous(breaks=c(0, 30, 60, 90, 22.5)) +
  labs(title = "Age Distribution in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Count",
       x = "Age") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14)))
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

## 1.2 Computation of average years of schooling and of education gap in school vears between males and females
data_raw %>%
  select(ind_education_years, wgt) %>%
  drop_na(ind_education_years, wgt) %>%
  filter(ind_education_years >= 0 && wgt >= 0) %>%
  summarise(mean_education_years = weighted.mean(ind_education_years, wgt, na.rm = TRUE))

data_raw %>%
  select(ind_education_years, wgt, ind_sex) %>%
  drop_na(ind_education_years, wgt, ind_sex) %>%
  filter(ind_education_years >= 0 && wgt >= 0 && ind_sex > 0) %>%
  summarise(gender_education_gap = weighted.mean(data.matrix(.[ind_sex == 1, "ind_education_years"]), data.matrix(.[ind_sex == 1, "wgt"]), na.rm = TRUE) -
                                   weighted.mean(data.matrix(.[ind_sex == 2, "ind_education_years"]), data.matrix(.[ind_sex == 2, "wgt"]), na.rm = TRUE))

## 1.3 Computation of share of people with ability to read and write (litercy rates) and plot the literacy rates by age groups 
literacy_rate <- 
  sum(data_raw$ind_literacy == 1, na.rm = TRUE) /
  (sum(data_raw$ind_literacy == 1, na.rm = TRUE) + sum(data_raw$ind_literacy == 2, na.rm = TRUE))
literacy_rate

literacy_rate_15_25 <- 
  sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 15 & data_raw$ind_age <= 25), na.rm = TRUE) /
  (sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 15 & data_raw$ind_age <= 25), na.rm = TRUE) + sum(data_raw$ind_literacy == 2 & (data_raw$ind_age >= 15 & data_raw$ind_age <= 25), na.rm = TRUE))
literacy_rate_25_35 <- 
  sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 25 & data_raw$ind_age <= 35), na.rm = TRUE) /
  (sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 25 & data_raw$ind_age <= 35), na.rm = TRUE) + sum(data_raw$ind_literacy == 2 & (data_raw$ind_age >= 25 & data_raw$ind_age <= 35), na.rm = TRUE))
literacy_rate_35_45 <- 
  sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 35 & data_raw$ind_age <= 45), na.rm = TRUE) /
  (sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 35 & data_raw$ind_age <= 45), na.rm = TRUE) + sum(data_raw$ind_literacy == 2 & (data_raw$ind_age >= 35 & data_raw$ind_age <= 45), na.rm = TRUE))
literacy_rate_45_55 <- 
  sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 45 & data_raw$ind_age <= 55), na.rm = TRUE) /
  (sum(data_raw$ind_literacy == 1 & (data_raw$ind_age >= 45 & data_raw$ind_age <= 55), na.rm = TRUE) + sum(data_raw$ind_literacy == 2 & (data_raw$ind_age >= 45 & data_raw$ind_age <= 55), na.rm = TRUE))

literacy_rate_overview <-
  data.frame(age = c("15-25", "25-35", "35-45", "45-55"),
             literacy_rate = c(literacy_rate_15_25, literacy_rate_25_35, literacy_rate_35_45, literacy_rate_45_55))
literacy_rate_overview

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Literacy_Rates.png")
ggplot(literacy_rate_overview, aes(x = age, y = literacy_rate)) +
  geom_bar(stat="identity") +
  geom_text(label = round(literacy_rate_overview$literacy_rate * 100, digits = 2), colour="white", size = 5, aes(y = literacy_rate_overview$literacy_rate * 0.5)) +
  annotate("text", x = 4.4, y = 0.67, label = "Mean", size = 3.5, color = "red") +
  geom_hline(yintercept = literacy_rate, color = "red") +
  scale_y_continuous(labels=scales::percent, breaks=c(0.25, 0.5, 0.75, 1, literacy_rate)) +
  labs(title = "Literacy Rates in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Literacy Rate",
       x = "Age Group") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14)))
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

## 1.4 Computation of share of people with primary/secondary/tertiary education and plot of shares per age groups (15-25, 25-35, 35-45, 45-55)
working_population <-
  filter(data_raw, ind_age >= 15 & ind_age <= 55)

share_primary_education <-
  nrow(subset(working_population, ind_education == 1)) / nrow(working_population)
share_secondary_education <-
  nrow(subset(working_population, ind_education == 2)) / nrow(working_population)
share_tertiary_education <-
  nrow(subset(working_population, ind_education == 3)) / nrow(working_population)

share_education_overview <-
  data.frame(education_stage = c("Primary", "Secondary", "Tertiary"),
             share = c(share_primary_education, share_secondary_education, share_tertiary_education))
share_education_overview

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Education_Rates.png")
ggplot(share_education_overview, aes(x = education_stage, y = share)) +
  geom_bar(stat="identity", fill = c("#262626", "#868686", "#c1c1c1")) +
  geom_text(label = round(share_education_overview$share * 100, digits = 2), colour="white", size = 5, aes(y = share_education_overview$share * 0.5)) +
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Education Rates in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Share of Working Age Population",
       x = "Education Level") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14)))
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

share_primary_education_15_25 <- 
  sum(data_raw$ind_education == 1 & (data_raw$ind_age >= 15 & data_raw$ind_age <= 25), na.rm = TRUE) /
  sum(data_raw$ind_age >= 15 & data_raw$ind_age <= 25, na.rm = TRUE)
share_primary_education_25_35 <- 
  sum(data_raw$ind_education == 1 & (data_raw$ind_age >= 25 & data_raw$ind_age <= 35), na.rm = TRUE) /
  sum(data_raw$ind_age >= 25 & data_raw$ind_age <= 35, na.rm = TRUE)
share_primary_education_35_45 <- 
  sum(data_raw$ind_education == 1 & (data_raw$ind_age >= 35 & data_raw$ind_age <= 45), na.rm = TRUE) /
  sum(data_raw$ind_age >= 35 & data_raw$ind_age <= 45, na.rm = TRUE)
share_primary_education_45_55 <- 
  sum(data_raw$ind_education == 1 & (data_raw$ind_age >= 45 & data_raw$ind_age <= 55), na.rm = TRUE) /
  sum(data_raw$ind_age >= 45 & data_raw$ind_age <= 55, na.rm = TRUE)

share_secondary_education_15_25 <- 
  sum(data_raw$ind_education == 2 & (data_raw$ind_age >= 15 & data_raw$ind_age <= 25), na.rm = TRUE) /
  sum(data_raw$ind_age >= 15 & data_raw$ind_age <= 25, na.rm = TRUE)
share_secondary_education_25_35 <- 
  sum(data_raw$ind_education == 2 & (data_raw$ind_age >= 25 & data_raw$ind_age <= 35), na.rm = TRUE) /
  sum(data_raw$ind_age >= 25 & data_raw$ind_age <= 35, na.rm = TRUE)
share_secondary_education_35_45 <- 
  sum(data_raw$ind_education == 2 & (data_raw$ind_age >= 35 & data_raw$ind_age <= 45), na.rm = TRUE) /
  sum(data_raw$ind_age >= 35 & data_raw$ind_age <= 45, na.rm = TRUE)
share_secondary_education_45_55 <- 
  sum(data_raw$ind_education == 2 & (data_raw$ind_age >= 45 & data_raw$ind_age <= 55), na.rm = TRUE) /
  sum(data_raw$ind_age >= 45 & data_raw$ind_age <= 55, na.rm = TRUE)

share_tertiary_education_15_25 <- 
  sum(data_raw$ind_education == 3 & (data_raw$ind_age >= 15 & data_raw$ind_age <= 25), na.rm = TRUE) /
  sum(data_raw$ind_age >= 15 & data_raw$ind_age <= 25, na.rm = TRUE)
share_tertiary_education_25_35 <- 
  sum(data_raw$ind_education == 3 & (data_raw$ind_age >= 25 & data_raw$ind_age <= 35), na.rm = TRUE) /
  sum(data_raw$ind_age >= 25 & data_raw$ind_age <= 35, na.rm = TRUE)
share_tertiary_education_35_45 <- 
  sum(data_raw$ind_education == 3 & (data_raw$ind_age >= 35 & data_raw$ind_age <= 45), na.rm = TRUE) /
  sum(data_raw$ind_age >= 35 & data_raw$ind_age <= 45, na.rm = TRUE)
share_tertiary_education_45_55 <- 
  sum(data_raw$ind_education == 3 & (data_raw$ind_age >= 45 & data_raw$ind_age <= 55), na.rm = TRUE) /
  sum(data_raw$ind_age >= 45 & data_raw$ind_age <= 55, na.rm = TRUE)

share_education_overview_age_groups <-
  data.frame(age_groups = c("15-25", "25-35", "35-45", "45-55"),
             primary = c(share_primary_education_15_25, share_primary_education_25_35, share_primary_education_35_45, share_primary_education_45_55),
             secondary = c(share_secondary_education_15_25, share_secondary_education_25_35, share_secondary_education_35_45, share_secondary_education_45_55),
             tertiary = c(share_tertiary_education_15_25, share_tertiary_education_25_35, share_tertiary_education_35_45, share_tertiary_education_45_55))

share_education_overview_age_groups <-
  share_education_overview_age_groups %>%
  gather(key, value, -age_groups)

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Education_Rates_By_Groups.png")
ggplot(share_education_overview_age_groups, aes(age_groups, value, fill = key)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Education Rates by Age Groups in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Share of Working Age Population per Age Group",
       x = "Age Group",
       fill = "Education") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14))) +
  scale_fill_grey()
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

# 2. Labor markets in low-income countries

## 2.1: Labor market status

### 2.1.1 Computation of the working age population 
share_working_age_population <- 
  nrow(subset(data_raw, ind_age >= 15 & ind_age <= 55)) / nrow(data_raw)
share_working_age_population

### 2.1.2 Computation of the employment rate within the working age population and split-up in wage and self-employed workers
employment_rate <-
  nrow(subset(data_raw, (ind_age >= 15 & ind_age <= 55) & ind_work == 1)) / nrow(subset(data_raw, ind_age >= 15 & ind_age <= 55))
employment_rate

share_wage_worker <-
 nrow(subset(data_raw, ind_main_job_type == 1)) / nrow(subset(data_raw, ind_age >= 15 & ind_age <= 55))
share_wage_worker

share_self_employed_worker <-
  nrow(subset(data_raw, ind_main_job_type == 2)) / nrow(subset(data_raw, ind_age >= 15 & ind_age <= 55))
share_self_employed_worker

## 2.2 Wage measurement

### 2.2.1 Computation of weekly hours worked by wage workers and their distribution
wage_workers <-
  filter(data_raw, ind_main_job_type == 1)

weekly_hours_worked <-
  select(wage_workers, ind_main_job_hours, wgt) %>%
  drop_na(ind_main_job_hours, wgt) %>%
  filter(ind_main_job_hours >= 0 && wgt >= 0) %>%
  rename(weekly_hours = ind_main_job_hours)

weekly_hours_worked <- 
  weekly_hours_worked[!is.infinite(rowSums(weekly_hours_worked)),]

describe(weekly_hours_worked) # Calculate 10th and 90th percentiles

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Weekly_Hours_Worked.png")
ggplot(weekly_hours_worked, aes(x = weekly_hours)) +
  geom_histogram() +
  geom_vline(xintercept = weighted.mean(weekly_hours_worked$weekly_hours, weekly_hours_worked$wgt, na.rm = TRUE), color = "red") +
  annotate("text", x = weighted.mean(weekly_hours_worked$weekly_hours, weekly_hours_worked$wgt, na.rm = TRUE) - 4, y = 94, label = "Mean", size = 3.5, color = "red", angle = 90) +
  geom_vline(xintercept = 12, color = "orange") +
  annotate("text", x = 12 - 5, y = 90, label = "10th Percentile", size = 3.5, color = "orange", angle = 90) +
  geom_vline(xintercept = 68, color = "orange") +
  annotate("text", x = 68 - 5, y = 90, label = "90th Percentile", size = 3.5, color = "orange", angle = 90) +
  labs(title = "Weekley Hours Worked by Wage Workers in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Count",
       x = "Weekley hours worked") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 12)))
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

### 2.2.2 Computation of the hourly wage per wage worker

wages <-
  select(data_raw, ind_main_job_wage_hourly, ind_main_job_wage) %>%
  drop_na(ind_main_job_wage_hourly) %>%
  filter(ind_main_job_wage_hourly >= 0)

wages <- 
  wages[!is.infinite(rowSums(wages)),]

describe(wages$ind_main_job_wage_hourly) # Calculate 10th and 90th percentiles

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Hourly_Wages.png")
ggplot(wages, aes(x = log(ind_main_job_wage_hourly))) +
  geom_histogram() +
  geom_vline(xintercept = log(329.7), color = "orange") +
  annotate("text", x = log(329.7 - 70), y = 110, label = "10th Percentile", size = 3.5, color = "orange", angle = 90) +
  geom_vline(xintercept = log(4615.4), color = "orange") +
  annotate("text", x = log(4615.4 + 2000), y = 110, label = "90th Percentile", size = 3.5, color = "orange", angle = 90) +
  scale_x_log10() +
  labs(title = "Hourly Wages in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Count",
       x = "Hourly wage (logarithmic scale)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14)))
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

## 2.3 Returns to education

### 2.3.1 Computation of the returns to education as the wage increase due to an additional year education (regression of hourly wage on the years of education)
regression_data <-
  select(data_raw, ind_main_job_wage_hourly, ind_education_years, wgt) %>%
  drop_na(ind_main_job_wage_hourly, ind_education_years, wgt) %>%
  filter(ind_main_job_wage_hourly > 0 & wgt >= 0 & ind_education_years >= 0) %>%
  rename(hourly_wage = ind_main_job_wage_hourly,
         education_years = ind_education_years)

regression_data <- 
  regression_data[!is.infinite(rowSums(regression_data)),]

return_to_education <- 
  lm(log(hourly_wage) ~ poly(education_years, 2, raw = TRUE), data = regression_data)
summary(return_to_education)$coefficients
texreg(return_to_education)

plot_data <-
  ggpredict(return_to_education, terms = "education_years", back.transform = FALSE)

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Regression_1.png")
ggplot(plot_data, aes(x = x, y = predicted)) + 
  geom_line(size = 2) +
  scale_y_log10() +
  labs(title = "Return to Education in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Hourly wage (logarithmic scale)",
       x = "Years of schooling") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14)))
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

prediction <-
  plot_data %>%
  select(x, predicted) %>%
  mutate(predicited_hourly_wage = exp(1)^predicted) %>%
  rename("education_years" = x,
         "log(predicited_hourly_wage)" = predicted)

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Latex Files")
print(xtable(prediction, type = "latex"), file = "prediction.tex")
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

### 2.3.2 Computation of the returns to education under addition of the controls gender, location and age
regression_control_data <-
  select(data_raw, ind_main_job_wage_hourly, ind_education_years, ind_age, ind_sex, rural, wgt) %>%
  drop_na(ind_main_job_wage_hourly, ind_education_years, ind_age, ind_sex, rural) %>%
  filter(ind_main_job_wage_hourly > 0 & ind_education_years >= 0 & wgt >= 0) %>%
  mutate(ind_sex_adjusted = ifelse(ind_sex == 2, 0, 1)) %>% # female = 0, male = 1
  mutate(rural_adjusted = ifelse(rural == 2, 0, 1)) %>% # rural = 0, urban = 1
  select(-ind_sex, -rural) %>%
  rename(hourly_wage = ind_main_job_wage_hourly,
         education_years = ind_education_years,
         age = ind_age,
         sex = ind_sex_adjusted,
         location = rural_adjusted)

regression_control_data <- 
  regression_control_data[!is.infinite(rowSums(regression_control_data)),]

return_to_education_controlled <- 
  lm(log(hourly_wage) ~ poly(education_years, 2, raw = TRUE) + sex + location + age, data = regression_control_data) 
summary(return_to_education_controlled)$coefficients
texreg(return_to_education_controlled)

plot_data_controlled <-
  ggpredict(return_to_education_controlled, terms = "education_years", back.transform = FALSE)

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Regression_2.png")
ggplot(plot_data_controlled, aes(x = x, y = predicted)) + 
  geom_line(size = 2) +
  scale_y_log10() +
  labs(title = "Controlled Return to Education in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Hourly wage (logarithmic scale)",
       x = "Years of schooling") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14))) 
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

prediction_controlled <-
  plot_data_controlled %>%
  select(x, predicted) %>%
  mutate(predicited_hourly_wage = exp(1)^predicted) %>%
  mutate(delta_to_model_1 = predicited_hourly_wage-prediction$predicited_hourly_wage) %>%
  rename("education_years" = x,
         "log(predicited_hourly_wage)" = predicted)

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Latex Files")
print(xtable(prediction_controlled, type = "latex"), file = "prediction_controlled.tex")
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

## 2.4. Computation of the different characteristics of self-employed and wage workers

### Computation of the distribution of years of schooling for wage workers and self-employed
education_wage_worker <- 
  data_raw %>%
  select(ind_main_job_type, ind_education_years, wgt) %>%
  filter(ind_main_job_type == 1 & ind_education_years >= 0) %>%
  drop_na(ind_main_job_type, ind_education_years, wgt) %>%
  rename(education_years = ind_education_years,
         job_type = ind_main_job_type)

education_wage_worker <- 
  education_wage_worker[!is.infinite(rowSums(education_wage_worker)),]

education_wage_worker %>%
  filter(wgt >= 0) %>%
  summarise(mean_education = weighted.mean(education_years, wgt, na.rm = TRUE))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Education_Wage_Worker.png")
ggplot(education_wage_worker, aes(x = education_years)) + 
  geom_bar() +
  geom_vline(xintercept = 8.04, color = "red") +
  annotate("text", x = 8.04 + 0.5, y = 237, label = "Mean", size = 3.5, color = "red", angle = 90) +
  scale_y_continuous(limits = c(0, 250), breaks = c(50, 100, 150, 200, 250)) +
  labs(title = "Education of Wage Workers in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Count",
       x = "Years of schooling") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14))) 
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

education_self_employed <- 
  data_raw %>%
  select(ind_main_job_type, ind_education_years, wgt) %>%
  filter(ind_main_job_type == 2 & ind_education_years >= 0) %>%
  drop_na(ind_main_job_type, ind_education_years, wgt) %>%
  rename(education_years = ind_education_years,
         job_type = ind_main_job_type)

education_self_employed <- 
  education_self_employed[!is.infinite(rowSums(education_self_employed)),]

education_self_employed %>%
  filter(wgt >= 0) %>%
  summarise(mean_education = weighted.mean(education_years, wgt, na.rm = TRUE))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Education_Self_Employed.png")
ggplot(education_self_employed, aes(x = education_years)) + 
  geom_bar() +
  geom_vline(xintercept = 7.37, color = "red") +
  annotate("text", x = 7.37 + 0.7, y = 237, label = "Mean", size = 3.5, color = "red", angle = 90) +
  scale_y_continuous(limits = c(0, 250), breaks = c(50, 100, 150, 200, 250)) +
  labs(title = "Education of Self-Employed in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Count",
       x = "Years of schooling") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14))) 
dev.off()

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

### Computation of the distribution of males/females as wage workers and self-employed
share_gender_employment_type <-
  data_raw %>%
  select(ind_sex, ind_main_job_type) %>%
  drop_na(ind_sex, ind_main_job_type) %>%
  summarise(share_male_wage_workers = round(sum(ind_sex == 1 & ind_main_job_type == 1) / sum((ind_sex == 1 | ind_sex == 2) & ind_main_job_type == 1) * 100, 2),
            share_female_wage_workers = round(sum(ind_sex == 2 & ind_main_job_type == 1) / sum((ind_sex == 1 | ind_sex == 2) & ind_main_job_type == 1) * 100, 2),
            share_male_self_employed = round(sum(ind_sex == 1 & ind_main_job_type == 2) / sum((ind_sex == 1 | ind_sex == 2) & ind_main_job_type == 2) * 100, 2),
            share_female_self_employed = round(sum(ind_sex == 2 & ind_main_job_type == 2) / sum((ind_sex == 1 | ind_sex == 2) & ind_main_job_type == 2) * 100, 2))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Latex Files")
print(xtable(share_gender_employment_type, type = "latex"), file = "gender_job_share.tex")
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

### Computation of the distribution of locations of wage workers and self-employed
share_location_employment_type <-
  data_raw %>%
  select(rural, ind_main_job_type) %>%
  drop_na(rural, ind_main_job_type) %>%
  summarise(share_urban_wage_workers = round(sum(rural == 1 & ind_main_job_type == 1) / sum((rural == 1 | rural == 2) & ind_main_job_type == 1) * 100, 2),
            share_rural_wage_workers = round(sum(rural == 2 & ind_main_job_type == 1) / sum((rural == 1 | rural == 2) & ind_main_job_type == 1) * 100, 2),
            share_urban_self_employed = round(sum(rural == 1 & ind_main_job_type == 2) / sum((rural == 1 | rural == 2) & ind_main_job_type == 2) * 100, 2),
            share_rural_self_employed = round(sum(rural == 2 & ind_main_job_type == 2) / sum((rural == 1 | rural == 2) & ind_main_job_type == 2) * 100, 2))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Latex Files")
print(xtable(share_location_employment_type, type = "latex"), file = "location_job_share.tex")
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

### Computation of the average ages of wage workers and self-employed
mean_ages_employment_type <-
  data_raw %>%
  select(ind_age, ind_main_job_type) %>%
  drop_na(ind_age, ind_main_job_type) %>%
  filter(ind_age >= 0 & ind_main_job_type >= 0) %>%
  summarise(mean_age_wage_worker = mean(data.matrix(.[ind_main_job_type == 1, "ind_age"]), na.rm = TRUE),
            mean_age_self_employed = mean(data.matrix(.[ind_main_job_type == 2, "ind_age"]), na.rm = TRUE))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Latex Files")
print(xtable(mean_ages_employment_type, type = "latex"), file = "mean_ages_job_types.tex")
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

### Computation of the average weekly hours worked of wage workers and self-employed
mean_weekly_hours_employment_type <-
  data_raw %>%
  select(ind_main_job_hours, ind_main_job_type) %>%
  drop_na(ind_main_job_hours, ind_main_job_type) %>%
  filter(ind_main_job_hours >= 0 & ind_main_job_type >= 0) %>%
  summarise(mean_weekly_hours_wage_worker = mean(data.matrix(.[ind_main_job_type == 1, "ind_main_job_hours"]), na.rm = TRUE),
            mean_weekly_hours_self_employed = mean(data.matrix(.[ind_main_job_type == 2, "ind_main_job_hours"]), na.rm = TRUE))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Latex Files")
print(xtable(mean_weekly_hours_employment_type, type = "latex"), file = "mean_weekly_hours_job_types.tex")
setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618")

### Computation of the average weekly hours worked of wage workers and self-employed
employment_type_regional_distribution <-
  data_raw %>%
  select(rural, ind_main_job_type) %>%
  drop_na(rural, ind_main_job_type) %>%
  filter(rural >= 0 & ind_main_job_type >= 0)

employment_type_regional_distribution <-
  data.frame(employment_type = c("urban", "rural", "urban", "rural"),
             key = c("self_employed", "self_employed", "wage_worker", "wage_worker"),
             value = c(round(sum(employment_type_regional_distribution$rural == 1 & employment_type_regional_distribution$ind_main_job_type == 2) / sum(employment_type_regional_distribution$rural == 1), 2),
                       round(sum(employment_type_regional_distribution$rural == 2 & employment_type_regional_distribution$ind_main_job_type == 2) / sum(employment_type_regional_distribution$rural == 2), 2),
                       round(sum(employment_type_regional_distribution$rural == 1 & employment_type_regional_distribution$ind_main_job_type == 1) / sum(employment_type_regional_distribution$rural == 1), 2),
                       round(sum(employment_type_regional_distribution$rural == 2 & employment_type_regional_distribution$ind_main_job_type == 1) / sum(employment_type_regional_distribution$rural == 2), 2)))

setwd("/Users/niklaskampe/Desktop/EmpiricalAssignment_NiklasLeanderKampe_16611618/00 resources/Plots")

png("Employment_Type_Region.png")
ggplot(employment_type_regional_distribution, aes(employment_type, value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Employment Types by Region in Uganda (2013-2014)",
       subtitle = "Data: World Bank | National Panel Survey Uganda 2013-2014",
       y = "Percentage",
       x = "Region",
       fill = "Type") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = (element_text(size = 14))) +
  scale_fill_grey()
dev.off()





