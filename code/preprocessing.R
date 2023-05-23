
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(corrplot)
library(ggplot2)

# Load data ---------------------------------------------------------------

df <- read_csv("data/population.csv")
df <- as.data.frame(df)

# Summary statistics ------------------------------------------------------

summary(df)

#Preprocessing ------------------------------------------------------------


#Change categorical values into factors
df$gender <- as.factor(df$gender)
df$race <- as.factor(df$race)
df$sepsis3 <- as.factor(df$sepsis3)
df$anticoagulants <- as.factor(df$anticoagulants)
df$icd_code <- as.factor(df$icd_code)
df$icd_version <- as.factor(df$icd_version)
df$hospital_expire_flag <- as.factor(df$hospital_expire_flag)


# Fix dates ---------------------------------------------------------------


# Calculate Deathtime to ICU_in
# Convert timestamps to POSIXct format
timestamp1 <- as.POSIXct(df$icu_intime, format = "%Y-%m-%d %H:%M:%OS")
timestamp2 <- as.POSIXct(df$deathtime, format = "%Y-%m-%d %H:%M:%OS")

# Calculate time difference
time_difference <- difftime(timestamp2, timestamp1, units = 'hours')

# Add it to the dataset
df$time_in_death <-as.numeric(time_difference)

# Calculate Deathtime to ICU_out
# Convert timestamps to POSIXct format
timestamp1 <- as.POSIXct(df$icu_outtime, format = "%Y-%m-%d %H:%M:%OS")
timestamp2 <- as.POSIXct(df$deathtime, format = "%Y-%m-%d %H:%M:%OS")

# Calculate time difference
time_difference <- difftime(timestamp2, timestamp1, units = 'hours')

# Add it to the dataset
df$time_out_death <-as.numeric(time_difference)


# Find out ICU elapsed time 
# Convert timestamps to POSIXct format
timestamp1 <- as.POSIXct(df$icu_intime, format = "%Y-%m-%d %H:%M:%OS")
timestamp2 <- as.POSIXct(df$icu_outtime, format = "%Y-%m-%d %H:%M:%OS")

# Calculate time difference
time_difference <- difftime(timestamp2, timestamp1, units = 'hours')

# Convert the time difference to desired format (HH:MM:SS)
time_difference_formatted <- format(time_difference, format = "%H:%M:%S")

# Add it to the dataset
df$time_icu_elapsed <-as.numeric(time_difference)

# Replace NAs with 0 as is did_not_die
#df$deathtime <- as.numeric(df$deathtime)
#df$deathtime <- ifelse(is.na(df$deathtime), as.POSIXct(0, origin = "1970-01-01"), df$deathtime)
df$deathtime[is.na(df$deathtime)] <- 0
df$time_in_death[is.na(df$time_in_death)] <- 0
df$time_out_death[is.na(df$time_out_death)] <- 0


#Remove NAs
df <- drop_na(df)


# Correlations ------------------------------------------------------------
# Initialize file path

df %>% 
  mutate(hospital_expire_flag = as.numeric(hospital_expire_flag), anticoagulants = as.numeric(anticoagulants), 
         gender = as.numeric(gender), race = as.numeric(race), age = as.numeric(age),
         icd_code = as.numeric(icd_code), time_icu_elapsed = as.numeric(time_icu_elapsed)) %>% 
  select(-hadm_id, -subject_id, -sepsis3, -icu_intime, -icu_outtime, -deathtime, -icd_version, 
         -time_in_death, -time_out_death) %>% 
  cor() %>% 
  corrplot.mixed(order = "hclust",
                 upper = "circle", 
                 lower = "number",
                 tl.pos = "lt",
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)
ggsave(path = "figures", filename = "Correlation matrix.png")

