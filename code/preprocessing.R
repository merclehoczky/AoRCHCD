
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(corrplot)
library(ggplot2)

# Load data ---------------------------------------------------------------

df <- read_csv("data/population.csv")
df <- as.data.frame(df)

# Summary statistics ------------------------------------------------------

summary(df)

# Preprocessing ------------------------------------------------------------

# Remove identifiers and extra columns
df <- df %>% 
  select(-subject_id, -hadm_id, -stay_id, 
         -first_hosp_stay, -first_icu_stay, -sepsis3) 

# Rename age column
df <- df %>% 
  rename(age = admission_age) 
  

#Change categorical values into factors
df$gender <- as.factor(df$gender)
df$race <- as.factor(df$race)
df$anticoagulants <- as.factor(df$anticoagulants)
df$hospital_expire_flag <- as.factor(df$hospital_expire_flag)
df$chronic_pulmonary_disease <- as.factor(df$chronic_pulmonary_disease)
df$diabetes_with_cc <- as.factor(df$diabetes_with_cc)
df$diabetes_without_cc <- as.factor(df$diabetes_without_cc)
df$congestive_heart_failure <- as.factor(df$congestive_heart_failure)
df$renal_disease <- as.factor(df$renal_disease)

# Regroup race ------------------------------------------------------------

subcategory_mapping <- c(
  #WHITE
  "WHITE" = "WHITE",
  "PORTUGUESE" = "WHITE",
  "WHITE - BRAZILIAN" = "WHITE",
  "WHITE - EASTERN EUROPEAN" = "WHITE",
  "WHITE - OTHER EUROPEAN" = "WHITE",
  "WHITE - RUSSIAN" = "WHITE",
  
  #BLACK 
  "BLACK" = "BLACK",
  "BLACK/AFRICAN AMERICAN" = "BLACK",
  "BLACK/AFRICAN" = "BLACK",
  "BLACK/CAPE VERDEAN" = "BLACK",
  "BLACK/CARIBBEAN ISLAND" = "BLACK",
  
  #HISPANIC
  "HISPANIC OR LATINO" = "HISPANIC",
  "HISPANIC/LATINO - CENTRAL AMERICAN" = "HISPANIC",
  "HISPANIC/LATINO - COLUMBIAN" = "HISPANIC",
  "HISPANIC/LATINO - CUBAN" = "HISPANIC",
  "HISPANIC/LATINO - DOMINICAN" = "HISPANIC",
  "HISPANIC/LATINO - GUATEMALAN" = "HISPANIC",
  "HISPANIC/LATINO - HONDURAN" = "HISPANIC",
  "HISPANIC/LATINO - MEXICAN" = "HISPANIC", 
  "HISPANIC/LATINO - PUERTO RICAN" = "HISPANIC",
  "HISPANIC/LATINO - SALVADORAN" = "HISPANIC",
  "SOUTH AMERICAN" = "HISPANIC",
  
  #ASIAN
  "ASIAN" = "ASIAN",
  "ASIAN - ASIAN INDIAN" = "ASIAN",
  "ASIAN - CHINESE" = "ASIAN",
  "ASIAN - KOREAN" = "ASIAN",
  "ASIAN - SOUTH EAST ASIAN" = "ASIAN",
  
  #NATIVE AMERICAN
  "AMERICAN INDIAN/ALASKA NATIVE" = "NATIVE AMERICAN",
  
  #PACIFIC ISLANDER
  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "PACIFIC ISLANDER",
  
  #MIXED
  "MULTIPLE RACE/ETHNICITY" = "MIXED",
  
  #UNKNOWN
  "UNKNOWN" = "UNKNOWN",
  "PATIENT DECLINED TO ANSWER" = "UNKNOWN",
  "UNABLE TO OBTAIN" = "UNKNOWN",
  "OTHER" = "UNKNOWN"
)

# Rename the subcategories into larger categories
df$race <- recode(df$race, !!!subcategory_mapping)


# Fix dates ---------------------------------------------------------------


# Find out ICU elapsed time  ----
# Convert timestamps to POSIXct format
timestamp1 <- as.POSIXct(df$icu_intime, format = "%Y-%m-%d %H:%M:%OS")
timestamp2 <- as.POSIXct(df$icu_outtime, format = "%Y-%m-%d %H:%M:%OS")

# Calculate time difference
time_difference <- difftime(timestamp2, timestamp1, units = 'hours')

# Convert the time difference to desired format (HH:MM:SS)
time_difference_formatted <- format(time_difference, format = "%H:%M:%S")

# Add it to the dataset
df$time_icu_elapsed <-as.numeric(time_difference)



# Replace NAs with 0 as is did_not_die ----
#df$deathtime <- as.numeric(df$deathtime)
#df$deathtime <- ifelse(is.na(df$deathtime), as.POSIXct(0, origin = "1970-01-01"), df$deathtime)
df$deathtime[is.na(df$deathtime)] <- 0

# Calculate deathtime: intime < deathtime <= dischargetime ----
# Convert time variables to numeric values
df$intime_numeric <- as.numeric(df$icu_intime)
df$deathtime_numeric <- as.numeric(df$deathtime)
df$discharge_time_numeric <- as.numeric(df$dischtime)

df$death <- 0

# Set 'death' column to 1 for rows satisfying the conditions
df[df$intime_numeric < df$deathtime_numeric & df$deathtime_numeric <= df$discharge_time_numeric, "death"] <- 1

# ONLY keep people who died during this admission----

df <- df[(df$hospital_expire_flag == 1 & df$death == 1) | (df$hospital_expire_flag == 0 & df$death == 0), ]


#Remove NAs----
df <- drop_na(df)

#Remove row_num
df <- df %>% select(-row_num)

# Correlations ------------------------------------------------------------
# Set the desired width and height of the image
image_width <- 10  # in inches
image_height <- 8  # in inches

# Calculate the corresponding resolution
resolution <- 300  # pixels per inch

# Calculate the actual width and height in pixels
image_width_pixels <- image_width * resolution
image_height_pixels <- image_height * resolution

# Set the resolution and size for the PNG device
png(file = "figures/Correlation matrix.png", width = image_width_pixels, height = image_height_pixels, res = resolution)

# Create plot
df %>% 
  mutate(gender = as.numeric(gender), age = as.numeric(age),  race = as.numeric(race), 
         sofa_score = as.numeric(sofa_score), oasis = as.numeric(oasis),
         charlson_comorbidity_index = as.numeric(charlson_comorbidity_index),
         hospital_expire_flag = as.numeric(hospital_expire_flag), 
         chronic_pulmonary_disease = as.numeric(chronic_pulmonary_disease),
         diabetes_with_cc = as.numeric(diabetes_with_cc), 
         diabetes_without_cc = as.numeric(diabetes_without_cc),
         congestive_heart_failure = as.numeric(congestive_heart_failure),
         renal_disease = as.numeric(renal_disease),
         anticoagulants = as.numeric(anticoagulants), 
         time_icu_elapsed = as.numeric(time_icu_elapsed)) %>% 
  select(-admittime, -dischtime, -icu_intime, -icu_outtime, -deathtime, 
         -intime_numeric, -deathtime_numeric, -discharge_time_numeric, -death) %>% 
  cor() %>% 
  corrplot.mixed(order = "hclust",
                 upper = "circle", 
                 lower = "number",
                 tl.pos = "lt",
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)
dev.off()
#ggsave(path = "figures", filename = "Correlation matrix.png")

