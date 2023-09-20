install.packages("rlang")
install.packages("tidymodels")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
library("tidymodels")
library("tidyverse")
library("stringr")
library(ggplot2)
library(lubridate)

seizures <- read_csv("CAB_serizures_medication_6_23_final.csv")

head(seizures)
names(seizures)

# remove syntactical marks
colnames(seizures)[colnames(seizures) == "No. of Seizures"] <- "CNT_Seizures"

# Column names to uppercase
colnames <- names(seizures)  
colnames <- toupper(colnames)       
names(seizures) <- colnames  # Assign the updated column names back to the data frame
names(seizures)

#Check columns were correctly imported
length(seizures$CNT_SEIZURES)
length(seizures$KEPPRA)
length(seizures$OXCARBAZEPINE)
length(seizures$ZEBENIX)

#Create aggregate of cataelyst medication 
seizures$AGG_CATALYST <- seizures$OXCARBAZEPINE + seizures$ZEBENIX

#identify any issues with data types
str(seizures)
seizures$DATE <- as.Date(seizures$DATE, format = "%m/%d/%y")

#Exclude dates from testing data
numeric_columns <- sapply(seizures, is.numeric)  # Identify numeric columns
correlation_matrix <- cor(seizures[, numeric_columns])  # Calculate correlation using only numeric columns
correlation_matrix

# Ready for regression analysis 
set.seed(1234)

seizures_split <- initial_split(seizures, prop=(0.75))
train_data <- training(seizures_split)
test_data <- testing(seizures_split)

str(train_data)
str(test_data)

ggplot(data = train_data, aes(CNT_SEIZURES, KEPPRA)) + 
  geom_point() 

ggplot(data=train_data, aes(CNT_SEIZURES, KEPPRA)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

linear_reg(mode = "regression", engine = "lm")
lm_model_seizures <- lm(CNT_SEIZURES ~ KEPPRA,  train_data)
summary(lm_model_seizures)
plot(lm_model_seizures)

linear_reg(mode = "regression", engine = "lm")
lm_model_seizures_ZO <- lm(CNT_SEIZURES ~ OXCARBAZEPINE + ZEBENIX,  train_data)
summary(lm_model_seizures_ZO)
plot(lm_model_seizures_ZO)

linear_reg(mode = "regression", engine = "lm")
lm_model_seizures_all <- lm(CNT_SEIZURES ~ KEPPRA +OXCARBAZEPINE + ZEBENIX,  train_data)
summary(lm_model_seizures_all)
plot(lm_model_seizures_all)

linear_reg(mode = "regression", engine = "lm")
lm_model_seizures_agg <- lm(CNT_SEIZURES ~ KEPPRA + AGG_CATALYST,  train_data)
summary(lm_model_seizures_agg)
plot(lm_model_seizures_agg)

# That didn't generate a strong correlation, so we try the past 24 months

latest_date <- max(seizures$DATE)  # Get the latest date in the 'seizures' dataset
start_date <- latest_date - years(2)  # Calculate the starting date by subtracting 2 years
date_range_data <- seizures[seizures$DATE >= start_date, ]

numeric_columns_drd <- sapply(date_range_data, is.numeric)  # Identify numeric columns
correlation_matrix_drd <- cor(date_range_data[, numeric_columns])  # Calculate correlation using only numeric columns
correlation_matrix_drd

drd_split <- initial_split(date_range_data, prop=(0.75))
train_data_drd <- training(drd_split)
test_data_drd <- testing(drd_split)

ggplot(data = train_data_drd, aes(CNT_SEIZURES, KEPPRA)) + 
  geom_point() 

ggplot(data=train_data_drd, aes(CNT_SEIZURES, KEPPRA)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

ggplot(data=train_data_drd, aes(CNT_SEIZURES, ZEBENIX)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

ggplot(data=train_data_drd, aes(CNT_SEIZURES, OXCARBAZEPINE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

linear_reg(mode = "regression", engine = "lm")
lm_model_drd_k <- lm(CNT_SEIZURES ~ KEPPRA,  train_data_drd)
summary(lm_model_drd_k)
plot(lm_model_drd_k)

linear_reg(mode = "regression", engine = "lm")
lm_model_drd_ZO <- lm(CNT_SEIZURES ~ OXCARBAZEPINE + ZEBENIX,  train_data_drd)
summary(lm_model_drd_ZO)
plot(lm_model_drd_ZO)

linear_reg(mode = "regression", engine = "lm")
lm_model_drd_all <- lm(CNT_SEIZURES ~ KEPPRA +OXCARBAZEPINE + ZEBENIX,  train_data_drd)
summary(lm_model_drd_all)
plot(lm_model_drd_all)

lm_poly_drd <- lm(CNT_SEIZURES ~ poly(KEPPRA, 2) + poly(OXCARBAZEPINE, 2) 
              + poly(ZEBENIX,2), train_data_drd)
summary(lm_poly_drd)
print(lm_poly_drd)

linear_reg(mode = "regression", engine = "lm")
lm_model_drd_agg <- lm(CNT_SEIZURES ~ KEPPRA + AGG_CATALYST,  train_data_drd)
summary(lm_model_drd_agg)
plot(lm_model_drd_agg)

lm_poly_drd_agg <- lm(CNT_SEIZURES ~ poly(KEPPRA, 2) + poly(AGG_CATALYST, 4), train_data_drd)
summary(lm_poly_drd_agg)
print(lm_poly_drd_agg)
plot(lm_poly_drd_agg)