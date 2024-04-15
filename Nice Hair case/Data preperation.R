# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations
# Feature engineering packages
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks
library(DataExplorer)

####Load the data:
data <- read.csv("nicehair_data_final.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

summary(data$device)
#Dropping the ID columns:
data <- select(data, -c(user_id, transaction_id, campaign_id, transaction_line_number, device))

#Checking the amount of the missing values.
sum(is.na(data)) # 59845 missing values 
plot_missing(data) 
#All the missing variables are in the revenue column. We might consider to just drop them because the data set is quite large

#Feature filtering:
caret::nearZeroVar(data, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
#The majority of the country variable consists of DK observations. We might want to eliminate this variable beacuse
#It does not add any value.
data <- select(data, -c(country))
summary(data)

#I choose to delete the missing data, because it seems to be the same rows that are missing observations.
data <- na.omit(data)
sum(is.na(data))

#Looking at the distribution of the target variable
plot_histogram(data$revenue)  
#It looks alright to be fair. A bit left skewed though
# display histograms for categorical values
plot_bar(data)

# Find categories with counts less than the threshold
library(forcats)
summary(data$operating_system)
small_categories1 <- names(table(data$browser))[table(data$browser) > 1873]
small_categories2 <- names(table(data$default_channel_group))[table(data$default_channel_group) > 1434]
small_categories3 <- names(table(data$operating_system))[table(data$operating_system) > 601]

# Replace small categories with "other"
data$browser <- fct_other(data$browser, keep = small_categories1, other_level = "Other")
data$default_channel_group <- fct_other(data$default_channel_group, keep = small_categories2, other_level = "Other")
data$operating_system <- fct_other(data$operating_system, keep = small_categories3, other_level = "Other")

#Plot the factors again
plot_bar(data)
