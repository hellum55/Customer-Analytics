# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations
# Feature engineering packages
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks
library(DataExplorer)
library(forcats)

####Load the data:
data <- read.csv("nicehair_data_final.csv", stringsAsFactors=TRUE,
                    na.strings = c("", " ", "NA", "N/A", ".", "NaN", "MISSING"))

summary(data$device)
#Dropping the ID columns:
data <- select(data, -c(transaction_id, campaign_id, transaction_line_number, device))

#Checking the amount of the missing values.
sum(is.na(data)) # 59845 missing values 
plot_missing(data) 
#All the missing variables are in the revenue column. We might consider to just drop them because the data set is quite large
summary(data$country)

##### Might not be relevant ####
#Feature filtering:
caret::nearZeroVar(data, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)
#The majority of the country variable consists of DK observations. We might want to eliminate this variable beacuse
#It does not add any value.
data <- select(data, -c(country))
summary(data)
####                      ####

#I choose to delete the missing data, because it seems to be the same rows that are missing observations.
data <- na.omit(data)
sum(is.na(data))

#Looking at the distribution of the target variable
plot_histogram(data$revenue)  
#It looks alright to be fair. A bit left skewed though
# display histograms for categorical values
plot_bar(data)

# Find categories with counts less than the threshold
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

#Extract all the date features from transaction_date
#Remove the first word (day of the week)
data$transaction_date <- as.character(data$transaction_date)
data$day_of_week <- sapply(data$transaction_date, FUN = function(x) {strsplit(x, split = '[, ]')[[1]][1]})

data$transaction_date <- gsub("^[^,]+, ", "", data$transaction_date)
data$transaction_date <- gsub(", ", " ", data$transaction_date)
data$transaction_date <- gsub(" ", "-", data$transaction_date)
data$transaction_date <- strsplit(data$transaction_date, "-")

# Convert the day component to two digits with leading zeros
data$transaction_date <- lapply(data$transaction_date, function(x) {
  x[2] <- sprintf("%02d", as.numeric(x[2]))
  x
})
data$transaction_date <- sapply(data$transaction_date, function(x) paste(x, collapse = "-"))

# Convert full month names to abbreviated forms
data$transaction_date <- gsub("January", "Jan", data$transaction_date)
data$transaction_date <- gsub("February", "Feb", data$transaction_date)
data$transaction_date <- gsub("March", "Mar", data$transaction_date)
data$transaction_date <- gsub("April", "Apr", data$transaction_date)
data$transaction_date <- gsub("May", "May", data$transaction_date)  # No abbreviation
data$transaction_date <- gsub("June", "Jun", data$transaction_date)
data$transaction_date <- gsub("July", "Jul", data$transaction_date)
data$transaction_date <- gsub("August", "Aug", data$transaction_date)
data$transaction_date <- gsub("September", "Sep", data$transaction_date)
data$transaction_date <- gsub("October", "Oct", data$transaction_date)
data$transaction_date <- gsub("November", "Nov", data$transaction_date)
data$transaction_date <- gsub("December", "Dec", data$transaction_date)

# Transform into date format
data$transaction_date <- as.Date(data$transaction_date, format = "%B-%d-%Y")

write_csv(data,"nicehair_clean.csv")

