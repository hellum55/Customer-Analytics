library(readr)
library(dplyr)
library(ggplot2)
library(DataExplorer)

data <- read.csv("nicehair_clean.csv")
#And we'll just add one more column at this stage, we'll calculate the line total by multiplying the Quantity by the UnitPrice for each line:

#Lets see the revenue through time:
options(repr.plot.width=8, repr.plot.height=3)
data %>%
  group_by(transaction_date) %>%
  summarise(revenue = sum(revenue)) %>%
  ggplot(aes(x = transaction_date, y = revenue)) + geom_line() + geom_smooth(method = 'auto', se = FALSE) + labs(x = 'Date', y = 'Revenue (DKK)', title = 'Revenue by Date')

#It appears as though sales are trending up, so that's a good sign, but that doesn't really generate any actionable insight, so let's dive into the data a bit farther

#Are people more likely to spend as the week goes on? Browsing to pass a Sunday afternoon? Procrastinating on that Friday afternoon at work? Cheering yourself up after a difficult Monday?
#Let's drill into the days of the week side of our data and see what we can uncover...
data %>%
  group_by(day_of_week) %>%
  summarise(revenue = sum(revenue)) %>%
  ggplot(aes(x = day_of_week, y = revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Revenue (£)', title = 'Revenue by Day of Week')

#Lets see which products are the most popular
# Count frequencies
product_freq <- table(data$product_name)

# Select top 15 products
top_15 <- head(sort(product_freq, decreasing = TRUE), 15)

# Plot
barplot(top_15, main = "Top 15 Product Frequencies",
        xlab = "Product Names", ylab = "Frequency",
        las = 2, cex.names = 0.7, srt = -90)

#NiceHair ships to a number of cities around Denmark. Let's drill into the data from that perspective and see what we can find out.
citySummary <- data %>%
  group_by(city)%>%
  summarise(revenue = sum(revenue), transactions = n_distinct(user_id)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(citySummary, n = 10)
sum(data$revenue)

custSummary <- data %>%
  group_by(user_id) %>%
  reframe(revenue = revenue) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(custSummary, n = 10)


