# https://www.datacamp.com/community/tutorials/market-basket-analysis-r
library(readxl)
library(plyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(RColorBrewer)
rm(list=ls())

#### READ IN THE DATA AND PREPARE IT ######
# Read excel into R dataframe.
retail <- read_excel('Online_Retail.xlsx')
# Complete.cases(data) will return a logical vector indicating which rows have 
# no missing values. Then use the vector to get only rows that are complete 
# using retail[,].
retail <- retail[complete.cases(retail), ]
# Mutate function is from dplyr package. It is used to edit or add new columns 
# to dataframe. Here Description column is being converted to factor column. 
# as.factor converts column to factor column. %>% is an operator with which you 
# may pipe values to another function or expression.
retail <- retail %>% 
  mutate(Description = as.factor(Description)) %>%
  mutate(Country = as.factor(Country)) %>%
  mutate(Date=as.Date(InvoiceDate)) %>%
  mutate(TransTime = format(InvoiceDate,"%H:%M:%S")) #%>% 
#  mutate(InvoiceNo = as.numeric(as.character(InvoiceNo))) 
# Get a glimpse of your data.
glimpse(retail) 

# What you need to do is group data in the retail dataframe either by 
# CustomerID, CustomerID, and Date or you can also group data using InvoiceNo 
# and Date. We need this grouping and apply a function on it and store the 
# output in another dataframe.
# This can be done by ddply.
# The following lines of code will combine all products from one InvoiceNo and 
# date and combine all products from that InvoiceNo and date as one row, with 
# each item, separated by ','.
# ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
# The R function paste() concatenates vectors to character and separated 
# results using collapse=[any optional character string ]. Here ',' is used.
transactionData <- plyr::ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,collapse = ","))
head(transactionData)
# Set column InvoiceNo of dataframe transactionData.
transactionData$InvoiceNo <- NULL 
# Set column Date of dataframe transactionData.
transactionData$Date <- NULL 
# Rename column to items.
colnames(transactionData) <- c("items") 
head(transactionData)
# TransactionData: Data to be written
# "market_basket.csv": location of file with file name to be written to.
# quote: If TRUE it will surround character or factor column with double 
# quotes. If FALSE nothing will be quoted.
# row.names: either a logical value indicating whether the row names of x are 
# to be written along with x, or a character vector of row names to be written.
write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, 
          row.names = FALSE)


#### READ IN THE DATA TO ARULES ######
# sep tell how items are separated. In this case you have separated using ','
# tr #is the result of reading the data into Arules)
tr <- read.transactions('market_basket_transactions.csv', format = 'basket', 
                        sep=',')


##### SUMMARY ######
summary(tr)


##### ITEM FREQUENCY ######
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  #install.packages("RColorBrewer")
  #include library RColorBrewer
#  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), 
                  main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot")


##### LOOK FOR ASSOCIATION RULES ######
# Min Support as 0.001, confidence as 0.8.
# We will maximum look at 10 items, so maximum length of a set is 10.
association.rules <- apriori(tr, 
                             parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)
#inspect the 10 first association rules
inspect(association.rules[1:10]) 
# Limiting the number and size of rules
shorter.association.rules <- apriori(tr, 
                                     parameter = list(supp=0.001, conf=0.8,maxlen=3))
# Removing redundant rules - get subset rules in vector
subset.rules <- which(colSums(is.subset(association.rules,association.rules))>1) 
length(subset.rules)  #> 3913
# Remove subset rules.
subset.association.rules. <- association.rules[-subset.rules] 
# We can sort the rules by lift.
sortedRules <- sort(association.rules,by="lift",decreasing=TRUE)
inspect(sortedRules[1:10]) 


##### MORE SPECIFIC SEARCH OF RULES ######
# For example, to find what customers buy before buying 'METAL' run the 
# following line of code
# lhs = antecedents of metal the left hand side is the unknown
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),
                                   appearance = list(default="lhs",rhs="METAL"))
# We get five association rules telling us what customers buy, that might lead 
# to buying metal as well.
inspect(metal.association.rules)
# Similarly, to find the answer to the question Customers who bought METAL 
# also bought.... you will keep METAL on lhs:
# rhs = consequence (decendent)
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),
                                   appearance = list(lhs="METAL",default="rhs"))
# Decoration -> Metal and Metal -> Decoration have the same support, lift, 
# and confidence. 
inspect(head(metal.association.rules))


##### VISUALIZING ASSOCIATION RULES #####
# Filter rules with confidence greater than 0.4 or 40%.
# The plot shows that rules with high lift have low support. You can use the 
# following options for the plot.
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules the more red color, the higher is the lift.
plot(subRules)
# Interactive plots. The order is the number of items in the rule
plot(subRules,method="two-key plot")
# Plot the top 10 (n=10)
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")


