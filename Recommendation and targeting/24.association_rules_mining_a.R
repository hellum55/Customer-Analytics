library(arules)
rm(list=ls())

# Load the grocery data
data(Groceries)
# Have a look at the data set/container
summary(Groceries)
# Have a look at the transactions
inspect(head(Groceries,3))
# Look at the more frequent items
tail(sort(itemFrequency(Groceries)),30)
itemFrequencyPlot(Groceries,support=0.01,cex.names=1.0)
# Extract association rules
groc.rules <- apriori(Groceries, parameter=list(supp=0.01, conf=0.3,
                                                target="rules"))
# Filter the rules such that they have a lift above 3
inspect(subset(groc.rules, lift > 3))
