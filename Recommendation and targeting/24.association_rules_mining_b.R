library(arules)
rm(list=ls())

## Load, inspect, and prepare data for mining
# Load the supermarket data
retail.raw <- readLines("http://goo.gl/FfjDAO")
# Have a look at the data set/container
head(retail.raw);tail(retail.raw)
summary(retail.raw)
# Prepare for mining
retail.list <- strsplit(retail.raw, " ")
# And check the result
str(retail.list)
# Assign a name to each row/transaction
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")
# Look at a randomly chosen element
library(car)
set.seed(1234)
some(retail.list)
# Remove original data
rm(retail.raw)
# Transform the data to a transactions object
retail.trans <- as(retail.list, "transactions") # takes a few seconds
summary(retail.trans)
# Remove data in list format
rm(retail.list)

## Find and visualize association rules
# Extract association rules
retail.rules <- apriori(retail.trans, parameter=list(supp=0.001, conf=0.4))
library(arulesViz)
# Plot the resulting rules
plot(retail.rules)
# Interactive plot - select area by marking two opposite corners of a rectangle
plot(retail.rules, engine="interactive")
# Find the subset of rules with highest lift
retail.hi <- head(sort(retail.rules, by="lift"), 50)
inspect(retail.hi)
# Plot this subset
plot(retail.hi, method="graph",engine = "htmlwidget")

## Prepare the introduction of item profitability (margin)
# First get item names
retail.itemnames <- sort(unique(unlist(as(retail.trans, "list"))))
head(retail.itemnames); tail(retail.itemnames)
# Simulate per-item margin data
set.seed(03870)
retail.margin <- data.frame(margin=rnorm(length(retail.itemnames),
                                         mean=0.30, sd=0.30))
# Inspect margin data
quantile(retail.margin$margin)
# Adjust rownames
rownames(retail.margin) <- retail.itemnames
head(retail.margin); tail(retail.margin)
some(retail.margin)
# Find the margin for the items in basket {39,48} and their sum
retail.margin[c("39", "48"), ]
sum(retail.margin[c("39", "48"), ])
# Get items in third transaction and calculate sum of their margins
(basket.items <- as(retail.trans[3], "list")[[1]])
retail.margin[basket.items, ]
sum(retail.margin[basket.items, ])
# Make a more generic function to do the calculations
retail.margsum <- function(items, itemMargins) {
  # Input: 
  #     "items" == item names, rules or transactions in arules format
  #     "itemMargins" == a data frame of profit margin indexed by name
  # Output: 
  #     look up the item margins, and return the sum
  library(arules)
  # check the class of "items" and coerce appropriately to an item list
  if (class(items) == "rules") {
    tmp.items <- as(items(items), "list") # rules ==> item list
  } else if (class(items) == "transactions") {
    tmp.items <- as(items, "list") # transactions ==> item list
  } else if (class(items) == "list") {
    tmp.items <- items # it’s already an item list!
  } else if (class(items) == "character") {
    tmp.items <- list(items) # characters ==> item list
  } else {
    stop("Don’t know how to handle margin for class ", class(items))
  }
  # make sure the items we found are all present in itemMargins
  good.items <- unlist(lapply(tmp.items, function (x)
    all(unlist(x) %in% rownames(itemMargins))))
  if (!all(good.items)) {
    warning("Some items not found in rownames of itemMargins. ",
            "Lookup failed for element(s):\n",
            which(!good.items), "\nReturning only good values.")
    tmp.items <- tmp.items[good.items]
  }
  # and add them up
  return(unlist(lapply(tmp.items, function(x) sum(itemMargins[x, ]))))
}
# Test the new function supplying the items in various formats
retail.margsum(c("39", "48"), retail.margin)
retail.margsum(list(t1=c("39", "45"), t2=c("31", "32")), retail.margin)
retail.margsum(retail.trans[101:103], retail.margin)
retail.margsum(retail.hi, retail.margin)
retail.margsum(c("hello", "world"), retail.margin) # error!
retail.margsum(list(a=c("39", "45"), b=c("hello", "world"), c=c("31", "32")),
               retail.margin) # only the first and third are OK
