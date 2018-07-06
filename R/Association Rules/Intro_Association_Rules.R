library(arules); library(tidyverse)

data("Groceries")
data(package="arules")

Groceries
str(Groceries)
summary(Groceries)
inspect(Groceries)
itemFrequency(Groceries)
itemFrequencyPlot(Groceries, support = .1)
itemFrequencyPlot(Groceries, topN = 15)
# support: % of transactions with item A. .2 = 20% of transactions have item.
# confidence: chance of B given A P(B|A). conf({A,B} -> C) = support({A,B,C}) / support({A,B})
# rule with high confidence: rule has high predictive accuracy.

# build model to predict C given A,B. give us "rules"
m1 <- apriori(Groceries) # no rules. support >= .1, confidence >= .8
m1 <- apriori(Groceries, parameter = list(support=.007, confidence=.25, minlen=2))

summary(m1)
inspect(m1[1:30])
test <- inspect(m1[1:30])
colnames(test)[2] <- "mark"
test %>% arrange(desc(confidence)) %>% View()
test


# lift = confidence / support(y). "how much more likely is the item to be purchased with x vs all other transactions"
test <- inspect(m1[1:50])
test %>% filter(lhs == "{herbs}")
inspect(sort(m1, by="lift")[1:10])



# reference: subsetting to end
length(m1)
inspect(m1[361:length(m1)])
inspect(m1[-(1:361)])
