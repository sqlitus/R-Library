# Titanic Association Rules
# kaggle data: https://www.kaggle.com/c/titanic/data
# kaggle walkthrough: http://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/
# association rules: http://www.rdatamining.com/examples/association-rules
# best explanation: https://www.kdnuggets.com/2016/04/association-rules-apriori-algorithm-tutorial.html


library(tidyverse)
train <- read.csv("D:\\Work\\Libraries\\R Library\\R\\Association Rules\\Data\\train.csv")
test <- read.csv("D:\\Work\\Libraries\\R Library\\R\\Association Rules\\Data\\test.csv")

# measure surival
train %>% group_by(Survived) %>% summarise(n = n()) %>% mutate(pct = n/sum(n))
test <- test %>% mutate(Survived = 0)

# submit #1: base rate F survival
submit <- test %>% select(PassengerId, Survived)
write.csv(submit, file = "D:\\Work\\Libraries\\R Library\\R\\Association Rules\\Data\\submit.csv", row.names = FALSE)



#### using kaggle titanic w/ arules
train %>% View()
trans <- as(object = train, "transactions")
train[,c(1,2,3,6,7,8,10)] %>% str()
itemInfo(trans)




#### assoc. rules dataset from rdatamining
load("D:\\Work\\Libraries\\R Library\\R\\Association Rules\\Data\\titanic.raw.rdata")
str(titanic.raw)

library(arules)
rules <- apriori(titanic.raw)
inspect(rules)

rules <- apriori(titanic.raw,
                 parameter = list(minlen = 2, supp = .005, conf = .08),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"), default = "lhs"),
                 control = list(verbose = F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
                 
# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#visualize
library(arulesViz)
plot(rules)
plot(rules.pruned)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

# convert rules to dataframe
labels(lhs(rules)) %>% View()
labels(rhs(rules))
rules@quality %>% str()

ruledf = data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality)


# test each visualization
# # import data, convert to factor, predict continuous variable?
