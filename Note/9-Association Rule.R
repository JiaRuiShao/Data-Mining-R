titanic=read.csv(file.choose(),header=T)
head(titanic)
names(titanic)
attach(titanic)

library(arules)

rules <- apriori(titanic,parameter = list(minlen=2, supp=0.1,conf=0.2))
rules
inspect(rules)

rules1 <- apriori(titanic,parameter = list(minlen=2, supp=0.1,conf=0.2),
  appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))
inspect(rules1)

rules2 <- apriori(titanic,parameter = list(minlen=2, supp=0.001,conf=0.02),
  appearance = list(rhs=c("Survived=Yes"),lhs=c("Class=1st","Class=2nd","Age=Child"),default="none"))
inspect(rules2)

rules.new <- sort(rules1, by="confidence")
inspect(rules.new)  

#Find redundant rules#
subset <- is.subset(rules.new, rules.new)
subset[lower.tri(subset, diag=T)] <- NA
redundant <- colSums(subset, na.rm=T) >= 1
which(redundant)

#Remove redundant rules
pruned.rules <- rules.new[!redundant]
inspect(pruned.rules)

###Frequent Item Set#####
items <- apriori(titanic,parameter = list(minlen=2, maxlen=2,supp=0.2,target="frequent itemsets"))
inspect(items)
  
  

