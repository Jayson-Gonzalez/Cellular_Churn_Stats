library(ggplot2)
library("datasets")
library("tidyverse")    

churn <- read.csv('churn.txt')
churning <- as_tibble(churn)

#PART 1
churn_IntPlan <- churning %>% group_by(Int.l.Plan) %>% summarise(Churn. = mean(Churn. == "True"))
ggplot(churn_IntPlan, aes(x = Int.l.Plan, y = Churn., fill = Int.l.Plan)) + geom_bar(stat = "identity", position = "dodge", fill = c("skyblue", "salmon")) + labs(title = "Churn Rate by International Plan", x = "Internationa Plan", y = "Churn Rate") + scale_fill_discrete(name = "International Plan", labels = c("No", "Yes"))

#PART 2 Total Charge to Churn chart
totalprice <- churning %>% mutate(Total.Charge = Day.Charge + Eve.Charge + Night.Charge + Intl.Charge)
ggplot(totalprice, aes(x = Churn., y = Total.Charge, fill = Churn.)) + geom_boxplot() + labs(title = "Total Charges by Churn Status")

#part 3 Customers with high Mins likely to churn?
totaltime <- churning %>% mutate(Total.Mins = Day.Mins + Eve.Mins + Night.Mins + Intl.Mins)
ggplot(totaltime, aes(x = Total.Mins, fill = Churn.)) + geom_density(alpha = 0.5) + labs(title = "Distribution of Number of Total Mins by Churn Status")

#Part 4 We can see that because the values are close, International Mins does no have a strong influence on Churn Stat
summary(churning$Intl.Mins)
t_test <- t.test(Intl.Mins ~ Churn., churning)
t_test

#PART 5 Decision Tree on Churning with stats

ch <- churn %>% select(-State, -Account.Length,-VMail.Message ,-Area.Code, -Phone, -Int.l.Plan, -VMail.Plan)
ch$Churn. <- factor(ch$Churn., levels = c("True", "False"), labels = c("True", "False"))
set.seed(100)

train <- sample(nrow(ch), 0.7 * nrow(ch))
ch.train <- ch[train, ]
ch.validate <- ch[-train, ]

print(table(ch.train$Churn.))
print(table(ch.validate$Churn.))

library(rpart)
dtree <- rpart(Churn. ~ ., data = ch.train, method = "class")

print(dtree$cptable)

dtree.pruned <- prune(dtree, cp = 0.01)

library(rpart.plot)
print(prp(dtree.pruned, type = 2, extra = 104, main = "Decision Tree"))

dtree.pred <- predict(dtree.pruned, ch.validate, type = "class")

dtree.perf <- table(ch.validate$Churn., dtree.pred, dnn = c("Actual", "Predicted"))
print(dtree.perf)

tn <- dtree.perf[1, 1]
fp <- dtree.perf[1, 2]
fn <- dtree.perf[2, 1]
tp <- dtree.perf[2, 2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
error_rate <- (fp + fn) / (tp + tn + fp + fn)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f_measure <- (2 * precision * recall) / (precision + recall)

print(accuracy)
print(error_rate)
print(sensitivity)
print(specificity)
print(precision)
print(recall)
print(f_measure)

#Part 6 Naïve Bayesian Classification
library(e1071)
nb.model <- naiveBayes(Churn.~.,data = ch.train)
nb.pred <- predict(nb.model, ch.validate)
nb.perf <- table(ch.validate$Churn., nb.pred, dnn=c("Actual", "Predicted"))
print(nb.perf)

tn <- nb.perf[1, 1]
fp <- nb.perf[1, 2]
fn <- nb.perf[2, 1]
tp <- nb.perf[2, 2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
error_rate <- (fp + fn) / (tp + tn + fp + fn)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f_measure <- (2 * precision * recall) / (precision + recall)

print(accuracy)
print(error_rate)
print(sensitivity)
print(specificity)
print(precision)
print(recall)
print(f_measure)

#Part 7
ggplot(churning, aes(x = Churn., y = CustServ.Calls, fill = Churn.)) + geom_boxplot() + labs(title = "CustServ.Calls by Churn Status")

#Part 8
stateChurn <- churning %>% group_by(State) %>% summarise(Churn. = mean(Churn. == "True"))

ggplot(stateChurn , aes(x = reorder(State, -Churn.), y = Churn.)) + geom_bar(stat = "identity", fill = "blue") + labs(title = "Churn Rate by State", x = "State", y = "Churn Rate") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Part 9
churn_by_voicemail <- churning %>% group_by(VMail.Plan) %>% summarise(Churn. = mean(Churn. == "True"))

ggplot(churn_by_voicemail, aes(x = VMail.Plan, y = Churn., fill = VMail.Plan)) + geom_bar(stat = "identity", position = "dodge", fill = c("red", "blue")) + labs(title = "Churn Rate by Voice Mail Plan", x = "Voice Mail Plan", y = "Churn Rate") + scale_fill_discrete(name = "Voice Mail Plan", labels = c("No", "Yes"))

#Part 10  there may be a slight trend of longer account lengths among churned customers

ggplot(churning, aes(x = Account.Length, color = Churn.)) + stat_ecdf(geom = "step") + labs(title = "Account Length for Churned Customers", x = "Account Length", y = "Cumulative Probability", color = "Churn.") 
