
#load training and test data sets
train <- read.csv("C:/Users/japas_000/Documents/Kaggle/Titanic/train.csv", header = TRUE)
test <- read.csv("C:/Users/japas_000/Documents/Kaggle/Titanic/test.csv", header = TRUE)

#combine the two for pre processing
combined <- bind_rows(train, test)

#investigate training data
summary(train)

#load packages
library(dplyr) 
library(tidyr)
library(rpart)
library(randomForest)
library(ggplot2)


#cleaning data
combined <- combined %>%
    replace_na(list(Age = 29.70))
    
#data visualization: survival by gender, class

combined$FamilySize <- combined$SibSp + combined$Parch + 1



#split combined data into training and test data
train_df <- combined[1:891, ]
test_df <- combined[892:1309, ]
test_df$Survived <- NULL

#visualization: survival based on family size
ggplot(train_df, aes(x = FamilySize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) 


#train model using decision tree classifier : predict survival based on age, gender, class and family size of a passenger
fit1<- rpart(Survived ~ Age + Pclass + Sex + FamilySize, method = "class", data = train_df)

#train model using random forest classifier : predict survival based on age, gender, class and family size of a passenger
fit2<- randomForest(Survived ~ Age + Pclass + Sex + FamilySize, method = "class", data = train_df)


#plot and view results 
plot(fit1, main="Classification Tree for Titanic survivors ")
text(fit1)

#apply on test data
prediction1 <- predict(fit1, test_df, type='class')
prediction2 <- predict(fit2, test_df, type='class')

#make a submission to Kaggle
solution1 <- data.frame(PassengerId = test_df$PassengerId, Survived = prediction1)
solution2 <- data.frame(PassengerId = test_df$PassengerId, Survived = prediction2)

write.csv(solution1[,c("PassengerId", "Survived")], file="C:/Users/japas_000/Documents/Kaggle/Titanic/output11.csv")
