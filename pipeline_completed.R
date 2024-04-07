# install.packages("randomForest")
# install.packages("tidyverse")
# install.packages("caret")

library(tidyverse)
library(randomForest)
library(caret)

red <- read.csv("winequality-red.csv") # for comma separated values (csv)
white <- read.csv("winequality-white.csv",sep = ";") # for semicolon separated values (ssv) : sep is set to ';' to delimit by on file read in

# Explore Data
#summary stats
View(summary(white))
View(summary(red))

# correlation matrix
View(cor(white))
View(cor(red))

# MODEL (this might take a bit to run)

# merge data
wines <- rbind(red,white)

# split into train and test sets (80/20% split respectively) 
set.seed(99) #set random seed
index <- createDataPartition(wines$quality, p = .8, list = FALSE)
train <- wines[index,]
test <- wines[-index,]

# run the model with quality as dependent variable and everything else as independent
set.seed(10)
model_rf <- train(quality ~ .,
                  data = train,
                  method = "rf",
                  tuneGrid= expand.grid(mtry = c(3,6)))

# results
model_rf

plot(model_rf)

summary(model_rf)

model_rf$bestTune # best depth 

# Variable Importance
plot(varImp(model_rf))

# Prediction
prob<- predict(model_rf, test)

# test accuracy with MAPE & MSE
# (mean absolute percentage error & mean squared error)
#MAPE
round(mean(abs((test$quality - bc_prob) / test$quality)) * 100, 2) 
#MSE
mean((bc_prob-test$quality)^2) 

# Reading in New Data
rank <- read.csv("StudentFinalWineSet.csv")

View(rank) # check format

quality <- predict(model_rf, rank) # predict new quality & bind back to data
final <- cbind(rank,quality)

output <- final %>% arrange(desc(quality)) # sort data descending and write the output

# write.csv(output,"RankedWines.csv")

# compare to basic tree

compare_model <- train(quality~.,
                       data = train,
                       method = "rpart")
#summary(compare_model)
cprob<- predict(compare_model, test)
#MAPE
round(mean(abs((test$quality - cprob) / test$quality)) * 100, 2) 
#MSE
mean((cprob-test$quality)^2) 

#qa2 <- cbind(test,cprob)
quality2 <- predict(compare_model, rank)
final2 <- cbind(rank,quality2)