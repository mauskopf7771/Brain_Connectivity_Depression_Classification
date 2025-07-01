### SVM ON DEPRESSION AND CONNECTIVITY DATASET ###

### LIBRARIES ###
#####
library(e1071)
library(ROCR)
library(readr)
library(caret)
library(caTools)
library(dplyr)
#####
#Load in the data and extract relevant variables
setwd("/Users/[redacted]/Research_Projects_and_Code/Connectivity Project")
df <- read_csv("SVM_Data_for_R.csv")
#Extract variables of interest
df['Alpha'] = df['LCC_Alpha']
df['Beta'] = df['LCC_Beta']
df2 <- df[, c('Neuroticism', 'Depression', 'Alpha', 'Beta')]

#Turn the variable depression into a variable called caseness
caseness <- df2['Depression']
caseness$Depression[caseness$Depression == 0] <- -1
caseness <- caseness$Depression
#Choose two variables for our SVM
vars <- df2[, c('Neuroticism', 'Alpha')]
vars <- scale(vars) #We can follow this backwards to fix it 
plot(vars, col = (3 - caseness)) #gives us the plot with people in caseness and not
#put together the dataframe called dat
dat <- data.frame(vars = vars, caseness = as.factor(caseness))

#Do a train test as done on https://rpubs.com/markloessi/506713 #Arguably from what the code below does, this is redundant as well...
split = sample.split(dat, SplitRatio = 0.9)
training_set = subset(dat, split == TRUE)
test_set = subset(dat, split == FALSE)

#identify best parameters
tune.out = tune(svm, caseness~., data = dat, #data = dat[train, ]
                kernel = "radial", 
                ranges = list(cost = c(0.1, 1, 10, 100, 1000), 
                              gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

svmfit <- svm(caseness~., data = dat, kernel = "radial", gamma = 0.5, cost = tune.out$best.parameters[1, 'cost'], main = 'SVM') 
#plot.title = title(main = "test :)")
plot(svmfit, dat, main = "fuck you")#, #xlab = "Alpha Global Clustering Coefficient", ylab = "Neuroticism")
#plot.title = title(main = "test :)")
#plot.xlab = "whaddup"
#plot.ylab = "whaddup"
summary(svmfit)
#This works so we can replace parts of the code with this

#####
# Make classification table (confusion matrix) and get a measure of accuracy. Arguably we don't need this anymore.
#classification_table <- table( 
#  true = training_set$caseness, #I think where the "y" is, this is meant to be our category variable.
#  pred = predict(tune.out$best.model, newdata=training_set 
#  ) 
#)
#classification_table
#accuracy <- classification_table[1] + classification_table[4] / sum(classification_table) *100
#accuracy
 #Old classification table thing


##### We'll put the K fold stuff here #####
folds = createFolds(dat$caseness, k = 10)
# cv aply created function of folds
cv = lapply(folds, function(x){
  #browser()
  training_fold = dat[-x, ]
  test_fold = dat[x, ]
  classifier = svm(formula = caseness~., 
                   data = training_fold, 
                   type = 'C-classification', 
                   kernel = 'radial', 
                   gamma = tune.out$best.parameters[1, 'gamma'], 
                   cost = tune.out$best.parameters[1, 'cost'])
  #calculate predictions and confusion matrix to predict accuracy
  y_pred = predict(classifier, newdata = test_fold)
  cm = table(test_fold[, 3], y_pred) #This was working at one point, not so much these days
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))
accuracy
#So here, I get the impression that it's doing it in a slightly different way?
# Visualise the training set - actually, it's fine as we have the gamma and cost
#So we can mix this in, we just need to visualise the SVM for training set, and the ROC.
# Visualise the test set

## GENERATING THE ROC CURVE - this will show the fit of the data and how good the model is
library(ROCR)
rocplot<-function(pred, truth, ...) {
  predob <-prediction(pred, truth) 
  perf<-performance(predob, "tpr", "fpr") 
  plot(perf, ...) 
}

#Run the SVM but with ROC plots as well?
svmfit.opt<-svm(caseness~., data = training_set, kernel = "radial", gamma = tune.out$best.parameters[1, 'gamma'], cost = tune.out$best.parameters[1, 'cost'], decision.values = T)
fitted <- attributes( predict(svmfit.opt, training_set, decision.values = TRUE))$decision.values
par(mfrow = c(1, 2)) #This enables 2 plots on the same image.
rocplot(-fitted, training_set$caseness, main = "Training Data") #Looks like main just gives it a title.
#When looking at it, it shows the model to be pretty good! 
#This is all on the test data, which is cool and all, but we're interested on the actual data
fitted <- attributes( predict(svmfit.opt, test_set, decision.values = T) )$decision.values
rocplot(-fitted, test_set$caseness, main = "Test Data")
