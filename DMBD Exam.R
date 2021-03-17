#########################################################################################
####                  Exam 20 - Data Mining for Business Decisions                 ####
#########################################################################################

# Loading packages: 


library(ISLR)
library(MASS)
library(class)
library(ggplot2)
library(car)
library(boot)
library(leaps)
library(glmnet)
library(pls)
library(dplyr)
library(splines)
library(gam)
library('mgcv')
library(tree)
library(randomForest)
library(gbm)
library(Hmisc)
library(e1071)
library(LiblineaR)
library(DataExplorer)
library(tidyverse)
library(broom)
library(akima)
library(tree)
library(DMwR2)
library(caret)
library(foreign)
library(caTools) 
library(visdat)
library(magrittr)
library(tidyr)
library(tidyverse)
library(rsample)
library(moments) # for calculating skewness
library(visdat)
library(recipes)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(lubridate)




# Load data: 
Data <- read.csv("HRDataset_v13.csv",header = TRUE)
View(Data)
names(Data)

str(Data)

########################################################################################
#                                         Method                                       #
########################################################################################

### Missing data: 
table(is.na(Data))
mean(is.na(Data))

summary(Data)

vis_miss(Data, cluster = TRUE, sort_miss=TRUE)
Data$ManagerID[is.na(Data$ManagerID)] <- 39
Data$DaysLateLast30[is.na(Data$DaysLateLast30)] <- 0

vis_miss(Data, cluster = TRUE, sort_miss=TRUE)

View(Data)

---------------
#on_roll_emp <-hr %>%
  filter(Reason.For.Term=="N/A - still employed") %>%
  group_by(Sex,Position) %>%
  summarise(Total=n())
-----------------


caret::nearZeroVar(Data, saveMetrics = TRUE) %>%
  tibble::rownames_to_column() %>%
  filter(nzv)

qplot(Data$ManagerID, Data$ManagerName)

Data$ManagerID <- as.character(Data$ManagerID)
Data$ManagerID[Data$ManagerID == "3"] <- "1"
Data$ManagerID[Data$ManagerID == "30"] <- "22"

qplot(Data$ManagerID, Data$ManagerName)

Data$ManagerID <- as.factor(Data$ManagerID)

qplot(Data$Termd, Data$State, geom="boxplot")

qplot(Data$Termd, Data$CitizenDesc, geom="boxplot")

Data <- subset(Data, select = -c(DaysLateLast30, Employee_Name, State, EmpID))


View(Data)


mutate(Data$DOB <- as.Date(Data$DOB, format="%m/%d/%Y"))



str(Data)




# Load data
Data <- read.csv("HRDataset_v13.csv",header = TRUE)
# Sanitize Data
# Convert date strings to date object
StringDates <- as.character(Data$DOB, format ='%y/%m/%d')
Dates <- as.Date(StringDates, format="%m/%d/%Y")
Data$DOB <- Dates
View(Data)
str(Data)














### Outliers:
boxplot(Data$EngagementSurvey)
boxplot(Data$PayRate)

Get back to this part




# Transform of variables

parse_date_time(x = Data$DOB,
                orders = c("y m d", "y b d", "y/m/d"),
                locale = "eng")
Data$DOB <- as.character(Data$DOB, format ='%y/%m/%d')

strptime(c("5/13/2015"),"%m/%d/%Y")


View(Data)





Data$DateofHire <- as.POSIXlt(Data$DateofHire,format="%m/%d/%y",)
Data$DateofHire <-as.Date(Data$DateofHire)

Data$DOB <- as.POSIXlt(Data$DOB,format="%m/%d/19%y")
Data$DOB <-as.Date(Data$DOB)

str(Data)















+Data$MarriedID=as.factor(Data$MarriedID)
Data$MaritalStatusID=as.factor(Data$MaritalStatusID)
Data$EmpStatusID=as.factor(Data$EmpStatusID)
Data$DeptID=as.factor(Data$DeptID)
Data$PerfScoreID=as.factor(Data$PerfScoreID)
Data$FromDiversityJobFairID=as.factor(Data$FromDiversityJobFairID)

Data$Termd=as.factor(Data$Termd)
Data$PositionID=as.factor(Data$PositionID)
Data$Position=as.factor(Data$Position)

Data$Sex=as.factor(Data$Sex)
Data$MaritalDesc=as.factor(Data$MaritalDesc)
Data$CitizenDesc=as.factor(Data$CitizenDesc)
Data$HispanicLatino=as.factor(Data$HispanicLatino)
Data$RaceDesc=as.factor(Data$RaceDesc)

Data$TermReason=as.factor(Data$TermReason)
Data$EmploymentStatus=as.factor(Data$EmploymentStatus)
Data$Department=as.factor(Data$Department)
Data$ManagerName=as.factor(Data$ManagerName)
Data$ManagerID=as.factor(Data$ManagerID)
Data$RecruitmentSource=as.factor(Data$RecruitmentSource)
Data$PerformanceScore=as.factor(Data$PerformanceScore)

Data$EmpSatisfaction=as.factor(Data$EmpSatisfaction)
Data$SpecialProjectsCount=as.factor(Data$SpecialProjectsCount)


Data$GenderID=as.factor(Data$GenderID)

Data$DOB=as.numeric(Data$DOB)
Data$Zip=as.numeric(Data$Zip)
Data$PayRate=as.numeric(Data$PayRate)
Data$EngagementSurvey=as.numeric(Data$EngagementSurvey)
Data$LastPerformanceReview_Date=as.numeric(Data$LastPerformanceReview_Date)
Data$DateofHire=as.numeric(Data$DateofHire)
Data$DateofTermination=as.numeric(Data$DateofTermination)

view(Data)

str(Data)


# train/test split
set.seed(100)
inTrain <- createDataPartition(y = Data$Termd, p=0.75, list=FALSE)

train <- Data[inTrain,]
test <- Data[-inTrain,]


view(train)

plot_bar(Data$Termd)
summary(Data$Termd)


# Fit Logistic Regression: 
LG <- glm(Termd~., data=train, family = binomial)  
summary(LG)

maxit = 100
view(train)






-------------Amanda----------------



price_outliers <- which(Data$price > 12000)
Data[price_outliers, "price"] 
Data[price_outliers,] 


### Target variable:
# Check for imbalanced data: 
plot_bar(Datar$best_seller)
summary(Data$best_seller) # 288 = not best-seller item, 49 = bestseller item. 
49/337 # = 14.54 %. 
288/337 # 85.46 %.


### Transforming the variables: 
# Excluding ID and image: 
Data <- Data %>% select(-ID, -image, -chalcedony, -rhodonite, -beryllium, -morganite, -citrine, -praseolite, -carnelian, -cry_sum, -mat_sum, -gem_sum)

# Removing 'SE' and comma form the price variable: 
Data$price <- gsub("[a-zA-Z ]", "", Data$price)
Data$price <- as.numeric(gsub(",","",Data$price))

# Removing correlation between ring and earring: 
Data$ring <- (Data$ring-Data$earring)

# Transform into factors: 
Data$best_seller=as.factor(Data$best_seller)
Data$bracelet=as.factor(Data$bracelet)
Data$ring=as.factor(Data$ring)
Data$necklace=as.factor(Data$necklace)
Data$earring=as.factor(Data$earring)
Data$choker=as.factor(Data$choker)
Data$pendant=as.factor(Data$pendant)
Data$mesh=as.factor(Data$mesh)
Data$gold=as.factor(Data$gold)
Data$silver=as.factor(Data$silver)
Data$white=as.factor(Data$white)
Data$titanium=as.factor(Data$titanium)
Data$ip=as.factor(Data$ip)
Data$rose=as.factor(Data$rose)
Data$steel=as.factor(Data$steel)
Data$oxidized=as.factor(Data$oxidized)
Data$pearl=as.factor(Data$pearl)
Data$opal=as.factor(Data$opal)
Data$topaz=as.factor(Data$topaz)
Data$amethyst=as.factor(Data$amethyst)
Data$malachite=as.factor(Data$malachite)
Data$amazonite=as.factor(Data$amazonite)
Data$labradorite=as.factor(Data$labradorite)
Data$sapphires=as.factor(Data$sapphires)
Data$agate=as.factor(Data$agate)
Data$quartzite=as.factor(Data$quartzite)
Data$gemstones=as.factor(Data$gemstones)
Data$spinel=as.factor(Data$spinel)
Data$onyx=as.factor(Data$onyx)
Data$diamonds=as.factor(Data$diamonds)
Data$ruby=as.factor(Data$ruby)

# Transform into numeric: 
Data$price=as.numeric(Data$price)
str(Data)

### Sampling and data partitioning:
set.seed(90)  # Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.

# train/test split
inTrain <- createDataPartition(y = Data$best_seller, p=0.75, list=FALSE)

train <- Data[inTrain,]
test <- Data[-inTrain,]




####################################################################
#                              Results:                            #
####################################################################


# Logistic regression:  ---------------------------------------------------

# Fit Logistic Regression: 
LG_model <- glm(best_seller~., data=train, family=binomial)  
summary(LG_model)

# Make prediction: 
LG.probs=predict(LG_model, test, type="response")

# Create a vector of class predictions: 
LG.pred=rep("0",84)
LG.pred[LG.probs >.5]="1"

#Confusion matrix:
caret::confusionMatrix(table(LG.pred, test$best_seller), positive = "1")    

# ROC and AUC: 
colAUC(as.numeric(LG.pred), test$best_seller, plotROC = TRUE)




# LDA: --------------------------------------------------------------------

# Fitting the LDA model: 
lda.model=lda(best_seller~., data=train)
lda.model

# Plot the function -> the linear discriminant: 
plot(lda.model)

# Make predictions on the test set: 
lda.pred=predict(lda.model, test)
names(lda.pred)

# Labling the prediction of the class so it can be compared to the actual (test set): 
lda.class=lda.pred$class

# Confusion matrix: 
table(lda.class ,test$best_seller)
mean(lda.class==test$best_seller) # 0.8636 Correct predicitons. 
(69+4)/84
(8+3)/84

# Applying a 50 % threshold to the posterior probabilities allows us to recreate the predictions contained in lda.pred$class: 
sum(lda.pred$posterior[,1]>=.5) 
sum(lda.pred$posterior[,1]<.5)  

# Alternative Confusion matrix:
caret::confusionMatrix(table(lda.class, test$best_seller), positive = "1") 

# ROC and AUC: 
colAUC(as.numeric(lda.class), test$best_seller, plotROC = TRUE)




# Decision-tree Method:  ---------------------------------------------------------

# Fit a tree based on the training data: 
tree.fit=tree(best_seller~., train, split="gini")
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty =0)

# Prediction on the test data: 
tree.pred=predict(tree.fit,test,type="class")

# Misclassification error: 
table(tree.pred ,test$best_seller)
(5+6)/84 
(67+6)/84 

# Do pruning of the tree lead to improvements?: 
set.seed (3)
cv.fit =cv.tree(tree.fit ,FUN=prune.misclass)
cv.fit

# Plot: 
par(mfrow=c(1,2))
plot(cv.fit$size ,cv.fit$dev ,type="b")
plot(cv.fit$k ,cv.fit$dev ,type="b")

# Final evaluation of Decision-tree method: 
#Alternative Confusion matrix:
caret::confusionMatrix(table(tree.pred, test$best_seller), positive = "1") 

# ROC and AUC: 
colAUC(as.numeric(tree.pred), test$best_seller, plotROC = TRUE)



# Random Forest  -----------------------------------------------------------

# mtry: 
variables <- sqrt(31)

set.seed (1)
rf.fit=randomForest(best_seller~.,data=train, mtry=variables,importance =TRUE)
rf.fit 
plot(rf.fit)
yhat.rf = predict(rf.fit , test, type= "class")
table(yhat.rf ,test$best_seller)
(8+1)/84 
(71+4)/84 

# view the importance of each variable: 
importance(rf.fit)

# Plot importance measures: 
varImpPlot(rf.fit)

# Final evaluation of Decision-tree method: 
#Alternative Confusion matrix:
caret::confusionMatrix(table(yhat.rf, test$best_seller), positive = "1") 

# ROC and AUC: 
dev.off()
colAUC(as.numeric(yhat.rf), test$best_seller, plotROC = TRUE)




# Bagging: ----------------------------------------------------------------

set.seed(1)
# Fitting the model: 
bag.fit=randomForest(best_seller~.,data= train, mtry=31,importance =TRUE) 
plot(bag.fit)

# Prediction: 
yhat.bag = predict(bag.fit ,test)

# Confusion matrix: 
caret::confusionMatrix(table(yhat.bag, test$best_seller), positive = "1") 

# ROC and AUC: 
dev.off()
colAUC(as.numeric(yhat.bag), test$best_seller, plotROC = TRUE)

importance(bag.fit)
varImpPlot(bag.fit)



# Selection the final model:  ---------------------------------------------
caret::confusionMatrix(table(LG.pred, test$best_seller), positive = "1")  # Sencitivity: 0.41667, accuracy: 0.869, specificity: 0.9444, Balanced Accuracy: 0.6805  
caret::confusionMatrix(table(lda.class, test$best_seller), positive = "1") # Sencitivity: 0.3333, accuracy: 0.869, specificity: 0.9583, Balanced Accuracy: 0.6458
caret::confusionMatrix(table(tree.pred, test$best_seller), positive = "1") # Sencitivity: 0.5000, accuracy: 0.869, specificity: 0.9305, Balanced Accuracy: 0.7152
caret::confusionMatrix(table(yhat.rf, test$best_seller), positive = "1")  # Sencitivity: 0.3333, accuracy: 0.8929, specificity: 0.9861, Balanced Accuracy: 0.6597
caret::confusionMatrix(table(yhat.bag, test$best_seller), positive = "1") 

dev.off()
par(mfrow=c(2,2))
colAUC(as.numeric(LG.pred), test$best_seller, plotROC = TRUE)
title("                              - LG")
colAUC(as.numeric(lda.class), test$best_seller, plotROC = TRUE)
title("                                - LDA")
colAUC(as.numeric(tree.pred), test$best_seller, plotROC = TRUE)
title("                               - Tree")
colAUC(as.numeric(yhat.bag), test$best_seller, plotROC = TRUE)
title("                                - BAG")
dev.off()
colAUC(as.numeric(yhat.rf), test$best_seller, plotROC = TRUE)
title("                              - RF")








