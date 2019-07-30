'''
##Project Objective:
#The aim of the marketing campaign was to get customers to subscribe to a bank term deposit product. Whether they did this or not is variable 'deposit' in the data set.
#The bank in question is considering how to optimize this campaign in future.

##Input variables:
## bank client data:
#1 - age (numeric)
#2 - job : type of job (categorical: "admin.","unemployed","management","housemaid","entrepreneur","student","blue-collar","self-employed","retired","technician","services","unknown") 
#3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
#4 - education (categorical: "unknown","secondary","primary","tertiary")
#5 - default: has credit in default? (binary: "yes","no")
#6 - balance: average yearly balance(numeric) 
#7 - housing: has housing loan? (binary: "yes","no")
#8 - loan: has personal loan? (binary: "yes","no")
## related with the last contact of the current campaign:
#9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
#10 - day: last contact day of the month (numeric)
#11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
#12 - duration: last contact duration, in seconds (numeric)
## other attributes:
#13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
#15 - previous: number of contacts performed before this campaign and for this client (numeric)
#16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")

##Output variable (desired target):
#17 - y - has the client subscribed a term deposit? (binary: "yes","no")
'''


########################### 1. Let us load the data ####################################
setwd("C:\\Users\\hp\\Desktop")
bank <- read.csv("bank.csv", header= TRUE)
View(bank)

##What kind of data set has been created
str(bank)
## It is a data frame with 17 variables
dim(bank)
## Summary provides a quick snapshot into the data set
summary(bank)

## Missing Values
# If newdata has same number of observation that implies no NA value present 
newdata <- na.omit(bank) 
nrow(newdata)==nrow(bank)


################################## EDA- Continuous Variables ###################################

library(ggplot2)
library(Rmisc)
library(gridExtra)

#################Plotting:Finding Overlap:Major predictor (if any)#############################
#For numerical  values in the new structure we can do box plot and check the overlap

h_age<- ggplot(bank, aes(x = age))+ geom_histogram(binwidth = 1,fill="green", colour="black")
b_age <- ggplot(bank, aes(factor(deposit), age)) + geom_boxplot(aes(fill = factor(deposit)))
grid.arrange(h_age, b_age, ncol=2)

h_balance<- ggplot(bank, aes(x = balance))+ geom_histogram(binwidth = 100,colour="blue")+coord_cartesian(ylim = c(0,800))+ scale_x_continuous(breaks = seq (-6847, 81204,20000))
b_balance <- ggplot(bank, aes(factor(deposit), balance)) + geom_boxplot(aes(fill = factor(deposit)))
grid.arrange(h_balance, b_balance, ncol=2)

h_day<- ggplot(bank, aes(x = day))+ geom_histogram(binwidth = 1,fill="green", colour="black")
b_day <- ggplot(bank, aes(factor(deposit), day)) + geom_boxplot(aes(fill = factor(deposit)))
grid.arrange(h_day, b_day, ncol=2)

h_duration<- ggplot(bank, aes(x = duration))+ geom_histogram(binwidth = 1,fill="green")
b_duration <- ggplot(bank, aes(factor(deposit), duration)) + geom_boxplot(aes(fill = factor(deposit)))
grid.arrange(h_duration, b_duration, ncol=2)

h_campaign<- ggplot(bank, aes(x = campaign))+ geom_histogram(binwidth = 1,fill="green", colour="black")
b_campaign <- ggplot(bank, aes(factor(deposit), campaign)) + geom_boxplot(aes(fill = factor(deposit)))
grid.arrange(h_campaign, b_campaign, ncol=2)

h_pdays<- ggplot(bank, aes(x = pdays))+ geom_histogram(binwidth = 1,fill="green")
b_pdays <- ggplot(bank, aes(factor(deposit), pdays)) + geom_boxplot(aes(fill = factor(deposit)))
grid.arrange(h_pdays, b_pdays, ncol=2)

h_previous<- ggplot(bank, aes(x = previous))+ geom_histogram(binwidth = 1,fill="green", colour="black")
b_previous <- ggplot(bank, aes(factor(deposit), previous)) + geom_boxplot(aes(fill = factor(deposit)))
grid.arrange(h_previous, b_previous, ncol=2)

#Various box_plots in a single interface
multiplot(b_age,b_balance,b_day,b_duration,b_campaign,b_pdays,b_previous, cols=4)

#Correaltion Matrix using heat map
num <- bank[, c(1,6,10,12,13,14,15)]
head(num)
cormat <- round(cor(num),2)
head(cormat)
library(reshape)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + geom_tile(color = "white")+
  geom_text(aes(X2, X1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1))

######################## EDA- Categorical Variables ##################################

g_job <- ggplot(bank, aes(x = job, fill = deposit)) +geom_bar() 
g_marital <- ggplot(bank, aes(x = marital, fill = deposit)) +geom_bar()
g_education <- ggplot(bank, aes(x = education, fill = deposit)) +geom_bar()
g_default <- ggplot(bank, aes(x = default, fill = deposit)) +geom_bar() 
g_housing <- ggplot(bank, aes(x = housing, fill = deposit)) +geom_bar()
g_loan <- ggplot(bank, aes(x = loan, fill = deposit)) +geom_bar()
g_contact <- ggplot(bank, aes(x = contact, fill = deposit)) +geom_bar()
g_month <- ggplot(bank, aes(x = month, fill = deposit)) +geom_bar()
g_poutcome <- ggplot(bank, aes(x = poutcome, fill = deposit)) +geom_bar()

#Various stacked_bar_plots in a single interface
multiplot(g_job,g_marital,g_education,g_default,g_loan,g_poutcome,g_contact,g_month, cols=4)


#################Building the model-Decision Tree##############################
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)

set.seed(123)
trainIndex <- createDataPartition(bank$deposit, p = .70,list = FALSE)

Train_set <- bank[ trainIndex,]
Test_set  <- bank[-trainIndex,]
dt_model<- rpart(deposit ~ age+job+balance+contact+month+duration+poutcome, data = Train_set)
dt_model
fancyRpartPlot(dt_model, cex = 0.7)


#################Testing Decision Tree###########################################

predictions <- predict(dt_model, Test_set, type = "class")
table(predictions)

# Lets look at the confusion matrix
confusion.matrix <- prop.table(table(predictions, Test_set$deposit))
confusion.matrix
confusionMatrix(predictions,Test_set$deposit)

library(ROCR)
library(pROC)
tree.predict <- predict(dt_model, Test_set)
roc_curve <- roc(Test_set$deposit, tree.predict[, 1], plot=TRUE, auc=TRUE, grid=TRUE, col="blue")
auc(roc_curve)


#################################################################################


