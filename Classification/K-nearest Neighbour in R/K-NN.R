dataa =  read.csv('Social_Network_Ads.csv')
dataset = read.csv('Social_Network_Ads.csv')
View(dataa)
#we need only age,estimated salary and whether they purchase or not column only
dataset = dataset[,3:5]
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split == FALSE)

#feature scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])




#fit K-NN to training set and predict test set result
library(class)
y_pred = knn(train = training_set[,-3],test = test_set[,-3],cl = training_set[,3],k=5)
y_pred
#create a confusion Matrix between actual value and predcited value
cm = table(test_set[,3],y_pred)
cm

#visualise train set
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[,1]) - 1,max(set[,1] + 1), by = 0.01)
X2 = seq(min(set[,2]) - 1,max(set[,2] + 1), by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = knn(train = training_set[,-3],test = grid_set,cl = training_set[,3],k=5)
plot(set[,-3],main = 'K-NN(Training Set)',xlab = 'Age',ylab = 'Estimated Salary',xlim = range(X1),ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add = TRUE)
points(grid_set,pch = '.',col = ifelse(y_grid == 1,'springgreen3','tomato'))
points(set,pch = 21,bg = ifelse(set[,3]==1,'green4','red3'))

#red area is people who do not buy SUV and green area is people who buy SUV

#visualise test set
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[,1]) - 1,max(set[,1] + 1), by = 0.01)
X2 = seq(min(set[,2]) - 1,max(set[,2] + 1), by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = knn(train = training_set[,-3],test = grid_set,cl = training_set[,3],k=5)
plot(set[,-3],main = 'K-NN(Test Set)',xlab = 'Age',ylab = 'Estimated Salary',xlim = range(X1),ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add = TRUE)
points(grid_set,pch = '.',col = ifelse(y_grid == 1,'springgreen3','tomato'))
points(set,pch = 21,bg = ifelse(set[,3]==1,'green4','red3'))
