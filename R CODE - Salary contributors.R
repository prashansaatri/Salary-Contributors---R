rm(list = ls())


# Set Path and Data-set Before Running
wd <- 'path where files is saved'
file <- 'employData2.csv'

#-------------------------------------------------------------------------------------------------
#CODE FOR REGRESSION

setwd(wd)

batch2 <- read.csv(file, stringsAsFactors = F )
str(batch2)
head(batch2)
# logistic regression for starting salary based on region 
lm.fit <- lm(salary ~ yearsExperience + jobType + industry, data = batch2)

s1 = batch2[1:2000, ]

s1.matrix = as.data.frame(data.matrix(s1))

plot(s1.matrix$yearsExperience, s1.matrix$salary, col=s1.matrix$jobType)

boxplot(salary ~ yearsExperience, data = s1, main = 'Salaries based on Years of Experience',
        xlab = 'Years of Experience', ylab = 'Salary')
boxplot(salary ~ jobType, data = s1, main = 'Salaries based on Job Type',
        xlab = 'Job Type', ylab = 'Salary', col = (c('red', 'blue')))
boxplot(salary ~ industry, data = s1, main = 'Salaries based on Industry',
        xlab = 'Industry', ylab = 'Salary', col = (c('red', 'blue')))


plot(salary ~ yearsExperience, data = s1)
plot(s1$salary, s1$yearsExperience)
summary(lm.fit)


# coefficients 
coef(lm.fit)
summary(lm.fit)$coef

# histogram of data 
hist(batch2$salary)

# box plot 

boxplot(batch2$salary)

# p-values 
summary(lm.fit)$coef[,4]

glm.probs <- predict(lm.fit, type = 'response')
glm.probs[1:10]

# create test and train 
train = sample(nrow(batch2), nrow(batch2) * .6)

batch.train = batch2[train, ]
batch.test = batch2[-train, ]

# cross validation -- mean squared error 
set.seed(300)
lm.fit <- lm(salary ~ yearsExperience + jobType + industry, data = batch2, 
             subset = train)
lm.fit <- lm(salary ~ yearsExperience + jobType + industry, data = batch2[train, ])
mean((batch2$salary - predict(lm.fit, batch2))[-train]^2)

#MSE 
# 625.3022



# 10-fold cross validation
library(boot)
set.seed(300)
lm.fit <- lm(salary ~ yearsExperience, data = batch2)
cv.error <- cv.glm(batch2, lm.fit, K = 10)$delta[1]


# dummy variables 
length(unique(batch2$jobType))
batch2$dummyjob <- ifelse(batch2$jobType == "CFO", 1, 0)

#scatter plot 
plot(x = batch2,y = batch2$salary,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(2.5,5),
     ylim = c(15,30),		 
     main = "Weight vs Milage"
)

# graphing ideas 
library(car)
avPlots(lm.fit)


unique(batch2$industry)


#----------------------------------------------------------------------------------------------------


#CODE FOR KNN


setwd(wd)
emp <- read.csv(file,stringsAsFactors = TRUE)
emp <- subset(emp,select=c(jobType, degree, major, industry, yearsExperience, salary))
str(emp)


emp <- as.data.frame(model.matrix(~ . , data = emp)[,-1])

train <- sample(nrow(emp), 200)
emp.train <- emp[train, ]
emp.test <- emp[-train, ]

head(emp)

# KNN Regression
# Uses knnreg()


# install.packages('caret')
library(caret)

# vk <- seq(1,100,2)

vk <- seq(1, 20)
MSE <- vk
for (i in 1:length(vk)) {
  knn.fit <- knnreg(salary ~ ., data=emp, subset = train, k = vk[i])
  knn.pred <- predict(knn.fit, emp)
  MSE[i] <- mean((emp$salary - predict(knn.fit, emp))[-train]^2)
  
  print(vk[i])
  print(MSE[i])
  print(postResample(pred=knn.pred, obs = emp$salary))
  print('------------------')
}
plot(vk, MSE, xlab = "k", ylab = "MSE", col = "blue")

# Lowest at k = 5
# MSE = 1040.22, RMSE = 32.2556812
# Rsqr = 0.3099161, MAE = 25.7134554

# --------------------------------------------

# KNN Regression
# Uses knn3


MSE3 <- vk
for (i in 1:length(vk)) {
  knn.fit <- knn3(salary ~ ., data=emp, subset = train, k = vk[i])
  knn.pred <- predict(knn.fit, emp)
  MSE3[i] <- mean((emp$salary - knn.pred)[-train]^2)
  
  print(vk[i])
  print(MSE3[i])
  print('------------------')
}
plot(vk, MSE, xlab = "k", ylab = "MSE", col = "blue")

# Lowest at k = 5
# MSE = 14967.25

#----------------------------------------------------------------------------------------------------
#CODE FOR DECISION TREES AND RANDOM FOREST


# reading the data
data<-read.csv(file,stringsAsFactors = TRUE)
data <- subset(data,data$major != "NONE")
data=data[c(1:60000),-1]
data=data[,-6]
head(data)
nrow(data)

#nulls
colSums(is.na(data))
data1 <- data
#dummy variables
data1.m <- as.data.frame(model.matrix(~ . , data = data1)[,-1])
nrow(data1.m)
View(data1.m)
head(data1.m)
data_train=data1.m[1:40000,]
data_test=tail(data1.m,-40000)




#----------------------------------------------------------------------------------
# TREES

# building the tree
library(tree)
head(data)
tree.1 <- tree(salary ~ . , data = data1.m)
summary(tree.1)

# plot and label the tree
plot(tree.1)
text(tree.1, pretty = 0)

# validation set approach
set.seed(200)
tree.1 <- tree(salary ~ . , data = data_train)
pred1 <- predict(tree.1, newdata = data_test)
plot(pred1, data_test$salary)

#MSE
mean((pred1 - data_test$salary)^2)
#839.0077



# validation set approach with larger tree
set.seed(200)
tree3.train <- tree(salary ~ . , data = data_train,
                    control = tree.control(nobs = nrow(data1.m), mindev = 0.001))

pred1 <- predict(tree3.train, newdata = data_test)
plot(pred1, data_test$salary)

#MSE
mean((pred1 - data_test$salary)^2)
#715.4429




# random forest
library(randomForest)
# random forest
set.seed(300)
forest.appt <- randomForest(salary ~ . , data = data_train, mtry = 5,rfcv=10)
pred.forest <- predict(forest.appt,data_test)
print(mean((pred.forest-data_test$salary)^2))
#629.0503



