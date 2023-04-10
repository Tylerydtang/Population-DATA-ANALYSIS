#################preprocess the data############################################
library(car)
library(pROC)
#import data 
data <- read.csv("D:/personal/study/555/555TERMPROJECT/world_population.csv")
#check and cleaning
data <- na.omit(data)
is.integer(data$Population..2020.)
is.integer(data$Land.Area..Km².)
is.integer(data$Urban.Pop..)
data$Urban.Pop..<-data$Urban.Pop..[!is.na(data$Urban.Pop..)]
as.integer(data$Urban.Pop..)
is.integer(data$Med..Age)
attach(data)


#Correlation 
# Calculate Correlation Coefficient
cor(Population..2020.,Land.Area..Km².)
cor(Med..Age,Urban.Pop..)

#Sample means 
mean(Urban.Pop..)
mean(Population..2020.)
#rural means
Ruralmean <- mean(Population..2020.)-mean(Urban.Pop..)
Ruralmean

#T-test
#whether the population is significantly different than 100000.
set.seed(688)
sd(Population..2020.)
mean(Population..2020.)
pop <- c(rnorm(30, mean = mean(Population..2020.), sd = sd(Population..2020.)))
t.test(pop, mu = 100000) # Ho: mu = 100000
#We reject the null that,the population is significantly different than 100000.

#RQ1 POPULATION~LANDAREA::Simple Linear Regression
myModel <- lm(Population..2020.~ Land.Area..Km².)
anova(myModel)
summary(myModel)
plot(Population..2020.~ Land.Area..Km².,xlab="Land Area in KM Square",ylab="Population in 2020",main="Population-Area")
# A line on the past plot
abline(myModel,col="red")

#ANOVA
anova_table<- anova(lm(Population..2020.~Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share))
anova_table

#create Analysis of Variance Table
SSE <-anova_table$`Sum Sq`[3]
SSE
SST <-anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]+anova_table$`Sum Sq`[3]
SST
R2 <- (anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]) / SST
R2

#ANCOVA
anova(glm(Population..2020.~ Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share), type=3)
#Variables are independent within each other


#Multiple Linear Regression 
m2 <- lm(Population..2020.~ Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share)
# Summar function can calculate almost everything that you need.
summary(m2)
#change model
m3 <- lm(Population..2020.~World.Share+Net.Change)
summary(m3)
# Calculate R squared Manually and the P-Value
# Total Sum of Squared.
totalss <- sum((Population..2020. - mean(Population..2020.))^2)
totalss
# Regression and Residual Sum of the Squared.
regss <- sum((fitted(m3) - mean(Population..2020.))^2)
regss
resiss <- sum((Population..2020.-fitted(m3))^2)
resiss
# Calculate the F Value.
fstatistic <- (regss/2)/(resiss/191)
fstatistic
# The P-Value for F-Statistic.
pvalue <- 1-pf(fstatistic, df1=9, df2=184)
pvalue

# Calculate R squared.
R2 <- regss/totalss
R2
# Regression Diagnostics
# Residual Plots
resid(m3)
par(mfrow=c(1,3))
plot(Population..2020., resid(m2), axes=TRUE, frame.plot=TRUE, xlab='pop', ylab='residue')
plot(fitted(m3), resid(m3), axes=TRUE, frame.plot=TRUE, xlab='fitted values', ylab='residue')
hist(resid(m3))
confint(m3, level=0.99)

#Access the MLR model
cooks.dist <- cooks.distance(m3)
which(cooks.dist > (4/(nrow(data)-2-1)))

#backward selection
b1 = step(lm(Population..2020.~Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share),direction="backward")

#forward selection
FwModel = step(lm(Population..2020.~1), direction="forward", scope=(~Land.Area..Km².+Yearly.Change+Net.Change+Density..P.Km².+Fert..Rate+Migrants..net.+Med..Age+Urban.Pop..+World.Share))
#backward and forward get the same result. 
#Summary of the best fitted model
summary(lm(Population..2020. ~ World.Share + Net.Change + Yearly.Change + Fert..Rate))


#Prediction of population if every country's urban population equals to 100
#Creating a data frame
variable_pop<-data.frame(Urban.Pop..=rep(100, times=length(Population..2020.)))

#fiting the linear model
liner_model<-lm(Population..2020.~Urban.Pop..+World.Share,data = data)

#predicts the future values
predict(liner_model,newdata = variable_pop)


#Cross Validation
# R program to implement
# validation set approach
# setting seed to generate a
# reproducible random sampling
library(tidyverse)
library(caret)
# installing package to
# import desired dataset
library("datarium")
data(data, package = "datarium")
set.seed(0)
# creating training data as 80% of the dataset
random_sample <- createDataPartition(data$Population..2020.,p=0.8,list = FALSE)
# generating training dataset
# from the random_sample
training_dataset <- data[random_sample, ]
# generating testing dataset
# from rows which are not
# included in random_sample
testing_dataset <- data[-random_sample, ]
# Building the model
# training the model by assigning column
# as target variable and rest other columns
# as independent variables
model <- lm(Population..2020. ~Land.Area..Km²., data = training_dataset)
# predicting the target variable
predictions <- predict(model, testing_dataset)
# computing model performance metrics
data.frame( R2 = R2(predictions, testing_dataset $ Population..2020.),
            RMSE = RMSE(predictions, testing_dataset $ Population..2020.),
            MAE = MAE(predictions, testing_dataset $ Population..2020.))


#LOOCV
train_control <- trainControl(method = "LOOCV")

# training the model by assigning column
# as target variable and rest other column
# as independent variable
model <- train(Population..2020. ~Land.Area..Km²., data = data,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)

#kfold
set.seed(688)

# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning column
# as target variable and rest other column
# as independent variable
model <- train(Population..2020. ~Land.Area..Km²., data = data,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)


#Logistics Regression

head(data)
data$levelofmedage <- ifelse(Med..Age>30,1,0)
cor(data$levelofmedage,Land.Area..Km².)
cor(data$levelofmedage,data$Population..2020.)
cor(data$levelofmedage,data$Yearly.Change)
cor(data$levelofmedage,data$Net.Change)
cor(data$levelofmedage,data$Density..P.Km².)
cor(data$levelofmedage,data$Migrants..net.)
cor(data$levelofmedage,data$Fert..Rate)
cor(data$levelofmedage,data$Urban.Pop..)
cor(data$levelofmedage,data$World.Share)
#choose the best correlation pairs
m <- glm(data$levelofmedage~Fert..Rate, family = "binomial")
anova(m)
summary(m)


# Odd ratio or ORs per 1 unit increase 
# same as calculation by hand (OR)

exp(m$coefficients[2])

exp(cbind(OR = coef(m), confint.default(m)))

# ROC plot and result
par(pty="s")

roc(data$levelofmedage~Fert..Rate, plot=TRUE)


#Multiple Logistics Regression
data$levelofmedage <- ifelse(data$Med..Age>30,1,0)
cor(data$levelofmedage,data$Land.Area..Km².)
cor(data$levelofmedage,data$Population..2020.)
cor(data$levelofmedage,data$Yearly.Change)
cor(data$levelofmedage,data$Net.Change)
cor(data$levelofmedage,data$Density..P.Km².)
cor(data$levelofmedage,data$Migrants..net.)
cor(data$levelofmedage,data$Fert..Rate)
cor(data$levelofmedage,data$Urban.Pop..)

#choose the best correlation pairs
m <- glm(data$levelofmedage~data$Fert..Rate+data$Yearly.Change, family = "binomial")
anova(m)
summary(m)

# Odd ratio or ORs per 1 unit increase 
# same as calculation by hand (OR)

exp(m$coefficients[2])

exp(cbind(OR = coef(m), confint.default(m)))

# ROC plot and result
par(mar=c(1,1,1,1))

roc(data$levelofmedage~data$Fert..Rate+data$Yearly.Change, plot=TRUE)


#One sample test proportion
library(sampling)
set.seed(seed = 4688)
sample(c(1:194),size=10) 
#FIND minimum n in n*p>=10,if p=0.5
n <- 10/0.5
subset <- sample(c(1:194),size=n)
newdata <- data[subset,]
p <- table(newdata$levelofmedage)[2]
prop.test(p,n,p=0.5, conf.level = 0.95, correct=TRUE)
# we have 95% of significant confidence to say that the proportion of getting higher median age in the dataset is not equal to 0.55 

