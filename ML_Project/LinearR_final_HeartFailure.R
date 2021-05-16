#reading the csv file
dataSet = read.csv("C:/abcd/DataSet/pjct_demo/heart_failure_clinical_records_dataset.csv")
summary(dataSet)

X<-dataSet$serum_creatinine
Y<-dataSet$serum_sodium

#Minimum and Maximum
max(X)
min(X)

max(Y)
min(Y)

#Standard Daviation
sd(X)
sd(Y)

return_val1<-hist(X,main = "histogram for the Serum Creatinine")
return_val1

return_val2<-hist(Y,main = "histogram for the Serum Sodium")
return_val2


#Apply Linear Regression on data set of heart_failure_clinical_records_dataset----->

#Checking is Not Available(NA) in data Set
dataSet$serum_creatinine = ifelse(is.na(dataSet$serum_creatinine),
                                 ave(dataSet$ serum_creatinine,
                                     fun=function(x)mean(x,na.rm=TRUE)),
                                 dataSet$serum_creatinine)
                     
dataSet$serum_sodium = ifelse(is.na(dataSet$serum_sodium),
                              ave(dataSet$serum_sodium,
                                  fun1=function(y)mean(y,na.rm=TRUE)),
                              dataSet$serum_sodium)
                                                          
                                                          
#Splitting the dataset into train and test set 
library(caTools)
set.seed(123)
split = sample.split(dataSet$serum_sodium,SplitRatio = 0.75)
train_set = subset(dataSet,split==TRUE)
test_set = subset(dataSet,split==FALSE)
                                                          
                                                          
                                                          
#Fitting the linear regression
regressor= lm(formula = serum_sodium~serum_creatinine,data = train_set)
                                                          
#Predicting the test set result
y_pred = predict(regressor,newdata = test_set)
y_pred




#visualization of test set result
library(ggplot2)
ggplot()+
  geom_point(aes(x=test_set$serum_creatinine,
                 y=test_set$serum_sodium),color = "red")+
  geom_line(aes(x = test_set$serum_creatinine,
                y=predict(regressor,newdata = test_set)),
            color = "blue") +
  ggtitle("Serum creatinine vs serum sodium(Test Set)")+
  xlab("serum creatinine")+
  ylab("serum Sodium")



#visualization of train set result

ggplot()+
  geom_point(aes(x=train_set$serum_creatinine,
                 y=train_set$serum_sodium),color = "red")+
  geom_line(aes(x = train_set$serum_creatinine,
                y=predict(regressor,newdata = train_set)),
            color = "blue")+
  ggtitle("Serum creatinine vs serum sodium(Train Set)")+
  xlab("serum creatinine")+
  ylab("serum Sodium")



