#####################LOGISTIC REGRESSION#####################
#importing dataset
#about the data
#Pregnancies: Number of times pregnant
#Glucose: Plasma glucose concentration a 2 hours in an oral glucose tolerance test
#BloodPressure: Diastolic blood pressure (mm Hg)
#SkinThickness: Triceps skin fold thickness (mm)
#Insulin: 2-Hour serum insulin (mu U/ml)
#BMI: Body mass index (weight in kg/(height in m)^2)
#DiabetesPedigreeFunction: Diabetes pedigree function
#Age: Age (years)
#Outcome: Class variable (0 or 1)
data<-read.csv(choose.files())
data
names(data)
## to check missing values
is.na(data)
colSums(is.na(data))
str(data)
dim(data)
summary(data)

###splitting the data into training and testing
##Building the model
library(caTools)
set.seed(1000)
split=sample.split(data$Outcome,SplitRatio = 0.75)
split
table(split)

training=subset(data,split==TRUE)
training
test=subset(data,split==FALSE)
nrow(training)
nrow(test)

##building logistic regression model with training dataset
###logistic regression model is knowm as generalised linear model
names(data)
log_reg=glm(Outcome~.,data=training)#family = 'binomial')
log_reg

###null devience==bydefault system will through error without checking independent variabla
##residual deviance==with independent variable
#nulldevience>=residual deviance
###AIC==r^2==akike information criteria
summary(log_reg)

##predict the model with test data set
pred=predict(log_reg,newdata=test)##,type=responce in case if you use output to be binomial
pred
##which is a probablity value so build athreshold value
pred=ifelse(pred>0.5,1,0)
pred
 
cbind=cbind(test$Outcome,pred)
cbind
###check accuracy
cm=table(test$Outcome,pred)
cm

accuracy=(115+36)/(115+10+31+36)
accuracy

##threshold=60%

pred=ifelse(pred>0.6,1,0)
pred
cm=table(test$Outcome,pred)
cm

###else if need to remove non significant variable ,but still in logistic it doesnot necessary
#log_reg=glm(Outcome~.-SkinThickness-Insulin-Age,data=training)
#log_reg
#summary(log_reg)
#pred=predict(log_reg,newdata=test)
#pred=ifelse(pred>=0.6,1,0)
#pred
#table(test$Outcome,pred)


#accuracy=(119+24)/(119+43+6+24)
#accuracy
names(data)
#pred=write.csv(pred, "fhjvdgf.csv")
##pred

##calculating confusion matrix
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(cm)







