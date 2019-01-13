rm(list=ls())
setwd("E:/data scientist/project edwiser/project 3")
getwd()

library(readxl)
library(ggplot2)
library(corrgram)
library(DMwR)
library(rpart)
library(randomForest)
library(RRF)
library(inTrees)
library(sp)
library(raster)
library(usdm)



# Data Set Load

fulldata=read_xls("dataset.xls")
View(fulldata)
class(fulldata)
fulldata=as.data.frame(fulldata)

str(fulldata)

names(fulldata)[2]="reason_for_absence"
names(fulldata)[3]="month_of_absence"
names(fulldata)[4]="day_of_the_week"
names(fulldata)[6]="transportation_expense"
names(fulldata)[7]="distance_from_residence_to_work"
names(fulldata)[8]="service_time"
names(fulldata)[10]="work_load_average_day"
names(fulldata)[11]="hit_target"
names(fulldata)[12]="disciplinary_failure"
names(fulldata)[15]="social_drinker"
names(fulldata)[16]="social_smoker"
names(fulldata)[20]="body_mass_index"
names(fulldata)[21]="absenteeism_time_in_hours"



hist(fulldata$reason_for_absence)
hist(fulldata$month_of_absence)
hist(fulldata$day_of_the_week)
hist(fulldata$Seasons)
hist(fulldata$transportation_expense)
hist(fulldata$absenteeism_time_in_hours,fulldata$Son)

# absentee of social smokers
ggplot(fulldata,aes_string(x=fulldata$social_smoker))+geom_bar(stat = "count")+xlab("social smoker")+ylab("total absentees hour")+ggtitle("absentee hours of smokers")
# absentee of ID
ggplot(fulldata,aes_string(x=fulldata$ID))+geom_bar(stat = "count")+xlab("emp. ID")+ylab("total absentees hour")+ggtitle("absentee hours By ID")
# absentee of education
ggplot(fulldata,aes_string(x=fulldata$Education))+geom_bar(stat = "count")+xlab("Education")+ylab("total absentees hour")+ggtitle("absentee hours By Education")
# absentee by reason of absent
ggplot(fulldata,aes_string(x=fulldata$reason_for_absence))+geom_bar(stat = "count")+xlab("reason of absence")+ylab("total absentees hour")+ggtitle("absentee hours By Reason")
# absentee by no of sons
ggplot(fulldata,aes_string(x=fulldata$Son))+geom_bar(stat = "count")+xlab("No of sons")+ylab("total absentees hour")+ggtitle("absentee hours By no of sons")
# absentee by disciplinary_failure
ggplot(fulldata,aes_string(x=fulldata$disciplinary_failure))+geom_bar(stat = "count")+xlab("Disciplinary failure")+ylab("total absentees hour")+ggtitle("absentee hours By Disciplinary failure")
# absentee by service time
ggplot(fulldata,aes_string(x=fulldata$social_drinker))+geom_bar(stat = "count")+xlab("social drinker")+ylab("total absentees hour")+ggtitle("absentee hours by social drinker")


ggplot(fulldata,aes_string(x=fulldata$transportation_expense))+geom_bar(stat = "count")+xlab("social drinker")+ylab("total absentees hour")+ggtitle("absentee hours by social drinker")


#convert to proper type

fulldata$reason_for_absence=as.factor(fulldata$reason_for_absence)
fulldata$month_of_absence=as.factor(fulldata$month_of_absence)
fulldata$day_of_the_week=as.factor(fulldata$day_of_the_week)
fulldata$Seasons=as.factor(fulldata$Seasons)

fulldata$disciplinary_failure=as.factor(fulldata$disciplinary_failure)
fulldata$Education=as.factor(fulldata$Education)
fulldata$social_drinker=as.factor(fulldata$social_drinker)
fulldata$social_smoker=as.factor(fulldata$social_smoker)


#missing value

sum(is.na(fulldata)) #135
missing_values=data.frame(apply(fulldata, 2,function(x){sum(is.na(x))}))

View(missing_values)
missing_values$columns=row.names(missing_values)
row.names(missing_values)=NULL
names(missing_values)[1]="total missing"
missing_values$missing_percentage=(missing_values$`total missing`/nrow(fulldata))*100
missing_values=missing_values[order(-missing_values$missing_percentage),]
missing_values=missing_values[,c(2,1,3)]
write.csv(missing_values,"missing_data.csv",row.names = F)

#it is clear that the missing percentage is below 30% so its better to impute it with best imputation method
#lets find which imputation method is best to imputation
#for that lets first take any obsevatation and make it NA

#actual value= 27
#mean= 26.7 
#median= 25
#knn= 27 

#impute with mean
fulldata$`Body mass index`[is.na(fulldata$`Body mass index`)]=mean(fulldata$`Body mass index`,na.rm = T)
#impute with median
fulldata$`Body mass index`[is.na(fulldata$`Body mass index`)]=median(fulldata$`Body mass index`,na.rm = T)
#impute with KNN
fulldata=knnImputation(fulldata,k=5)
sum(is.na(fulldata))


numeric_var=sapply(fulldata, is.numeric) #storing numeric varables
numeric_data=fulldata[,numeric_var] #storing numeric varables data
numric_cnames=colnames(numeric_data) #storing column names of numeric data


#checking outliers with boxplot of numeric varables And replacing with max value

boxplot(fulldata$absenteeism_time_in_hours)
boxplot(fulldata$transportation_expense)
boxplot(fulldata$service_time)
boxplot(fulldata$work_load_average_day)
boxplot(fulldata$Age)
boxplot(fulldata$hit_target)

#replace all outlier with max value and min value
for (i in numric_cnames) {
  print(i)  
  maxi=quantile(fulldata[,i],c(.75))+(1.5*IQR(fulldata[,i]))
  mini=quantile(fulldata[,i],c(.75))-(1.5*IQR(fulldata[,i]))
  
    #val=fulldata[,i][fulldata[,i] %in% boxplot.stats(fulldata[,i])$out]    
     
      #fulldata[,i][fulldata[,i] %in% val]=last
      fulldata[,i][fulldata[,i] > maxi]=maxi
      fulldata[,i][fulldata[,i] <mini]=mini
      
}
ff=fulldata[,sapply(fulldata, is.factor)]
ff$absentism=fulldata[,21]

avv=aov(absentism ~.,data=ff)
summary(avv)
class(ff)


cc=cor(fulldata[,sapply(fulldata, is.numeric)])

#Feature Selection
# finding correlation between numerical varables
corrgram(fulldata[sapply(fulldata, is.numeric)])
cor(fulldata$Weight,fulldata$body_mass_index)  #collelation of weight and body mass index is .90 highly correlated
cor(fulldata$service_time,fulldata$Age)  # .68

var(fulldata$distance_from_residence_to_work,fulldata$absenteeism_time_in_hours)

#for categorial varable

cvar=sapply(fulldata, is.factor)
cdata=fulldata[,cvar]
cdata
for (u in 1:8) {
  print(names(cdata[u]))
  print(chisq.test(table(cdata[,u])))
}

fulldata=subset(fulldata,select= -c(Weight,day_of_the_week)) # pvalue of varable  day of week = .2397 and seasons= .5635

View(fulldata)

cna=colnames(fulldata[,sapply(fulldata, is.numeric)])

for (i in cna) {
  fulldata[,i]= round(fulldata[,i])
  print(i)
}
View(round(fulldata))

fulldata[,19][fulldata[,19] == 0]=1

# Sampling techniques... divide data in train and test
data=sample(1:nrow(fulldata),0.8*nrow(fulldata))
train=fulldata[data,]
test=fulldata[-data,]


# model Building
#         ***************Decision tree*****************

dmodel=rpart(absenteeism_time_in_hours ~ . ,data = train, method = "anova")
summary(dmodel)
plot(dmodel,margin = 0.1)
text(dmodel,pretty=0)

pred=predict(dmodel,test[,-19])
pred
pred=round(pred)
pred
mape= function(y,yhat){
  mean(abs((y-yhat))/y) * 100
}
mape(test[,19],pred)

regr.eval(test[,19],pred,stats = c("mae","mse","rmse","mape"))

# mape =  85.89 
# mae  =  2.38
# mse  =  12.004
# rmse =  3.46

#   ***************Random Forest*****************

ran=randomForest(absenteeism_time_in_hours ~. ,data = train,ntree=1000)
ran
importance(ran)

treelist=RF2List(ran)
exec=extractRules(treelist,train[,-18])
exec[1:2,]
readable=presentRules(exec,colnames(train))
readable[1:2,]
targ=getRuleMetric(exec,train[,-18],train$absenteeism_time_in_hours)
targ[1:2,]

rr=presentRules(targ,colnames(train))
rr[1:2,]
pred=predict(ran,test[,-16])
pred=round(pred)
mape(test[,16],pred)

regr.eval(test[,16],pred,stats = c("mae","mse","rmse","mape"))

# mape =  67.30 
# mae  =  2.47
# mse  =  15.39
# rmse =  3.92


#   ***************Linear Regression*****************
vif(fulldata[,sapply(fulldata[,-16], is.numeric)])
vifcor(fulldata[,sapply(fulldata[,-16], is.numeric)])

lm_model=lm(absenteeism_time_in_hours ~.,data = train)
summary(lm_model)

pr=predict(lm_model,test[,-16])

mape(test[,16],pr)


regr.eval(test[,16],pr,stats = c("mae","mse","rmse","mape"))

# mape =  83.05 
# mae  =  2.71
# mse  =  17.67
# rmse =  4.20