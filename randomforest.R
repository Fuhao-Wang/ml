#### 5.1 机器学习概述 ####
rm(list=ls())
getwd()
library(caret)
dat0=read.csv('D:/R语言从数据思维到数据实战/第五章 R语言与机器学习/5.1 机器学习概述/相亲数据.csv',fileEncoding = "UTF-8")
head(dat0)
nrow(dat0)
dat =na.omit(dat0)
nrow(dat)
dat$决定=factor(dat$决定,levels=c(0,1),labels=c("拒绝","接受"))
dat$性别=factor(dat$性别,levels=c(0,1),labels=c("女","男"))
dat$种族=factor(dat$种族,levels=c(1,2,3,4,5,6),labels=c("非洲裔","欧洲裔","拉丁裔","亚裔","印第安土著","其他"))
dat$从事领域=factor(dat$从事领域,levels=1:18,labels=c("法律","数学","社会科学或心理学","医学或药物学或生物技术","工程学","写作或新闻","历史或宗教或哲学","商业或经济或金融","教育或学术","生物科学或化学或物理","社会工作","大学在读或未择方向","政治学或国际事务","电影","艺术管理","语言","建筑学","其他"))
dat$对方决定=factor(dat$对方决定,levels=c(0,1),labels=c("拒绝","接收"))
dat$对方种族=factor(dat$对方种族,levels=c(1,2,3,4,5,6),labels=c("非洲裔","欧洲裔","拉丁裔","亚裔","印第安土著","其他"))
dat$是否同一种族=factor(dat$是否同一种族,levels=c(0,1),labels=c("非同一种族","同一种族"))
set.seed(1234)
trainIndex=createDataPartition(dat$决定,p=0.8,
                               list=FALSE,
                               times=1)

datTrain=dat[trainIndex, ]
datTest=dat[-trainIndex, ]
table(dat$决定)/nrow(dat)
table(datTrain$决定)/nrow(datTrain)
table(datTest$决定)/nrow(datTest)


preProcValues=preProcess(datTrain,method=c("center","scale"))
trainTransformed=predict(preProcValues, datTrain)
testTransformed=predict(preProcValues,datTest)

### 2.变量选择 ###

subsets=c(2,5,10,15,20)
ctrl=rfeControl(functions=rfFuncs,method="cv")
x=trainTransformed[,-which(colnames(trainTransformed)%in%"决定")]
y=trainTransformed[,"决定"]
Profile=rfe(x,y,sizes=subsets,rfeControl = ctrl)
Profile$optVariables


dat.train=trainTransformed[,c(Profile$optVariables,"决定")]
dat.test=testTransformed[,c(Profile$optVariables,"决定")]

##随机森林##
set.seed(1234)
gbmFit1=train(决定~.,data=dat.train,method="rf")
importance=varImp(gbmFit1,scale=FALSE)
plot(importance,xlab = "重要性")


data.predict=predict(gbmFit1,newdata=dat.test)
confusionMatrix(data.predict,dat.test$决定)
