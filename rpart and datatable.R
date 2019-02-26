library(data.table)
library(rpart)
library(randomForest)
install.packages("MLmetrics")
library(MLmetrics)
library(tree)

train <-fread("C:/Users/akabo/OneDrive/Desktop/title.csv",stringsAsFactors=TRUE)

TrainData1 <-fread("C:/Users/akabo/OneDrive/Desktop/GaryTrain/Train-006.csv")
TrainData2 <-fread("C:/Users/akabo/OneDrive/Desktop/GaryTrain/Train-007.csv")
#TrainData3 <-fread("C:/Users/akabo/OneDrive/Desktop/GaryTrain/Train-008.csv")
#TrainData4 <-fread("C:/Users/akabo/OneDrive/Desktop/GaryTrain/Train-009.csv")
TrainData <- rbind(TrainData1,TrainData2)
colnames(TrainData) <- colnames(train)
TrainData <-transform(TrainData, Date = substr(hour, 5, 6), hour = substr(hour, 7, 8))
ValData <-fread("C:/Users/akabo/OneDrive/Desktop/GaryTrain/Train-010.csv")

colnames(ValData) <- colnames(train)
TrainData[,15:24] <- lapply(TrainData[,15:24], as.character)
TrainData[,2:5]<-lapply(TrainData[,2:5], as.character)
sapply(TrainData,class)
cat=list("site_id","site_domain","app_id","app_domain","device_model","C14","C17","C19","C20","C21")
for (i in 1:length(cat)){
  TrainData <-TrainData[,feq_site_id:=.N/2000000,by=eval(cat[[i]])]
  TrainData[feq_site_id<0.002,cat[[i]]]="other"
}
lapply(TrainData,unique)
model1 <- glm(click~factor(C17),family=binomial(link = "logit"),data=TrainData)
A<-as.list(unique(TrainData$C17))
A[abs(model1$coefficients)<0.3]<-"other"
TrainData$C17 <-TrainData$C17[TrainData$C17%in%A]
model2 <- glm(click~factor(C14),family=binomial(link = "logit"),data=TrainData)
B<-as.list(unique(TrainData$C14))
B[abs(model2$coefficients)<0.35]<-"other"
TrainData$C14 <-TrainData$C14[TrainData$C14%in%B]
lapply(TrainData, unique)
TrainData[,15:24] <- lapply(TrainData[,15:24], as.factor)
TrainData[,2:14] <- lapply(TrainData[,2:14], as.factor)
#TrainData[,15:24] <- lapply(15:24, function(x) as.factor(TrainData[[x]]))


ValData[,15:24] <- lapply(ValData[,15:24], as.character)
cat=list("site_id","site_domain","app_id","app_domain","device_model","C14","C17","C19","C20","C21")
for (i in 1:length(cat)){
  ValData <-ValData[,feq_site_id:=.N/1000000,by=eval(cat[[i]])]
  ValData[feq_site_id<0.008,cat[[i]]]="other"
}
ValData[,15:24] <- lapply(ValData[,15:24], as.factor)
ValData[,2:14] <- lapply(ValData[,2:14], as.factor)
lapply(ValData,unique)

# RPerm <- sample(nrow(train),10000)
# train <- train[RPerm,]
# 
# TrainInd <- ceiling(nrow(train)*0.6)
# ValInd <- ceiling(nrow(train)*0.4)+TrainInd
# 
# TrainData <- train[1:TrainInd,]
# ValData <- train[(TrainInd+1):ValInd,]
# TrainData$click <-factor(TrainData$click)
# ValData$click <-factor(ValData$click)
# TrainData$site_id<-factor(TrainData$site_id)
# TrainData$device_id<-factor(TrainData$device_id)
# TrainData$device_type <- factor(TrainData$device_type)
# TrainData$device_conn_type <-factor(TrainData$device_conn_type)
# TrainData$C1 <- factor(TrainData$C1)
# TrainData$C14 <- factor(TrainData$C14)
# TrainData$C15 <- factor(TrainData$C15)
# TrainData$C16 <- factor(TrainData$C16)
# TrainData$C17 <- factor(TrainData$C17)
# TrainData$C18 <- factor(TrainData$C18)
# TrainData$C19 <- factor(TrainData$C19)
# TrainData$C20 <- factor(TrainData$C20)
# TrainData$C21 <- factor(TrainData$C21)
# TrainData$banner_pos <- factor(TrainData$banner_pos)
# ValData$C1 <- factor(ValData$C1)
# ValData$device_type <- factor(ValData$device_type)
# ValData$device_conn_type <-factor(ValData$device_conn_type)
# ValData$banner_pos <-factor(ValData$banner_pos)
# ValData$C14 <- factor(ValData$C14)
# ValData$C15 <- factor(ValData$C15)
# ValData$C16 <- factor(ValData$C16)
# ValData$C17 <- factor(ValData$C17)
# ValData$C18 <- factor(ValData$C18)
# ValData$C19 <- factor(ValData$C19)
# ValData$C20 <- factor(ValData$C20)
# ValData$C21 <- factor(ValData$C21)

Vars <-names(TrainData)

BigFm <- paste("click","~",paste(Vars[c(4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,24)],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)


#drop column device_model,device_ip

Traindf<- TrainData[,c(2,4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,24)]
rpc <- rpart.control(minsplit=1,minbucket=1,cp=0,usesurrogate=0,xval=10)
out <- rpart(BigFm,data=TrainData,control=rpc,method="class")

# Find the cp parameter for the best model and prune the tree.
bestcp <- out$cptable[which.min(out$cptable[,"xerror"]),"CP"]

# Store the "best" model in out1
out1 <- prune(out,cp=bestcp)
Valdf <- ValData[,c(2,4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,24)]
Valdf <- Valdf[(site_category!="110ab22d"&site_category!="74073276"&site_category!="c706e647"&site_category!="a72a0145"&site_category!="site_category"),]
Valdf <- Valdf[(app_category!="52de74cf"&app_category!="bd41f328"&app_category!="0d82db25"),]
#Valdf[((Valdf$C14=="15702")&(Valdf$C14=="15704")&(Valdf$C14=="15707")&(Valdf$C14=="19772")&(Valdf$C14=="21191")&(Valdf$C14=="21767")&(Valdf$C14=="8330")),12]<-"other"
Valdf[(Valdf$C14=="15704"),12]<-"other"
Valdf[(Valdf$C14=="15702"),12]<-"other"
Valdf[(Valdf$C14=="15707"),12]<-"other"
Valdf[(Valdf$C14=="19772"),12]<-"other"
Valdf[(Valdf$C14=="21191"),12]<-"other"
Valdf[(Valdf$C14=="21767"),12]<-"other"
Valdf[(Valdf$C14=="8330"),12]<-"other"
Valdf[(Valdf$C17=="1872"),15]<-"other"
Valdf[(Valdf$C17=="1993"),15]<-"other"
Valdf[(Valdf$C17=="1994"),15]<-"other"
Valdf[(Valdf$C17=="2201"),15]<-"other"
Valdf[(Valdf$C17=="2502"),15]<-"other"
Valdf[(Valdf$C17=="2545"),15]<-"other"
Valdf[(Valdf$C17=="423"),15]<-"other"
y <- predict(out1,newdata = Valdf,type="prob")
y <- y[,2]

LogLoss(as.numeric(y),as.numeric(Valdf$click))

out2 <- randomForest(BigFm,data=Traindf,
                     ntree=5000,maxnodes=50)

Valdf1$C21<-factor(Valdf1$C21, levels=levels(Traindf$C21))
for(i in 1:ncol(Valdf)){
  Valdf[[i]]<-factor(Valdf[[i]], levels=levels(Traindf[[i]]))
}
Valdf1<-Valdf[,-1]
y1 <- predict(out2,newdata=Valdf1,type="prob")
y1 <- y1[,2]

LogLoss(as.numeric(y1),as.numeric(Valdf$click))
