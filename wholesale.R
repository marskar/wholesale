dataset <-read.csv('Wholesale customers data.csv')
# Encoding the target feature as factor
dataset$Channel = factor(dataset$Channel, levels = c(0, 1))
dataset[is.na(dataset)] <- 0
dataset$Region = factor(dataset$Region)
class(dataset$Region)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Channel, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set[,3:8] = scale(training_set[,3:8])
test_set[,3:8] = scale(test_set[,3:8])

library(ggplot2)
#p <- ggplot(dataset,aes(x=Fresh,y=Milk))
#p + geom_point(aes(colour=Region,alpha=0.5))+
  #scale_size_continuous(range=c(2,5))+
  #facet_wrap(~Channel,ncol=1)+
  #labs(title='wo cao ni mama',x='hehe',y='yamiedai')#用labs改title等
#install.packages("GGally")
library("GGally")
ggcorr(dataset, palette = "RdYlGn", name = "rho", 
       label = TRUE, label_color = "black")
ggpairs(dataset,  title = "")
  
#q <- ggplot(dataset, aes(x=Grocery,y=Milk,colour=Channel),
            #xlim <- c(0,3000),ylim <- c(0,3000))
#q + geom_point(alpha=0.3)

##### scattered plot ###########
# grocery & milk
qplot(Milk, Grocery, data = dataset, colour= Channel,geom = 'point', 
      alpha = 0.3,
      xlim = c(0, 30000), 
      ylim = c(0, 30000), 
      main='Grocery & Milk'  # 主标题: 字符串
)
# Grocery & Detergents_Paper
qplot(Detergents_Paper, Grocery, data = dataset, colour= Channel,geom = 'point',
      alpha = 0.3,
      xlim = c(0, 20000), 
      ylim = c(0, 30000), 
      main=' Grocery & Detergents_Paper'  # 主标题: 字符串
) 

# Milk & Detergents_Paper
qplot(Detergents_Paper, Milk, data = dataset, colour= Channel,geom = 'point', 
      alpha = 0.3,
      xlim = c(0, 20000), 
      ylim = c(0, 30000), 
      main=' Milk & Detergents_Paper'  # 主标题: 字符串
)
################# box plot and violin plot ##############
# grocery & Channel
q<- ggplot(dataset,aes(x=Channel,y=Grocery))
q+geom_boxplot(aes(fill=Channel))
##violin
q+geom_violin(aes(fill=Channel),alpha=0.3,width=0.8)+
  geom_jitter(shape=2)
# Detergents_Paper & Channel
q<- ggplot(dataset,aes(x=Channel,y=Detergents_Paper))
q+geom_boxplot(aes(fill=Channel))
##violin
q+geom_violin(aes(fill=Channel),alpha=0.3,width=0.8)+
  geom_jitter(shape=1)
# Milk & Channel
q<- ggplot(dataset,aes(x=Channel,y=Milk))
q+geom_boxplot(aes(fill=Channel))
##violin
q+geom_violin(aes(fill=Channel),alpha=0.3,width=0.8)+
  geom_jitter(shape=3)
########################################################################
################ knn ###########################################
#install.packages("pROC")
library(pROC)
library(class)
cl = training_set[,1, drop = TRUE]
y_pred = knn(training_set,
             test_set,
             cl,
             k = 5,
             prob = TRUE)

cm = table(test_set[,1], y_pred)
cm
(35+72)/(35+72+3)
#### ROC - knn 有问题
#install.packages("prediction")
library(prediction)
prob <- attr(y_pred, "prob")
prob <- 2*ifelse(y_pred == "-1", 1-prob, prob) - 1
pred_knn <- prediction(prob, test_set)
pred_knn <- performance(pred_knn, "tpr", "fpr")
plot(pred_knn, avg= "threshold", colorize=T, lwd=3, main="Voilà, a ROC curve!")


################ logistic regression #################
# Fitting Logistic Regression to the Training set
classifier = glm(Channel ~ .,
                 family = binomial,
                 data = training_set)
summary(classifier)
test_set$log_odd<-predict(classifier,newdata=test_set)                         
test_set$logit_pred_prob<-predict(classifier,newdata=test_set,type="response") 
test_set$logit_pred_class<-ifelse(test_set$logit_pred_prob>0.5,1,0)
mean(test_set$Channel==test_set$logit_pred_class)
table(test_set$Channel==test_set$logit_pred_class)
table(test_set$logit_pred_class,test_set$Channel, dnn=c("predicted","actual"))  # confusion table on test data

#### ROC lr #######
ct_roc<-roc(test_set$Channel,test_set$logit_pred_prob,auc=TRUE)
plot(ct_roc,print.auc=TRUE,col="blue")
roc<-roc(test_set$Channel,test_set$logit_pred_prob,auc=TRUE)
plot(roc,print.auc=TRUE,print.auc.y=.4, col="red",main = "ROC")


#################### tree ##################
library(rpart)
library(rpart.plot)
classifier<-rpart(Channel ~ .,
                      data=training_set,                     
                      method="class",                   
                      control=rpart.control(cp=0.02))

rpart.plot(classifier)
test_set$dt_pred_prob<-predict(classifier,test_set,type = "prob")[,2]
test_set$dt_pred_class<-predict(classifier,test_set,type="class")
mean(test_set$Channel==test_set$dt_pred_class)
table(test_set$Channel==test_set$dt_pred_class)
table(test_set$dt_pred_class,test_set$Channel, dnn=c("predicted","actual"))  # confusion table on test data
#### roc -rf
dt_roc<-roc(test_set$Channel,test_set$dt_pred_prob,auc=TRUE)
plot(dt_roc,print.auc=TRUE,col="blue")
roc<-roc(test_set$Channel,test_set$dt_pred_prob,auc=TRUE)
plot(roc,print.auc=TRUE,print.auc.y=.4, col="red",add=TRUE)


################ svm ##################
#install.packages("pROC")
library(pROC)
library(e1071)
classifier <- svm (Channel ~  .,
                   training_set,
                   probability=TRUE)
test_set$svm_pred_prob<-predict(classifier,newdata= test_set,probability=TRUE)[,2]
test_set$svm_pred_class<-predict(classifier,newdata = test_set,type="class")
mean(test_set$Channel==test_set$svm_pred_class)
table(test_set$Channel==test_set$svm_pred_class)
table(test_set$svm_pred_class,test_set$Channel, dnn=c("predicted","actual"))  # confusion table on test data
#### roc -rf
svm_roc<-roc(test_set$Channel,test_set$svm_pred_prob,auc=TRUE)
plot(svm_roc,print.auc=TRUE,col="blue")
roc<-roc(test_set$Channel,test_set$svm_pred_prob,auc=TRUE)
plot(roc,print.auc=TRUE,print.auc.y=.4, col="red",add=TRUE)

################ random forest ##################
library(randomForest)
classifier = randomForest(Channel ~ .,
                          data = training_set,
                          ntree = 100)

test_set$rf_pred_prob<-predict(classifier,test_set,type = "prob")[,2]
test_set$rf_pred_class<-predict(classifier,test_set,type="class")
mean(test_set$Channel==test_set$rf_pred_class)
table(test_set$Channel==test_set$rf_pred_class)
table(test_set$rf_pred_class,test_set$Channel, dnn=c("predicted","actual"))  # confusion table on test data
#### roc -rf
rf_roc<-roc(test_set$Channel,test_set$rf_pred_prob,auc=TRUE)
plot(rf_roc,print.auc=TRUE,col="blue")
roc<-roc(test_set$Channel,test_set$rf_pred_prob,auc=TRUE)
plot(roc,print.auc=TRUE,print.auc.y=.4, col="red",add=TRUE)

################ naive ##################
#install.packages("pROC")
library(pROC)
classifier = naiveBayes(Channel ~ .,
                        data = training_set)
test_set$nb_pred_prob<-predict(classifier,test_set,type = "raw")[,2]
test_set$nb_pred_class<-predict(classifier,newdata = test_set,type="class")
mean(test_set$Channel==test_set$nb_pred_class)
table(test_set$Channel==test_set$nb_pred_class)
table(test_set$nb_pred_class,test_set$Channel, dnn=c("predicted","actual"))  # confusion table on test data

#### roc -rf
nb_roc<-roc(test_set$Channel,test_set$nb_pred_prob,auc=TRUE)
plot(nb_roc,print.auc=TRUE,col="blue")
roc<-roc(test_set$Channel,test_set$nb_pred_prob,auc=TRUE)
plot(roc,print.auc=TRUE,print.auc.y=.4, col="red",add=TRUE)

#################################################################################
######################  k-FOLD cv ###############################################
#Randomly shuffle the data
training_set<-training_set[sample(nrow(training_set)),]
#Create 3 equally size folds
folds <- cut(seq(1,nrow(training_set)),breaks=3,labels=FALSE)
#Perform 3 fold cross validation
for(i in 1:3){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- training_set[testIndexes, ]
  trainData <- training_set[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}
############## k-fold cv knn ####################
cl = trainData[,1, drop = TRUE]
y_pred = knn(trainData,
             testData,
             cl,
             k = 5,
             prob = TRUE)

cm = table(testData[,1], y_pred)
cm
(30+76)/(30+76+4)
################ k-FOLD logistic regression #################
classifier = glm(Channel ~ .,
                 family = binomial,
                 data = trainData)
summary(classifier)
testData$ct_pred_class<-predict(classifier,newdata = testData,type="class")
testData$ct_pred_prob<-predict(classifier,newdata = testData)
mean(testData$Channel==testData$ct_pred_class)
table(testData$Channel==testData$ct_pred_class)
table(testData$ct_pred_class,testData$Channel, dnn=c("predicted","actual"))  # confusion table on test data

#################### k-FOLD  tree ##################
library(rpart)
library(rpart.plot)
classifier<-rpart(Channel ~ .,
                  data=trainData,                     
                  method="class",                   
                  control=rpart.control(cp=0.02))

rpart.plot(classifier)
testData$ct_pred_class<-predict(classifier,newdata = testData,type="class")
testData$ct_pred_prob<-predict(classifier,newdata = testData)
mean(testData$Channel==testData$ct_pred_class)
table(testData$Channel==testData$ct_pred_class)
table(testData$ct_pred_class,testData$Channel, dnn=c("predicted","actual"))  # confusion table on test data

################ k-FOLD svm ##################
library(e1071)
classifier = svm(formula = Channel ~ .,
                 data = trainData,
                 type = 'C-classification',
                 kernel = 'linear')

testData$ct_pred_class<-predict(classifier,newdata = testData,type="class")
testData$ct_pred_prob<-predict(classifier,newdata = testData)
mean(testData$Channel==testData$ct_pred_class)
table(testData$Channel==testData$ct_pred_class)
table(testData$ct_pred_class,testData$Channel, dnn=c("predicted","actual"))  # confusion table on test data


################ k-FOLD random forest ##################
library(randomForest)
classifier = randomForest(Channel ~ .,
                          data = trainData,
                          ntree = 50)

testData$ct_pred_class<-predict(classifier,newdata = testData,type="class")
testData$ct_pred_prob<-predict(classifier,newdata = testData)
mean(testData$Channel==testData$ct_pred_class)
table(testData$Channel==testData$ct_pred_class)
table(testData$ct_pred_class,testData$Channel, dnn=c("predicted","actual"))  # confusion table on test data


##### k-FOLD cv naiveBayes #######
classifier = naiveBayes(Channel ~ .,
                        data = trainData)
testData$ct_pred_class<-predict(classifier,newdata = testData,type="class")
testData$ct_pred_prob<-predict(classifier,newdata = testData)
mean(testData$Channel==testData$ct_pred_class)
table(testData$Channel==testData$ct_pred_class)
table(testData$ct_pred_class,testData$Channel, dnn=c("predicted","actual"))  # confusion table on test data
