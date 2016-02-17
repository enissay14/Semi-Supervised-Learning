library(data.table)
library(MASS)
library(upclass)
library(DMwR)
library(e1071)
library(spa)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#load subdata
data <- fread("/home/yassine/EMSE 2015-2016/Projet Industruel/Homesite Quote Conversion/data/subtrainCleaned20")
data <- data[,"V1":= NULL, with = FALSE]

##################################
#                                #
#       Data Preparation         #
#                                #
##################################
#mclust won't work with categorical data
data <- data[,names(data)[which(sapply(data, is.numeric))], with = FALSE]

#Checking for constant columns & removing them from the training 
col_ct <- sapply(data, function(x) length(unique(x)))
colCnt.df <- data.frame(colName = names(data), colCount = col_ct)
cat("\nNumber of columns with constant values: ",sum(col_ct==1))
cat("\nName of constant columns: ", names(data)[which(col_ct==1)])

cat("\n\nRemoving the constant fields from data")
data <- data[, names(data)[which(col_ct == 1)] := NULL, with = FALSE]
cat("\ndata dimensions: ", dim(data))

#upclass wont work with more than 100 colomns
#choosing 100 variables from 253 left
names(data)
p <- ncol(data) 
cat("\n4 Fields: ",(4/(p-2))*100,"%
    \n14 CoverageField: ",(14/(p-2))*100,"%
    \n16 SalesField: ",(16/(p-2))*100,"%
    \n68 Personalfield: ",(68/(p-2))*100,"%
    \n28 PropertyField: ",(28/(p-2))*100,"%
    \n121 GeographicField: ",(121/(p-2))*100,"%")

set.seed(1)
id1<-sample(3:6,round((4/p)*100, digits = 0))
id2<-sample(7:20,round((14/p)*100, digits = 0))
id3<-sample(21:36,round((16/p)*100, digits = 0))
id4<-sample(37:113,round((77/p)*100, digits = 0))
id5<-sample(114:145,round((32/p)*100, digits = 0))
id6<-sample(146:264,round((119/p)*100, digits = 0))
data <- subset(data,select = c(1,2,id1,id2,id3,id4,id5,id6))

##################################
#                                #
#       Experiment Design        #
#                                #
##################################
N <- nrow(data)
ls <- data
ls.sub <- ls[1:(nrow(ls)/3)]
label <- unclass(data$QuoteConversion_Flag)

n <- seq(0.1,0.5,0.1)       #number of labeled documents
step <- 40      #number of repeated experiments

s.up <- matrix(NA, nrow = 5, ncol = step )
ss.up <- matrix(NA, nrow = 5, ncol = step )
s.st <- matrix(NA, nrow = 5, ncol = step )
ss.st <- matrix(NA, nrow = 5, ncol = step )
s.grph <- matrix(NA, nrow = 5, ncol = step )
ss.grph <- matrix(NA, nrow = 5, ncol = step )
s.svm <- matrix(NA, nrow = 5, ncol = step )
ss.svm <- matrix(NA, nrow = 5, ncol = step )

ptm <- proc.time()
for(j in 1:length(n)){
     for(i in 1:step){
       
       tmp <- proc.time()
           set.seed(i)
           indtrain <- sort(sample(1:nrow(ls),nrow(ls)*n[j] ))
           X <- subset(ls[indtrain,], select = -c(1,2))
           y <- label[indtrain] + 1
           
           indtest <- setdiff(1:nrow(ls), indtrain)
           Xtest <- subset(ls[indtest,], select = -c(1,2))
           ytest <- label[indtest] + 1
       
       ##################################
       #                                #
       #       Generative Models        #
       #                                #
       ##################################
#                Xmat <- as.matrix(X)
#                Xtestmat <- as.matrix(Xtest)
#                
#                #supervised learning
#                supervised = Mclust(Xmat,y)
#                pred <- predict(supervised, Xtestmat)
#                predmat <- as.numeric(pred$classification)
#                predmat[which(is.na(predmat))] = 0
#                s.up[j,i] <- sum(abs( predmat - ytest))/length(ytest)
#                
#                #semi-supervised learning
#                semisupervised <- upclassify(Xmat, y, Xtestmat)
#                ss.up[j,i] <- sum(abs(semisupervised$Best$test$cl - ytest))/length(ytest)
#                
#                write.csv(s.up, "s-up", sep =",")
#                write.csv(ss.up, "ss-up", sep =",")
       
       ##########################
       #                        #
       #     Self Training      #
       #                        #
       ##########################
       
       #     X.selfTrain <- subset(ls[indtrain,], select = -c(1))
       #     X.selfTrain$QuoteConversion_Flag <- factor(X.selfTrain$QuoteConversion_Flag)
       #     y <- label[indtrain] + 1
       #     
       #     Xtest.selfTrain <- subset(ls[indtest,], select = -c(1))
       #     Xtest.selfTrain$QuoteConversion_Flag <- NA
       #     ytest <- label[indtest] + 1
       #     
       #     #supervised
       #     stdTree <- naiveBayes(QuoteConversion_Flag~ .,X.selfTrain)
       #     s.st[j,i] <- sum(abs(as.numeric(predict(stdTree,Xtest.selfTrain,type='class')) - ytest)/length(ytest))
       #     
       #     #semi-supervised
       #     f <- function(m,d) { 
       #       p <- predict(m,d,type='raw')
       #       data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max))
       #     }
       #     
       #     treeSelfT <- SelfTrain(QuoteConversion_Flag~ .,rbind(Xtest.selfTrain,X.selfTrain),learner('naiveBayes',list()),'f')
       #     ss.st[j,i] <- sum(abs(as.numeric(predict(treeSelfT,Xtest.selfTrain,type='class')) - (ytest))/length(ytest))
       #     
       #     write.csv(s.st, "s-st", sep =",")
       #     write.csv(ss.st, "ss-st", sep =",")
       #     
       #     ##########################
       #     #                        #
       #     #     Semi-parametric    #
       #     #     Graph-based        #
       #     #                        #
       #     ##########################
       #     
               set.seed(i)
               indtrain <- sort(sample(1:nrow(ls.sub),nrow(ls.sub)*n[j] ))
               X <- subset(ls.sub[indtrain,], select = -c(1,2))
               y <- label[indtrain] + 1
               
               indtest <- setdiff(1:nrow(ls.sub), indtrain)
               Xtest <- subset(ls.sub[indtest,], select = -c(1,2))
               ytest <- label[indtest] + 1
               
               Dat <- rbind(X,Xtest)
               L=seq(1,nrow(X),1)
               U=seq(1,nrow(Xtest),1)
               Dij <- as.matrix(daisy(Dat))
               
               #supervised
               gsup <- spa(y,graph=Dij[L,L],control=spa.control(gcv="aGCV"))
               
               pred <- predict(gsup, gnew = Dij[U,L])
               pred <- range01(pred)
               tab <- table(pred > 0.5 , ytest)
               s.grph[j,i] <- 1 - sum(diag(tab)) / sum(tab)
               
               #semi-supervised
               gsemi<-spa(c(y,rep(NA,nrow(Xtest))),graph=Dij,control=spa.control(gcv="aGCV"))
               
               fitted <- range01(fitted(gsemi)[U])
               tab <- table(fitted > 0.5, ytest)
               ss.grph[j,i] <- 1 - sum(diag(tab)) / sum(tab)  
           
               write.csv(s.grph, "s-grph", sep =",")
               write.csv(ss.grph, "ss-grph", sep =",")
       
       ##########################
       #                        #
       #          TSVM          #
       #                        #
       ##########################
       #data preparation    
       #         cl <- label
       #         cl[cl == 0] <- -1
       #         
       #         set.seed(i)
       #         indtrain <- sort(sample(1:nrow(ls.sub),nrow(ls.sub)*n[j] ))
       #         X <- subset(ls.sub[indtrain,], select = -c(1,2))
       #         y <- cl[indtrain] 
       #         
       #         indtest <- setdiff(1:nrow(ls.sub), indtrain)
       #         Xtest <- subset(ls.sub[indtest,], select = -c(1,2))
       #         ytest <- cl[indtest]
       #         
       #         training <- rbind(X,Xtest)
       #         target <- c(y, rep(0,length(indtest)))
       #         
       #         training <- t(apply(training, 1, function(x) paste0(1:ncol(training),":",x)))
       #         training <- apply(training, 1, paste, collapse = " ")
       #         
       #         test <- t(apply(Xtest, 1, function(x) paste0(1:ncol(Xtest),":",x)))
       #         test <- apply(test, 1, paste, collapse = " ")
       #         
       #         write(training, file.path("/home/yassine/EMSE 2015-2016/Projet Industruel/Homesite Quote Conversion/data/svm/",n[j],"/",i,"/training"))
       #         write(paste(target), file.path("/home/yassine/EMSE 2015-2016/Projet Industruel/Homesite Quote Conversion/data/svm/",n[j],"/",i,"/target"))
       #         write(test,file.path("/home/yassine/EMSE 2015-2016/Projet Industruel/Homesite Quote Conversion/data/svm/",n[j],"/",i,"/test"))
       #         write(paste(ytest),file.path("/home/yassine/EMSE 2015-2016/Projet Industruel/Homesite Quote Conversion/data/svm/",n[j],"/",i,"/ytest"))
       #   
       #parameter estimtion
       
#        setwd('/home/yassine/EMSE 2015-2016/Projet Industruel/Homesite Quote Conversion/data/svm/')    
#        system(paste0("./svmlin ",n[j],"/",i,"/training ",n[j],"/",i,"/target "))
#        system(paste0("./svmlin -f  training.weights ",n[j],"/",i,"/test ",n[j],"/",i,"/ytest"))
#        s.svm[j,i] <- as.matrix(read.table("accuracy"))
#        
#        system(paste0("./svmlin -A 2 -W 0.001 -U 10 -R 0.1 ",n[j],"/",i,"/training ",n[j],"/",i,"/target "))
#        system(paste0("./svmlin -f  training.weights ",n[j],"/",i,"/test ",n[j],"/",i,"/ytest"))
#        ss.svm[j,i] <- as.matrix(read.table("accuracy"))
#        
#        setwd('/home/yassine/')
#        write.csv(s.svm, "s-svm", sep =",")
#        write.csv(ss.svm, "ss-svm", sep =",")
#        
       
       #Time log
       cat("\n pass:",i,"|"
           ,n[j]*100,"% known labels done in : "
           ,round((proc.time() - tmp)[3]/60,2),"min. Elapsed :"
           ,round((proc.time() - ptm)[3]/60,2),"min")
     } 
}

#analysing result
s.up <- read.csv("/home/yassine/s-up")
s.up <- subset(s.up, select = -1)
s.up <- as.matrix(s.up)

ss.up <- read.csv("/home/yassine/ss-up")
ss.up <- subset(ss.up, select = -1)
ss.up <- as.matrix(ss.up)

plot(1:5,apply(s.up,1,mean),,xaxt = "n",type="l",col="red",ylim=c(0.1,0.6)
     ,ylab="% of Missclassified",xlab="% of unlabled",
     main="Supervised Vs Semi-supervised",sub = "Generative Method")
axis(1, at=1:5, labels=c("10%","20%","30%","40%","50%"))
lines(apply(ss.up,1,mean),type="l",col="blue")
legend("topright",c("Semi-Supervised","Supervised"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red")) 
plot(s.up[5,],type="l")
mean(s.up[5,1:20])


s.st <- read.csv("/home/yassine/s-st")
s.st <- subset(s.st, select = -1)
s.st <- as.matrix(s.st)

ss.st <- read.csv("/home/yassine/ss-st")
ss.st <- subset(ss.st, select = -1)
ss.st <- as.matrix(ss.st)

plot(1:5,apply(s.st,1,mean),,xaxt = "n",type="l",col="red",ylim=c(0.1,0.6)
     ,ylab="% of Missclassified",xlab="% of unlabled",
     main="Supervised Vs Semi-supervised",sub = "Self-Train Method")
axis(1, at=1:5, labels=c("10%","20%","30%","40%","50%"))
lines(apply(ss.st,1,mean),type="l",col="blue")
legend("topright",c("Semi-Supervised","Supervised"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red")) 
plot(s.st[5,],type="l")
mean(s.st[5,1:20])

s.grph <- read.csv("/home/yassine/s-grph")
s.grph <- subset(s.grph, select = -1)
s.grph <- as.matrix(s.grph)

ss.grph <- read.csv("/home/yassine/ss-grph")
ss.grph <- subset(ss.grph, select = -1)
ss.grph <- as.matrix(ss.grph)

plot(1:5,apply(s.grph,1,mean),xaxt = "n",type="l",col="red",ylim=c(0.1,0.6),
     ylab="% of Missclassified",xlab="% of unlabled",
     main="Supervised Vs Semi-supervised",sub = "Graph-based Method")
axis(1, at=1:5, labels=c("10%","20%","30%","40%","50%"))
lines(apply(ss.grph,1,mean),type="l",col="blue")
legend("bottomright",c("Semi-Supervised","Supervised"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red")) 
plot(ss.grph[5,],type="l")
mean(ss.grph[5,1:20])


s.svm <- read.csv("/home/yassine/s-svm")
s.svm <- subset(s.svm, select = -1)
s.svm <- as.matrix(s.svm)

ss.svm <- read.csv("/home/yassine/ss-svm")
ss.svm <- subset(ss.svm, select = -1)
ss.svm <- as.matrix(ss.svm)


plot(1:5,apply(s.svm,1,mean),xaxt = "n",type="l",col="red",ylim=c(0.1,0.3),
     ylab="% of Missclassified",xlab="% of labled",
     main="Supervised Vs Semi-supervised",sub = "TSVM Method")
axis(1, at=1:5, labels=c("10%","20%","30%","40%","50%"))
lines(apply(ss.svm,1,mean),type="l",col="blue")
legend("topright",c("Semi-Supervised","Supervised"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red")) 
plot(s.svm[5,],type="l")
mean(s.svm[5,])
mean(ss.svm[5,])
